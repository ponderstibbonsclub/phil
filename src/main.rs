//! Phil Collins - the simple Discord bot for the Ponder Stibbons Club
//!
//! Modified from Songbird voice_events_queue example
use std::{
    collections::{HashSet, VecDeque},
    env,
    sync::Arc,
    time::Duration,
};

use anyhow::Context;
use serenity::{
    async_trait,
    builder::CreateMessage,
    client::{Client, Context as DiscordContext, EventHandler},
    framework::{
        standard::{
            help_commands,
            macros::{command, group, help},
            Args, CommandGroup, CommandResult, HelpOptions,
        },
        StandardFramework,
    },
    http::Http,
    model::{
        channel::Message,
        gateway::Ready,
        guild::Member,
        id::{ChannelId, GuildId, UserId},
        misc::Mentionable,
    },
    utils::Colour,
    Result as SerenityResult,
};

use songbird::{
    input::{restartable::Restartable, Metadata},
    Event, EventContext, EventHandler as SongbirdEventHandler, SerenityInit, TrackEvent,
};

use tokio::{fs::File, io::AsyncReadExt, sync::Mutex};

struct Handler;

#[async_trait]
impl EventHandler for Handler {
    async fn ready(&self, _: DiscordContext, ready: Ready) {
        tracing::info!("{} is connected!", ready.user.name);
    }
}

struct TrackStartNotifier {
    channel_id: ChannelId,
    ctx: DiscordContext,
}

#[async_trait]
impl SongbirdEventHandler for TrackStartNotifier {
    async fn act(&self, ctx: &EventContext<'_>) -> Option<Event> {
        if let EventContext::Track(track_list) = ctx {
            let meta = track_list
                .first()
                .expect("Something must be starting")
                .1
                .metadata();
            check_msg(
                self.channel_id
                    .send_message(&self.ctx.http, |m| {
                        track_embed(m, meta, QueuePos::Now, None);
                        m
                    })
                    .await,
            );
            // self.ctx
            //     .set_presence(
            //         Some(Activity::playing(
            //             meta.title.unwrap_or_else(|| "untitled".to_string()),
            //         )),
            //         OnlineStatus::Online,
            //     )
            //     .await;
        }

        None
    }
}

// struct TrackPauseNotifier {
//     ctx: DiscordContext,
// }

// #[async_trait]
// impl SongbirdEventHandler for TrackPauseNotifier {
//     async fn act(&self, _ctx: &EventContext<'_>) -> Option<Event> {
//         self.ctx.set_presence(None, OnlineStatus::Idle).await;
//         None
//     }
// }

#[group]
#[commands(
    join, leave, mute, pause, play, playi, plays, queue, remove, resume, skip, stop, unmute, volume
)]
struct General;

#[help]
#[command_not_found_text = "No command named: {}"]
#[max_levenshtein_distance(3)]
async fn help(
    ctx: &DiscordContext,
    msg: &Message,
    args: Args,
    help_options: &'static HelpOptions,
    groups: &[&'static CommandGroup],
    owners: HashSet<UserId>,
) -> CommandResult {
    let _ = help_commands::with_embeds(ctx, msg, args, help_options, groups, owners).await;
    Ok(())
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    tracing_subscriber::fmt::init();

    let token = env::var("DISCORD_TOKEN").context("DISCORD_TOKEN environment variable not set")?;

    let (owners, bot_id) = {
        let mut owners = HashSet::new();
        let http = Http::new_with_token(&token);
        let info = http
            .get_current_application_info()
            .await
            .context("Failed to access application info")?;
        match info.team {
            Some(team) => owners.extend(team.members.iter().map(|tm| tm.user.id)),
            None => {
                owners.insert(info.owner.id);
            }
        }

        let bot_id = http
            .get_current_user()
            .await
            .context("Could not access current user info")?
            .id;

        (owners, bot_id)
    };

    let framework = StandardFramework::new()
        .configure(|c| {
            c.prefixes([".", "~"])
                .owners(owners)
                .on_mention(Some(bot_id))
        })
        .help(&HELP)
        .group(&GENERAL_GROUP);

    let mut client = Client::builder(&token)
        .event_handler(Handler)
        .framework(framework)
        .register_songbird()
        .await
        .context("Client builder error")?;

    client
        .start()
        .await
        .context("An error occurred in client loop")?;

    Ok(())
}

#[command]
#[only_in(guilds)]
#[aliases("j")]
#[description("Instruct the bot to join the voice channel you are currently connected to.")]
async fn join(ctx: &DiscordContext, msg: &Message) -> CommandResult {
    let guild = msg.guild(&ctx.cache).await.unwrap();
    let guild_id = guild.id;

    let channel_id = guild
        .voice_states
        .get(&msg.author.id)
        .and_then(|voice_state| voice_state.channel_id);

    let connect_to = match channel_id {
        Some(channel) => channel,
        None => {
            check_msg(msg.reply(ctx, "Not in a voice channel").await);

            return Ok(());
        }
    };

    let manager = songbird::get(ctx)
        .await
        .expect("Songbird Voice client placed in at initialisation.")
        .clone();

    let (handle_lock, success) = manager.join(guild_id, connect_to).await;

    if let Ok(_channel) = success {
        check_msg(
            msg.channel_id
                .say(&ctx.http, &format!("Joined {}", connect_to.mention()))
                .await,
        );

        let mut handle = handle_lock.lock().await;

        handle.add_global_event(
            Event::Track(TrackEvent::Play),
            TrackStartNotifier {
                channel_id: msg.channel_id,
                ctx: ctx.clone(),
            },
        );

        handle.add_global_event(
            Event::Track(TrackEvent::End),
            DefaultQueue::new(ctx.clone(), guild_id).await,
        );

        // handle.add_global_event(
        //     Event::Track(TrackEvent::Pause),
        //     TrackPauseNotifier { ctx: ctx.clone() },
        // )
    } else {
        check_msg(
            msg.channel_id
                .say(&ctx.http, "Error joining the channel")
                .await,
        );
    }

    Ok(())
}

#[command]
#[only_in(guilds)]
#[aliases("d")]
#[description("Leave the voice channel that the bot is in.")]
async fn leave(ctx: &DiscordContext, msg: &Message) -> CommandResult {
    let guild_id = msg.guild_id.expect("Command can only be used in guilds");

    let manager = songbird::get(ctx)
        .await
        .expect("Songbird Voice client placed in at initialisation.")
        .clone();
    let has_handler = manager.get(guild_id).is_some();

    if has_handler {
        if let Err(e) = manager.remove(guild_id).await {
            check_msg(
                msg.channel_id
                    .say(&ctx.http, format!("Failed: {:?}", e))
                    .await,
            );
        }

        check_msg(msg.channel_id.say(&ctx.http, "Left voice channel").await);
    } else {
        check_msg(msg.reply(ctx, "Not in a voice channel").await);
    }

    Ok(())
}

#[command]
#[only_in(guilds)]
#[aliases("m")]
#[description("Mute the bot's audio source. Track will continue playing, but no one will hear it.")]
async fn mute(ctx: &DiscordContext, msg: &Message) -> CommandResult {
    let guild_id = msg.guild_id.expect("Command can only be used in guilds");

    let manager = songbird::get(ctx)
        .await
        .expect("Songbird Voice client placed in at initialisation.")
        .clone();

    let handler_lock = match manager.get(guild_id) {
        Some(handler) => handler,
        None => {
            check_msg(msg.reply(ctx, "Not in a voice channel").await);

            return Ok(());
        }
    };

    let mut handler = handler_lock.lock().await;

    if handler.is_mute() {
        check_msg(msg.channel_id.say(&ctx.http, "Already muted").await);
    } else {
        if let Err(e) = handler.mute(true).await {
            check_msg(
                msg.channel_id
                    .say(&ctx.http, format!("Failed: {:?}", e))
                    .await,
            );
        }

        check_msg(msg.channel_id.say(&ctx.http, "Now muted").await);
    }

    Ok(())
}

#[command]
#[only_in(guilds)]
#[aliases("h")]
#[description("Pause the currently playing track.")]
async fn pause(ctx: &DiscordContext, msg: &Message) -> CommandResult {
    let guild_id = msg.guild_id.expect("Command can only be used in guilds");

    let manager = songbird::get(ctx)
        .await
        .expect("Songbird Voice client placed in at initialisation.")
        .clone();

    if let Some(handler_lock) = manager.get(guild_id) {
        let handler = handler_lock.lock().await;
        let queue = handler.queue();
        let _ = queue.pause();

        check_msg(
            msg.channel_id
                .say(&ctx.http, "Playback paused".to_string())
                .await,
        );
    } else {
        check_msg(
            msg.channel_id
                .say(&ctx.http, "Not currently playing in this server")
                .await,
        );
    }

    Ok(())
}

#[command]
#[only_in(guilds)]
#[aliases("p")]
#[description("Play a specified track either by link or keyword search. It is inserted at the end of the queue.")]
async fn play(ctx: &DiscordContext, msg: &Message, args: Args) -> CommandResult {
    let query = match args.remains() {
        Some(q) => q.to_string(),
        None => {
            check_msg(
                msg.channel_id
                    .say(&ctx.http, "You forgot to specify what to play...")
                    .await,
            );

            return Ok(());
        }
    };

    let guild_id = msg.guild_id.expect("Command can only be used in guilds");

    let manager = songbird::get(ctx)
        .await
        .expect("Songbird Voice client placed in at initialisation.")
        .clone();

    if let Some(handler_lock) = manager.get(guild_id) {
        let mut handler = handler_lock.lock().await;

        let source = if query.starts_with("http") {
            Restartable::ytdl(query, true).await
        } else {
            Restartable::ytdl_search(query, true).await
        };
        let source = match source {
            Ok(source) => source,
            Err(why) => {
                tracing::error!("Err starting source: {:?}", why);

                check_msg(
                    msg.channel_id
                        .say(&ctx.http, format!("Something went wrong!\n{}", why))
                        .await,
                );

                return Ok(());
            }
        };

        handler.enqueue_source(source.into());
        let (track, length) = {
            let queue = handler.queue();
            (
                queue
                    .current_queue()
                    .last()
                    .expect("Just queued something, there better be a last...")
                    .metadata()
                    .clone(),
                queue.len(),
            )
        };

        let pos = match length {
            0 => unreachable!(),
            1 => QueuePos::Now,
            2 => QueuePos::Next,
            n => QueuePos::Later(n),
        };

        let blame = msg
            .member(&ctx)
            .await
            .expect("Member must have sent this message");

        check_msg(
            msg.channel_id
                .send_message(&ctx.http, |m| {
                    track_embed(m, &track, pos, Some(blame));
                    m
                })
                .await,
        );
    } else {
        check_msg(
            msg.channel_id
                .say(&ctx.http, "Not currently playing in this server")
                .await,
        );
    }

    Ok(())
}

#[command]
#[only_in(guilds)]
#[aliases("pi")]
#[description("Play a specified track either by link or keyword search. It will play immediately; the current track will be requeued next.")]
async fn playi(ctx: &DiscordContext, msg: &Message, args: Args) -> CommandResult {
    let query = match args.remains() {
        Some(q) => q.to_string(),
        None => {
            check_msg(
                msg.channel_id
                    .say(&ctx.http, "You forgot to specify what to play...")
                    .await,
            );

            return Ok(());
        }
    };

    let guild_id = msg.guild_id.expect("Command can only be used in guilds");

    let manager = songbird::get(ctx)
        .await
        .expect("Songbird Voice client placed in at initialisation.")
        .clone();

    if let Some(handler_lock) = manager.get(guild_id) {
        let mut handler = handler_lock.lock().await;

        let source = if query.starts_with("http") {
            Restartable::ytdl(query, true).await
        } else {
            Restartable::ytdl_search(query, true).await
        };
        let source = match source {
            Ok(source) => source,
            Err(why) => {
                tracing::error!("Err starting source: {:?}", why);

                check_msg(
                    msg.channel_id
                        .say(&ctx.http, format!("Something went wrong!\n{}", why))
                        .await,
                );

                return Ok(());
            }
        };

        handler.enqueue_source(source.into());
        let queue = handler.queue();
        if queue.len() > 1 {
            let queue_vec = queue.current_queue();
            let cur_first = queue_vec.first().expect("len > 1");
            let _ = cur_first.pause();
            let _ = cur_first.seek_time(Duration::ZERO);
            queue.modify_queue(|q| q.rotate_right(1));
            let _ = queue.current().expect("len > 1").play();
        }
    } else {
        check_msg(
            msg.channel_id
                .say(&ctx.http, "Not currently playing in this server")
                .await,
        );
    }

    Ok(())
}

#[command]
#[only_in(guilds)]
#[aliases("ps")]
#[description("Play a specified track either by link or keyword search. It will play immediately after the current track finishes.")]
async fn plays(ctx: &DiscordContext, msg: &Message, args: Args) -> CommandResult {
    let query = match args.remains() {
        Some(q) => q.to_string(),
        None => {
            check_msg(
                msg.channel_id
                    .say(&ctx.http, "You forgot to specify what to play...")
                    .await,
            );

            return Ok(());
        }
    };

    let guild_id = msg.guild_id.expect("Command can only be used in guilds");

    let manager = songbird::get(ctx)
        .await
        .expect("Songbird Voice client placed in at initialisation.")
        .clone();

    if let Some(handler_lock) = manager.get(guild_id) {
        let mut handler = handler_lock.lock().await;

        let source = if query.starts_with("http") {
            Restartable::ytdl(query, true).await
        } else {
            Restartable::ytdl_search(query, true).await
        };
        let source = match source {
            Ok(source) => source,
            Err(why) => {
                tracing::error!("Err starting source: {:?}", why);

                check_msg(
                    msg.channel_id
                        .say(&ctx.http, format!("Something went wrong!\n{}", why))
                        .await,
                );

                return Ok(());
            }
        };

        handler.enqueue_source(source.into());
        let queue = handler.queue();

        let (track, length) = {
            (
                queue
                    .current_queue()
                    .last()
                    .expect("Just queued something, it should be there")
                    .metadata()
                    .clone(),
                queue.len(),
            )
        };

        if queue.len() > 1 {
            queue.modify_queue(|q| {
                let t = q.pop_back().unwrap();
                q.insert(1, t);
            });
        }

        let pos = match length {
            1 => QueuePos::Now,
            _ => QueuePos::Next,
        };

        let blame = msg
            .member(&ctx)
            .await
            .expect("Member must have sent this message");

        check_msg(
            msg.channel_id
                .send_message(&ctx.http, |m| {
                    track_embed(m, &track, pos, Some(blame));
                    m
                })
                .await,
        );
    } else {
        check_msg(
            msg.channel_id
                .say(&ctx.http, "Not currently playing in this server")
                .await,
        );
    }

    Ok(())
}

#[command]
#[only_in(guilds)]
#[aliases("q", "ls")]
#[description("Check the current queue")]
async fn queue(ctx: &DiscordContext, msg: &Message, args: Args) -> CommandResult {
    let guild_id = msg.guild_id.expect("Command can only be used in guilds");

    if !args.is_empty() {
        return play(ctx, msg, args).await;
    }

    let manager = songbird::get(ctx)
        .await
        .expect("Songbird Voice client placed in at initialisation.")
        .clone();

    if let Some(handler_lock) = manager.get(guild_id) {
        let handler = handler_lock.lock().await;
        let queue = handler.queue().current_queue();

        check_msg(
            msg.channel_id
                .send_message(&ctx.http, |m| {
                    m.embed(|e| {
                        let desc = queue
                            .iter()
                            .enumerate()
                            .fold(String::new(), |mut s, (p, t)| {
                                s.push_str(&format!(
                                    "{}: {}\n",
                                    p + 1,
                                    t.metadata()
                                        .title
                                        .clone()
                                        .unwrap_or_else(|| "untitled".to_string())
                                ));
                                s
                            });
                        e.title("Current Queue").description(desc)
                    })
                })
                .await,
        );
    } else {
        check_msg(
            msg.channel_id
                .say(&ctx.http, "Not currently playing in this server")
                .await,
        );
    }

    Ok(())
}

#[command]
#[only_in(guilds)]
#[aliases("r")]
#[description("Remove a track from the queue, identified by index")]
async fn remove(ctx: &DiscordContext, msg: &Message, mut args: Args) -> CommandResult {
    let position = match args.single::<usize>() {
        Ok(p) => p,
        Err(_) => {
            check_msg(
                msg.channel_id
                    .say(
                        &ctx.http,
                        "You must specify what to remove using its position in the queue",
                    )
                    .await,
            );
            return Ok(());
        }
    };

    let guild_id = msg.guild_id.expect("Command can only be used in guilds");

    let manager = songbird::get(ctx)
        .await
        .expect("Songbird Voice client placed in at initialisation.")
        .clone();

    if let Some(handler_lock) = manager.get(guild_id) {
        let handler = handler_lock.lock().await;
        let queue = handler.queue();
        let removed = queue.modify_queue(|q| q.remove(position - 1));

        if let Some(track) = removed {
            let _ = track.stop();
            check_msg(
                msg.channel_id
                    .say(&ctx.http, format!("Removed {} from queue", position))
                    .await,
            );
        } else {
            check_msg(
                msg.channel_id
                    .say(&ctx.http, format!("Track not found: {}", position))
                    .await,
            );
        }
    } else {
        check_msg(
            msg.channel_id
                .say(&ctx.http, "Not currently playing in this server")
                .await,
        );
    }

    Ok(())
}

#[command]
#[only_in(guilds)]
#[aliases("c")]
#[description("Resume playback of a paused track")]
async fn resume(ctx: &DiscordContext, msg: &Message) -> CommandResult {
    let guild_id = msg.guild_id.expect("Command can only be used in guilds");

    let manager = songbird::get(ctx)
        .await
        .expect("Songbird Voice client placed in at initialisation.")
        .clone();

    if let Some(handler_lock) = manager.get(guild_id) {
        let handler = handler_lock.lock().await;
        let queue = handler.queue();
        let _ = queue.resume();

        check_msg(
            msg.channel_id
                .say(&ctx.http, "Playback resumed".to_string())
                .await,
        );
    } else {
        check_msg(
            msg.channel_id
                .say(&ctx.http, "Not currently playing in this server")
                .await,
        );
    }

    Ok(())
}

#[command]
#[only_in(guilds)]
#[aliases("s")]
#[description("Skip the current track")]
async fn skip(ctx: &DiscordContext, msg: &Message, _args: Args) -> CommandResult {
    let guild_id = msg.guild_id.expect("Command can only be used in guilds");

    let manager = songbird::get(ctx)
        .await
        .expect("Songbird Voice client placed in at initialisation.")
        .clone();

    if let Some(handler_lock) = manager.get(guild_id) {
        let handler = handler_lock.lock().await;
        let queue = handler.queue();
        let _ = queue.skip();

        check_msg(
            msg.channel_id
                .say(
                    &ctx.http,
                    format!("Song skipped: {} in queue.", queue.len()),
                )
                .await,
        );
    } else {
        check_msg(
            msg.channel_id
                .say(&ctx.http, "Not in a voice channel to play in")
                .await,
        );
    }

    Ok(())
}

#[command]
#[only_in(guilds)]
#[aliases("x")]
#[description("Stop playback, clearing the bot's queue")]
async fn stop(ctx: &DiscordContext, msg: &Message, _args: Args) -> CommandResult {
    let guild_id = msg.guild_id.expect("Command can only be used in guilds");

    let manager = songbird::get(ctx)
        .await
        .expect("Songbird Voice client placed in at initialisation.")
        .clone();

    if let Some(handler_lock) = manager.get(guild_id) {
        let handler = handler_lock.lock().await;
        let queue = handler.queue();
        let _ = queue.stop();

        check_msg(msg.channel_id.say(&ctx.http, "Queue cleared.").await);
    } else {
        check_msg(
            msg.channel_id
                .say(&ctx.http, "Not in a voice channel to play in")
                .await,
        );
    }

    Ok(())
}

#[command]
#[only_in(guilds)]
#[aliases("um")]
#[description("Unmute the bot's audio")]
async fn unmute(ctx: &DiscordContext, msg: &Message) -> CommandResult {
    let guild_id = msg.guild_id.expect("Command can only be used in guilds");

    let manager = songbird::get(ctx)
        .await
        .expect("Songbird Voice client placed in at initialisation.")
        .clone();

    if let Some(handler_lock) = manager.get(guild_id) {
        let mut handler = handler_lock.lock().await;
        if let Err(e) = handler.mute(false).await {
            check_msg(
                msg.channel_id
                    .say(&ctx.http, format!("Failed: {:?}", e))
                    .await,
            );
        }

        check_msg(msg.channel_id.say(&ctx.http, "Unmuted").await);
    } else {
        check_msg(
            msg.channel_id
                .say(&ctx.http, "Not in a voice channel to unmute in")
                .await,
        );
    }

    Ok(())
}

#[command]
#[only_in(guilds)]
#[aliases("v")]
#[description("Set the volume for the current track only")]
async fn volume(ctx: &DiscordContext, msg: &Message, mut args: Args) -> CommandResult {
    let volume = match args.single::<f32>() {
        Ok(p) => p,
        Err(_) => {
            check_msg(
                msg.channel_id
                    .say(
                        &ctx.http,
                        "You must tell me how loud I should be using a number between 0.0 and 10.0",
                    )
                    .await,
            );
            return Ok(());
        }
    };

    let guild_id = msg.guild_id.expect("Command can only be used in guilds");

    let manager = songbird::get(ctx)
        .await
        .expect("Songbird Voice client placed in at initialisation.")
        .clone();

    if let Some(handler_lock) = manager.get(guild_id) {
        let handler = handler_lock.lock().await;

        match handler.queue().current() {
            Some(t) => {
                let _ = t.set_volume(volume);
            }
            None => {
                check_msg(
                    msg.channel_id
                        .say(&ctx.http, "Nothing is playing currently")
                        .await,
                );
                return Ok(());
            }
        }

        check_msg(msg.channel_id.say(&ctx.http, "Volume set").await);
    } else {
        check_msg(
            msg.channel_id
                .say(&ctx.http, "Not in a voice channel to unmute in")
                .await,
        );
    }

    Ok(())
}

/// Checks that a message successfully sent; if not, then logs why using tracing.
fn check_msg(result: SerenityResult<Message>) {
    if let Err(why) = result {
        tracing::error!("Error sending message: {:?}", why);
    }
}

enum QueuePos {
    Now,
    Next,
    Later(usize),
}

fn track_embed(
    message: &mut CreateMessage<'_>,
    meta: &Metadata,
    pos: QueuePos,
    blame: Option<Member>,
) {
    message.embed(|e| {
        match pos {
            QueuePos::Now => {
                e.title("🎵 Now Playing 🎵");
                e.colour(Colour::DARK_GREEN);
            }
            QueuePos::Next => {
                e.title("Next Up");
                e.colour(Colour::DARK_GREEN);
            }
            QueuePos::Later(pos) => {
                e.title(format!("Queued at {}", pos));
                e.colour(Colour::GOLD);
            }
        };

        let mut desc = String::new();
        if let Some(title) = &meta.title {
            desc.push_str(title);
        } else {
            desc.push_str("untitled");
        }
        desc.push('\n');
        if let Some(artist) = &meta.artist {
            desc.push_str(&format!("By {}", artist));
        }
        e.description(desc);

        if let Some(url) = &meta.thumbnail {
            e.thumbnail(url);
        }
        if let Some(blame) = blame {
            e.footer(|f| f.text(format!("Queued by {}", blame.display_name())));
        }
        e
    });
}

pub struct DefaultQueue {
    inner: Arc<Mutex<VecDeque<String>>>,
    ctx: DiscordContext,
    guild_id: GuildId,
}

impl DefaultQueue {
    /// Create a new default queue from file
    async fn new(ctx: DiscordContext, guild_id: GuildId) -> Self {
        let inner = Arc::new(Mutex::new(VecDeque::new()));

        if let Ok(path) = env::var("PHIL_DEFAULT") {
            if let Ok(mut file) = File::open(path).await {
                let mut defaults = String::new();
                match file.read_to_string(&mut defaults).await {
                    Ok(_) => {
                        let mut queue = inner.lock().await;
                        for default in defaults.lines() {
                            queue.push_back(default.to_owned());
                        }
                    }
                    Err(why) => {
                        tracing::error!("Error reading defaults: {:?}", why);
                    }
                }
            }
        }

        Self {
            inner,
            ctx,
            guild_id,
        }
    }
}

#[async_trait]
impl SongbirdEventHandler for DefaultQueue {
    async fn act(&self, _ctx: &EventContext<'_>) -> Option<Event> {
        let mut defaults = self.inner.lock().await;
        if !defaults.is_empty() {
            if let Some(manager) = songbird::get(&self.ctx).await.clone() {
                if let Some(handler_lock) = manager.get(self.guild_id) {
                    let mut handler = handler_lock.lock().await;
                    let queue = handler.queue().current_queue();

                    if queue.is_empty() {
                        let next = defaults.pop_front().unwrap();
                        let source = if next.starts_with("http") {
                            Restartable::ytdl(next, true).await
                        } else {
                            Restartable::ytdl_search(next, true).await
                        };
                        match source {
                            Ok(source) => {
                                handler.enqueue_source(source.into());
                            }
                            Err(why) => {
                                tracing::error!("Err starting source: {:?}", why);
                                return None;
                            }
                        };
                    }
                }
            }
        }

        None
    }
}
