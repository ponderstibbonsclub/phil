//! Phil Collins - the simple Discord bot for the Ponder Stibbons Club
//!
//! Modified from Songbird voice_events_queue example
use std::{env, sync::Arc};

use anyhow::Context;
use serenity::{
    async_trait,
    client::{Client, Context as DiscordContext, EventHandler},
    framework::{
        standard::{
            macros::{command, group},
            Args, CommandResult,
        },
        StandardFramework,
    },
    http::Http,
    model::{channel::Message, gateway::Ready, misc::Mentionable, prelude::ChannelId},
    Result as SerenityResult,
};

use songbird::{
    input::restartable::Restartable, Event, EventContext, EventHandler as VoiceEventHandler,
    SerenityInit, TrackEvent,
};

struct Handler;

#[async_trait]
impl EventHandler for Handler {
    async fn ready(&self, _: DiscordContext, ready: Ready) {
        tracing::info!("{} is connected!", ready.user.name);
    }
}

struct TrackEndNotifier {
    chan_id: ChannelId,
    http: Arc<Http>,
}

#[async_trait]
impl VoiceEventHandler for TrackEndNotifier {
    async fn act(&self, ctx: &EventContext<'_>) -> Option<Event> {
        if let EventContext::Track(track_list) = ctx {
            check_msg(
                self.chan_id
                    .say(&self.http, &format!("Tracks ended: {}.", track_list.len()))
                    .await,
            );
        }

        None
    }
}

#[group]
#[commands(
    join, leave, mute, pause, play, queue, remove, resume, skip, stop, unmute, volume
)]
struct General;

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    tracing_subscriber::fmt::init();

    let token = env::var("DISCORD_TOKEN").context("DISCORD_TOKEN environment variable not set")?;

    let framework = StandardFramework::new()
        .configure(|c| c.prefix("."))
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

        let chan_id = msg.channel_id;

        let send_http = ctx.http.clone();

        let mut handle = handle_lock.lock().await;

        handle.add_global_event(
            Event::Track(TrackEvent::End),
            TrackEndNotifier {
                chan_id,
                http: send_http,
            },
        );
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
                .say(&ctx.http, format!("Playback paused"))
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

        let blame = msg
            .member(&ctx)
            .await
            .expect("Member must have sent this message");

        check_msg(
            msg.channel_id
                .send_message(&ctx.http, |m| {
                    m.embed(|e| {
                        e.title(track.title.unwrap_or_else(|| "Untitled".to_string()));
                        if let Some(url) = track.thumbnail {
                            e.thumbnail(url);
                        }
                        if let Some(artist) = track.artist {
                            e.description(format!("By {}", artist));
                        }
                        if length > 1 {
                            e.field("Position", length, true);
                        }
                        e.footer(|f| {
                            f.text(format!("Queued by {}", blame.display_name()));
                            f
                        })
                    });
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
async fn queue(ctx: &DiscordContext, msg: &Message) -> CommandResult {
    let guild_id = msg.guild_id.expect("Command can only be used in guilds");

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
                .say(&ctx.http, format!("Playback resumed"))
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
async fn undeafen(ctx: &DiscordContext, msg: &Message) -> CommandResult {
    let guild_id = msg.guild_id.expect("Command can only be used in guilds");

    let manager = songbird::get(ctx)
        .await
        .expect("Songbird Voice client placed in at initialisation.")
        .clone();

    if let Some(handler_lock) = manager.get(guild_id) {
        let mut handler = handler_lock.lock().await;
        if let Err(e) = handler.deafen(false).await {
            check_msg(
                msg.channel_id
                    .say(&ctx.http, format!("Failed: {:?}", e))
                    .await,
            );
        }

        check_msg(msg.channel_id.say(&ctx.http, "Undeafened").await);
    } else {
        check_msg(
            msg.channel_id
                .say(&ctx.http, "Not in a voice channel to undeafen in")
                .await,
        );
    }

    Ok(())
}

#[command]
#[only_in(guilds)]
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
