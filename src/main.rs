//! Phil Collins - the simple Discord bot for the Ponder Stibbons Club
//!
//! Modified from Songbird voice_events_queue example
use std::{
    collections::HashSet,
    env,
    fs::File,
    hash::Hash,
    io::{BufReader, BufWriter},
    path::Path,
    sync::Arc,
    time::{Duration, SystemTime, UNIX_EPOCH},
};

use anyhow::Context;
use once_cell::sync::Lazy;
use rand::seq::IteratorRandom;
use regex::Regex;
use reqwest::Client as HttpClient;
use serde::{Deserialize, Serialize};
use serenity::{
    async_trait,
    builder::{
        CreateActionRow, CreateButton, CreateEmbed, CreateEmbedFooter, CreateInteractionResponse,
        CreateInteractionResponseMessage, CreateMessage, EditMessage,
    },
    client::{Client, Context as DiscordContext, EventHandler},
    framework::{
        standard::{
            help_commands,
            macros::{command, group, help},
            Args, Configuration, CommandGroup, CommandResult, HelpOptions,
        },
        StandardFramework,
    },
    http::Http,
    model::{
        application::{ButtonStyle, Interaction},
        channel::Message,
        colour::Colour,
        gateway::Ready,
        guild::Member,
        id::{ChannelId, GuildId, UserId},
        mention::Mentionable,
    },
    prelude::{GatewayIntents, TypeMapKey},
    Result as SerenityResult,
};
use songbird::{
    input::{AuxMetadata, Compose, YoutubeDl},
    tracks::{Track, TrackQueue},
    Call, Event, EventContext, EventHandler as SongbirdEventHandler, SerenityInit, TrackEvent,
};
use time::{Date, OffsetDateTime};
use tokio::sync::Mutex;
use tracing::error;

struct Handler;

#[async_trait]
impl EventHandler for Handler {
    async fn ready(&self, _: DiscordContext, ready: Ready) {
        tracing::info!("{} is connected!", ready.user.name);
    }

    async fn interaction_create(&self, ctx: DiscordContext, interaction: Interaction) {
        match interaction {
            Interaction::Component(mut i) => {
                let ident = i.data.custom_id.clone();
                let blame = i.user.id;
                let content;

                {
                    let mut data = ctx.data.write().await;
                    if let Some(fallback_tracks) = data.get_mut::<FallbackTracksKey>() {
                        if fallback_tracks.add_track(ident, blame).await {
                            if let Err(e) = fallback_tracks.write_file("fallback.json") {
                                tracing::warn!("Error writing fallback!\n{e}");
                            };
                            content = "Added!";
                        } else {
                            content = "This track is already in the archives"
                        }
                    } else {
                        content = "No fallback list exists"
                    };
                }

                let response = CreateInteractionResponse::Message(
                    CreateInteractionResponseMessage::new()
                        .content(content)
                        .ephemeral(true),
                );
                check_msg(i.create_response(&ctx.http, response).await);
                if let Err(e) = i
                    .message
                    .edit(
                        &ctx,
                        EditMessage::new().components(vec![CreateActionRow::Buttons(vec![
                            CreateButton::new("null")
                                .label("Confirm")
                                .style(ButtonStyle::Success)
                                .disabled(true),
                        ])]),
                    )
                    .await
                {
                    tracing::warn!("Error editing interaction source: {}\n{}", i.message.id, e);
                };
            }
            _ => tracing::warn!("Unexpected interaction received {:?}", interaction),
        }
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
            let th = track_list.first().expect("Something must be starting").1;

            let (meta, blame) = {
                let tm = th.typemap().read().await;
                let Some(meta) = tm.get::<MetadataKey>().cloned() else {
                    return Some(Event::Delayed(Duration::from_secs(1)));
                };
                let blame = tm.get::<BlameKey>().cloned();
                (meta, blame)
            };

            check_msg(
                self.channel_id
                    .send_message(&self.ctx.http, track_embed(&meta, QueuePos::Now, blame))
                    .await,
            );
        }

        None
    }
}

#[derive(Debug, Serialize, Deserialize)]
struct FallbackTrack {
    ident: String,
    blame: UserId,
    added: Date,
}

impl PartialEq for FallbackTrack {
    fn eq(&self, other: &Self) -> bool {
        self.ident == other.ident
    }
}

impl Eq for FallbackTrack {}

impl Hash for FallbackTrack {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.ident.hash(state);
    }
}

#[derive(Debug, Default, Serialize, Deserialize)]
#[serde(transparent)]
struct FallbackTracks {
    tracks: HashSet<FallbackTrack>,
}

impl FallbackTracks {
    async fn next_track(&self, ctx: &DiscordContext) -> Option<(Track, UserId)> {
        let track_desc = {
            let mut rng = rand::thread_rng();
            self.tracks.iter().choose(&mut rng)?
        };

        let client_http = http_from_context(ctx).await;

        let source = YoutubeDl::new(
            client_http,
            format!("https://youtube.com/watch?v={}", track_desc.ident),
        );

        Some((Track::new(source.into()), track_desc.blame))
    }

    async fn add_track(&mut self, ident: String, blame: UserId) -> bool {
        let added: Date = OffsetDateTime::from_unix_timestamp(
            SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .expect("hopefully this doesn't go back...")
                .as_secs() as i64,
        )
        .expect("now is a valid time")
        .date();

        let track = FallbackTrack {
            ident,
            blame,
            added,
        };

        self.tracks.insert(track)
    }

    fn from_file(path: impl AsRef<Path>) -> anyhow::Result<Self> {
        let path = path.as_ref();
        let reader = BufReader::new(File::open(path).context("Opening tracks file")?);
        let tracks: FallbackTracks = serde_json::from_reader(reader)?;

        Ok(tracks)
    }

    fn write_file(&self, path: impl AsRef<Path>) -> anyhow::Result<()> {
        let path = path.as_ref();
        let writer = BufWriter::new(File::create(path)?);
        serde_json::to_writer(writer, self)?;
        Ok(())
    }
}

struct FallbackTracksKey;

impl TypeMapKey for FallbackTracksKey {
    type Value = FallbackTracks;
}

#[derive(Clone)]
struct FallbackTracksHandler {
    queue: TrackQueue,
    driver: Arc<Mutex<Call>>,
    ctx: DiscordContext,
    guild_id: GuildId,
}

#[async_trait]
impl SongbirdEventHandler for FallbackTracksHandler {
    async fn act(&self, _evt_ctx: &EventContext<'_>) -> Option<Event> {
        if !self.queue.is_empty() {
            return None;
        }

        tracing::info!("Queue is empty, pulling from fallback queue");

        let (mut track, blame_uid) = {
            let data = self.ctx.data.read().await;
            match data.get::<FallbackTracksKey>()?.next_track(&self.ctx).await {
                Some(t) => t,
                None => return Some(Event::Delayed(Duration::from_secs(5))),
            }
        };

        let blame = {
            let guild_ref = self.ctx.cache.guild(self.guild_id).map(|gr| gr.clone());
            if let Some(guild) = guild_ref {
                guild
                    .member(&self.ctx, blame_uid)
                    .await
                    .ok()
                    .map(|mr| Blame::Fallback((*mr).clone()))
            } else {
                None
            }
        };

        let meta = track.input.aux_metadata().await.ok()?;
        let track = self.driver.lock().await.enqueue(track).await;

        {
            let mut tm = track.typemap().write().await;
            tm.insert::<MetadataKey>(meta);
            if let Some(blame) = blame {
                tm.insert::<BlameKey>(blame);
            }
        }

        None
    }
}

struct HttpKey;

impl TypeMapKey for HttpKey {
    type Value = HttpClient;
}

async fn http_from_context(ctx: &DiscordContext) -> HttpClient {
    let data = ctx.data.read().await;
    data.get::<HttpKey>()
        .cloned()
        .expect("HTTP client should be initialised at runtime")
}

struct MetadataKey;

impl TypeMapKey for MetadataKey {
    type Value = AuxMetadata;
}

struct BlameKey;

impl TypeMapKey for BlameKey {
    type Value = Blame;
}

#[group]
#[commands(
    join,
    leave,
    add_fallback,
    mute,
    pause,
    play,
    playi,
    plays,
    queue,
    remove,
    resume,
    skip,
    stop,
    unmute,
    volume
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

    let (owners, app_id, bot_id) = {
        let mut owners = HashSet::new();
        let http = Http::new(&token);
        let info = http
            .get_current_application_info()
            .await
            .context("Failed to access application info")?;
        match info.team {
            Some(team) => owners.extend(team.members.iter().map(|tm| tm.user.id)),
            None => {
                owners.insert(info.owner.expect("must be owned").id);
            }
        }

        let bot_id = http
            .get_current_user()
            .await
            .context("Could not access current user info")?
            .id;

        (owners, info.id, bot_id)
    };

    let framework = StandardFramework::new().help(&HELP).group(&GENERAL_GROUP);
    framework.configure(
        Configuration::new().prefixes([".", "~"])
            .owners(owners)
            .on_mention(Some(bot_id))
    );

    let intents = GatewayIntents::DIRECT_MESSAGES
        | GatewayIntents::GUILDS
        | GatewayIntents::GUILD_INTEGRATIONS
        | GatewayIntents::GUILD_MESSAGES
        | GatewayIntents::GUILD_VOICE_STATES
        | GatewayIntents::MESSAGE_CONTENT;

    let mut client = Client::builder(&token, intents)
        .event_handler(Handler)
        .application_id(app_id)
        .framework(framework)
        .register_songbird()
        .type_map_insert::<HttpKey>(HttpClient::new())
        .await
        .context("Client builder error")?;

    let fallback_tracks = match FallbackTracks::from_file("fallback.json") {
        Ok(t) => t,
        Err(e) => {
            tracing::warn!("Unable to open fallback file (this is normal on first run!)\n{e}");
            FallbackTracks::default()
        }
    };

    {
        let mut data = client.data.write().await;
        data.insert::<FallbackTracksKey>(fallback_tracks);
    }

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
    let (guild_id, channel_id) = {
        let guild = msg.guild(&ctx.cache).unwrap();
        let channel_id = guild
            .voice_states
            .get(&msg.author.id)
            .and_then(|voice_state| voice_state.channel_id);
        (guild.id, channel_id)
    };

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

    let handle_lock = match manager.join(guild_id, connect_to).await {
        Ok(hl) => hl,
        Err(e) => {
            check_msg(
                msg.channel_id
                    .say(&ctx.http, "Error joining the channel")
                    .await,
            );
            error!("Error joining a voice channel\n{e:?}");
            return Ok(());
        }
    };
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
    let queue = handle.queue().clone();
    let fallback_handler = FallbackTracksHandler {
        queue,
        driver: Arc::clone(&handle_lock),
        ctx: ctx.clone(),
        guild_id: msg.guild_id.expect("Command only allowed in guilds"),
    };
    handle.add_global_event(Event::Track(TrackEvent::End), fallback_handler.clone());
    handle.add_global_event(Event::Delayed(Duration::from_secs(30)), fallback_handler);

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
#[owners_only]
#[aliases("a")]
#[description("Add a track to the fallback list")]
async fn add_fallback(ctx: &DiscordContext, msg: &Message, args: Args) -> CommandResult {
    let query = match args.remains() {
        Some(q) => q.to_string(),
        None => {
            check_msg(
                msg.channel_id
                    .say(&ctx.http, "You forgot to specify what to add...")
                    .await,
            );

            return Ok(());
        }
    };

    let http_client = http_from_context(ctx).await;

    let mut source = if query.starts_with("http") {
        YoutubeDl::new(http_client, query)
    } else {
        YoutubeDl::new(http_client, format!("ytsearch:{query}"))
    };

    let Ok(mut meta) = source.aux_metadata().await else {
        check_msg(msg.channel_id.say(&ctx, "Failed to get track meta").await);
        return Ok(())
    };

    static YT_RE: Lazy<Regex> = Lazy::new(|| {
        Regex::new(r"^.*(?:(?:youtu\.be/|v/|vi/|u/\w/|embed/|shorts/)|(?:(?:watch)?\?vi?=|&vi?=))([^#&?]*).*").expect("regex should be valid")
    });

    let url = meta
        .source_url
        .take()
        .expect("YouTube tracks should have a URL");
    let Some(caps) = YT_RE.captures(&url) else {
        tracing::info!("Could not extract id from URL {:?}", url);
        return Ok(());
    };
    let Some(id) = caps.get(1) else {
        tracing::info!("No match in {caps:?}");
        return Ok(())
    };

    check_msg(
        msg.channel_id
            .send_message(&ctx.http, {
                track_embed(&meta, QueuePos::Fallback, None)
                    .reference_message(msg)
                    .components(vec![CreateActionRow::Buttons(vec![CreateButton::new(
                        id.as_str(),
                    )
                    .label("Confirm")
                    .style(ButtonStyle::Success)])])
            })
            .await,
    );

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

    let http_client = http_from_context(ctx).await;

    let manager = songbird::get(ctx)
        .await
        .expect("Songbird Voice client placed in at initialisation.")
        .clone();

    if let Some(handler_lock) = manager.get(guild_id) {
        let mut source = if query.starts_with("http") {
            YoutubeDl::new(http_client, query)
        } else {
            YoutubeDl::new(http_client, format!("ytsearch:{query}"))
        };

        let Ok(meta) = source.aux_metadata().await else {
            check_msg(msg.channel_id.say(&ctx, "Failed to get track meta").await);
            return Ok(());
        };

        let (track, length) = {
            let mut handler = handler_lock.lock().await;
            let track = handler.enqueue(Track::new(source.into())).await;
            (track, handler.queue().len())
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
                .send_message(
                    &ctx.http,
                    track_embed(&meta, pos, Some(Blame::Manual(blame.clone()))),
                )
                .await,
        );

        {
            let mut tm = track.typemap().write().await;
            tm.insert::<MetadataKey>(meta);
            tm.insert::<BlameKey>(Blame::Manual(blame));
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

    let http_client = http_from_context(ctx).await;

    let manager = songbird::get(ctx)
        .await
        .expect("Songbird Voice client placed in at initialisation.")
        .clone();

    if let Some(handler_lock) = manager.get(guild_id) {
        let mut source = if query.starts_with("http") {
            YoutubeDl::new(http_client, query)
        } else {
            YoutubeDl::new(http_client, format!("ytsearch:{query}"))
        };

        let Ok(meta) = source.aux_metadata().await else {
            check_msg(msg.channel_id.say(&ctx, "Failed to get track meta").await);
            return Ok(());
        };

        {
            let mut handler = handler_lock.lock().await;
            let track = handler.enqueue(Track::new(source.into())).await;
            {
                let mut tm = track.typemap().write().await;
                tm.insert::<MetadataKey>(meta);
            }
            let queue = handler.queue();
            if queue.len() > 1 {
                let queue_vec = queue.current_queue();
                let cur_first = queue_vec.first().expect("len > 1");
                let _ = cur_first.pause();
                let _ = cur_first.seek(Duration::ZERO);
                queue.modify_queue(|q| q.rotate_right(1));
                let _ = queue.current().expect("len > 1").play();
            }
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

    let http_client = http_from_context(ctx).await;

    let manager = songbird::get(ctx)
        .await
        .expect("Songbird Voice client placed in at initialisation.")
        .clone();

    if let Some(handler_lock) = manager.get(guild_id) {
        let mut source = if query.starts_with("http") {
            YoutubeDl::new(http_client, query)
        } else {
            YoutubeDl::new(http_client, format!("ytsearch:{query}"))
        };

        let Ok(meta) = source.aux_metadata().await else {
            check_msg(msg.channel_id.say(&ctx, "Failed to get track meta").await);
            return Ok(());
        };

        let (track, length) = {
            let mut handler = handler_lock.lock().await;
            let track = handler.enqueue(Track::new(source.into())).await;
            let queue = handler.queue();

            let length = queue.len();

            if length > 1 {
                queue.modify_queue(|q| {
                    let t = q.pop_back().unwrap();
                    q.insert(1, t);
                });
            }

            (track, length)
        };

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
                .send_message(
                    &ctx.http,
                    track_embed(&meta, pos, Some(Blame::Manual(blame.clone()))),
                )
                .await,
        );

        {
            let mut tm = track.typemap().write().await;
            tm.insert::<MetadataKey>(meta);
            tm.insert::<BlameKey>(Blame::Manual(blame));
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
        let queue = {
            let handler = handler_lock.lock().await;
            handler.queue().current_queue()
        };

        check_msg(
            msg.channel_id
                .send_message(
                    &ctx.http,
                    CreateMessage::new().embed({
                        let e = CreateEmbed::new();
                        let metas = futures::future::join_all(queue.iter().map(|t| async {
                            let tm = t.typemap().read().await;
                            tm.get::<MetadataKey>().cloned()
                        }))
                        .await;
                        let desc = metas
                            .iter()
                            .enumerate()
                            .fold(String::new(), |mut s, (p, m)| {
                                s.push_str(&format!(
                                    "{}: {}\n",
                                    p + 1,
                                    m.as_ref()
                                        .and_then(|m| m.title.clone())
                                        .unwrap_or_else(|| "untitled".to_string())
                                ));
                                s
                            });
                        e.title("Current Queue").description(desc)
                    }),
                )
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
        Ok(0 | 1) => {
            check_msg(
                msg.channel_id
                    .say(
                        &ctx.http,
                        "Can't remove the currently playing track, use skip",
                    )
                    .await,
            );
            return Ok(());
        }
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
                    .say(&ctx.http, format!("Removed {position} from queue"))
                    .await,
            );
        } else {
            check_msg(
                msg.channel_id
                    .say(&ctx.http, format!("Track not found: {position}"))
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

        if queue.is_empty() {
            check_msg(msg.channel_id.say(&ctx, "No songs in queue").await);
            return Ok(());
        }

        let _ = queue.skip();
        check_msg(
            msg.channel_id
                .say(
                    &ctx.http,
                    format!("Song skipped: {} in queue.", queue.len() - 1),
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
        queue.stop();

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
fn check_msg<T>(result: SerenityResult<T>) {
    if let Err(why) = result {
        error!("Error sending message: {why:?}");
    }
}

enum QueuePos {
    Now,
    Next,
    Later(usize),
    Fallback,
}

#[derive(Clone)]
enum Blame {
    Manual(Member),
    Fallback(Member),
}

/// Create an embed for describing a track
fn track_embed(meta: &AuxMetadata, pos: QueuePos, blame: Option<Blame>) -> CreateMessage {
    let mut embed = CreateEmbed::new();
    embed = match pos {
        QueuePos::Now => embed.title("🎵 Now Playing 🎵").colour(Colour::DARK_GREEN),
        QueuePos::Next => embed.title("Next Up").colour(Colour::DARK_GREEN),
        QueuePos::Later(pos) => embed
            .title(format!("Queued at {pos}"))
            .colour(Colour::GOLD),
        QueuePos::Fallback => embed.title("Add as fallback?").colour(Colour::LIGHT_GREY),
    };

    let mut desc = String::new();
    if let Some(title) = &meta.title {
        desc.push_str(title);
    } else {
        desc.push_str("untitled");
    }
    desc.push('\n');
    if let Some(artist) = &meta.artist {
        desc.push_str(&format!("By {artist}"));
    }
    embed = embed.description(desc);

    if let Some(url) = &meta.thumbnail {
        embed = embed.thumbnail(url);
    }
    embed = match blame {
        Some(Blame::Manual(blame)) => embed.footer(CreateEmbedFooter::new(format!(
            "Queued by {}",
            blame.display_name()
        ))),
        Some(Blame::Fallback(blame)) => embed.footer(CreateEmbedFooter::new(format!(
            "From the archives, courtesy of {}",
            blame.display_name()
        ))),
        None => embed,
    };
    CreateMessage::new().add_embed(embed)
}
