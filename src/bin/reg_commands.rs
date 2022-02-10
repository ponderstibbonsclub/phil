use std::env;

use twilight_http::Client;

use phil::config::Config;

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let config = {
        let path = match env::var("PHIL_CONFIG").ok() {
            Some(s) => s,
            None => "phil.toml".to_string(),
        };
        Config::from_file(&path).await?
    };

    // Create client w/ ID retrieved from Discord
    let bot_cl = Client::new(config.discord.token);
    let app_info = bot_cl
        .current_user_application()
        .exec()
        .await?
        .model()
        .await?;
    let int_cl = bot_cl.interaction(app_info.id);

    

    Ok(())
}
