use std::{io::Error as IOError, path::Path};

use serde::Deserialize;
use tokio::{fs::File, io::AsyncReadExt};

#[derive(Debug, Deserialize)]
pub struct Config {
    pub discord: DiscordConfig,
}

impl Config {
    /// Read configuration file
    pub async fn from_file(path: impl AsRef<Path>) -> Result<Self, IOError> {
        let mut file = File::open(path).await?;
        // TODO: If file does not exist, create it with default values
        let mut buf = String::new();
        file.read_to_string(&mut buf).await?;

        let config = toml::from_str(&buf)?;

        Ok(config)
    }
}

#[derive(Debug, Deserialize)]
pub struct DiscordConfig {
    pub token: String,
}
