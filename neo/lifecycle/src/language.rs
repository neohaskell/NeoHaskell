use serde::Deserialize;

#[derive(Deserialize)]
pub(crate) struct English {
    pub(crate) help: String,
    pub(crate) version: String,
    pub(crate) invalid_arguments: String,
}
