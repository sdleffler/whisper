use ::{
    serde::{Deserialize, Serialize},
    whisper::ir::IrNode,
};

whisper::module! {
    fn validator();

    // Valid when tls is enabled and there's a key and cert present
    valid {
        key: ("Some" : _),
        cert: ("Some" : _),
        tls_enabled: true,
    };

    // Or when tls is enabled, we don't care about the key/cert
    valid {
        key: _,
        cert: _,
        tls_enabled: false,
    };
}

whisper::query! {
    fn validate(config: IrNode);

    valid #config;
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Config {
    key: Option<String>,
    cert: Option<String>,
    tls_enabled: bool,
}

fn main() {}
