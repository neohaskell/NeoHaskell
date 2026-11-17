mod identity;
#[cfg(unix)]
mod installation;
mod language;

use std::sync::OnceLock;

static ENGLISH: OnceLock<language::English> = OnceLock::new();

pub struct Exit {
    code: i32,
}

impl Exit {
    pub fn code(self) -> i32 {
        self.code
    }
}

pub fn run() -> Exit {
    let identity = identity::BuildIdentity::current();
    let arguments: Vec<_> = std::env::args_os().skip(1).collect();
    #[cfg(unix)]
    if arguments.as_slice() == [std::ffi::OsString::from(installation::process::WORKER_ARGUMENT)] {
        return Exit {
            code: installation::process::run_worker(),
        };
    }
    let english = english();

    let code = match arguments.as_slice() {
        [] => {
            println!("{}", english.help);
            0
        }
        [argument] if argument == "--help" || argument == "-h" => {
            println!("{}", english.help);
            0
        }
        [argument] if argument == "--version" || argument == "-V" => {
            println!(
                "{}",
                english
                    .version
                    .replace("{version}", identity.product_version)
                    .replace("{revision}", identity.source_revision)
                    .replace("{target}", identity.target)
            );
            0
        }
        _ => {
            eprintln!("{}", english.invalid_arguments);
            2
        }
    };

    Exit { code }
}

fn english() -> &'static language::English {
    ENGLISH.get_or_init(|| {
        serde_json::from_str(include_str!("english.json")).expect("valid English lifecycle text")
    })
}
