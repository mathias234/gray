use gray::builtins::declare_functions;
use gray::GrayError;
use std::env;
use std::fs::{metadata, read_dir};
use std::path::Path;
use std::time::Instant;

fn get_dir(dir: &Path) -> Vec<String> {
    let mut result = Vec::new();
    if dir.is_dir() {
        for entry in read_dir(dir).unwrap() {
            let entry = entry.unwrap();
            let path = entry.path();
            if path.is_dir() {
                result.append(&mut get_dir(&path));
            } else {
                result.push(String::from(entry.path().to_str().unwrap()));
            }
        }
    }

    return result;
}

fn main() -> Result<(), GrayError> {
    let args: Vec<String> = env::args().collect();

    for i in 1..args.len() {
        let arg = &args[i];

        let md = metadata(arg).unwrap();

        let mut files = Vec::new();
        if md.is_dir() {
            files.append(&mut get_dir(Path::new(arg)));
        } else {
            files.push(arg.clone());
        }

        for file in files {
            #[cfg(debug_assertions)]
            println!("Running file {:?}", file);
            #[cfg(debug_assertions)]
            let now = Instant::now();

            let functions = declare_functions();

            let mut interpreter = gray::load_file(&file, functions)?;
            #[cfg(debug_assertions)]
            println! {"Compilation took {}ms", now.elapsed().as_millis()}

            #[cfg(debug_assertions)]
            let now = Instant::now();
            interpreter.run(None);

            #[cfg(debug_assertions)]
            println! {"Execution took {}ms", now.elapsed().as_millis()}
        }
    }

    Ok({})
}
