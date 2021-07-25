use gray::GrayError;
use std::time::Instant;
use std::env;
use std::fs::{metadata, read_dir};
use std::path::Path;
use gray::built_in_functions::declare_functions;

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
            println!("Running file {:?}", file);

            let now = Instant::now();

            let functions = declare_functions();

            let mut interpreter = gray::load_file(&file, functions)?;
            println! {"Compilation took {}ms", now.elapsed().as_millis()}


            let now = Instant::now();
            println!("Starting execution");
            interpreter.run(None);
            println! {"Execution took {}ms", now.elapsed().as_millis()}
        }
    }

    Ok({})
}