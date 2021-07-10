pub fn print_error_line(line: &str, line_nr: usize, from_col: usize, to_col: usize) {
    let (trim_count, line) = trim_begin_with_count(line);

    let first_segment = format!("{:04}", line_nr);
    println!("{} |    {}", first_segment, line);

    for _ in 0..first_segment.len() {
        print!(" ");
    }
    print!(" |    ");

    let from_col = from_col - trim_count;
    let to_col = to_col - trim_count;

    for i in 0..line.len() {
        if i >= to_col - 1 {
            break;
        }
        if i >= from_col - 1 {
            print!("^");
        } else {
            print!(" ");
        }
    }
}

fn trim_begin_with_count(string: &str) -> (usize, &str) {
    let mut count = 0;

    for c in string.chars() {
        if c.is_whitespace() {
            count += 1;
            continue;
        }
        break;
    }

    (count, &string[count..string.len()])
}