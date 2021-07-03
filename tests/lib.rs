use built_in_functions::declare_functions;

fn run_file(file: &str) {
    let mut interpreter = gray::load_file(file).unwrap();
    declare_functions(&mut interpreter);
    interpreter.run(None);
}

#[test]
fn array_test() {
    run_file("./tests/array_test.gray");
}

#[test]
fn basic_test() {
    run_file("./tests/basic_test.gray");
}

#[test]
fn if_statement_test() {
    run_file("./tests/if_statement_test.gray");
}

#[test]
fn math_precedence_test() {
    run_file("./tests/math_precedence_test.gray");
}

#[test]
fn math_test() {
    run_file("./tests/math_test.gray");
}

#[test]
fn namespace_test() {
    run_file("./tests/namespace_test.gray");
}

#[test]
fn object_test() {
    run_file("./tests/object_test.gray");
}

#[test]
fn io_test() {
    run_file("./tests/io_test.gray");
}

#[test]
fn break_continue_test() {
    run_file("./tests/break_continue_test.gray");
}
