use std::collections::HashMap;
use crate::bytecode::code_block::CodeBlock;

pub fn print_blocks_as_code(code: &HashMap<String, CodeBlock>) {
    for (name, block) in code {
        println!("fn {} {{", name);
        print_block(block);
        println!("}}\n");
    }
}

pub fn print_block(block: &CodeBlock) {
    let mut cur_indent = 2;
    let mut idx = 0;
    for ins in block.get_instructions() {
        if ins.to_string() == "PopScope" {
            cur_indent -= 2;
        }

        indent(cur_indent);
        println!("// Segment ${:?}", block.code_mapping[idx]);
        indent(cur_indent);
        println!("{}", ins.to_string());

        if ins.to_string() == "PushScope" {
            cur_indent += 2;
        }

        idx += 1;
    }
}

pub fn indent(count: usize) {
    for _ in 0..count {
        print!(" ");
    }
}
