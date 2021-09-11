use crate::bytecode::code_block::CodeBlock;
use crate::bytecode::instructions::instructions::Instruction;
use crate::interpreter::executor::to_string;
use std::collections::HashMap;

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
        match ins {
            Instruction::PopScope => cur_indent -= 2,
            _ => {}
        }

        indent(cur_indent);
        println!("// Segment ${:?}", block.code_mapping[idx]);
        indent(cur_indent);
        println!("{}", to_string(ins));

        match ins {
            Instruction::PushScope => cur_indent += 2,
            _ => {}
        }

        idx += 1;
    }
}

pub fn indent(count: usize) {
    for _ in 0..count {
        print!(" ");
    }
}
