use hematita::{ast::{lexer, parser}, compiler, vm, lua_lib, lua_tuple};

struct Script {
    code: String,
}

impl Script {
    pub fn from_bytes(bytes: Vec<u8>) -> Script {
        let code = String::from_utf8(bytes).unwrap();
        let lexer = lexer::Lexer {source: code.chars().peekable()}.peekable();
        let parsed = parser::parse_block(&mut parser::TokenIterator(lexer)).unwrap();
        let compiled = compiler::compile_block(&parsed);

        Script { compiled }
    }

    pub fn execute(&self) {
        // Prepare the global scope.
        let global = lua_lib::standard_globals();
        // Create the virtual machine...
        let virtual_machine = vm::VirtualMachine::new(global);
        // And run the byte code.
        return virtual_machine.execute(&compiled.into(), lua_tuple![].arc()).unwrap();
    }
}