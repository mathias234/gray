use std::fmt;

#[derive(Copy, Clone, PartialEq)]
pub struct Register {
    pub index: usize,
}

impl Register {
    pub fn new(index: usize) -> Register {
        Register { index }
    }
}

impl fmt::Display for Register {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "(r: {})", self.index)
    }
}
