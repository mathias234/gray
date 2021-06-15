use std::fmt;

#[derive(Copy, Clone)]
pub struct Label {
    pub position: usize,
}

impl Label {
    pub fn new(position: usize) -> Label {
        Label { position }
    }
}

impl fmt::Display for Label {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "(l: {})", self.position)
    }
}
