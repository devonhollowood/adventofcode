#[derive(Debug)]
pub struct ComputerState {
    current: bool,
    left: Vec<bool>,
    right: Vec<bool>,
}

impl ComputerState {
    pub fn new() -> ComputerState {
        ComputerState {
            current: false,
            left: vec![],
            right: vec![],
        }
    }
    pub fn read(&self) -> bool {
        self.current
    }
    pub fn write(&mut self, value: bool) {
        self.current = value;
    }
    pub fn move_left(&mut self) {
        self.right.push(self.current);
        self.current = self.left.pop().unwrap_or_default();
    }
    pub fn move_right(&mut self) {
        self.left.push(self.current);
        self.current = self.right.pop().unwrap_or_default();
    }
    pub fn checksum(&self) -> usize {
        self.right
            .iter()
            .chain(self.left.iter())
            .chain(::std::iter::once(&self.current))
            .filter(|b| **b)
            .count()
    }
}
