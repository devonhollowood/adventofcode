extern crate clap;
extern crate regex;

use std::io::prelude::*;
use std::fs::File;

use regex::Regex;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum Pixel {
    On,
    Off,
}

impl Pixel {
    #[inline(always)]
    fn is_on(&self) -> bool {
        match *self {
            Pixel::On => true,
            Pixel::Off => false,
        }
    }
}

#[derive(Debug)]
struct Screen {
    rows: Vec<Vec<Pixel>>,
}

impl Screen {
    fn new(width: usize, height: usize) -> Screen {
        Screen { rows: vec![vec![Pixel::Off; width]; height] }
    }
    fn apply(&mut self, instr: &Instruction) {
        match *instr {
            Instruction::Rect(width, height) => self.rect(width, height),
            Instruction::RotRow(row, amt) => self.rotate_rows(row, amt),
            Instruction::RotCol(col, amt) => self.rotate_cols(col, amt),
        }
    }
    fn rect(&mut self, width: usize, height: usize) {
        for row in self.rows.iter_mut().take(height) {
            for pix in row.iter_mut().take(width) {
                *pix = Pixel::On;
            }
        }
    }
    fn rotate_rows(&mut self, row_num: usize, amount: usize) {
        let len = self.rows[row_num].len();
        let shift = gcd(len, amount);
        for start_idx in 0..shift {
            let temp = self.rows[row_num][start_idx];
            let mut to = start_idx;
            let mut from = (len + to - amount) % len;
            while from != start_idx {
                self.rows[row_num][to] = self.rows[row_num][from];
                to = from;
                from = (len + from - amount) % len;
            }
            self.rows[row_num][to] = temp;
        }
    }
    fn rotate_cols(&mut self, col_num: usize, amount: usize) {
        let len = self.rows.len();
        let shift = gcd(len, amount);
        for start_idx in 0..shift {
            let temp = self.rows[start_idx][col_num];
            let mut to = start_idx;
            let mut from = (len + to - amount) % len;
            while from != start_idx {
                self.rows[to][col_num] = self.rows[from][col_num];
                to = from;
                from = (len + from - amount) % len;
            }
            self.rows[to][col_num] = temp;
        }
    }
    fn display(&self) -> String {
        let mut buf = String::new();
        for row in self.rows.iter() {
            for pix in row.iter() {
                buf.push(if pix.is_on() { '#' } else { '.' });
            }
            buf.push('\n');
        }
        buf
    }
    fn number_lit(&self) -> usize {
        self.rows
            .iter()
            .flat_map(|r| r.iter())
            .filter(|pix| pix.is_on())
            .count()
    }
}

// why is this not already in the standard library?
fn gcd(x: usize, y: usize) -> usize {
    let mut x = x;
    let mut y = y;
    while y != 0 {
        let temp = y;
        y = x % y;
        x = temp;
    }
    x
}

impl std::fmt::Display for Screen {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.write_str(&self.display())
    }
}

#[derive(Debug)]
enum Instruction {
    Rect(usize, usize),
    RotRow(usize, usize),
    RotCol(usize, usize),
}

fn invalid_data(msg: &str) -> std::io::Error {
    std::io::Error::new(std::io::ErrorKind::InvalidData, msg)
}

fn parse_args() -> std::io::Result<Box<Read>> {
    let source = clap::App::new("Day 08")
        .author("Devon Hollowood")
        .arg(clap::Arg::with_name("instructions")
            .index(1)
            .short("f")
            .long("instructions")
            .help("file to read instructions from. Reads from stdin \
                   otherwise")
            .takes_value(true))
        .get_matches()
        .value_of_os("instructions")
        .map(|str| str.to_owned());
    match source {
        Some(filename) => Ok(Box::new(File::open(filename)?)),
        None => Ok(Box::new(std::io::stdin())),
    }
}


fn read_instructions<R: Read>(source: &mut R)
                              -> std::io::Result<Vec<Instruction>> {
    use Instruction::*;
    let rect_re = Regex::new(r"^rect (\d+)x(\d+)$").unwrap();
    let row_re = Regex::new(r"^rotate row y=(\d+) by (\d+)$").unwrap();
    let col_re = Regex::new(r"^rotate column x=(\d+) by (\d+)$").unwrap();
    let mut contents = String::new();
    source.read_to_string(&mut contents)?;
    contents.lines()
        .map(|line| if let Some(caps) = rect_re.captures(line.trim()) {
            Ok(Rect(caps[1].parse().unwrap(), caps[2].parse().unwrap()))
        } else if let Some(caps) = row_re.captures(line.trim()) {
            Ok(RotRow(caps[1].parse().unwrap(), caps[2].parse().unwrap()))
        } else if let Some(caps) = col_re.captures(line.trim()) {
            Ok(RotCol(caps[1].parse().unwrap(), caps[2].parse().unwrap()))
        } else {
            Err(invalid_data(&format!("Bad line: '{}'", line)))
        })
        .collect()
}

fn main() {
    let mut source = parse_args()
        .unwrap_or_else(|err| panic!("Error reading file: {}", err));
    let instructions = read_instructions(&mut source)
        .unwrap_or_else(|err| panic!("Error reading instructions: {}", err));
    let mut screen = Screen::new(50, 6);
    for instruction in instructions {
        screen.apply(&instruction);
    }
    println!("Number of lit pixels: {}", screen.number_lit());
    println!("{}", screen);
}
