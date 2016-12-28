extern crate clap;
extern crate regex;

#[macro_use]
extern crate lazy_static;

use std::io::prelude::*;
use std::fs::File;
use regex::Regex;

#[derive(Debug)]
struct Password {
    chars: Vec<u8>,
}

impl Password {
    fn scramble(&mut self, instruction: &Instruction) {
        use Instruction::*;
        match *instruction {
            SwapPos(m, n) => self.chars.swap(m, n),
            SwapLet(a, b) => {
                // record positions of `a`s
                let a_pos: Vec<_> = self.chars
                    .iter()
                    .cloned()
                    .enumerate()
                    .filter(|&(_, ch)| ch == a)
                    .map(|(n, _)| n)
                    .collect();
                // replace `b`s with `a`s
                for ch in &mut self.chars.iter_mut() {
                    if *ch == b {
                        *ch = a;
                    }
                }
                // replace recorded `a`s with `b`s
                for pos in a_pos {
                    self.chars[pos] = b;
                }
            }
            RotLeft(amt) => rotate_left(&mut self.chars, amt),
            RotRight(amt) => rotate_right(&mut self.chars, amt),
            RotLet(ch) => {
                let mut idx = None;
                for i in 0..self.chars.len() {
                    if self.chars[i] == ch {
                        idx = Some(i);
                    }
                }
                if let Some(i) = idx {
                    let shift = 1 + i + if i >= 4 { 1 } else { 0 };
                    rotate_right(&mut self.chars, shift);
                }
            }
            RevPos(m, n) => {
                if m <= n {
                    self.chars[m..n + 1].reverse()
                } else {
                    self.chars[n..m + 1].reverse()
                }
            }
            Mov(m, n) => {
                if m <= n {
                    rotate_left(&mut self.chars[m..n + 1], 1);
                } else {
                    rotate_right(&mut self.chars[n..m + 1], 1);
                }
            }
        }
    }
}

impl std::str::FromStr for Password {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Password { chars: s.bytes().collect() })
    }
}

impl std::fmt::Display for Password {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        write!(f,
               "{}",
               std::str::from_utf8(&self.chars).map_err(|_| std::fmt::Error::default())?)
    }
}

#[derive(Debug)]
enum Instruction {
    SwapPos(usize, usize),
    SwapLet(u8, u8),
    RotLeft(usize),
    RotRight(usize),
    RotLet(u8),
    RevPos(usize, usize),
    Mov(usize, usize),
}

impl std::str::FromStr for Instruction {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        lazy_static! {
            static ref SWAPPOS_RE: Regex =
                Regex::new(r"swap position (\d+) with position (\d+)")
                .expect("bad regex SWAPPOS_RE");
            static ref SWAPLET_RE: Regex =
                Regex::new(r"swap letter ([A-Za-z]) with letter ([A-Za-z])")
                .expect("bad regex SWAPLET_RE");
            static ref ROTLEFT_RE: Regex =
                Regex::new(r"rotate left (\d+) steps?")
                .expect("bad regex ROTLEFT_RE");
            static ref ROTRIGHT_RE: Regex =
                Regex::new(r"rotate right (\d+) steps?")
                .expect("bad regex ROTRIGHT_RE");
            static ref ROTLET_RE: Regex =
                Regex::new(r"rotate based on position of letter ([A-Za-z])")
                .expect("bad regex ROTLET_RE");
            static ref REVPOS_RE: Regex =
                Regex::new(r"reverse positions (\d+) through (\d+)")
                .expect("bad regex REVPOS_RE");
            static ref MOV_RE: Regex =
                Regex::new(r"move position (\d+) to position (\d+)")
                .expect("bad regex MOV_RE");
        }
        use Instruction::*;
        if let Some(caps) = SWAPPOS_RE.captures(s) {
            Ok(SwapPos(caps[1].parse().unwrap(), caps[2].parse().unwrap()))
        } else if let Some(caps) = SWAPLET_RE.captures(s) {
            Ok(SwapLet(caps[1].bytes().next().unwrap(),
                       caps[2].bytes().next().unwrap()))
        } else if let Some(caps) = ROTLEFT_RE.captures(s) {
            Ok(RotLeft(caps[1].parse().unwrap()))
        } else if let Some(caps) = ROTRIGHT_RE.captures(s) {
            Ok(RotRight(caps[1].parse().unwrap()))
        } else if let Some(caps) = ROTLET_RE.captures(s) {
            Ok(RotLet(caps[1].bytes().next().unwrap()))
        } else if let Some(caps) = REVPOS_RE.captures(s) {
            Ok(RevPos(caps[1].parse().unwrap(), caps[2].parse().unwrap()))
        } else if let Some(caps) = MOV_RE.captures(s) {
            Ok(Mov(caps[1].parse().unwrap(), caps[2].parse().unwrap()))
        } else {
            Err(format!("Could not parse instruction from '{}'", s))
        }
    }
}

fn gcd(mut a: usize, mut b: usize) -> usize {
    while b != 0 {
        let t = b;
        b = a % b;
        a = t;
    }
    a
}

fn rotate_left<T: Clone + std::fmt::Display>(slice: &mut [T], amount: usize) {
    let len = slice.len();
    let amount = amount % len; // normalize
    let shift = gcd(len, amount);
    for start_idx in 0..shift {
        let temp = slice[start_idx].clone();
        let mut to = start_idx;
        let mut from = (to + amount) % len;
        while from != start_idx {
            slice[to] = slice[from].clone();
            to = from;
            from = (to + amount) % len;
        }
        slice[to] = temp;
    }
}

fn rotate_right<T: Clone>(slice: &mut [T], amount: usize) {
    let len = slice.len();
    let amount = amount % len; // normalize
    let shift = gcd(len, amount);
    for start_idx in 0..shift {
        let temp = slice[start_idx].clone();
        let mut to = start_idx;
        let mut from = (len + to - amount) % len;
        while from != start_idx {
            slice[to] = slice[from].clone();
            to = from;
            from = (len + to - amount) % len;
        }
        slice[to] = temp;
    }
}

fn parse_args() -> std::io::Result<String> {
    let matches = clap::App::new("Day 20")
        .author("Devon Hollowood")
        .arg(clap::Arg::with_name("IP ranges")
            .index(1)
            .help("file containing IP ranges for puzzle. Reads from stdin otherwise"))
        .get_matches();
    let source = matches.value_of_os("IP ranges");
    let mut contents = String::new();
    match source {
        Some(filename) => {
            let mut input = File::open(filename)?;
            input.read_to_string(&mut contents)?;
        }
        None => {
            let mut input = std::io::stdin();
            input.read_to_string(&mut contents)?;
        }
    }
    Ok(contents)
}

fn main() {
    let input = parse_args().unwrap_or_else(|err| panic!("Error reading args: {}", err));
    let instructions: Vec<Instruction> = input.lines()
        .map(|line| line.trim().parse())
        .collect::<Result<_, _>>()
        .unwrap_or_else(|err| panic!("Error parsing ranges: {}", err));
    let mut password = "abcdefgh".parse::<Password>().unwrap();
    for instruction in instructions {
        password.scramble(&instruction);
    }
    println!("Scramble: {}", password);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn example() {
        let mut password = "abcde".parse::<Password>().unwrap();
        let instructions: Vec<_> = ["swap position 4 with position 0",
                                    "swap letter d with letter b",
                                    "reverse positions 0 through 4",
                                    "rotate left 1 step",
                                    "move position 1 to position 4",
                                    "move position 3 to position 0",
                                    "rotate based on position of letter b",
                                    "rotate based on position of letter d"]
            .iter()
            .map(|s| s.parse().unwrap())
            .collect();
        for instruction in instructions {
            password.scramble(&instruction);
        }
        assert_eq!(std::str::from_utf8(&password.chars), Ok("decab"));
    }
}
