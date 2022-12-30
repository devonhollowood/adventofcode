use anyhow::{bail, Result};
use regex::bytes::Regex;

pub type Stack = Vec<u8>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Instruction {
    count: usize,
    from: usize,
    to: usize,
}

impl Instruction {
    fn apply1(&self, stacks: &mut [Stack]) {
        for _ in 0..self.count {
            match stacks[self.from - 1].pop() {
                Some(popped) => stacks[self.to - 1].push(popped),
                None => break,
            }
        }
    }

    fn apply2(&self, stacks: &mut [Stack]) {
        let pos = stacks[self.from - 1].len().saturating_sub(self.count);
        let popped = stacks[self.from - 1].split_off(pos);
        stacks[self.to - 1].extend_from_slice(&popped);
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Puzzle {
    stacks: Vec<Stack>,
    instructions: Vec<Instruction>,
}

impl Puzzle {
    fn new() -> Self {
        Self {
            stacks: Vec::new(),
            instructions: Vec::new(),
        }
    }
}

pub fn parse(input: &str) -> Result<Puzzle> {
    let stack_line = Regex::new(r"\[[A-Z]\]").unwrap();
    let stack_name_line = Regex::new(r"^ 1").unwrap();
    let instr_line = Regex::new(r"move (\d+) from (\d) to (\d)").unwrap();
    let mut puzzle = Puzzle::new();
    for line in input.lines() {
        let line = line.as_bytes();
        if line.is_empty() {
            continue;
        }
        if stack_line.is_match(line) {
            for mat in stack_line.find_iter(line) {
                let stack_num = mat.start() / 4;
                if puzzle.stacks.len() <= stack_num {
                    puzzle.stacks.resize(stack_num + 1, Stack::new());
                }
                puzzle.stacks[stack_num].push(mat.as_bytes()[1]);
            }
        } else if stack_name_line.is_match(line) {
            continue;
        } else if let Some(caps) = instr_line.captures(line) {
            let count = unsafe { std::str::from_utf8_unchecked(&caps[1]) }.parse()?;
            let from = unsafe { std::str::from_utf8_unchecked(&caps[2]) }.parse()?;
            let to = unsafe { std::str::from_utf8_unchecked(&caps[3]) }.parse()?;
            let instr = Instruction { count, from, to };
            puzzle.instructions.push(instr);
        } else {
            bail!("Invalid line: {}", unsafe {
                std::str::from_utf8_unchecked(line)
            });
        }
    }
    for stack in puzzle.stacks.iter_mut() {
        stack.reverse();
    }
    Ok(puzzle)
}

pub fn part1(input: &Puzzle) -> String {
    let mut stacks = input.stacks.clone();
    for instr in input.instructions.iter() {
        instr.apply1(&mut stacks);
    }
    let mut result = Vec::new();
    for mut stack in stacks {
        if let Some(ch) = stack.pop() {
            result.push(ch);
        }
    }
    unsafe { String::from_utf8_unchecked(result) }
}

pub fn part2(input: &Puzzle) -> String {
    let mut stacks = input.stacks.clone();
    for instr in input.instructions.iter() {
        instr.apply2(&mut stacks);
    }
    let mut result = Vec::new();
    for mut stack in stacks {
        if let Some(ch) = stack.pop() {
            result.push(ch);
        }
    }
    unsafe { String::from_utf8_unchecked(result) }
}

#[cfg(test)]
mod tests {
    use super::*;
    const INPUTS: &str = include_str!("test-data/day05.txt");

    #[test]
    fn test_parse() {
        assert_eq!(
            parse(INPUTS).unwrap(),
            Puzzle {
                stacks: vec![vec![b'Z', b'N'], vec![b'M', b'C', b'D'], vec![b'P']],
                instructions: vec![
                    Instruction {
                        count: 1,
                        from: 2,
                        to: 1
                    },
                    Instruction {
                        count: 3,
                        from: 1,
                        to: 3
                    },
                    Instruction {
                        count: 2,
                        from: 2,
                        to: 1
                    },
                    Instruction {
                        count: 1,
                        from: 1,
                        to: 2
                    }
                ],
            }
        );
    }

    #[test]
    fn test_part1() {
        let parsed = parse(INPUTS).unwrap();
        assert_eq!(part1(&parsed), "CMZ");
    }

    #[test]
    fn test_part2() {
        let parsed = parse(INPUTS).unwrap();
        assert_eq!(part2(&parsed), "MCD");
    }
}
