use std::collections::HashMap;
use std::path::PathBuf;
use structopt::StructOpt;

#[derive(Debug, Clone, Copy)]
enum Direction {
    North,
    East,
    South,
    West,
}

use Direction::*;

impl Direction {
    fn left(self) -> Direction {
        match self {
            North => West,
            East => North,
            South => East,
            West => South,
        }
    }

    fn right(self) -> Direction {
        match self {
            North => East,
            East => South,
            South => West,
            West => North,
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum Color {
    Black,
    White,
}

use Color::*;

impl Color {
    fn as_isize(self) -> isize {
        match self {
            Black => 0,
            White => 1,
        }
    }
    fn from_isize(n: isize) -> Color {
        match n {
            0 => Black,
            1 => White,
            _ => panic!("Invalid color: {}", n),
        }
    }
}

struct Bot {
    interpreter: intcode::Interpreter,
    position: (isize, isize),
    facing: Direction,
}

impl Bot {
    pub fn new(tape: &[isize]) -> Bot {
        Bot {
            interpreter: intcode::Interpreter::new(tape.to_owned()),
            position: (0, 0),
            facing: North,
        }
    }

    fn turn_and_advance(&mut self, turn: isize) {
        self.facing = match turn {
            0 => self.facing.left(),
            1 => self.facing.right(),
            n => panic!("invalid facing: {}", n),
        };
        self.position = match self.facing {
            North => (self.position.0, self.position.1 + 1),
            East => (self.position.0 + 1, self.position.1),
            South => (self.position.0, self.position.1 - 1),
            West => (self.position.0 - 1, self.position.1),
        }
    }

    pub fn run(mut self, grid: &mut HashMap<(isize, isize), Color>) {
        let mut ints_read = 0;
        while let Err((err, prog)) = self
            .interpreter
            .clone()
            .with_input(&[grid.entry(self.position).or_insert(Black).as_isize()])
            .run()
        {
            if err != intcode::ProgramError::NoMoreInput {
                panic!("Unexpected program error: {:?}", err);
            }
            let painted = Color::from_isize(prog.output()[ints_read]);
            grid.insert(self.position, painted);
            self.turn_and_advance(prog.output()[ints_read + 1]);
            self.interpreter = prog;
            ints_read += 2;
        }
    }
}

fn part1(tape: &[isize]) -> usize {
    let bot = Bot::new(&tape);
    let mut grid = HashMap::new();
    bot.run(&mut grid);
    grid.len()
}

fn part2(tape: &[isize]) -> String {
    let bot = Bot::new(&tape);
    let mut grid = HashMap::new();
    grid.insert((0, 0), White);
    bot.run(&mut grid);
    let grid = grid; // immutable from here
    assert!(grid.len() > 0);
    let bot = grid.keys().map(|k| k.1).min().unwrap();
    let top = grid.keys().map(|k| k.1).max().unwrap();
    let left = grid.keys().map(|k| k.0).min().unwrap();
    let right = grid.keys().map(|k| k.0).max().unwrap();
    let mut out = String::new();
    for y in (bot..=top).rev() {
        for x in left..=right {
            out.push(match grid.get(&(x, y)).unwrap_or(&Black) {
                Black => '.',
                White => '#',
            });
        }
        out.push('\n');
    }
    out
}

fn main() {
    let input: Vec<_> = std::fs::read_to_string(Opt::from_args().input)
        .expect("error reading file")
        .split(',')
        .map(|num| {
            num.trim()
                .parse::<isize>()
                .unwrap_or_else(|err| panic!("could not parse token \"{}\": {:?}", num, err))
        })
        .collect();
    println!("Part 1: {}", part1(&input));
    println!("Part 2: \n{}", part2(&input));
}

#[derive(StructOpt)]
struct Opt {
    #[structopt(parse(from_os_str))]
    input: PathBuf,
}
