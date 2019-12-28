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
            North => (self.position.0 + 1, self.position.1),
            East => (self.position.0, self.position.1 + 1),
            South => (self.position.0 - 1, self.position.1),
            West => (self.position.0, self.position.1 - 1),
        }
    }

    pub fn run(mut self) -> HashMap<(isize, isize), Color> {
        let mut grid = HashMap::new();
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
        grid
    }
}

fn part1(tape: &[isize]) -> usize {
    let bot = Bot::new(&tape);
    bot.run().len()
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
}

#[derive(StructOpt)]
struct Opt {
    #[structopt(parse(from_os_str))]
    input: PathBuf,
}
