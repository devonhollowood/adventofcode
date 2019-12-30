use std::collections::HashMap;
use std::path::PathBuf;
use structopt::StructOpt;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Tile {
    Empty,
    Wall,
    Block,
    Paddle,
    Ball,
}

use Tile::*;

impl std::convert::From<isize> for Tile {
    fn from(n: isize) -> Self {
        match n {
            0 => Empty,
            1 => Wall,
            2 => Block,
            3 => Paddle,
            4 => Ball,
            _ => panic!("Unrecognized tile: {}", n),
        }
    }
}

type GameState = HashMap<(isize, isize), Tile>;

fn read_gamestate(state: &[isize]) -> GameState {
    state
        .chunks(3)
        .map(|chunk| match chunk {
            [x, y, ty] => ((*x, *y), Tile::from(*ty)),
            _ => panic!("wrong length gamestate"),
        })
        .collect()
}

fn part1(tape: &[isize]) -> usize {
    let output = intcode::Interpreter::new(tape.to_vec())
        .with_input(&[1])
        .run()
        .unwrap_or_else(|err| panic!("Error running intcode: {:?}", err));
    read_gamestate(output.output())
        .values()
        .filter(|ty| **ty == Block)
        .count()
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
