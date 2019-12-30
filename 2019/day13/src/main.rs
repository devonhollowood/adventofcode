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

#[derive(Debug)]
struct GameState {
    board: HashMap<(isize, isize), Tile>,
    score: Option<isize>,
}

impl GameState {
    fn ball_pos(&self) -> (isize, isize) {
        *self
            .board
            .iter()
            .find(|(_, ty)| **ty == Ball)
            .expect("No ball found")
            .0
    }

    fn paddle_x(&self) -> isize {
        self.board
            .iter()
            .find(|(_, ty)| **ty == Paddle)
            .expect("No paddle found")
            .0
             .0
    }
}

fn read_gamestate(state: &[isize]) -> GameState {
    let mut score = None;
    let board = state
        .chunks(3)
        .filter_map(|chunk| match chunk {
            [x, y, ty] => {
                if (*x, *y) == (-1, 0) {
                    score = Some(*ty);
                    None
                } else {
                    Some(((*x, *y), Tile::from(*ty)))
                }
            }
            _ => panic!("wrong length gamestate"),
        })
        .collect();
    GameState { board, score }
}

fn part1(tape: &[isize]) -> usize {
    let output = intcode::Interpreter::new(tape.to_vec())
        .with_input(&[1])
        .run()
        .unwrap_or_else(|err| panic!("Error running intcode: {:?}", err));
    read_gamestate(output.output())
        .board
        .values()
        .filter(|ty| **ty == Block)
        .count()
}

fn part2(tape: &[isize]) -> isize {
    let mut tape = tape.to_owned();
    tape[0] = 2;
    let mut bytes_read = 0;
    let mut interpreter = intcode::Interpreter::new(tape).with_input(&[0]);
    let mut last_ball_x: Option<isize> = None;
    let mut state = GameState {
        board: HashMap::new(),
        score: None,
    };
    while let Err((err, prog)) = interpreter.clone().run() {
        if err != intcode::ProgramError::NoMoreInput {
            panic!("Unrecoverable error: {:?}", err);
        }
        {
            let update = read_gamestate(&prog.output()[bytes_read..]);
            for (pos, tile) in update.board.iter() {
                state.board.insert(*pos, *tile);
            }
            state.score = update.score.or(state.score);
        }
        dbg!(state.board.values().filter(|tile| **tile == Block).count());
        if state.board.values().filter(|tile| **tile == Block).count() == 0 {
            break;
        }
        let new_ball_pos = state.ball_pos();
        let expected_ball_x = if let Some(last_ball_x) = last_ball_x {
            let ball_next = (
                new_ball_pos.0 + (new_ball_pos.0 - last_ball_x).signum(),
                new_ball_pos.1,
            );
            if *state.board.get(&ball_next).unwrap_or(&Empty) == Wall {
                last_ball_x
            } else {
                ball_next.0
            }
        } else {
            new_ball_pos.0
        };
        let input = (expected_ball_x - state.paddle_x()).signum();
        dbg!(last_ball_x);
        dbg!(new_ball_pos.0);
        dbg!(expected_ball_x);
        dbg!(state.paddle_x());
        dbg!(input);
        dbg!(state.score);
        bytes_read = prog.output().len();
        last_ball_x = Some(new_ball_pos.0);
        interpreter = prog.with_input(&[input]);
    }
    state.score.unwrap_or(0)
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
    println!("Part 2: {}", part2(&input));
}

#[derive(StructOpt)]
struct Opt {
    #[structopt(parse(from_os_str))]
    input: PathBuf,
}
