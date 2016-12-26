#[macro_use]
extern crate clap;

use std::collections::{BTreeSet, VecDeque};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct Point {
    x: usize,
    y: usize,
}

impl std::fmt::Display for Point {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        write!(f, "({}, {})", self.x, self.y)
    }
}

fn is_wall(Point { x, y }: Point, key: usize) -> bool {
    (x * x + 3 * x + 2 * x * y + y + y * y + key).count_ones() % 2 != 0
}

fn possible_moves(Point { x, y }: Point, key: usize) -> Vec<Point> {
    let mut moves = Vec::new();
    if !is_wall(Point { x: x, y: y + 1 }, key) {
        moves.push(Point { x: x, y: y + 1 })
    }
    if !is_wall(Point { x: x + 1, y: y }, key) {
        moves.push(Point { x: x + 1, y: y })
    }
    if y > 0 && !is_wall(Point { x: x, y: y - 1 }, key) {
        moves.push(Point { x: x, y: y - 1 })
    }
    if x > 0 && !is_wall(Point { x: x - 1, y: y }, key) {
        moves.push(Point { x: x - 1, y: y })
    }
    moves
}

fn shortest_path(start: Point, end: Point, key: usize) -> Option<Vec<Point>> {
    let mut move_queue = VecDeque::new();
    move_queue.push_back((start, Vec::new()));
    let mut visited = BTreeSet::new();
    while let Some((pos, moves)) = move_queue.pop_front() {
        for mov in possible_moves(pos, key) {
            if visited.contains(&mov) {
                continue;
            }
            let new_moves = moves.iter().cloned().chain(std::iter::once(mov)).collect();
            if mov == end {
                return Some(new_moves);
            }
            visited.insert(mov);
            move_queue.push_back((mov, new_moves));
        }
    }
    None
}

fn parse_args() -> std::io::Result<usize> {
    let matches = clap::App::new("Day 13")
        .author("Devon Hollowood")
        .arg(clap::Arg::with_name("key")
            .index(1)
            .short("k")
            .long("key")
            .help("key for finding path")
            .required(true)
            .takes_value(true))
        .get_matches();
    matches.value_of("key")
        .unwrap()
        .parse()
        .map_err(|err| std::io::Error::new(std::io::ErrorKind::InvalidData, err))
}

fn main() {
    let key = parse_args().unwrap_or_else(|err| panic!("Error reading args: {}", err));
    println!("part 1 path:");
    match shortest_path(Point { x: 1, y: 1 }, Point { x: 31, y: 39 }, key) {
        Some(path) => {
            for mov in &path {
                println!("{}", mov);
            }
            println!("Total length: {}", path.len());
        }
        None => println!("No path found!"),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn example_path() {
        assert_eq!(shortest_path(Point { x: 1, y: 1 }, Point { x: 7, y: 4 }, 10)
                       .expect("No path found")
                       .len(),
                   11)
    }
}
