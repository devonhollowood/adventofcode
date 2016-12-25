#[macro_use]
extern crate clap;

extern crate regex;

use std::io::prelude::*;
use std::fs::File;
use std::collections::{VecDeque, BTreeSet, BTreeMap};
use regex::Regex;

type ElementId = usize;
type FloorNum = usize;

const BOTTOM_FLOOR: FloorNum = 0;
const TOP_FLOOR: FloorNum = 3;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Item {
    Rtg(ElementId),
    Chip(ElementId),
}

impl Item {
    fn is_rtg(&self) -> bool {
        match *self {
            Item::Rtg(_) => true,
            Item::Chip(_) => false,
        }
    }
    fn is_chip(&self) -> bool {
        match *self {
            Item::Chip(_) => true,
            Item::Rtg(_) => false,
        }
    }
    fn element_id(&self) -> ElementId {
        match *self {
            Item::Chip(id) | Item::Rtg(id) => id,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Direction {
    Up,
    Down,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Move {
    item1: Item,
    item2: Option<Item>,
    direction: Direction,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
struct CompareState {
    current_floor: FloorNum,
    pairs: Vec<(FloorNum, FloorNum)>, /* list of pairs, and each element within
                                       * each pair, must be kept in order */
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct State {
    moves: Vec<Move>,
    current_floor: FloorNum,
    floors: Vec<BTreeSet<Item>>,
}

impl State {
    fn compare(&self) -> CompareState {
        let mut pairs = Vec::new();
        let mut unpaired = BTreeMap::new();
        for floor in 0..self.floors.len() {
            for item in &self.floors[floor] {
                if let Some(paired_floor) = unpaired.remove(&item.element_id()) {
                    if floor <= paired_floor {
                        pairs.push((floor, paired_floor));
                    } else {
                        pairs.push((paired_floor, floor));
                    }
                } else {
                    unpaired.insert(item.element_id(), floor);
                }
            }
        }
        if !unpaired.is_empty() {
            panic!("Set contained unpaired items! {:?}", unpaired);
        }
        pairs.sort();
        CompareState {
            current_floor: self.current_floor,
            pairs: pairs,
        }
    }
    fn possible_moves(&self) -> Vec<Move> {
        let mut moves = Vec::new();
        let floor_items = &self.floors[self.current_floor];
        for item1 in floor_items.iter().cloned() {
            for item2 in floor_items.iter()
                .cloned()
                .map(Some)
                .chain([None].iter().cloned()) {
                if item2 == Some(item1) {
                    continue;
                }
                if self.current_floor != TOP_FLOOR {
                    moves.push(Move {
                        item1: item1,
                        item2: item2,
                        direction: Direction::Up,
                    })
                }
                if self.current_floor != BOTTOM_FLOOR {
                    moves.push(Move {
                        item1: item1,
                        item2: item2,
                        direction: Direction::Down,
                    })
                }
            }
        }
        moves
    }
    fn make_move(&self, mov: Move) -> State {
        if self.current_floor == BOTTOM_FLOOR && mov.direction == Direction::Down ||
           self.current_floor == TOP_FLOOR && mov.direction == Direction::Up {
            panic!("tried to go to invalid floor");
        }
        let mut next = self.clone();
        next.current_floor = match mov.direction {
            Direction::Up => self.current_floor + 1,
            Direction::Down => self.current_floor - 1,
        };
        next.floors[self.current_floor].remove(&mov.item1);
        next.floors[next.current_floor].insert(mov.item1);
        if let Some(item) = mov.item2 {
            next.floors[self.current_floor].remove(&item);
            next.floors[next.current_floor].insert(item);
        }
        next.moves.push(mov);
        next
    }
    fn is_valid(&self) -> bool {
        for chip in self.floors[self.current_floor].iter().filter(|it| it.is_chip()) {
            let mut shielded = false;
            let mut in_danger = false;
            for rtg in self.floors[self.current_floor].iter().filter(|it| it.is_rtg()) {
                if chip.element_id() == rtg.element_id() {
                    shielded = true
                } else {
                    in_danger = true
                }
            }
            if !shielded && in_danger {
                return false;
            }
        }
        true
    }
    fn end_state(&self) -> CompareState {
        let mut end = State {
            moves: Vec::new(),
            current_floor: TOP_FLOOR,
            floors: vec![BTreeSet::new(); self.floors.len()],
        };
        for item in self.floors.iter().flat_map(|floor| floor.iter()) {
            end.floors[TOP_FLOOR].insert(*item);
        }
        end.compare()
    }
}

fn solve(begin: &State) -> Option<Vec<Move>> {
    let end = begin.end_state();
    let mut state_queue = VecDeque::new();
    state_queue.push_back(begin.clone());
    let mut visited = BTreeSet::new();
    visited.insert(begin.compare());
    while let Some(state) = state_queue.pop_front() {
        for mov in state.possible_moves() {
            let possibility = state.make_move(mov);
            let compare = possibility.compare();
            if possibility.is_valid() && !visited.contains(&compare) {
                if possibility.compare() == end {
                    return Some(possibility.moves);
                }
                state_queue.push_back(possibility);
                visited.insert(compare);
            }
        }
    }
    None
}

fn parse_state(s: &str) -> Result<(State, BTreeMap<String, ElementId>), String> {
    let microchip_re = Regex::new(r"(?P<elem>[:alpha:]+)-compatible microchip").unwrap();
    let rtg_re = Regex::new(r"(?P<elem>[:alpha:]+) generator").unwrap();
    let mut elem_ids = BTreeMap::new();
    let mut floors = vec![BTreeSet::new(); 4];
    let mut current_id = 0;
    let mut lines = s.lines();
    for mut floor in &mut floors {
        let line = match lines.next() {
            Some(l) => l,
            None => return Err("Not enough lines".into()),
        };
        for cap in microchip_re.captures_iter(line) {
            let elem_name = &cap["elem"];
            if !elem_ids.contains_key(elem_name) {
                elem_ids.insert(elem_name.to_owned(), current_id);
                current_id += 1;
            }
            floor.insert(Item::Chip(elem_ids[elem_name]));
        }
        for cap in rtg_re.captures_iter(line) {
            let elem_name = &cap["elem"];
            if !elem_ids.contains_key(elem_name) {
                elem_ids.insert(elem_name.to_owned(), current_id);
                current_id += 1;
            }
            floor.insert(Item::Rtg(elem_ids[elem_name]));
        }
    }
    Ok((State {
        moves: Vec::new(),
        current_floor: 0,
        floors: floors,
    },
        elem_ids))
}

fn parse_args() -> std::io::Result<String> {
    let matches = clap::App::new("Day 11")
        .author("Devon Hollowood")
        .arg(clap::Arg::with_name("manifest")
            .index(1)
            .short("f")
            .long("manifest")
            .help("manifest to solve. Reads from stdin otherwise")
            .takes_value(true))
        .get_matches();
    let source = matches.value_of_os("manifest");
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

fn move_string(mov: &Move, id_elems: &BTreeMap<ElementId, String>) -> String {
    use std::fmt::Write;
    fn item_type(item: &Item) -> &str {
        match *item {
            Item::Rtg(_) => "RTG",
            Item::Chip(_) => "microchip",
        }
    }
    let mut s = String::new();
    write!(s,
           "Move {} {} ",
           id_elems[&mov.item1.element_id()],
           item_type(&mov.item1))
        .unwrap();
    if let Some(item) = mov.item2 {
        write!(s,
               "and {} {} ",
               id_elems[&item.element_id()],
               item_type(&item))
            .unwrap();
    }
    write!(s,
           "{}",
           match mov.direction {
               Direction::Up => "up",
               Direction::Down => "down",
           })
        .unwrap();
    s
}

fn main() {
    let input = parse_args().unwrap_or_else(|err| panic!("Error reading input: {}", err));
    let (begin1, mut elem_ids) = parse_state(&input)
        .unwrap_or_else(|err| panic!("Error parsing input: {}", err));
    let next_id = elem_ids.values().cloned().max().unwrap_or_default() + 1;
    elem_ids.insert("elerium".to_owned(), next_id);
    elem_ids.insert("dilithium".to_owned(), next_id + 1);
    let id_elems = elem_ids.into_iter().map(|(elem, id)| (id, elem)).collect();
    let mut begin2 = begin1.clone();
    begin2.floors[BOTTOM_FLOOR].extend(vec![Item::Rtg(next_id),
                                            Item::Chip(next_id),
                                            Item::Rtg(next_id + 1),
                                            Item::Chip(next_id + 1)]);
    let puzzles = [begin1, begin2];
    for puzzle_num in 1..puzzles.len() + 1 {
        match solve(&puzzles[puzzle_num - 1]) {
            Some(moves) => {
                println!("Part {} solution:", puzzle_num);
                for mov in &moves {
                    println!("{}", move_string(&mov, &id_elems));
                }
                println!("Number of moves (part {}): {}", puzzle_num, moves.len());
            }
            None => println!("No part {} solution found!", puzzle_num),
        }
        println!();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn example() {
        use std::iter::FromIterator;
        let begin = State {
            moves: Vec::new(),
            current_floor: BOTTOM_FLOOR,
            floors: vec![
                BTreeSet::from_iter(vec![Item::Chip(0), Item::Chip(1)]),
                BTreeSet::from_iter(vec![Item::Rtg(0)]),
                BTreeSet::from_iter(vec![Item::Rtg(1)]),
                BTreeSet::new(),
            ],
        };
        if let Some(solution) = solve(&begin) {
            assert_eq!(solution.len(), 11)
        } else {
            panic!("No solution found!")
        }
    }
}
