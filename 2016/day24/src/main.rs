extern crate clap;

use std::io::prelude::*;
use std::fs::File;
use std::collections::{HashMap, VecDeque};

type Distance = usize;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Point {
    x: usize,
    y: usize,
}

impl std::fmt::Display for Point {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        write!(f, "({}, {})", self.x, self.y)
    }
}

impl Point {
    fn is_interesting(&self, map: &Map) -> bool {
        match map[*self] {
            Element::Wall => false,
            Element::Goal(_) => true,
            Element::Open => map.possible_moves(*self).len() > 2,
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum Element {
    Wall,
    Open,
    Goal(usize),
}

impl Element {
    fn from_char(ch: char) -> Result<Element, String> {
        if let Some(digit) = ch.to_digit(10) {
            Ok(Element::Goal(digit as usize))
        } else if ch == '.' {
            Ok(Element::Open)
        } else if ch == '#' {
            Ok(Element::Wall)
        } else {
            Err(format!("Invalid element: {}", ch))
        }
    }
    fn is_wall(&self) -> bool {
        match *self {
            Element::Wall => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone)]
struct Map {
    elements: Vec<Vec<Element>>,
}

impl Map {
    fn walk_to_interesting_point(&self,
                                 home: Point,
                                 first_step: Point)
                                 -> Option<(Point, Distance)> {
        let mut prev = home;
        let mut current = first_step;
        let mut dist = 1;
        while !current.is_interesting(self) {
            if let Some(next) = self.possible_moves(current)
                .into_iter()
                .filter(|p| *p != prev)
                .next() {
                current = next;
                prev = current;
                dist += 1;
            } else {
                return None;
            }
        }
        Some((current, dist))
    }
    fn interesting_points(&self) -> InterestingPointGraph {
        let mut ips = HashMap::new();
        let mut types = HashMap::new();
        for y in 0..self.elements.len() {
            for x in 0..self.elements[0].len() {
                let point = Point { x: x, y: y };
                if point.is_interesting(self) {
                    let kind = match self[point] {
                        Element::Open => InterestingPoint::Intersection,
                        Element::Goal(n) => InterestingPoint::Goal(n),
                        _ => panic!("Thought a wall was interesting"),
                    };
                    let connections = self.possible_moves(point)
                        .into_iter()
                        .map(|mov| self.walk_to_interesting_point(point, mov))
                        .filter_map(|o| o)
                        .collect::<HashMap<_, _>>();
                    if !connections.is_empty() {
                        ips.insert(point, connections);
                        types.insert(point, kind);
                    }
                }
            }
        }
        InterestingPointGraph {
            points: ips,
            types: types,
        }
    }
    fn possible_moves(&self, Point { x, y }: Point) -> Vec<Point> {
        if self[Point { x: x, y: y }].is_wall() {
            return Vec::new();
        }
        [Point { x: x, y: y - 1 },
         Point { x: x + 1, y: y },
         Point { x: x, y: y + 1 },
         Point { x: x - 1, y: y }]
            .iter()
            .cloned()
            .filter(|point| !self[*point].is_wall())
            .collect()
    }
}

impl std::ops::Index<Point> for Map {
    type Output = Element;
    fn index(&self, Point { x, y }: Point) -> &Self::Output {
        &self.elements[x][y]
    }
}

impl std::str::FromStr for Map {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Map {
            elements: s.lines()
                .map(|line| {
                    line.trim()
                        .chars()
                        .map(|ch| Element::from_char(ch))
                        .collect()
                })
                .collect::<Result<_, _>>()?,
        })
    }
}

#[derive(Debug, Clone, Copy)]
enum InterestingPoint {
    Intersection,
    Goal(usize),
}

#[derive(Debug, Clone)]
struct InterestingPointGraph {
    points: HashMap<Point, HashMap<Point, Distance>>,
    types: HashMap<Point, InterestingPoint>,
}

impl InterestingPointGraph {
    fn start(&self) -> Point {
        unimplemented!()
    }
    fn solve(&self) -> Vec<(Point, Distance)> {
        unimplemented!()
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
    let map = input.parse::<Map>().unwrap_or_else(|err| panic!("Error reading map: {}", err));
    let ips = map.interesting_points();
    let mut total_dist = 0;
    println!("{}", ips.start());
    for (mov, dist) in ips.solve() {
        total_dist += dist;
        println!(" -> {}", mov);
    }
    println!("Total distance: {}", total_dist);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn example() {
        let map = "###########\n#0.1.....2#\n#.#######.#\n#4.......3#\n###########"
            .parse::<Map>()
            .unwrap();
        let ips = map.interesting_points();
        assert_eq!(ips.solve().into_iter().fold(0, |acc, (_, dist)| acc + dist),
                   14);
    }
}
