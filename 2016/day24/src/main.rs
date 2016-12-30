extern crate clap;
extern crate revord;

use revord::RevOrd;
use std::io::prelude::*;
use std::fs::File;
use std::collections::{BTreeMap, BTreeSet, BinaryHeap};

type Distance = usize;
type GoalNum = usize;

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
    Goal(GoalNum),
}

impl Element {
    fn from_char(ch: char) -> Result<Element, String> {
        if let Some(digit) = ch.to_digit(10) {
            Ok(Element::Goal(digit as GoalNum))
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
                prev = current;
                current = next;
                dist += 1;
            } else {
                return None;
            }
        }
        Some((current, dist))
    }
    fn interesting_points(&self) -> InterestingPointGraph {
        let mut ips = BTreeMap::new();
        let mut types = BTreeMap::new();
        for y in 0..self.elements.len() {
            for x in 0..self.elements[0].len() {
                let point = Point { x: x, y: y };
                if point.is_interesting(self) {
                    let kind = match self[point] {
                        Element::Open => InterestingPoint::Intersection,
                        Element::Goal(n) => InterestingPoint::Goal(n),
                        _ => panic!("Thought a wall was interesting"),
                    };
                    let mut connections = BTreeMap::new();
                    for (ip, dist) in self.possible_moves(point)
                        .into_iter()
                        .map(|mov| self.walk_to_interesting_point(point, mov))
                        .filter_map(|o| o) {
                        if !connections.contains_key(&ip) || connections[&ip] > dist {
                            connections.insert(ip, dist);
                        }
                    }
                    if !connections.is_empty() {
                        ips.insert(point, connections);
                        types.insert(point, kind);
                    }
                }
            }
        }
        InterestingPointGraph {
            neighbors: ips,
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
        &self.elements[y][x]
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
    Goal(GoalNum),
}

#[derive(Debug, Clone)]
struct InterestingPointGraph {
    neighbors: BTreeMap<Point, BTreeMap<Point, Distance>>,
    types: BTreeMap<Point, InterestingPoint>,
}

impl InterestingPointGraph {
    fn goals(&self) -> BTreeMap<Point, GoalNum> {
        self.types
            .iter()
            .filter_map(|(point, ip)| match *ip {
                InterestingPoint::Goal(num) => Some((*point, num)),
                InterestingPoint::Intersection => None,
            })
            .collect()
    }
    fn goal_graph(&self) -> GoalGraph {
        GoalGraph {
            goals: self.goals()
                .into_iter()
                .map(|(point, num)| (num, self.shortest_dist_to_goals(point)))
                .collect(),
        }
    }
    fn shortest_dist_to_goals(&self, start: Point) -> BTreeMap<GoalNum, Distance> {
        let mut goals = BTreeMap::new();
        let mut visited = BTreeSet::new();
        let mut queue = BinaryHeap::new();
        queue.push((RevOrd(0), start));
        while let Some((RevOrd(dist_so_far), current_point)) = queue.pop() {
            if visited.contains(&current_point) {
                continue;
            }
            visited.insert(current_point);
            if let InterestingPoint::Goal(n) = self.types[&current_point] {
                if !goals.contains_key(&n) || dist_so_far < goals[&n] {
                    goals.insert(n, dist_so_far);
                }
            }
            for (ip, next_dist) in self.neighbors[&current_point].iter() {
                let total_dist = dist_so_far + next_dist;
                if !visited.contains(ip) {
                    queue.push((RevOrd(total_dist), *ip));
                }
            }
        }
        if let InterestingPoint::Goal(n) = self.types[&start] {
            goals.remove(&n);
        }
        goals
    }
}

#[derive(Debug)]
struct GoalGraph {
    goals: BTreeMap<GoalNum, BTreeMap<GoalNum, Distance>>,
}

impl GoalGraph {
    fn shortest_circuit(&self) -> Distance {
        let mut queue = BinaryHeap::new();
        let initial_obtained: BTreeSet<GoalNum> = std::iter::once(0).collect();
        let all_goals: BTreeSet<GoalNum> = self.goals.keys().cloned().collect();
        let mut visited = BTreeSet::new();
        queue.push((RevOrd(0), 0, initial_obtained));
        while let Some((RevOrd(dist_so_far), current, obtained)) = queue.pop() {
            if visited.contains(&(current, obtained.clone())) {
                continue;
            }
            visited.insert((current, obtained.clone()));
            if obtained == all_goals {
                return dist_so_far;
            }
            for (next, next_dist) in self.goals[&current].iter() {
                if obtained.contains(&next) {
                    continue;
                }
                let total_dist = dist_so_far + next_dist;
                let mut new_obtained = obtained.clone();
                new_obtained.insert(*next);
                if !visited.contains(&(*next, new_obtained.clone())) {
                    queue.push((RevOrd(total_dist), *next, new_obtained));
                }
            }
        }
        panic!("Could not reach all goals!")
    }
    fn shortest_complete_circuit(&self) -> Distance {
        let mut queue = BinaryHeap::new();
        let initial_obtained: BTreeSet<GoalNum> = std::iter::once(0).collect();
        let all_goals: BTreeSet<GoalNum> = self.goals.keys().cloned().collect();
        let mut visited = BTreeSet::new();
        let mut best = None;
        queue.push((RevOrd(0), 0, initial_obtained));
        while let Some((RevOrd(dist_so_far), current, obtained)) = queue.pop() {
            if visited.contains(&(current, obtained.clone())) {
                continue;
            }
            visited.insert((current, obtained.clone()));
            if obtained == all_goals {
                let round_trip_dist = dist_so_far + self.goals[&current][&0];
                match best {
                    Some(record) => {
                        if round_trip_dist < record {
                            best = Some(round_trip_dist);
                        }
                    }
                    None => best = Some(round_trip_dist),
                }
            }
            for (next, next_dist) in self.goals[&current].iter() {
                if obtained.contains(&next) {
                    continue;
                }
                let total_dist = dist_so_far + next_dist;
                let mut new_obtained = obtained.clone();
                new_obtained.insert(*next);
                if !visited.contains(&(*next, new_obtained.clone())) {
                    queue.push((RevOrd(total_dist), *next, new_obtained));
                }
            }
        }
        return best.expect("Could not complete round trip!");
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
    let goal_graph = ips.goal_graph();
    println!("Part 1 total distance: {}", goal_graph.shortest_circuit());
    println!("Part 2 total distance: {}",
             goal_graph.shortest_complete_circuit());
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
        let goal_graph = ips.goal_graph();
        assert_eq!(goal_graph.shortest_circuit(), 14);
    }

    #[test]
    fn example_round_trip() {
        let map = "###########\n#0.1.....2#\n#.#######.#\n#4.......3#\n###########"
            .parse::<Map>()
            .unwrap();
        let ips = map.interesting_points();
        let goal_graph = ips.goal_graph();
        assert_eq!(goal_graph.shortest_complete_circuit(), 20);
    }
}
