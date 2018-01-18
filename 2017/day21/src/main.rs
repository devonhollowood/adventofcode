extern crate itertools;

extern crate structopt;
#[macro_use]
extern crate structopt_derive;

use structopt::StructOpt;
use std::fs::File;
use std::io::Read;
use std::path::PathBuf;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum GridBuildError {
    Unsquare,
    // ^ Trying to build grid from non-square number of elements
    BadSideLen,
    // ^ Side length not divisible by 2 or 3
}

impl std::fmt::Display for GridBuildError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use GridBuildError::*;
        match *self {
            Unsquare => write!(f, "Unsquare"),
            BadSideLen => write!(f, "Bad Side Length"),
        }
    }
}

impl std::error::Error for GridBuildError {
    fn description(&self) -> &str {
        use GridBuildError::*;
        match *self {
            Unsquare => "Non-square number of elements",
            BadSideLen => "Side length not divisible by 2 or 3",
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Rule {
    before: Grid,
    after: Grid,
}

type RuleSet = std::collections::HashMap<Grid, Grid>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Grid {
    side_len: usize,     // this shall always be divisible by 2 or 3
    contents: Vec<bool>, // this shall always have side_len * side_len rows
}

impl Grid {
    fn empty() -> Grid {
        Grid {
            side_len: 0,
            contents: Vec::new(),
        }
    }

    fn from_components(
        side_len: usize,
        contents: Vec<bool>,
    ) -> Result<Grid, GridBuildError> {
        if side_len * side_len != contents.len() {
            return Err(GridBuildError::Unsquare);
        }
        if side_len % 2 != 0 && side_len % 3 != 0 {
            return Err(GridBuildError::BadSideLen);
        }
        Ok(Grid {
            side_len: side_len,
            contents: contents,
        })
    }

    fn from_subgrids<T>(container: T) -> Result<Grid, GridBuildError>
    where
        T: IntoIterator<Item = Grid>,
        T::IntoIter: ExactSizeIterator,
    {
        // "sg" = "subgrid"
        let mut iter = container.into_iter().peekable();
        let sg_side_len = match iter.peek() {
            Some(sg) => sg.side_len,
            None => return Ok(Grid::empty()),
        };
        let side_len = ((iter.len() as f64).sqrt() as usize) * sg_side_len;
        let mut contents = vec![false; side_len * side_len];
        for (b, sg) in iter.enumerate() {
            for r in 0..sg_side_len {
                for c in 0..sg_side_len {
                    let src_row =
                        b / (side_len / sg_side_len) * sg_side_len + r;
                    let src_col =
                        b % (side_len / sg_side_len) * sg_side_len + c;
                    contents[src_row * side_len + src_col] =
                        sg.contents[r * sg_side_len + c];
                }
            }
        }
        Grid::from_components(side_len, contents)
    }

    fn from_bools<T>(container: T) -> Result<Grid, GridBuildError>
    where
        T: IntoIterator<Item = bool>,
        T::IntoIter: ExactSizeIterator,
    {
        let iter = container.into_iter();
        let side_len = (iter.len() as f64).sqrt() as usize;
        Grid::from_components(side_len, iter.collect())
    }

    fn split(&self) -> Vec<Grid> {
        // in this function, "sg" = "subgrid"
        let sg_side_len = if self.contents.len() % 4 == 0 { 2 } else { 3 };
        let mut grids = Vec::new();
        for b in 0..self.contents.len() / (sg_side_len * sg_side_len) {
            let mut sg_contents = vec![false; sg_side_len * sg_side_len];
            for r in 0..sg_side_len {
                for c in 0..sg_side_len {
                    let src_row =
                        b / (self.side_len / sg_side_len) * sg_side_len + r;
                    let src_col =
                        b % (self.side_len / sg_side_len) * sg_side_len + c;
                    sg_contents[r * sg_side_len + c] =
                        self.contents[src_row * self.side_len + src_col];
                }
            }
            grids.push(Grid {
                side_len: sg_side_len,
                contents: sg_contents,
            });
        }
        grids
    }

    fn enhance(&self, rules: &RuleSet) -> Option<Grid> {
        rules.get(self).cloned().map(|g| {
            Self::from_bools(g.contents).expect("Error enhancing grid")
        })
    }

    fn iterate(&self, rules: &RuleSet) -> Grid {
        Grid::from_subgrids(self.split().iter().map(|grid| {
            grid.enhance(rules)
                .expect(&format!("Rule not found: \n{}", grid))
        })).expect("Grid Iteration: Invalid subgrid produced")
    }

    fn count(&self) -> usize {
        self.contents.iter().filter(|b| **b).count()
    }

    fn rotate_ccw(&self) -> Grid {
        let mut contents = vec![false; self.contents.len()];
        for r in 0..self.side_len {
            for c in 0..self.side_len {
                let dst_row = self.side_len - c - 1;
                let dst_col = r;
                contents[dst_row * self.side_len + dst_col] =
                    self.contents[r * self.side_len + c];
            }
        }
        Grid::from_components(self.side_len, contents).expect("Error rotating")
    }

    fn flip_lr(&self) -> Grid {
        let mut contents = vec![false; self.contents.len()];
        for r in 0..self.side_len {
            for c in 0..self.side_len {
                contents[r * self.side_len + (self.side_len - c - 1)] =
                    self.contents[r * self.side_len + c];
            }
        }
        Grid::from_components(self.side_len, contents).expect("Error flipping")
    }
}

impl std::str::FromStr for Grid {
    type Err = GridBuildError;
    fn from_str(s: &str) -> Result<Grid, Self::Err> {
        let contents: Vec<_> = s.bytes()
            .filter_map(|b| match b {
                b'#' => Some(true),
                b'.' => Some(false),
                _ => None,
            })
            .collect();
        Self::from_bools(contents)
    }
}

impl std::fmt::Display for Grid {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        for (pos, val) in self.contents.iter().enumerate() {
            if pos != 0 && pos % self.side_len == 0 {
                write!(f, "\n")?
            }
            write!(f, "{}", if *val { '#' } else { '.' })?
        }
        Ok(())
    }
}

fn parse(input: &str) -> Vec<Rule> {
    use std::str::FromStr;
    input
        .lines()
        .map(|line| {
            let (before, after) = line.split_at(
                line.find("=>").expect(&format!("Invalid line: {}", line)),
            );
            Rule {
                before: Grid::from_str(before)
                    .expect(&format!("Could not parse grid from {}", before)),
                after: Grid::from_str(after)
                    .expect(&format!("Could not parse grid from {}", after)),
            }
        })
        .collect()
}

fn make_ruleset(rule_list: &[Rule]) -> RuleSet {
    let mut rule_set = std::collections::HashMap::new();
    for rule in rule_list {
        let mut new_rule = rule.before.clone();
        rule_set.insert(new_rule.clone(), rule.after.clone());
        new_rule = new_rule.rotate_ccw();
        rule_set.insert(new_rule.clone(), rule.after.clone());
        new_rule = new_rule.rotate_ccw();
        rule_set.insert(new_rule.clone(), rule.after.clone());
        new_rule = new_rule.rotate_ccw();
        rule_set.insert(new_rule.clone(), rule.after.clone());
        new_rule = new_rule.flip_lr();
        rule_set.insert(new_rule.clone(), rule.after.clone());
        new_rule = new_rule.rotate_ccw();
        rule_set.insert(new_rule.clone(), rule.after.clone());
        new_rule = new_rule.rotate_ccw();
        rule_set.insert(new_rule.clone(), rule.after.clone());
        new_rule = new_rule.rotate_ccw();
        rule_set.insert(new_rule.clone(), rule.after.clone());
    }
    rule_set
}

fn start() -> Grid {
    Grid {
        side_len: 3,
        contents: vec![
            false, true, false, false, false, true, true, true, true
        ],
    }
}

fn part1(rules: &RuleSet) -> usize {
    itertools::iterate(start(), |g| g.iterate(rules))
        .nth(5)
        .unwrap()
        .count()
}

fn part2(rules: &RuleSet) -> usize {
    itertools::iterate(start(), |g| g.iterate(rules))
        .nth(18)
        .unwrap()
        .count()
}

fn main() {
    let opt = Opt::from_args();
    let mut contents = String::new();
    if opt.input.to_str() == Some("-") {
        std::io::stdin()
            .read_to_string(&mut contents)
            .expect("could not read stdin");
    } else {
        let mut file = File::open(&opt.input)
            .expect(&format!("file {} not found", opt.input.display()));
        file.read_to_string(&mut contents)
            .expect(&format!("could not read file {}", opt.input.display()));
    }
    let rule_list = parse(&contents);
    let rules = make_ruleset(&rule_list);
    println!("Part 1: {}", part1(&rules));
    println!("Part 2: {}", part2(&rules));
}

#[derive(StructOpt, Debug)]
#[structopt(name = "day20", about = "Advent of code 2017 day 20")]
struct Opt {
    #[structopt(help = "Input file", parse(from_os_str))]
    input: PathBuf,
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::str::FromStr;

    #[test]
    fn rotate_ccw_test() {
        assert_eq!(
            Grid::from_str("#...##...").unwrap().rotate_ccw(),
            Grid::from_str(".#..#.#..").unwrap(),
        );
        assert_eq!(
            Grid::from_str("#.#.").unwrap().rotate_ccw(),
            Grid::from_str("..##").unwrap(),
        );
        assert_eq!(
            Grid::from_str("#...##...")
                .unwrap()
                .rotate_ccw()
                .rotate_ccw()
                .rotate_ccw()
                .rotate_ccw(),
            Grid::from_str("#...##...").unwrap()
        );
    }

    #[test]
    fn flip_lr_test() {
        assert_eq!(
            Grid::from_str("#...##...").unwrap().flip_lr(),
            Grid::from_str("..###....").unwrap(),
        );
        assert_eq!(
            Grid::from_str("#.#.").unwrap().flip_lr(),
            Grid::from_str(".#.#").unwrap(),
        );
        assert_eq!(
            Grid::from_str("#...##...").unwrap().flip_lr().flip_lr(),
            Grid::from_str("#...##...").unwrap(),
        );
    }

    #[test]
    fn split_2_test() {
        assert_eq!(
            Grid::from_str("#.##.#|#.#.##|......|.##.##|###...|#.##.#")
                .unwrap()
                .split(),
            vec![
                Grid::from_str("#.#.").unwrap(),
                Grid::from_str("###.").unwrap(),
                Grid::from_str(".###").unwrap(),
                Grid::from_str("...#").unwrap(),
                Grid::from_str("..#.").unwrap(),
                Grid::from_str("..##").unwrap(),
                Grid::from_str("###.").unwrap(),
                Grid::from_str("#.##").unwrap(),
                Grid::from_str("...#").unwrap(),
            ],
        )
    }

    #[test]
    fn from_subgrids_test() {
        assert_eq!(
            Grid::from_subgrids(vec![
                Grid::from_str("#.#.").unwrap(),
                Grid::from_str("###.").unwrap(),
                Grid::from_str(".###").unwrap(),
                Grid::from_str("...#").unwrap(),
                Grid::from_str("..#.").unwrap(),
                Grid::from_str("..##").unwrap(),
                Grid::from_str("###.").unwrap(),
                Grid::from_str("#.##").unwrap(),
                Grid::from_str("...#").unwrap(),
            ]).unwrap(),
            Grid::from_str("#.##.#|#.#.##|......|.##.##|###...|#.##.#")
                .unwrap(),
        )
    }

    #[test]
    fn enhance_test() {
        let rules: RuleSet = std::iter::once((
            Grid::from_str("#.#.").unwrap(),
            Grid::from_str(".##.##.##").unwrap(),
        )).collect();
        assert_eq!(
            Grid::from_str("#.#.").unwrap().enhance(&rules),
            Some(Grid::from_str(".##.##.##").unwrap())
        );
        assert_eq!(Grid::from_str(".#.#").unwrap().enhance(&rules), None);
    }

    #[test]
    fn iterate_test() {
        let input: Vec<Rule> = vec![
            Rule {
                before: Grid::from_str("../.#").unwrap(),
                after: Grid::from_str("##./#../...").unwrap(),
            },
            Rule {
                before: Grid::from_str(".#./..#/###").unwrap(),
                after: Grid::from_str("#..#/..../..../#..#").unwrap(),
            },
        ];
        let ruleset = make_ruleset(&input);
        let res = start().iterate(&ruleset).iterate(&ruleset).count();
        assert_eq!(res, 12);
    }
}
