use anyhow::{anyhow, Context, Result};
use nalgebra::{Matrix3, Vector3};
use std::collections::{BTreeMap, HashSet};

type Vec3 = Vector3<i64>;
type Mat3 = Matrix3<i64>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Scanner {
    /// relative positions of beacons
    beacons: Vec<Vec3>,
}

/// map of the area
#[derive(Debug, Clone, PartialEq, Eq)]
struct Map {
    // beacons relative to initial scanner
    beacons: HashSet<Vec3>,
    // scanner index -> scanner location relative to initial
    scanners: BTreeMap<usize, Vec3>,
}

impl Map {
    fn from_scanner0(scanner: &Scanner) -> Self {
        Map {
            beacons: scanner.beacons.iter().cloned().collect(),
            scanners: BTreeMap::from([(0, Vec3::new(0, 0, 0))]),
        }
    }

    /// attempt to accept scanner into map. Return whether matching succeeded
    fn accept(&mut self, scanner: &Scanner, index: usize, threshold: usize) -> bool {
        for rot in rotations() {
            let rotated: Vec<Vec3> = scanner.beacons.iter().map(|point| rot * point).collect();

            // translate each rotated scanner beacon to each map beacon, and see if there are >= 12
            // total points in common
            for candidate in rotated.iter() {
                for map_beacon in self.beacons.iter() {
                    // translation needed to map `candidate` to `map_beacon`
                    let offset = map_beacon - candidate;
                    let points: Vec<Vec3> = rotated.iter().map(|point| point + offset).collect();
                    let n_matched = points
                        .iter()
                        .filter(|point| self.beacons.contains(point))
                        .count();
                    if n_matched >= threshold {
                        for point in points {
                            self.beacons.insert(point);
                        }
                        // offset maps (0, 0) in the candidate fram to the correct point in the map
                        // frame
                        self.scanners.insert(index, offset);
                        return true;
                    }
                }
            }
        }

        false
    }
}

/// all reachable rotation matrices
fn rotations() -> Vec<Mat3> {
    // 90° ccw rotation about x axis
    let x = Mat3::new(
        1, 0, 0, //
        0, 0, -1, //
        0, 1, 0,
    );

    // 180° ccw rotation about x axis
    let x2 = Mat3::new(
        1, 0, 0, //
        0, -1, 0, //
        0, 0, -1,
    );

    // 90° cw rotation about x axis
    let x_inv = Mat3::new(
        1, 0, 0, //
        0, 0, 1, //
        0, -1, 0,
    );

    let y = Mat3::new(
        0, 0, 1, //
        0, 1, 0, //
        -1, 0, 0,
    );

    let y_inv = Mat3::new(
        0, 0, -1, //
        0, 1, 0, //
        1, 0, 0,
    );

    let z = Mat3::new(
        0, -1, 0, //
        1, 0, 0, //
        0, 0, 1,
    );

    let z2 = Mat3::new(
        -1, 0, 0, //
        0, -1, 0, //
        0, 0, 1,
    );

    let z_inv = Mat3::new(
        0, 1, 0, //
        -1, 0, 0, //
        0, 0, 1,
    );

    // rotations about x axis
    let rotations = vec![Mat3::identity(), x, x2, x_inv];

    // matrices to map (1, 0, 0) to other axes
    let facings = vec![
        // point to x axis
        Mat3::identity(),
        // rotate around z to point to y axis
        z,
        // rotate around z to point to -x axis
        z2,
        // rotate around z to point to -y axis
        z_inv,
        // rotate around y to point to z axis
        y_inv,
        // rotate around y to point to -z axis
        y,
    ];

    rotations
        .into_iter()
        .flat_map(|rot| facings.iter().map(move |facing| facing * rot))
        .collect()
}

pub fn parse(input: &str) -> Result<Vec<Scanner>> {
    let mut scanners = Vec::new();
    for line in input.lines() {
        // skip blank lines
        if line.trim().is_empty() {
            continue;
        }
        // start a new scanner on --- lines
        if line.trim().starts_with("---") {
            scanners.push(Scanner { beacons: vec![] });
            continue;
        }
        // this should be a number line, meaning we already have a scanner started
        if scanners.is_empty() {
            return Err(anyhow!(
                "encountered numeric line before scanner started: {}",
                line
            ));
        }
        let fields: Vec<&str> = line.split(',').collect();
        if fields.len() != 3 {
            return Err(anyhow!(
                "expected exactly 3 fields, got {}, in line {}",
                fields.len(),
                line
            ));
        }
        let parsed = fields
            .into_iter()
            .map(|f| f.parse::<i64>())
            .collect::<std::result::Result<Vec<_>, _>>()
            .with_context(|| format!("parsing line {}", line))?;
        let point = Vec3::from_vec(parsed);
        scanners.last_mut().unwrap().beacons.push(point);
    }
    Ok(scanners)
}

fn manhattan(a: &Vec3, b: &Vec3) -> i64 {
    (b - a).abs().iter().sum()
}

// separate to make testable with given data
fn part1_impl(input: &[Scanner], threshold: usize) -> Map {
    assert!(!input.is_empty());

    let mut map = Map::from_scanner0(&input[0]);

    // map of index -> scanner for easier processing
    let mut unmatched: BTreeMap<usize, Scanner> =
        input.iter().cloned().enumerate().skip(1).collect();
    while !unmatched.is_empty() {
        let mut matched = Vec::new();
        for (&index, scanner) in unmatched.iter() {
            if map.accept(scanner, index, threshold) {
                matched.push(index);
            }
        }
        assert!(!matched.is_empty(), "no scanners matched");
        for index in matched {
            unmatched.remove(&index);
        }
    }

    map
}

pub fn part1(input: &[Scanner]) -> usize {
    let map = part1_impl(input, 12);
    map.beacons.len()
}

pub fn part2(input: &[Scanner]) -> i64 {
    let map = part1_impl(input, 12);
    map.scanners
        .values()
        .flat_map(|a| map.scanners.values().map(|b| manhattan(a, b)))
        .max()
        .unwrap()
}

#[cfg(test)]
mod tests {
    use super::*;

    const MATCHING_SCANNERS: &str = r"
--- scanner 0 ---
-1,-1,1
-2,-2,2
-3,-3,3
-2,-3,1
5,6,-4
8,0,7

--- scanner 1 ---
1,-1,1
2,-2,2
3,-3,3
2,-1,3
-5,4,-6
-8,-7,0
";

    #[test]
    fn test_parse() {
        assert_eq!(
            parse(MATCHING_SCANNERS).unwrap(),
            vec![
                Scanner {
                    beacons: vec![
                        Vec3::new(-1, -1, 1),
                        Vec3::new(-2, -2, 2),
                        Vec3::new(-3, -3, 3),
                        Vec3::new(-2, -3, 1),
                        Vec3::new(5, 6, -4),
                        Vec3::new(8, 0, 7),
                    ]
                },
                Scanner {
                    beacons: vec![
                        Vec3::new(1, -1, 1),
                        Vec3::new(2, -2, 2),
                        Vec3::new(3, -3, 3),
                        Vec3::new(2, -1, 3),
                        Vec3::new(-5, 4, -6),
                        Vec3::new(-8, -7, 0),
                    ]
                },
            ]
        );
    }

    #[test]
    fn test_accept_simple() {
        let scanners = parse(MATCHING_SCANNERS).unwrap();
        let mut map = Map::from_scanner0(&scanners[0]);
        assert!(map.accept(&scanners[1], 1, 6));
    }

    #[test]
    fn test_part1() {
        const DATA: &str = include_str!("test-data/day19.txt");

        let scanners = parse(DATA).unwrap();
        assert_eq!(scanners.len(), 5);

        let map = part1_impl(&scanners, 12);
        assert_eq!(map.beacons.len(), 79);

        let solution: HashSet<Vec3> = include_str!("test-data/day19-solution.txt")
            .lines()
            .map(|line| Vec3::from_iterator(line.split(',').map(|f| f.parse::<i64>().unwrap())))
            .collect();

        assert_eq!(map.beacons, solution);

        let solution_scanners = vec![
            Vec3::new(0, 0, 0),
            Vec3::new(68, -1246, -43),
            Vec3::new(1105, -1205, 1229),
            Vec3::new(-92, -2380, -20),
            Vec3::new(-20, -1133, 1061),
        ];

        assert_eq!(
            map.scanners.values().cloned().collect::<Vec<_>>(),
            solution_scanners
        );
    }

    #[test]
    fn test_manhattan() {
        assert_eq!(
            manhattan(&Vec3::new(1105, -1205, 1229), &Vec3::new(-92, -2380, -20)),
            3621
        );
    }

    #[test]
    fn test_part2() {
        const DATA: &str = include_str!("test-data/day19.txt");

        let scanners = parse(DATA).unwrap();
        assert_eq!(scanners.len(), 5);

        assert_eq!(part2(&scanners), 3621);
    }
}
