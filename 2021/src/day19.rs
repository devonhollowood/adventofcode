use anyhow::{anyhow, Context, Result};
use cached::proc_macro::cached;
use nalgebra::{Matrix3, Vector3};
use std::collections::{BTreeMap, HashMap};

type Vec3 = Vector3<i64>;
type Mat3 = Matrix3<i64>;

#[derive(Debug, Clone, PartialEq, Eq)]
/// A signature is a location- and orientation-independent way of comparing beacons
/// It keeps track of the distance to each point within range, and matches another signature when
/// enough distances match
struct Signature {
    /// map of distance -> # of points at that distance
    distances: BTreeMap<i64, usize>,
}

impl Signature {
    fn empty() -> Self {
        Signature {
            distances: BTreeMap::new(),
        }
    }

    fn from_points<'a>(origin: &Vec3, points: impl IntoIterator<Item = &'a Vec3>) -> Self {
        let mut distances = BTreeMap::new();
        for point in points.into_iter() {
            if chebyshev(origin, point) > 2000 {
                // no one scanner can see both
                continue;
            }
            let dist = manhattan(origin, point);
            *distances.entry(dist).or_default() += 1;
        }
        Signature { distances }
    }

    fn matches(&self, other: &Signature, threshold: usize) -> bool {
        let common_distances: usize = self
            .distances
            .iter()
            .map(|(dist, count)| {
                other
                    .distances
                    .get(dist)
                    .copied()
                    .unwrap_or_default()
                    .min(*count)
            })
            .sum();
        common_distances >= threshold
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Scanner {
    /// relative positions of beacons and their signatures
    beacons: HashMap<Vec3, Signature>,
}

/// map of the area
#[derive(Debug, Clone, PartialEq, Eq)]
struct Map {
    /// beacons relative to initial scanner -> Signature
    beacons: HashMap<Vec3, Signature>,
    /// scanner index -> scanner location relative to initial
    scanners: BTreeMap<usize, Vec3>,
}

impl Map {
    fn from_scanner0(scanner: &Scanner) -> Self {
        Map {
            beacons: scanner.beacons.clone(),
            scanners: BTreeMap::from([(0, Vec3::new(0, 0, 0))]),
        }
    }

    /// attempt to accept scanner into map. Return whether matching succeeded
    fn accept(&mut self, scanner: &Scanner, index: usize, threshold: usize) -> bool {
        // loop over each pair of (map beacon, canididate scanner beacon)
        for (map_beacon, map_beacon_sig) in self.beacons.iter() {
            for (candidate, candidate_sig) in scanner.beacons.iter() {
                // skip signatures which do not match
                if !map_beacon_sig.matches(candidate_sig, threshold) {
                    continue;
                }
                // since we have a strong indication that their may be a match, go ahead with
                // expensive matching procedure

                // since we think scanner_beacon may be the same as the map_beacon, we apply a
                // transformations to confirm:
                // - apply each rotation in turn to all scanner beacons
                // - translate our rotated `canididate` to `map_beacon`
                // - count how many matches we have
                for rot in rotations() {
                    let rotated: Vec<Vec3> =
                        scanner.beacons.keys().map(|point| rot * point).collect();
                    // translates from rotated scanner frame to map frame
                    let offset = map_beacon - (rot * candidate);
                    let points: Vec<Vec3> = rotated.iter().map(|point| point + offset).collect();
                    let n_matched = points
                        .iter()
                        .filter(|point| self.beacons.contains_key(point))
                        .count();
                    if n_matched >= threshold {
                        let scanner_loc = offset; // since it was at (0, 0) in its own frame
                        self.scanners.insert(index, scanner_loc);

                        // add new points, without each other in signatures for now
                        for point in points.iter() {
                            self.beacons
                                .insert(*point, Signature::from_points(point, self.beacons.keys()));
                        }

                        // now update all points in map to have new points in their signature
                        for (map_point, map_point_sig) in self.beacons.iter_mut() {
                            for point in points.iter() {
                                if chebyshev(map_point, point) > 2000 {
                                    // no one scanner can see both
                                    continue;
                                }
                                let dist = manhattan(map_point, point);
                                *map_point_sig.distances.entry(dist).or_default() += 1;
                            }
                        }

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
    // go through and generate scanners without signatures
    for line in input.lines() {
        // skip blank lines
        if line.trim().is_empty() {
            continue;
        }
        // start a new scanner on --- lines
        if line.trim().starts_with("---") {
            scanners.push(Scanner {
                beacons: HashMap::new(),
            });
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
        scanners
            .last_mut()
            .unwrap()
            .beacons
            .insert(point, Signature::empty());
    }

    // now insert signatures
    for scanner in scanners.iter_mut() {
        let beacon_locs: Vec<Vec3> = scanner.beacons.keys().cloned().collect();
        for (position, signature) in scanner.beacons.iter_mut() {
            *signature = Signature::from_points(position, &beacon_locs);
        }
    }

    Ok(scanners)
}

/// max of distances along each axis
fn chebyshev(a: &Vec3, b: &Vec3) -> i64 {
    *(b - a).abs().iter().max().unwrap()
}

/// sum of distances along each axis
fn manhattan(a: &Vec3, b: &Vec3) -> i64 {
    (b - a).abs().iter().sum()
}

// separate to make testable with given data
#[cached(key = "String", convert = r#"{ format!("{}{:?}", threshold, input) }"#)]
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
    use std::collections::HashSet;

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
        let parsed = parse(MATCHING_SCANNERS).unwrap();
        assert_eq!(parsed.len(), 2);
        {
            assert_eq!(
                parsed[0].beacons.keys().cloned().collect::<HashSet<Vec3>>(),
                HashSet::from([
                    Vec3::new(-1, -1, 1),
                    Vec3::new(-2, -2, 2),
                    Vec3::new(-3, -3, 3),
                    Vec3::new(-2, -3, 1),
                    Vec3::new(5, 6, -4),
                    Vec3::new(8, 0, 7),
                ])
            );
        }
        assert_eq!(
            parsed[1].beacons.keys().cloned().collect::<HashSet<Vec3>>(),
            HashSet::from([
                Vec3::new(1, -1, 1),
                Vec3::new(2, -2, 2),
                Vec3::new(3, -3, 3),
                Vec3::new(2, -1, 3),
                Vec3::new(-5, 4, -6),
                Vec3::new(-8, -7, 0),
            ])
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

        assert_eq!(
            map.beacons.keys().cloned().collect::<HashSet<Vec3>>(),
            solution
        );

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
