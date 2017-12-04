extern crate structopt;
#[macro_use]
extern crate structopt_derive;

use structopt::StructOpt;

use std::collections::BTreeMap;

type Position = (i64, i64);

fn location(num: u64) -> Position {
    if num < 2 {
        // 0 and 1 are kind of special cases
        return (0, 0);
    }
    // edge_len is next even number after square root
    // it is defined such that the "circumference" of the square = 4 * edge_len
    // note that this means a 5x5 square has edge_len 4
    let edge_len = {
        let x = (num as f64).sqrt().ceil() as u64;
        if x % 2 == 0 {
            x
        } else {
            x - 1
        }
    };

    // last point on that square. Next square "starts" one point to the right.
    let square_end = (edge_len as i64 / 2, -(edge_len as i64 / 2));
    // amount "into" this square that num is--i.e. subtract off previous squares
    let into_square = num - (edge_len - 1) * (edge_len - 1);
    let y_diff = if into_square < edge_len {
        into_square
    } else if into_square < edge_len * 2 {
        edge_len
    } else if into_square < edge_len * 3 {
        edge_len - (into_square - edge_len * 2)
    } else {
        0
    };
    // make this positive, even though it is in negative direction
    let x_diff = if into_square < edge_len {
        0
    } else if into_square < edge_len * 2 {
        into_square - edge_len
    } else if into_square < edge_len * 3 {
        edge_len
    } else {
        edge_len - (into_square - edge_len * 3)
    };
    (square_end.0 - x_diff as i64, square_end.1 + y_diff as i64)
}


fn manhattan_distance(a: Position, b: Position) -> u64 {
    ((a.0 - b.0).abs() + (a.1 - b.1).abs()) as u64
}

fn part1(input: u64) -> u64 {
    let loc = location(input);
    manhattan_distance(loc, (0, 0))
}

fn index(loc: Position) -> u64 {
    if loc == (0, 0) {
        // special case
        return 1;
    }
    let edge_dist = i64::max(loc.0.abs(), loc.1.abs());
    let edge_len = edge_dist * 2;
    let square_sum = (edge_len + 1) * (edge_len + 1);
    (if loc.1 == -edge_dist {
        // bottom edge
        square_sum - edge_dist + loc.0
    } else if loc.0 == -edge_dist {
        // left edge
        square_sum - edge_len - edge_dist - loc.1
    } else if loc.1 == edge_dist {
        // upper edge
        square_sum - edge_len * 2 - edge_dist - loc.0
    } else {
        // right edge
        square_sum - edge_len * 3 - edge_dist + loc.1
    }) as u64
}

fn value(loc: Position, mut memo: &mut BTreeMap<Position, u64>) -> u64 {
    if loc == (0, 0) {
        // base case
        return 1;
    }
    if memo.contains_key(&loc) {
        return memo[&loc];
    }
    let neighbors = vec![
        (loc.0 - 1, loc.1 + 1),
        (loc.0, loc.1 + 1),
        (loc.0 + 1, loc.1 + 1),
        (loc.0 - 1, loc.1),
        (loc.0 + 1, loc.1),
        (loc.0 - 1, loc.1 - 1),
        (loc.0, loc.1 - 1),
        (loc.0 + 1, loc.1 - 1),
    ];
    let val = neighbors
        .into_iter()
        .filter(|pos| index(*pos) < index(loc))
        .map(|pos| value(pos, &mut memo))
        .sum();
    memo.insert(loc, val);
    val
}

fn part2(target: u64) -> u64 {
    let mut memo = BTreeMap::new();
    let mut idx = 1;
    loop {
        let loc = location(idx);
        let val = value(loc, &mut memo);
        if val > target {
            return val;
        }
        idx += 1;
    }
}

fn main() {
    let opt = Opt::from_args();
    let input = opt.input
        .parse::<u64>()
        .expect("could not parse argument as unsigned integer");
    println!("Part 1: {}", part1(input));
    println!("Part 2: {}", part2(input));
}

#[derive(StructOpt, Debug)]
#[structopt(name = "day03", about = "Advent of code 2017 day 03")]
struct Opt {
    #[structopt(help = "Input")] input: String,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn location_test() {
        assert_eq!(location(1), (0, 0));
        assert_eq!(location(10), (2, -1));
        assert_eq!(location(13), (2, 2));
        assert_eq!(location(14), (1, 2));
        assert_eq!(location(17), (-2, 2));
        assert_eq!(location(18), (-2, 1));
        assert_eq!(location(21), (-2, -2));
        assert_eq!(location(22), (-1, -2));
        assert_eq!(location(25), (2, -2));
    }

    #[test]
    fn part1_test() {
        assert_eq!(part1(1), 0);
        assert_eq!(part1(12), 3);
        assert_eq!(part1(23), 2);
        assert_eq!(part1(1024), 31);
    }

    #[test]
    fn index_test() {
        assert_eq!(index((0, 0)), 1);
        assert_eq!(index((2, -1)), 10);
        assert_eq!(index((2, 2)), 13);
        assert_eq!(index((1, 2)), 14);
        assert_eq!(index((-2, 2)), 17);
        assert_eq!(index((-2, 1)), 18);
        assert_eq!(index((-2, -2)), 21);
        assert_eq!(index((-1, -2)), 22);
        assert_eq!(index((2, -2)), 25);
    }

    #[test]
    fn value_test() {
        assert_eq!(value((0, 0), &mut BTreeMap::new()), 1);
        assert_eq!(value((2, -1), &mut BTreeMap::new()), 26);
        assert_eq!(value((2, 2), &mut BTreeMap::new()), 59);
        assert_eq!(value((1, 2), &mut BTreeMap::new()), 122);
        assert_eq!(value((-2, 2), &mut BTreeMap::new()), 147);
        assert_eq!(value((-2, 1), &mut BTreeMap::new()), 304);
        assert_eq!(value((-2, -2), &mut BTreeMap::new()), 362);
        assert_eq!(value((-1, -2), &mut BTreeMap::new()), 747);
    }
}
