extern crate structopt;
#[macro_use]
extern crate structopt_derive;

use structopt::StructOpt;

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

fn main() {
    let opt = Opt::from_args();
    println!(
        "Part 1: {}",
        part1(
            opt.input
                .parse::<u64>()
                .expect("could not parse argument as unsigned integer")
        )
    );
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
}
