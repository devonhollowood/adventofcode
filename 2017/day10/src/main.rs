extern crate structopt;
#[macro_use]
extern crate structopt_derive;

use structopt::StructOpt;
use std::fs::File;
use std::io::Read;

fn reverse<T>(input: &mut [T], mut start: usize, mut len: usize) {
    loop {
        let a = start % input.len();
        let b = (start + len - 1) % input.len();
        input.swap(a, b);
        start += 1;
        if len <= 2 {
            break;
        } else {
            len -= 2;
        }
    }
}

fn shuffle<T>(input: &mut [T], lengths: &[usize]) {
    let mut position = 0;
    for (skip, length) in lengths.iter().enumerate() {
        reverse(input, position, *length);
        position += length + skip;
    }
}

fn part1(input: &str) -> Vec<u8> {
    let mut output: Vec<u8> = (0..256u16).map(|i| i as u8).collect();
    let lengths: Vec<usize> = input
        .trim()
        .split(',')
        .map(|val| val.parse().expect(&format!("could not parse {}", val)))
        .collect();
    shuffle(&mut output, &lengths);
    output
}

fn part2(input: &str) -> Vec<u8> {
    let mut lengths = input.trim().as_bytes().to_owned();
    lengths.append(&mut vec![17, 31, 73, 47, 23]);
    let repeated: Vec<usize> = lengths
        .iter()
        .cycle()
        .map(|i| *i as usize)
        .take(lengths.len() * 64)
        .collect();
    let mut output: Vec<u8> = (0..256u16).map(|i| i as u8).collect();
    // get sparse hash
    shuffle(&mut output, &repeated);
    output
        .chunks(16)
        .map(|chunk| chunk.iter().fold(0, |n, m| n ^ m))
        .collect()
}

fn as_hex(input: &[u8]) -> String {
    let mut s = String::with_capacity(input.len());
    for elem in input {
        s.push_str(&format!("{:02x}", elem));
    }
    s
}

fn main() {
    let opt = Opt::from_args();
    let mut contents = String::new();
    if opt.input == "-" {
        std::io::stdin()
            .read_to_string(&mut contents)
            .expect("could not read stdin");
    } else {
        let mut file = File::open(&opt.input)
            .expect(&format!("file {} not found", opt.input));
        file.read_to_string(&mut contents)
            .expect(&format!("could not read file {}", opt.input));
    }
    let shuffled = part1(&contents);
    println!(
        "Part 1: {}*{} = {}",
        shuffled[0],
        shuffled[1],
        shuffled[0] as usize * shuffled[1] as usize
    );
    println!("Part 2: {}", as_hex(&part2(&contents)));
}

#[derive(StructOpt, Debug)]
#[structopt(name = "day10", about = "Advent of code 2017 day 10")]
struct Opt {
    #[structopt(help = "Input file")] input: String,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn shuffle_test() {
        let mut input = vec![0, 1, 2, 3, 4];
        let lengths = vec![3, 4, 1, 5];
        shuffle(&mut input, &lengths);
        assert_eq!(input, vec![3, 4, 2, 1, 0])
    }

    #[test]
    fn reverse_test() {
        let mut a = vec![0, 1, 2, 3, 4];
        reverse(&mut a, 1, 4);
        assert_eq!(a, vec![0, 4, 3, 2, 1]);
        let mut b = vec![0, 1, 2, 3, 4];
        reverse(&mut b, 5, 3);
        assert_eq!(b, vec![2, 1, 0, 3, 4]);
    }

    #[test]
    fn part2_test() {
        assert_eq!(as_hex(&part2("")), "a2582a3a0e66e6e86e3812dcb672a272");
        assert_eq!(
            as_hex(&part2("AoC 2017")),
            "33efeb34ea91902bb2f59c9920caa6cd"
        );
        assert_eq!(as_hex(&part2("1,2,3")), "3efbe78a8d82f29979031a4aa0b16a9d");
        assert_eq!(as_hex(&part2("1,2,4")), "63960835bcdc130f0b66d7ff4f6a5a8e");
    }
}
