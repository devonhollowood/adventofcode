extern crate structopt;
#[macro_use]
extern crate structopt_derive;

use structopt::StructOpt;

#[derive(Debug)]
struct Generator {
    current: usize,
    factor: usize,
}

impl Iterator for Generator {
    type Item = usize;
    fn next(&mut self) -> Option<Self::Item> {
        self.current = self.current * self.factor % 2_147_483_647;
        Some(self.current)
    }
}

fn part1(a: usize, b: usize) -> usize {
    let gen_a = Generator {
        current: a,
        factor: 16_807,
    };
    let gen_b = Generator {
        current: b,
        factor: 48_271,
    };
    gen_a
        .zip(gen_b)
        .take(40_000_000)
        .filter(|&(a, b)| a & 0xffff == b & 0xffff)
        .count()
}

fn part2(a: usize, b: usize) -> usize {
    let gen_a = Generator {
        current: a,
        factor: 16_807,
    };
    let gen_b = Generator {
        current: b,
        factor: 48_271,
    };
    gen_a
        .filter(|x| x % 4 == 0)
        .zip(gen_b.filter(|x| x % 8 == 0))
        .take(5_000_000)
        .filter(|&(a, b)| a & 0xffff == b & 0xffff)
        .count()
}

fn main() {
    let opt = Opt::from_args();
    let a = opt.a
        .parse::<usize>()
        .expect(&format!("Could not parse {} as usize", opt.a));
    let b = opt.b
        .parse::<usize>()
        .expect(&format!("Could not parse {} as usize", opt.b));
    println!("Part 1: {}", part1(a, b));
    println!("Part 2: {}", part2(a, b));
}

#[derive(StructOpt, Debug)]
#[structopt(name = "day13", about = "Advent of code 2017 day 13")]
struct Opt {
    #[structopt(help = "input for generator a")] a: String,
    #[structopt(help = "input for generator b")] b: String,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn generator_test() {
        let gen_a = Generator {
            current: 65,
            factor: 16_807,
        };
        let gen_b = Generator {
            current: 8921,
            factor: 48271,
        };
        let results: Vec<_> = gen_a.zip(gen_b).take(5).collect();
        assert_eq!(
            &results,
            &[
                (1_092_455, 430_625_591),
                (1_181_022_009, 1_233_683_848),
                (245_556_042, 1_431_495_498),
                (1_744_312_007, 137_874_439),
                (1_352_636_452, 285_222_916),
            ]
        );
    }

    #[test]
    fn part1_test() {
        assert_eq!(part1(65, 8921), 588);
    }

    #[test]
    fn part2_test() {
        assert_eq!(part2(65, 8921), 309);
    }
}
