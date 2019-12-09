use std::path::PathBuf;
use structopt::StructOpt;

fn valid(mut num: usize) -> bool {
    let mut found_pair = false;
    let mut prev_digit = None;
    // process digits right-to-left
    while num > 0 {
        let digit = num % 10;
        if let Some(prev) = prev_digit {
            if prev < digit {
                return false;
            }
            if prev == digit {
                found_pair = true;
            }
        }
        num /= 10;
        prev_digit = Some(digit);
    }
    found_pair
}

fn valid2(mut num: usize) -> bool {
    let mut found_pair = false;
    let mut prev_digit = None;
    let mut match_len = 1;
    // process digits right-to-left
    while num > 0 {
        let digit = num % 10;
        if let Some(prev) = prev_digit {
            if prev < digit {
                return false;
            } else if prev == digit {
                match_len += 1;
            } else {
                if match_len == 2 {
                    found_pair = true;
                }
                match_len = 1;
            }
        }
        num /= 10;
        prev_digit = Some(digit);
    }
    found_pair || match_len == 2 // would have marked pair found, but ran out of digits
}

fn part1(low: usize, high: usize) -> usize {
    (low..=high).filter(|x| valid(*x)).count()
}

fn part2(low: usize, high: usize) -> usize {
    (low..=high).filter(|x| valid2(*x)).count()
}

fn main() {
    let input: Vec<_> = std::fs::read_to_string(Opt::from_args().input)
        .expect("error reading file")
        .split('-')
        .map(|num| {
            num.trim()
                .parse::<usize>()
                .unwrap_or_else(|err| panic!("could not parse token \"{}\": {:?}", num, err))
        })
        .collect();
    assert_eq!(input.len(), 2, "expected exactly two input numbers");
    println!("Part 1: {}", part1(input[0], input[1]));
    println!("Part 2: {}", part2(input[0], input[1]));
}

#[derive(StructOpt)]
struct Opt {
    #[structopt(parse(from_os_str))]
    input: PathBuf,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_valid() {
        assert_eq!(valid(111_111), true);
        assert_eq!(valid(223_450), false);
        assert_eq!(valid(123_789), false);
    }

    #[test]
    fn test_valid2() {
        assert_eq!(valid2(112233), true);
        assert_eq!(valid2(123444), false);
        assert_eq!(valid2(111_122), true);
        assert_eq!(valid2(112_222), true);
    }
}
