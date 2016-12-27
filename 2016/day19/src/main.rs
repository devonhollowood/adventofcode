extern crate clap;

fn parse_args() -> u64 {
    let matches = clap::App::new("Day 18")
        .author("Devon Hollowood")
        .arg(clap::Arg::with_name("number")
            .help("number of elves")
            .required(true))
        .get_matches();
    let num_str = matches.value_of("number").unwrap();
    match num_str.parse() {
        Ok(num) => num,
        Err(_) => panic!("Invalid number: {}", num_str)
    }
}

fn josephus(num: u64) -> u64 {
    let prev_power_2 = 0b1u64.rotate_right(1) >> num.leading_zeros();
    (num ^ prev_power_2) << 1 | 0b1
}

fn main() {
    let num = parse_args();
    println!("Last elf standing: {}", josephus(num));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn elf_example() {
        assert_eq!(josephus(5), 3);
    }

    #[test]
    fn historical() {
        assert_eq!(josephus(41), 19);
    }

    #[test]
    fn power_of_two() {
        assert_eq!(josephus(8), 1);
    }
}
