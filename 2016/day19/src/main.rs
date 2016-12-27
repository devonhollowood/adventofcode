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
        Err(_) => panic!("Invalid number: {}", num_str),
    }
}

fn josephus(num: u64) -> u64 {
    let prev_power_2 = 0b1u64.rotate_right(1) >> num.leading_zeros();
    (num ^ prev_power_2) << 1 | 0b1
}

fn josephus_across(num: u64) -> u64 {
    if num < 2 {
        return 1;
    }
    let prev_power_3 = {
        let mut count = 0;
        let mut a = num - 1;
        while a != 0 {
            a /= 3;
            count += 1
        }
        3u64.pow(count - 1)
    };
    let rem = num - prev_power_3;
    if rem <= prev_power_3 {
        rem
    } else {
        prev_power_3 + (rem - prev_power_3) * 2
    }
}

fn main() {
    let num = parse_args();
    println!("Last elf standing (part 1): {}", josephus(num));
    println!("Last elf standing (part 2): {}", josephus_across(num));
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

    #[test]
    fn josephus_across_examples() {
        assert_eq!(josephus_across(1), 1);
        assert_eq!(josephus_across(2), 1);
        assert_eq!(josephus_across(3), 3);
        assert_eq!(josephus_across(4), 1);
        assert_eq!(josephus_across(5), 2);
        assert_eq!(josephus_across(6), 3);
        assert_eq!(josephus_across(7), 5);
        assert_eq!(josephus_across(8), 7);
        assert_eq!(josephus_across(9), 9);
        assert_eq!(josephus_across(10), 1);
    }
}
