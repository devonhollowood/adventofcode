extern crate clap;

#[derive(Debug, Clone, PartialEq, Eq)]
struct Bits {
    bits: Vec<bool>,
}

impl std::str::FromStr for Bits {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut bits = Vec::with_capacity(s.len());
        for ch in s.chars() {
            match ch {
                '0' => bits.push(false),
                '1' => bits.push(true),
                _ => return Err(format!("Invalid digit: {}", ch)),
            }
        }
        Ok(Bits { bits: bits })
    }
}

impl std::fmt::Display for Bits {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        for bit in &self.bits {
            write!(f, "{}", if *bit { '1' } else { '0' })?
        }
        Ok(())
    }
}

fn run_dragon_curve(mut state: Bits, target_size: usize) -> Bits {
    while state.bits.len() < target_size {
        let initial_len = state.bits.len();
        state.bits.reserve(initial_len + 1);
        state.bits.push(false);
        for idx in (0..initial_len).rev() {
            let next = !state.bits[idx];
            state.bits.push(next);
        }
    }
    state.bits.truncate(target_size);
    state
}

fn checksum(bits: &Bits) -> Bits {
    let mut checksum = bits.clone();
    while checksum.bits.len() % 2 == 0 {
        let mut idx = 0;
        while idx < checksum.bits.len() {
            if checksum.bits[idx] == checksum.bits[idx + 1] {
                checksum.bits[idx / 2] = true;
            } else {
                checksum.bits[idx / 2] = false;
            }
            idx += 2
        }
        let new_size = checksum.bits.len() / 2;
        checksum.bits.truncate(new_size);
    }
    checksum
}

fn parse_args() -> Result<Bits, String> {
    let matches = clap::App::new("Day 16")
        .author("Devon Hollowood")
        .arg(clap::Arg::with_name("initial")
            .index(1)
            .short("i")
            .long("initial")
            .help("initial state for dragon curve")
            .required(true)
            .takes_value(true))
        .get_matches();
    matches.value_of("initial").unwrap().parse()
}

fn main() {
    let initial_state = parse_args().expect("Invalid initial state!");
    let checksum = checksum(&run_dragon_curve(initial_state, 272));
    println!("checksum: {}", checksum);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn simple_curves() {
        assert_eq!(run_dragon_curve("1".parse().unwrap(), 3),
                   "100".parse().unwrap());
        assert_eq!(run_dragon_curve("0".parse().unwrap(), 3),
                   "001".parse().unwrap());
        assert_eq!(run_dragon_curve("11111".parse().unwrap(), 11),
                   "11111000000".parse().unwrap());
        assert_eq!(run_dragon_curve("111100001010".parse().unwrap(), 25),
                   "1111000010100101011110000".parse().unwrap());
    }

    #[test]
    fn example() {
        let initial_state = "10000".parse().unwrap();
        let final_state = run_dragon_curve(initial_state, 20);
        assert_eq!(final_state, "10000011110010000111".parse().unwrap());
        assert_eq!(checksum(&final_state), "01100".parse().unwrap());
    }
}
