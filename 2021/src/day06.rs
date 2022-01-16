use anyhow::{Context, Result};

// count of fish in state given by index
type SimState = [u64; 9];

pub fn parse(input: &str) -> Result<SimState> {
    let parsed = input
        .split(',')
        .map(|i| {
            i.trim()
                .parse::<usize>()
                .with_context(|| format!("could not parse {} as integer", i))
        })
        .collect::<Result<Vec<usize>>>()?;

    let mut state = [0; 9];
    for i in parsed {
        state[i] += 1;
    }

    Ok(state)
}

pub fn simulate(mut state: SimState, days: usize) -> u64 {
    for _ in 0..days {
        let zeros = state[0];
        for idx in 0..8 {
            state[idx] = state[idx + 1];
        }
        state[6] += zeros; // reset existing fish
        state[8] = zeros; // add new fish
    }

    state.iter().sum()
}

pub fn part1(input: &SimState) -> u64 {
    simulate(*input, 80)
}

pub fn part2(input: &SimState) -> u64 {
    simulate(*input, 256)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse() {
        let input = "3,4,3,1,2";
        assert_eq!(parse(input).unwrap(), [0, 1, 1, 2, 1, 0, 0, 0, 0]);
    }

    #[test]
    fn test_part1() {
        let input = "3,4,3,1,2";
        let parsed = parse(input).unwrap();
        assert_eq!(part1(&parsed), 5934);
    }
}
