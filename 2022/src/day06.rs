use anyhow::Result;

pub fn parse(input: &str) -> Result<&str> {
    Ok(input)
}

fn find_unique_span(input: &str, width: usize) -> usize {
    let bytes = input.as_bytes();
    let mut i = 0;
    'outer: while i + width < bytes.len() {
        // For width=4: check 0 against 1-3, 1 against 2-3, and 2 against 3
        for j in 0..width - 1 {
            if bytes[i + j + 1..i + width].contains(&bytes[i + j]) {
                i += j + 1; // can skip ahead by j
                continue 'outer;
            }
        }
        return i + width;
    }
    panic!("No start sequence found in {}", input);
}

pub fn part1(input: &str) -> usize {
    find_unique_span(input, 4)
}

pub fn part2(input: &str) -> usize {
    find_unique_span(input, 14)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        assert_eq!(part1("mjqjpqmgbljsphdztnvjfqwrcgsmlb"), 7);
        assert_eq!(part1("bvwbjplbgvbhsrlpgdmjqwftvncz"), 5);
        assert_eq!(part1("nppdvjthqldpwncqszvftbrmjlhg"), 6);
        assert_eq!(part1("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"), 10);
        assert_eq!(part1("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"), 11);
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2("mjqjpqmgbljsphdztnvjfqwrcgsmlb"), 19);
        assert_eq!(part2("bvwbjplbgvbhsrlpgdmjqwftvncz"), 23);
        assert_eq!(part2("nppdvjthqldpwncqszvftbrmjlhg"), 23);
        assert_eq!(part2("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"), 29);
        assert_eq!(part2("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"), 26);
    }
}
