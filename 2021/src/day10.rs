use anyhow::Result;

pub fn parse(input: &str) -> Result<&[u8]> {
    Ok(input.as_bytes())
}

enum ParseResult {
    Ok,
    Incomplete(Vec<u8>),
    Unexpected(u8),
}

fn parse_line(line: &[u8]) -> ParseResult {
    let mut stack = Vec::with_capacity(line.len());
    for ch in line.iter().copied() {
        match ch {
            b'(' | b'[' | b'{' | b'<' => stack.push(ch),
            b')' => {
                if let Some(b'(') = stack.last() {
                    stack.pop();
                } else {
                    return ParseResult::Unexpected(b')');
                }
            }
            b']' => {
                if let Some(b'[') = stack.last() {
                    stack.pop();
                } else {
                    return ParseResult::Unexpected(b']');
                }
            }
            b'}' => {
                if let Some(b'{') = stack.last() {
                    stack.pop();
                } else {
                    return ParseResult::Unexpected(b'}');
                }
            }
            b'>' => {
                if let Some(b'<') = stack.last() {
                    stack.pop();
                } else {
                    return ParseResult::Unexpected(b'>');
                }
            }
            _ => continue,
        }
    }
    if stack.is_empty() {
        ParseResult::Ok
    } else {
        let needed = stack
            .into_iter()
            .rev()
            .map(|c| match c {
                b'(' => b')',
                b'[' => b']',
                b'{' => b'}',
                b'<' => b'>',
                other => panic!("invalid stack char: {}", other),
            })
            .collect();
        ParseResult::Incomplete(needed)
    }
}

pub fn part1(input: &[u8]) -> usize {
    input
        .split(|c| *c == b'\n')
        .filter_map(|l| match parse_line(l) {
            ParseResult::Unexpected(c) => match c {
                b')' => Some(3),
                b']' => Some(57),
                b'}' => Some(1197),
                b'>' => Some(25137),
                other => panic!("invalid unexpected char: {}", other),
            },
            _ => None,
        })
        .sum()
}

fn score_incomplete(needed: &[u8]) -> usize {
    needed.iter().copied().fold(0, |acc, c| {
        let add = match c {
            b')' => 1,
            b']' => 2,
            b'}' => 3,
            b'>' => 4,
            other => panic!("invalid needed char: {}", other),
        };
        5 * acc + add
    })
}

pub fn part2(input: &[u8]) -> usize {
    let mut scores: Vec<usize> = input
        .split(|c| *c == b'\n')
        .filter_map(|l| match parse_line(l) {
            ParseResult::Incomplete(needed) => Some(score_incomplete(&needed)),
            _ => None,
        })
        .collect();
    scores.sort_unstable();
    scores[scores.len() / 2]
}

#[cfg(test)]
mod tests {
    use super::*;

    const INPUT: &str = r"
    [({(<(())[]>[[{[]{<()<>>
    [(()[<>])]({[<{<<[]>>(
    {([(<{}[<>[]}>{[]{[(<()>
    (((({<>}<{<{<>}{[]{[]{}
    [[<[([]))<([[{}[[()]]]
    [{[{({}]{}}([{[{{{}}([]
    {<[[]]>}<{[{[{[]{()[[[]
    [<(<(<(<{}))><([]([]()
    <{([([[(<>()){}]>(<<{{
    <{([{{}}[<[[[<>{}]]]>[]]";

    #[test]
    fn test_part1() {
        assert_eq!(part1(INPUT.as_bytes()), 26397);
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2(INPUT.as_bytes()), 288957);
    }
}
