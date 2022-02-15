use anyhow::Result;

pub fn parse(input: &str) -> Result<&[u8]> {
    Ok(input.as_bytes())
}

enum ParseResult {
    Ok,
    Incomplete,
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
        ParseResult::Incomplete
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

pub fn part2(_input: &[u8]) -> &'static str {
    "not implemented"
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
}
