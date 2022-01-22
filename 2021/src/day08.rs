use anyhow::{anyhow, Result};
use itertools::Itertools;
use std::convert::TryInto;

type AsciiChar = u8;

#[derive(Debug, Clone)]
pub struct Entry {
    input: [Vec<AsciiChar>; 10],
    output: [Vec<AsciiChar>; 4],
}

#[derive(Debug, Clone, Copy)]
#[allow(dead_code)] // since we don't technically need the input
/// fields represent the reconstructed numbers
pub struct Decoded {
    input: [u8; 10],
    output: [u8; 4],
}

impl Decoded {
    fn read_output(&self) -> u32 {
        let mut result: u32 = 0;
        for n in self.output.iter() {
            result *= 10;
            result += *n as u32;
        }
        result
    }
}

#[derive(Debug, Clone, Copy)]
/// Represents the possibilities of what letter can represent what, effectively mapping input
/// segment -> possible output segments
///
/// The ith entry's jth least-significant-bit represents is 1 if the ith encoded segment can
/// represent the jth decoded segment, 0 if it cannot. Here, letters are encoded as 'a' -> 0,
/// 'b' -> 1, ... 'g' -> 6.
///
/// Example: if input segment 0 (='a') could possibly be decoded segment 1 (='b'), then
/// self.0[0] has bit 1 set.
struct Segments([u8; 7]);

impl Segments {
    fn zero_knowledge() -> Self {
        Segments([0x7f; 7])
    }

    /// checks that every output can be assigned from at least one input
    fn is_consistent(&self) -> bool {
        self.0
            .iter()
            .copied()
            .reduce(|acc, b| acc | b)
            .unwrap()
            .count_ones()
            == 7
    }
}

fn segments(digit: u8) -> &'static [u8] {
    match digit {
        0 => &[0, 1, 2, 4, 5, 6],
        1 => &[2, 5],
        2 => &[0, 2, 3, 4, 6],
        3 => &[0, 2, 3, 5, 6],
        4 => &[1, 2, 3, 5],
        5 => &[0, 1, 3, 5, 6],
        6 => &[0, 1, 3, 4, 5, 6],
        7 => &[1, 2, 5],
        8 => &[0, 1, 2, 3, 4, 5, 6],
        9 => &[0, 1, 2, 3, 5, 6],
        _ => panic!("invalid digit {}", digit),
    }
}

fn possibilities(segments_count: usize) -> &'static [u8] {
    match segments_count {
        2 => &[1],
        3 => &[7],
        4 => &[4],
        5 => &[2, 3, 5],
        6 => &[0, 6, 9],
        7 => &[8],
        _ => panic!("invalid segment count: {}", segments_count),
    }
}

#[derive(Debug, Clone, Copy)]
struct ProblemState<'a> {
    entry: &'a Entry,
    // input segment -> decoded segment
    segments: Segments,
    // input number index -> decoded number. 255 if unassigned
    numbers: [u8; 10],
}

impl<'a> ProblemState<'a> {
    fn zero_knowledge(entry: &'a Entry) -> Self {
        ProblemState {
            entry,
            segments: Segments::zero_knowledge(),
            numbers: [255; 10],
        }
    }

    /// identify the number at `idx` as representing `digit`
    fn identify(&self, idx: usize, digit: u8) -> Self {
        let mut result = *self;

        result.numbers[idx] = digit;

        // encoded segments at `idx` can only correspond to decoded segments from `digit`
        let mask = segments(digit)
            .iter()
            .fold(0, |mask, seg| mask | (1 << seg));
        for encoded_segment in &self.entry.input[idx] {
            let seg_idx = (encoded_segment - b'a') as usize;
            // set all bits not in `mask` to zero
            result.segments.0[seg_idx] &= mask;
        }
        result
    }

    fn decode(&self) -> Decoded {
        assert!(self.numbers.iter().copied().all(|n| n <= 9));
        let mut decoded_output = <[u8; 4]>::default();

        let mut sorted_inputs = self.entry.input.clone();
        for unsorted in sorted_inputs.iter_mut() {
            unsorted.sort_unstable();
        }

        for (encoded, decoded) in self.entry.output.iter().zip(decoded_output.iter_mut()) {
            let mut sorted = encoded.clone();
            sorted.sort_unstable();
            let input_idx = sorted_inputs
                .iter()
                .position(|e| *e == sorted)
                .unwrap_or_else(|| {
                    panic!(
                        "could not output {} in inputs [{}]",
                        std::str::from_utf8(&sorted).unwrap(),
                        sorted_inputs
                            .iter()
                            .map(|v| std::str::from_utf8(v).unwrap())
                            .join(", ")
                    )
                });
            *decoded = self.numbers[input_idx];
        }

        Decoded {
            input: self.numbers,
            output: decoded_output,
        }
    }
}

fn solve_helper(state: &ProblemState, input: &[Vec<AsciiChar>; 10], idx: usize) -> Option<Decoded> {
    if idx >= input.len() {
        return Some(state.decode());
    }

    for possibility in possibilities(input[idx].len()) {
        if state.numbers.contains(possibility) {
            continue; // already assigned
        }
        let updated = state.identify(idx, *possibility);
        if !updated.segments.is_consistent() {
            continue;
        }
        if let Some(solution) = solve_helper(&updated, input, idx + 1) {
            return Some(solution);
        }
    }
    None
}

fn solve(entry: &Entry) -> Decoded {
    let state = ProblemState::zero_knowledge(entry);
    solve_helper(&state, &entry.input, 0)
        .unwrap_or_else(|| panic!("no valid solution found for entry {:?}", entry))
}

pub fn parse(input: &str) -> Result<Vec<Entry>> {
    let mut entries = Vec::new();
    for line in input.lines() {
        if line.trim().is_empty() {
            continue;
        }
        let (i, o) = line
            .split_once("|")
            .ok_or_else(|| anyhow!("line did not contain a |: {}", line))?;
        // split lines and collect into vec of vec of bytes
        let i_vec: Vec<Vec<AsciiChar>> = i
            .trim()
            .split_ascii_whitespace()
            .map(|i| i.bytes().collect())
            .collect();
        let o_vec: Vec<Vec<AsciiChar>> = o
            .trim()
            .split_ascii_whitespace()
            .map(|i| i.bytes().collect())
            .collect();
        // turn those into arrays of vecs of bytes of the correct length
        let input = i_vec
            .try_into()
            .map_err(|v: Vec<_>| anyhow!("{}: expected 10 values but got {}", i, v.len()))?;
        let output = o_vec
            .try_into()
            .map_err(|v: Vec<_>| anyhow!("{}: expected 10 values but got {}", i, v.len()))?;
        entries.push(Entry { input, output });
    }
    Ok(entries)
}

pub fn part1(input: &[Entry]) -> usize {
    input
        .iter()
        .flat_map(|e| e.output.iter())
        .filter(|signal| [2, 4, 3, 7].contains(&signal.len()))
        .count()
}

pub fn part2(input: &[Entry]) -> u32 {
    input.iter().map(|entry| solve(entry).read_output()).sum()
}

#[cfg(test)]
mod tests {
    use super::*;

    const INPUT: &str = include_str!("test-data/day08.txt");

    #[test]
    fn test_parse() {
        let parsed = parse(INPUT).unwrap();
        assert_eq!(parsed.len(), 10);
        assert_eq!(
            parsed[0].input,
            [
                b"be".to_vec(),
                b"cfbegad".to_vec(),
                b"cbdgef".to_vec(),
                b"fgaecd".to_vec(),
                b"cgeb".to_vec(),
                b"fdcge".to_vec(),
                b"agebfd".to_vec(),
                b"fecdb".to_vec(),
                b"fabcd".to_vec(),
                b"edb".to_vec()
            ]
        );
        assert_eq!(
            parsed[0].output,
            [
                b"fdgacbe".to_vec(),
                b"cefdb".to_vec(),
                b"cefbgd".to_vec(),
                b"gcbe".to_vec()
            ]
        );
    }

    #[test]
    fn test_part1() {
        let parsed = parse(INPUT).unwrap();
        assert_eq!(part1(&parsed), 26);
    }

    #[test]
    fn test_solve() {
        let input = "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | \
            cdfeb fcadb cdfeb cdbaf";
        let parsed = parse(input).unwrap();
        let decoded = solve(&parsed[0]);
        assert_eq!(decoded.input, [8, 5, 2, 3, 7, 9, 6, 4, 0, 1]);
        assert_eq!(decoded.output, [5, 3, 5, 3]);
        assert_eq!(decoded.read_output(), 5353);
    }

    #[test]
    fn test_part2() {
        let parsed = parse(INPUT).unwrap();
        assert_eq!(part2(&parsed), 61229);
    }
}
