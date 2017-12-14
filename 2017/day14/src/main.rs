extern crate structopt;
#[macro_use]
extern crate structopt_derive;

use structopt::StructOpt;

fn bits(byte: u8) -> Vec<bool> {
    vec![
        byte >> 7 & 0x1 == 0x1,
        byte >> 6 & 0x1 == 0x1,
        byte >> 5 & 0x1 == 0x1,
        byte >> 4 & 0x1 == 0x1,
        byte >> 3 & 0x1 == 0x1,
        byte >> 2 & 0x1 == 0x1,
        byte >> 1 & 0x1 == 0x1,
        byte & 0x1 == 0x1,
    ]
}

fn reverse<T>(input: &mut [T], mut start: usize, mut len: usize) {
    loop {
        let a = start % input.len();
        let b = (start + len - 1) % input.len();
        input.swap(a, b);
        start += 1;
        if len <= 2 {
            break;
        } else {
            len -= 2;
        }
    }
}

fn shuffle<T, Iter>(input: &mut [T], lengths: Iter)
where
    Iter: IntoIterator<Item = usize>,
{
    let mut position = 0;
    for (skip, length) in lengths.into_iter().enumerate() {
        reverse(input, position, length);
        position += length + skip;
    }
}

fn knot_hash(input: &str) -> Vec<u8> {
    let mut output: Vec<u8> = (0..256u16).map(|i| i as u8).collect();
    let mut lengths = input.trim().as_bytes().to_owned();
    lengths.extend(vec![17, 31, 73, 47, 23]);
    let repeated = lengths
        .iter()
        .cycle()
        .map(|i| *i as usize)
        .take(lengths.len() * 64);
    shuffle(&mut output, repeated);
    output
        .chunks(16)
        .map(|chunk| chunk.iter().fold(0, |n, m| n ^ m))
        .collect()
}

fn generate_state(input: &str) -> Vec<Vec<bool>> {
    (0..128)
        .map(|row| {
            knot_hash(&format!("{}-{}", input, row))
                .into_iter()
                .flat_map(|byte| bits(byte).into_iter())
                .collect()
        })
        .collect()
}

fn neighbors(start: (usize, usize), map: &[Vec<bool>]) -> Vec<(usize, usize)> {
    let (r, c) = start;
    let (max_r, max_c) = (
        map.len() - 1,
        map.get(0).map(|v| v.len().max(1) - 1).unwrap_or(0),
    );
    let mut neighbors = Vec::new();
    if r != 0 {
        neighbors.push((r - 1, c));
    }
    if c != 0 {
        neighbors.push((r, c - 1));
    }
    if c != max_c {
        neighbors.push((r, c + 1));
    }
    if r != max_r {
        neighbors.push((r + 1, c));
    }
    neighbors
}

fn find_group(
    start: (usize, usize),
    map: &[Vec<bool>],
    visited: &mut Vec<Vec<bool>>,
) -> Vec<(usize, usize)> {
    let mut group = Vec::new();
    let mut edge = vec![start];
    while let Some(node) = edge.pop() {
        if !map[node.0][node.1] || visited[node.0][node.1] {
            continue;
        }
        visited[node.0][node.1] = true;
        edge.extend(
            neighbors(node, map)
                .into_iter()
                .filter(|&(r, c)| map[r][c] && !visited[r][c]),
        );
        group.push(node);
    }
    group
}

fn part1(input: &str) -> usize {
    let rows = generate_state(input);
    rows.iter()
        .map(|row| row.iter().filter(|x| **x).count())
        .sum::<usize>()
}

fn part2(input: &str) -> usize {
    let rows = generate_state(input);
    let mut visited = vec![vec![false; 128]; 128];
    let mut groups = Vec::new();
    for (r, row) in rows.iter().enumerate() {
        for (c, &bit) in row.iter().enumerate() {
            if bit && !visited[r][c] {
                let new_group = find_group((r, c), &rows, &mut visited);
                for loc in &new_group {
                    visited[loc.0][loc.1] = true;
                }
                groups.push(new_group);
            }
        }
    }
    groups.len()
}

fn main() {
    let opt = Opt::from_args();
    println!("Part 1: {}", part1(&opt.input));
    println!("Part 2: {}", part2(&opt.input));
}

#[derive(StructOpt, Debug)]
#[structopt(name = "day13", about = "Advent of code 2017 day 13")]
struct Opt {
    #[structopt(help = "Input")] input: String,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn part1_test() {
        assert_eq!(part1("flqrgnkx"), 8108);
    }

    #[test]
    fn part2_test() {
        assert_eq!(part2("flqrgnkx"), 1242);
    }
}
