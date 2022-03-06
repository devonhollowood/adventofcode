use anyhow::{anyhow, Result};
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct Graph {
    adjacencies: HashMap<String, Vec<String>>,
}

pub fn parse(input: &str) -> Result<Graph> {
    let mut adjacencies: HashMap<String, Vec<String>> = HashMap::new();
    for line in input.lines().map(|l| l.trim()).filter(|l| !l.is_empty()) {
        if let Some((a, b)) = line.split_once('-') {
            adjacencies
                .entry(a.to_string())
                .or_default()
                .push(b.to_string());
            adjacencies
                .entry(b.to_string())
                .or_default()
                .push(a.to_string());
        } else {
            return Err(anyhow!("line \"{}\" did not contain a dash", line));
        }
    }
    Ok(Graph { adjacencies })
}

fn paths(start: &str, end: &str, graph: &Graph, path_so_far: &[String]) -> Vec<Vec<String>> {
    if start == end {
        return vec![vec![end.to_string()]];
    }
    let mut paths_found = vec![];
    let mut updated_path = path_so_far.to_vec();
    updated_path.push(start.to_string());
    for neighbor in graph.adjacencies[start].iter() {
        let is_small = neighbor.chars().all(|c| c.is_ascii_lowercase());
        if is_small && path_so_far.contains(neighbor) {
            continue;
        }
        for found in paths(neighbor, end, graph, &updated_path) {
            paths_found.push(
                std::iter::once(start.to_owned())
                    .chain(found.into_iter())
                    .collect(),
            )
        }
    }
    paths_found
}

fn paths_2(
    start: &str,
    end: &str,
    graph: &Graph,
    path_so_far: &[String],
    revisited: bool,
) -> Vec<Vec<String>> {
    if start == end {
        return vec![vec![end.to_string()]];
    }
    let mut paths_found = vec![];
    let mut updated_path = path_so_far.to_vec();
    updated_path.push(start.to_string());
    for neighbor in graph.adjacencies[start].iter() {
        let mut revisited = revisited; // this one applies for this neighbor and child calls
        let is_small = neighbor.chars().all(|c| c.is_ascii_lowercase());
        if neighbor == "start" {
            continue;
        }
        if is_small && path_so_far.contains(neighbor) {
            if revisited {
                continue;
            } else {
                revisited = true;
            }
        }
        for found in paths_2(neighbor, end, graph, &updated_path, revisited) {
            paths_found.push(
                std::iter::once(start.to_owned())
                    .chain(found.into_iter())
                    .collect(),
            )
        }
    }
    paths_found
}

pub fn part1(input: &Graph) -> usize {
    paths("start", "end", input, &[]).len()
}

pub fn part2(input: &Graph) -> usize {
    paths_2("start", "end", input, &[], false).len()
}

#[cfg(test)]
mod tests {
    use super::*;

    const A: &str = r"
start-A
start-b
A-c
A-b
b-d
A-end
b-end";

    const B: &str = r"
dc-end
HN-start
start-kj
dc-start
dc-HN
LN-dc
HN-end
kj-sa
kj-HN
kj-dc";

    const C: &str = r"
fs-end
he-DX
fs-he
start-DX
pj-DX
end-zg
zg-sl
zg-pj
pj-he
RW-he
fs-DX
pj-RW
zg-RW
start-pj
he-WI
zg-he
pj-fs
start-RW
";

    #[test]
    fn test_part1() {
        assert_eq!(part1(&parse(A).unwrap()), 10);
        assert_eq!(part1(&parse(B).unwrap()), 19);
        assert_eq!(part1(&parse(C).unwrap()), 226);
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2(&parse(A).unwrap()), 36);
        assert_eq!(part2(&parse(B).unwrap()), 103);
        assert_eq!(part2(&parse(C).unwrap()), 3509);
    }
}
