use anyhow::{anyhow, Result};
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct Graph {
    adjacencies: Vec<Vec<usize>>,  // idx -> list of neighboring idxs
    smalls: Vec<usize>,            // index of small caves
    names: HashMap<String, usize>, // name -> idx
}

pub fn parse(input: &str) -> Result<Graph> {
    let mut adjacencies = Vec::new();
    let mut smalls = Vec::new();
    let mut names = HashMap::new();
    for line in input.lines().map(|l| l.trim()).filter(|l| !l.is_empty()) {
        if let Some((a, b)) = line.split_once('-') {
            let a_idx = *names.entry(a.to_string()).or_insert_with(|| {
                adjacencies.push(vec![]);
                let idx = adjacencies.len() - 1;
                if a.chars().all(|c| c.is_ascii_lowercase()) {
                    smalls.push(idx);
                }
                idx
            });
            let b_idx = *names.entry(b.to_string()).or_insert_with(|| {
                adjacencies.push(vec![]);
                let idx = adjacencies.len() - 1;
                if b.chars().all(|c| c.is_ascii_lowercase()) {
                    smalls.push(idx);
                }
                idx
            });
            adjacencies[a_idx].push(b_idx);
            adjacencies[b_idx].push(a_idx);
        } else {
            return Err(anyhow!("line \"{}\" did not contain a dash", line));
        }
    }
    Ok(Graph {
        adjacencies,
        smalls,
        names,
    })
}

fn part1_helper(start: usize, end: usize, graph: &Graph, path_so_far: &mut Vec<usize>) -> usize {
    if start == end {
        return 1;
    }
    let mut paths_found = 0;
    path_so_far.push(start);
    for neighbor in graph.adjacencies[start].iter().copied() {
        if graph.smalls.contains(&neighbor) && path_so_far.contains(&neighbor) {
            continue;
        }
        paths_found += part1_helper(neighbor, end, graph, path_so_far);
    }
    path_so_far.pop();
    paths_found
}

fn part2_helper(
    start: usize,
    end: usize,
    graph: &Graph,
    path_so_far: &mut Vec<usize>,
    revisited: bool,
) -> usize {
    if start == end {
        return 1;
    }
    let mut paths_found = 0;
    path_so_far.push(start);
    let origin = graph.names["start"];
    for neighbor in graph.adjacencies[start].iter().copied() {
        if neighbor == origin {
            continue;
        }
        let mut revisited = revisited; // this one applies for this neighbor and child calls
        if graph.smalls.contains(&neighbor) && path_so_far.contains(&neighbor) {
            if revisited {
                continue;
            } else {
                revisited = true;
            }
        }
        paths_found += part2_helper(neighbor, end, graph, path_so_far, revisited);
    }
    path_so_far.pop();
    paths_found
}

pub fn part1(input: &Graph) -> usize {
    part1_helper(input.names["start"], input.names["end"], input, &mut vec![])
}

pub fn part2(input: &Graph) -> usize {
    part2_helper(
        input.names["start"],
        input.names["end"],
        input,
        &mut vec![],
        false,
    )
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
