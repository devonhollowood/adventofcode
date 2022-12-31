use anyhow::{bail, Context, Result};
use std::collections::BTreeMap;

#[derive(Debug, Clone)]
pub enum DirEntry {
    /// file -> name, size
    File(String, usize),
    /// dir -> name
    Dir(String),
}

#[derive(Debug, Clone)]
pub enum Command {
    /// cd + destination
    Cd(String),
    /// ls + list of entries in response
    Ls(Vec<DirEntry>),
}

type Path = Vec<String>;

type DirTree = BTreeMap<Path, Vec<DirEntry>>;

type DirSizes = BTreeMap<Path, usize>;

fn parse_dir_tree(commands: &[Command]) -> DirTree {
    let mut cwd = Vec::new();
    let mut tree = DirTree::new();
    for command in commands {
        match command {
            Command::Cd(dir) => {
                if dir == ".." {
                    cwd.pop();
                } else if dir == "/" {
                    cwd = Vec::new();
                } else {
                    cwd.push(dir.to_string());
                }
            }
            Command::Ls(entries) => {
                *tree.entry(cwd.clone()).or_default() = entries.clone();
            }
        }
    }
    tree
}

fn generate_subdir_sizes(tree: &DirTree, root: &Path, dir_sizes: &mut DirSizes) -> usize {
    if let Some(dir_size) = dir_sizes.get(root) {
        return *dir_size;
    }
    let mut dir_size = 0;
    for entry in tree[root].iter() {
        match entry {
            DirEntry::Dir(name) => {
                let mut next_dir = root.clone();
                next_dir.push(name.to_string());
                let subdir_size = generate_subdir_sizes(tree, &next_dir, dir_sizes);
                dir_size += subdir_size;
            }
            DirEntry::File(_name, file_size) => dir_size += file_size,
        }
    }
    let prev = dir_sizes.insert(root.to_vec(), dir_size);
    if let Some(old) = prev {
        panic!(
            "tried to insert size {} for dir {}, but had previous value {}",
            dir_size,
            root.join("/"),
            old
        );
    }
    dir_size
}

pub fn parse(input: &str) -> Result<DirSizes> {
    let mut parsed = Vec::new();
    let mut lines = input.lines().peekable();
    while let Some(line) = lines.next() {
        if let Some(dir) = line.strip_prefix("$ cd ") {
            parsed.push(Command::Cd(dir.to_string()));
            continue;
        }
        if line.starts_with("$ ls") {
            let mut entries = Vec::new();
            while lines.peek().map(|l| !l.starts_with('$')).unwrap_or(false) {
                let line = lines.next().unwrap();
                let (size, name) = line
                    .split_once(' ')
                    .with_context(|| format!("unexpected ls response: {}", line))?;
                if size == "dir" {
                    entries.push(DirEntry::Dir(name.to_string()));
                    continue;
                }
                let size = size
                    .parse()
                    .with_context(|| format!("could not parse size in line: {}", line))?;
                entries.push(DirEntry::File(name.to_string(), size));
            }
            parsed.push(Command::Ls(entries));
            continue;
        }
        bail!("unrecognized line type: {}", line);
    }
    let tree = parse_dir_tree(&parsed);
    let mut dir_sizes = BTreeMap::new();
    generate_subdir_sizes(&tree, &Vec::new(), &mut dir_sizes);
    Ok(dir_sizes)
}

pub fn part1(input: &DirSizes) -> usize {
    input
        .iter()
        .filter_map(|(dir, size)| if dir.is_empty() { None } else { Some(*size) })
        .filter(|size| *size < 100000)
        .sum()
}

pub fn part2(input: &DirSizes) -> usize {
    let capacity = 70000000;
    let used = input[&Vec::new()];
    let avail = capacity - used;
    let needed = 30000000 - avail;
    input
        .iter()
        .filter_map(|(_dir, size)| if *size > needed { Some(*size) } else { None })
        .min()
        .expect("no directories were large enough")
}

#[cfg(test)]
mod tests {
    use super::*;

    const INPUT: &str = include_str!("test-data/day07.txt");

    #[test]
    fn test_part1() {
        let parsed = parse(INPUT).unwrap();
        assert_eq!(part1(&parsed), 95437);
    }

    #[test]
    fn test_part2() {
        let parsed = parse(INPUT).unwrap();
        assert_eq!(part2(&parsed), 24933642);
    }
}
