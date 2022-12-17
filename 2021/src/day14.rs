use anyhow::{Context, Result};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Rule {
    left: u8,
    right: u8,
    insert: u8,
}

type Counts = BTreeMap<u8, usize>;

pub fn parse(input: &str) -> Result<Vec<i32>> {
    todo!()
}

// actually let's just do this ourselves and save ourselves the copies
#[cached(
    key = "(Rule, usize, Vec<Rule>)",
    convert = r#"{ (rule.clone(), depth, rules.to_owned()) }"#
)]
fn generate(rule: &Rule, depth: usize, rules: &[Rule]) -> Counts {
    if depth == 0 {
        return [(rule.left, 1), (rule.right, 1)].into_iter().collect();
    }
    let mut lhs = BTreeMap::new();
    for next_rule in rules {
        if next_rule.left == rule.left && next_rule.right == rule.insert {
            lhs = generate(next_rule, depth - 1, rules);
        }
    }
    let mut rhs = BTreeMap::new();
    for next_rule in rules {
        if next_rule.left == rule.left && next_rule.right == rule.insert {
            lhs = generate(next_rule, depth - 1, rules);
        }
        if next_rule.left == rule.insert && next_rule.right == rule.right {
            rhs = generate(next_rule, depth - 1, rules);
        }
    }
    for (k, v) in rhs {
        *lhs.entry(k).or_default() += v;
    }
    lhs
}

fn apply_rules(input: &str, rules: &[Rule], depth: usize) -> Counts {
    // strategy: use dynamic programming + recursion
    todo!()
}

pub fn part1(input: &[i32]) -> usize {
    todo!()
}

pub fn part2(input: &[i32]) -> usize {
    todo!()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        todo!()
    }
}
