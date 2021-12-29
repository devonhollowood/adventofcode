mod day01;
mod day02;

use anyhow::{Context, Result};

macro_rules! aoc {
    ($i:ident) => {
        let s = stringify!($i);
        let n = s
            .strip_prefix("day")
            .expect("argument to aoc!() must start with 'day'");
        let path = format!("input/{}.txt", s);
        let input = std::fs::read_to_string(&path).with_context(|| {
            format!(
                "could not read file '{}'. Please run from the crate root.",
                &path
            )
        })?;
        let parsed = $i::parse(&input)?;
        println!("day {} part 1: {}", n, $i::part1(&parsed)?);
        println!("day {} part 2: {}", n, $i::part2(&parsed)?);
    };
}

fn main() -> Result<()> {
    aoc!(day01);
    aoc!(day02);

    Ok(())
}
