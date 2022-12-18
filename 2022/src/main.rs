mod day01;
mod day02;

use anyhow::{Context, Result};
use std::time::Instant;

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

        let parsed = {
            let start = Instant::now();
            let parsed = $i::parse(&input)?;
            let end = Instant::now();
            println!("day {} parsed in {} µs", n, (end - start).as_micros());
            parsed
        };

        {
            let start = Instant::now();
            let answer = $i::part1(&parsed);
            let end = Instant::now();
            println!(
                "day {} part 1: {} ({} µs)",
                n,
                answer,
                (end - start).as_micros()
            );
        }
        {
            let start = Instant::now();
            let answer = $i::part2(&parsed);
            let end = Instant::now();
            println!(
                "day {} part 2: {} ({} µs)",
                n,
                answer,
                (end - start).as_micros()
            );
        }
    };
}

fn main() -> Result<()> {
    let start = Instant::now();
    aoc!(day01);
    aoc!(day02);
    let end = Instant::now();
    println!("total runtime: {} µs", (end - start).as_micros());

    Ok(())
}