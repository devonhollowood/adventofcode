use std::path::PathBuf;
use structopt::StructOpt;

fn part1(input: &[isize]) -> isize {
    intcode::compute(12, 2, input.to_vec())
}

fn part2(input: &[isize]) -> isize {
    for noun in 0..99 {
        for verb in 0..=noun {
            if intcode::compute(noun, verb, input.to_vec()) == 1969_0720 {
                return 100 * noun + verb;
            }
        }
    }
    panic!("No noun / verb pair found")
}

fn main() {
    let input: Vec<_> = std::fs::read_to_string(Opt::from_args().input)
        .expect("error reading file")
        .split(',')
        .map(|num| {
            num.trim()
                .parse::<isize>()
                .unwrap_or_else(|err| panic!("could not parse token \"{}\": {:?}", num, err))
        })
        .collect();
    println!("Part 1: {}", part1(&input));
    println!("Part 2: {}", part2(&input));
}

#[derive(StructOpt)]
struct Opt {
    #[structopt(parse(from_os_str))]
    input: PathBuf,
}
