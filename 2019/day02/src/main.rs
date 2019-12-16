use std::path::PathBuf;
use structopt::StructOpt;

fn part1(input: &[isize]) -> isize {
    intcode::Interpreter::new(input.to_vec())
        .run_with_noun_and_verb(12, 2)
        .unwrap_or_else(|err| panic!("Error running intcode: {:?}", err))
        .first_elem()
}

fn part2(input: &[isize]) -> isize {
    for noun in 0..99 {
        for verb in 0..=noun {
            if intcode::Interpreter::new(input.to_vec())
                .run_with_noun_and_verb(noun, verb)
                .unwrap_or_else(|err| panic!("Error running intcode: {:?}", err))
                .first_elem()
                == 1969_0720
            {
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
