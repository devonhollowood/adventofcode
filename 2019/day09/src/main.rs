use std::path::PathBuf;
use structopt::StructOpt;

fn part1(tape: &[isize]) -> isize {
    let output = intcode::Interpreter::new(tape.to_vec())
        .with_input(&[1])
        .run()
        .unwrap_or_else(|err| panic!("Error running intcode: {:?}", err));
    let (final_result, test_results) = output.output().split_last().expect("No output produced");
    for res in test_results {
        assert_eq!(*res, 0, "test failed with output: {}", res);
    }
    *final_result
}

fn part2(tape: &[isize]) -> isize {
    let output = intcode::Interpreter::new(tape.to_vec())
        .with_input(&[2])
        .run()
        .unwrap_or_else(|err| panic!("Error running intcode: {:?}", err));
    assert_eq!(output.output().len(), 1);
    output.output()[0]
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
