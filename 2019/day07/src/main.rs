use std::path::PathBuf;
use structopt::StructOpt;

fn part1(tape: &[isize]) -> isize {
    use itertools::Itertools;
    (0..=4)
        .permutations(5)
        .map(|ordering| {
            let mut signal = 0;
            for phase in ordering {
                let input = [phase, signal];
                let interpreter = intcode::Interpreter::with_input(tape, &input);
                let output = interpreter
                    .run()
                    .unwrap_or_else(|err| panic!("Invalid program: {:?}", err));
                assert_eq!(output.output().len(), 1);
                signal = output.output()[0];
            }
            signal
        })
        .max()
        .expect("Expected >0 possible permutations")
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
}

#[derive(StructOpt)]
struct Opt {
    #[structopt(parse(from_os_str))]
    input: PathBuf,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        let expected = [
            (
                vec![
                    3, 15, 3, 16, 1002, 16, 10, 16, 1, 16, 15, 15, 4, 15, 99, 0, 0,
                ],
                43210,
            ),
            (
                vec![
                    3, 23, 3, 24, 1002, 24, 10, 24, 1002, 23, -1, 23, 101, 5, 23, 23, 1, 24, 23,
                    23, 4, 23, 99, 0, 0,
                ],
                54321,
            ),
            (
                vec![
                    3, 31, 3, 32, 1002, 32, 10, 32, 1001, 31, -2, 31, 1007, 31, 0, 33, 1002, 33, 7,
                    33, 1, 33, 31, 31, 1, 32, 31, 31, 4, 31, 99, 0, 0, 0,
                ],
                65210,
            ),
        ];
        for (input, output) in &expected {
            assert_eq!(part1(input), *output);
        }
    }
}
