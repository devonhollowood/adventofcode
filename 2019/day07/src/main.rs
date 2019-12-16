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
                let interpreter = intcode::Interpreter::new(tape.to_vec()).with_input(&input);
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

fn part2(tape: &[isize]) -> isize {
    use itertools::Itertools;
    (5..=9)
        .permutations(5)
        .map(|ordering| {
            let mut signal = 0;
            let mut inputs: Vec<_> = ordering.iter().map(|phase| vec![*phase]).collect();
            let mut interpreters = vec![intcode::Interpreter::new(tape.to_vec()); 5];
            for amp in (0..=4).cycle() {
                let mut input = inputs[amp].clone();
                input.push(signal);
                let interpreter = interpreters[amp].clone().with_input(&input);
                match interpreter.run() {
                    Ok(output) => {
                        signal = *output.output().last().expect("Empty output stream");
                        if amp == 4 {
                            break;
                        }
                    }
                    Err((intcode::ProgramError::NoMoreInput, int)) => {
                        signal = *int.output().last().expect("Empty output stream");
                        inputs[amp] = int.input();
                        interpreters[amp] = int.with_input(&inputs[amp]);
                    }
                    Err(err) => panic!("Invalid program: {:?}", err),
                };
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
    println!("Part 2: {}", part2(&input));
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

    #[test]
    fn test_part2() {
        let expected = [
            (
                vec![
                    3, 26, 1001, 26, -4, 26, 3, 27, 1002, 27, 2, 27, 1, 27, 26, 27, 4, 27, 1001,
                    28, -1, 28, 1005, 28, 6, 99, 0, 0, 5,
                ],
                139629729,
            ),
            (
                vec![
                    3, 52, 1001, 52, -5, 52, 3, 53, 1, 52, 56, 54, 1007, 54, 5, 55, 1005, 55, 26,
                    1001, 54, -5, 54, 1105, 1, 12, 1, 53, 54, 53, 1008, 54, 0, 55, 1001, 55, 1, 55,
                    2, 53, 55, 53, 4, 53, 1001, 56, -1, 56, 1005, 56, 6, 99, 0, 0, 0, 0, 10,
                ],
                18216,
            ),
        ];
        for (input, output) in &expected {
            assert_eq!(part2(input), *output);
        }
    }
}
