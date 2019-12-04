use std::path::PathBuf;
use structopt::StructOpt;

fn run_program(tape: &mut [isize]) {
    let mut ip = 0; // instruction pointer
    loop {
        match tape[ip] {
            1 => {
                let plhs = tape[ip + 1] as usize;
                let prhs = tape[ip + 2] as usize;
                let pout = tape[ip + 3] as usize;
                tape[pout] = tape[plhs] + tape[prhs];
                ip += 4;
            }
            2 => {
                let plhs = tape[ip + 1] as usize;
                let prhs = tape[ip + 2] as usize;
                let pout = tape[ip + 3] as usize;
                tape[pout] = tape[plhs] * tape[prhs];
                ip += 4;
            }
            99 => break,
            bad_op => panic!("Invalid instruction at position {}: {}", ip, bad_op),
        }
    }
}

fn part1(input: &[isize]) -> isize {
    let mut input = input.to_vec();
    input[1] = 12;
    input[2] = 2;
    run_program(&mut input);
    input[0]
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
    fn test_run_program() {
        let expected = vec![
            (
                vec![1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50],
                vec![3500, 9, 10, 70, 2, 3, 11, 0, 99, 30, 40, 50],
            ),
            (vec![1, 0, 0, 0, 99], vec![2, 0, 0, 0, 99]),
            (vec![2, 3, 0, 3, 99], vec![2, 3, 0, 6, 99]),
            (vec![2, 4, 4, 5, 99, 0], vec![2, 4, 4, 5, 99, 9801]),
            (
                vec![1, 1, 1, 4, 99, 5, 6, 0, 99],
                vec![30, 1, 1, 4, 2, 5, 6, 0, 99],
            ),
        ];
        for (input, output) in expected {
            let mut input = input.clone();
            run_program(&mut input);
            assert_eq!(input, output);
        }
    }
}
