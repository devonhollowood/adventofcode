/// Run a program, using `noun` in position 1 and `verb` in position 2.
/// Return the value at position 0 in the end.
pub fn compute(noun: isize, verb: isize, mut tape: Vec<isize>) -> isize {
    tape[1] = noun;
    tape[2] = verb;
    run_tape(&mut tape);
    tape[0]
}

/// Run a program tape through to the end.
pub fn run_tape(tape: &mut [isize]) {
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_run_tape() {
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
            run_tape(&mut input);
            assert_eq!(input, output);
        }
    }
}
