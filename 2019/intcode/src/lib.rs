use std::convert::TryFrom;

/// Intcode Programming error
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ProgramError {
    /// Unrecognized opcode. Argument is the given opcode
    UnknownOpcode(isize),
    /// Invalid parameter mode for the given opcode. Argument is the given parameter mode
    InvalidParameterMode(isize),
    /// Address out of bounds. Argument is the given address
    InvalidAddress(isize),
    /// Expected a position parameter, but received a non-position parameter
    ExpectedPosition,
    /// Ran out of input
    NoMoreInput,
}

#[derive(Debug, Clone, Copy)]
enum Parameter {
    Ptr(usize),
    Value(isize),
}

impl Default for Parameter {
    fn default() -> Self {
        Self::Value(0)
    }
}

#[derive(Debug, Clone)]
pub struct Interpreter<'a> {
    tape: Vec<isize>,
    position: usize,
    input: &'a [isize],
    output: Vec<isize>,
}

impl<'a> Interpreter<'a> {
    /// Set up a new interpreter with the given program tape
    pub fn new(tape: &[isize]) -> Self {
        Self::with_input(tape, &[])
    }
    /// Set up a new interpreter with the given program tape and input
    pub fn with_input(tape: &[isize], input: &'a [isize]) -> Self {
        Self {
            tape: tape.to_vec(),
            position: 0,
            input,
            output: Vec::new(),
        }
    }

    /// Run the interpreter
    pub fn run(mut self) -> Result<Output, ProgramError> {
        loop {
            let instruction = self.tape[self.position];
            if self.run_instruction(instruction)? {
                break;
            }
        }
        Ok(Output {
            tape: self.tape,
            output: self.output,
        })
    }

    pub fn run_with_noun_and_verb(
        mut self,
        noun: isize,
        verb: isize,
    ) -> Result<Output, ProgramError> {
        self.tape[1] = noun;
        self.tape[2] = verb;
        self.run()
    }

    fn deref_mut(&mut self, param: Parameter) -> Result<&mut isize, ProgramError> {
        match param {
            Parameter::Ptr(idx) => Ok(&mut self.tape[idx]),
            Parameter::Value(_) => Err(ProgramError::ExpectedPosition),
        }
    }

    fn value_of(&self, param: Parameter) -> isize {
        match param {
            Parameter::Ptr(idx) => self.tape[idx],
            Parameter::Value(val) => val,
        }
    }

    /// Fill the given buffer with parameters for an opcode.
    /// The number of parameters read equals the length of the buffer, and parameters are filled
    /// from left to right.
    /// Assumes current program position is on the opcode for which we are filling in the
    /// parameters.
    fn get_params(
        &self,
        parameter_mode: isize,
        buffer: &mut [Parameter],
    ) -> Result<(), ProgramError> {
        use ProgramError::*;
        let mut mode = parameter_mode;
        for (offset, elem) in buffer.iter_mut().enumerate() {
            let value: isize = self.tape[self.position + offset + 1];
            match mode % 10 {
                0 => {
                    // position mode
                    *elem = Parameter::Ptr(
                        usize::try_from(value).map_err(|_| ProgramError::InvalidAddress(value))?,
                    );
                }
                1 => {
                    // immediate mode
                    *elem = Parameter::Value(value);
                }
                _ => return Err(InvalidParameterMode(parameter_mode)),
            }
            mode /= 10;
        }
        Ok(())
    }

    /// Run a single instruction, mutating self as necessary. On success, return a boolean
    /// expressing whether program should terminate
    fn run_instruction(&mut self, instruction: isize) -> Result<bool, ProgramError> {
        let opcode = instruction % 100;
        let parameter_mode = instruction / 100;
        match opcode {
            1 => {
                // add
                let mut params = [Parameter::default(); 3];
                self.get_params(parameter_mode, &mut params)?;
                *self.deref_mut(params[2])? = self.value_of(params[0]) + self.value_of(params[1]);
                self.position += 4;
            }
            2 => {
                // multiply
                let mut params = [Parameter::default(); 3];
                self.get_params(parameter_mode, &mut params)?;
                *self.deref_mut(params[2])? = self.value_of(params[0]) * self.value_of(params[1]);
                self.position += 4;
            }
            3 => {
                // input
                let mut params = [Parameter::default(); 1];
                self.get_params(parameter_mode, &mut params)?;
                let (input, rest) = self.input.split_first().ok_or(ProgramError::NoMoreInput)?;
                *self.deref_mut(params[0])? = *input;
                self.input = rest;
                self.position += 2;
            }
            4 => {
                // output
                let mut params = [Parameter::default(); 1];
                self.get_params(parameter_mode, &mut params)?;
                self.output.push(self.value_of(params[0]));
                self.position += 2;
            }
            5 => {
                // jump if true
                let mut params = [Parameter::default(); 2];
                self.get_params(parameter_mode, &mut params)?;
                if self.value_of(params[0]) != 0 {
                    let new_pos = self.value_of(params[1]);
                    self.position = usize::try_from(new_pos)
                        .map_err(|_| ProgramError::InvalidAddress(new_pos))?;
                } else {
                    self.position += 3;
                }
            }
            6 => {
                // jump if false
                let mut params = [Parameter::default(); 2];
                self.get_params(parameter_mode, &mut params)?;
                if self.value_of(params[0]) == 0 {
                    let new_pos = self.value_of(params[1]);
                    self.position = usize::try_from(new_pos)
                        .map_err(|_| ProgramError::InvalidAddress(new_pos))?;
                } else {
                    self.position += 3;
                }
            }
            7 => {
                // less than
                let mut params = [Parameter::default(); 3];
                self.get_params(parameter_mode, &mut params)?;
                if self.value_of(params[0]) < self.value_of(params[1]) {
                    *self.deref_mut(params[2])? = 1;
                } else {
                    *self.deref_mut(params[2])? = 0;
                }
                self.position += 4;
            }
            8 => {
                // equals
                let mut params = [Parameter::default(); 3];
                self.get_params(parameter_mode, &mut params)?;
                if self.value_of(params[0]) == self.value_of(params[1]) {
                    *self.deref_mut(params[2])? = 1;
                } else {
                    *self.deref_mut(params[2])? = 0;
                }
                self.position += 4;
            }
            99 => return Ok(true),
            _ => return Err(ProgramError::UnknownOpcode(opcode)),
        }
        Ok(self.position > self.tape.len())
    }
}

pub struct Output {
    output: Vec<isize>,
    tape: Vec<isize>,
}

impl Output {
    pub fn output(&self) -> &[isize] {
        &self.output
    }
    pub fn first_elem(&self) -> isize {
        self.tape[0]
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_run() {
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
            let result = Interpreter::new(&input).run().map(|out| out.tape);
            assert_eq!(result, Ok(output));
        }
    }

    #[test]
    fn test_less() -> Result<(), ProgramError> {
        let tapes = vec![
            vec![3, 9, 7, 9, 10, 9, 4, 9, 99, -1, 8],
            vec![3, 3, 1107, -1, 8, 3, 4, 3, 99],
        ];
        for tape in tapes {
            for input in &[7, 8, 9] {
                let result = Interpreter::with_input(&tape, &[*input]).run()?;
                assert_eq!(result.output()[0], (*input < 8) as isize);
            }
        }
        Ok(())
    }

    #[test]
    fn test_equal() -> Result<(), ProgramError> {
        let tapes = vec![
            vec![3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8],
            vec![3, 3, 1108, -1, 8, 3, 4, 3, 99],
        ];
        for tape in tapes {
            for input in &[7, 8, 9] {
                let result = Interpreter::with_input(&tape, &[*input]).run()?;
                assert_eq!(result.output()[0], (*input == 8) as isize);
            }
        }
        Ok(())
    }

    #[test]
    fn test_run_with_inputs_long() -> Result<(), ProgramError> {
        let tape = vec![
            3, 21, 1008, 21, 8, 20, 1005, 20, 22, 107, 8, 21, 20, 1006, 20, 31, 1106, 0, 36, 98, 0,
            0, 1002, 21, 125, 20, 4, 20, 1105, 1, 46, 104, 999, 1105, 1, 46, 1101, 1000, 1, 20, 4,
            20, 1105, 1, 46, 98, 99,
        ];
        for (input, expected) in &[(7, 999), (8, 1000), (9, 1001)] {
            let result = Interpreter::with_input(&tape, &[*input]).run()?;
            assert_eq!(result.output()[0], *expected);
        }
        Ok(())
    }
}
