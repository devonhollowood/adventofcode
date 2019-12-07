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
        use ProgramError::*;
        let opcode = instruction % 100;
        let parameter_mode = instruction / 100;
        match opcode {
            1 => {
                let mut params = [Parameter::default(); 3];
                self.get_params(parameter_mode, &mut params)?;
                *self.deref_mut(params[2])? = self.value_of(params[0]) + self.value_of(params[1]);
                self.position += 4;
            }
            2 => {
                let mut params = [Parameter::default(); 3];
                self.get_params(parameter_mode, &mut params)?;
                *self.deref_mut(params[2])? = self.value_of(params[0]) * self.value_of(params[1]);
                self.position += 4;
            }
            3 => {
                let mut params = [Parameter::default(); 1];
                self.get_params(parameter_mode, &mut params)?;
                *self.deref_mut(params[0])? = self.input[0];
                self.input = &self.input[1..];
                self.position += 2;
            }
            4 => {
                let mut params = [Parameter::default(); 1];
                self.get_params(parameter_mode, &mut params)?;
                self.output.push(self.value_of(params[0]));
                self.input = &self.input[1..];
                self.position += 2;
            }
            99 => return Ok(true),
            _ => return Err(UnknownOpcode(opcode)),
        }
        Ok(false)
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
}
