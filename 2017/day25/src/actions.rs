use cpu::ComputerState;

pub fn input_action(state: &mut char, cpu: &mut ComputerState) {
    match *state {
        'A' => {
            if cpu.read() {
                cpu.write(false);
                cpu.move_right();
                *state = 'F';
            } else {
                cpu.write(true);
                cpu.move_right();
                *state = 'B';
            }
        }
        'B' => {
            if cpu.read() {
                cpu.write(true);
                cpu.move_left();
                *state = 'C';
            } else {
                cpu.write(false);
                cpu.move_left();
                *state = 'B';
            }
        }
        'C' => {
            if cpu.read() {
                cpu.write(false);
                cpu.move_right();
                *state = 'C';
            } else {
                cpu.write(true);
                cpu.move_left();
                *state = 'D';
            }
        }
        'D' => {
            if cpu.read() {
                cpu.write(true);
                cpu.move_right();
                *state = 'A';
            } else {
                cpu.write(true);
                cpu.move_left();
                *state = 'E';
            }
        }
        'E' => {
            if cpu.read() {
                cpu.write(false);
                cpu.move_left();
                *state = 'D';
            } else {
                cpu.write(true);
                cpu.move_left();
                *state = 'F';
            }
        }
        'F' => {
            if cpu.read() {
                cpu.write(false);
                cpu.move_left();
                *state = 'E';
            } else {
                cpu.write(true);
                cpu.move_right();
                *state = 'A';
            }
        }
        _ => panic!("Invalid state: {}", state),
    }
}

#[cfg(test)]
pub fn test_action(state: &mut char, cpu: &mut ComputerState) {
    match *state {
        'A' => {
            if cpu.read() {
                cpu.write(false);
                cpu.move_left();
                *state = 'B';
            } else {
                cpu.write(true);
                cpu.move_right();
                *state = 'B';
            }
        }
        'B' => {
            if cpu.read() {
                cpu.write(true);
                cpu.move_right();
                *state = 'A';
            } else {
                cpu.write(true);
                cpu.move_left();
                *state = 'A';
            }
        }
        _ => panic!("Invalid state: {}", state),
    }
}
