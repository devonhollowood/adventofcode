mod actions;
mod cpu;

use cpu::ComputerState;

struct Blueprint {
    state: char,
    action: Box<FnMut(&mut char, &mut ComputerState)>,
}

impl Blueprint {
    fn act(&mut self, cpu: &mut ComputerState) {
        (self.action)(&mut self.state, cpu);
    }
}

fn part1(mut blueprint: Blueprint, steps: usize) -> usize {
    let mut cpu = ComputerState::new();
    for _ in 0..steps {
        blueprint.act(&mut cpu);
    }
    cpu.checksum()
}

fn main() {
    let blueprint = Blueprint {
        state: 'A',
        action: Box::new(actions::input_action),
    };
    println!("Part 1: {}", part1(blueprint, 12_425_180));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn part1_test() {
        let blueprint = Blueprint {
            state: 'A',
            action: Box::new(actions::test_action),
        };
        assert_eq!(part1(blueprint, 6), 3);
    }
}
