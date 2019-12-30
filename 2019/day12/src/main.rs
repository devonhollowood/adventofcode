use regex::Regex;
use std::path::PathBuf;
use structopt::StructOpt;

#[derive(Debug, Clone, PartialEq, Eq)]
struct Vec3d {
    x: isize,
    y: isize,
    z: isize,
}

impl Vec3d {
    fn get(&self, dim: Dimension) -> isize {
        match dim {
            X => self.x,
            Y => self.y,
            Z => self.z,
        }
    }

    fn get_mut(&mut self, dim: Dimension) -> &mut isize {
        match dim {
            X => &mut self.x,
            Y => &mut self.y,
            Z => &mut self.z,
        }
    }
}

impl std::ops::Add for &Vec3d {
    type Output = Vec3d;
    fn add(self, rhs: Self) -> Self::Output {
        Vec3d {
            x: self.x + rhs.x,
            y: self.y + rhs.y,
            z: self.z + rhs.z,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Dimension {
    X,
    Y,
    Z,
}

use Dimension::*;

#[derive(Debug, Clone, PartialEq, Eq)]
struct Moon {
    position: Vec3d,
    velocity: Vec3d,
}

impl Moon {
    fn potential(&self) -> isize {
        self.position.x.abs() + self.position.y.abs() + self.position.z.abs()
    }

    fn kinetic(&self) -> isize {
        self.velocity.x.abs() + self.velocity.y.abs() + self.velocity.z.abs()
    }
}

fn parse(input: &str) -> Vec<Moon> {
    let moon_re = Regex::new(r"^<x=(?P<x>-?\d+), y=(?P<y>-?\d+), z=(?P<z>-?\d+)>$").unwrap();
    input
        .lines()
        .map(|line| {
            let caps = moon_re
                .captures(line.trim())
                .unwrap_or_else(|| panic!("unreadable line: {}", line));
            Moon {
                position: Vec3d {
                    x: caps.name("x").unwrap().as_str().parse().unwrap(),
                    y: caps.name("y").unwrap().as_str().parse().unwrap(),
                    z: caps.name("z").unwrap().as_str().parse().unwrap(),
                },
                velocity: Vec3d { x: 0, y: 0, z: 0 },
            }
        })
        .collect()
}

fn update(moons: &[Moon]) -> Vec<Moon> {
    // update velocity via gravity
    let mut new_moons = moons.to_owned();
    for mut moon in new_moons.iter_mut() {
        for other in moons {
            moon.velocity.x += (other.position.x - moon.position.x).signum();
            moon.velocity.y += (other.position.y - moon.position.y).signum();
            moon.velocity.z += (other.position.z - moon.position.z).signum();
        }
    }
    // update position via velocity
    for mut moon in new_moons.iter_mut() {
        moon.position = &moon.position + &moon.velocity;
    }
    new_moons
}

fn update_along_dimension(moons: &[Moon], dim: Dimension) -> Vec<Moon> {
    // update velocity via gravity
    let mut new_moons = moons.to_owned();
    for moon in new_moons.iter_mut() {
        for other in moons {
            *moon.velocity.get_mut(dim) +=
                (other.position.get(dim) - moon.position.get(dim)).signum();
        }
    }
    // update position via velocity
    for moon in new_moons.iter_mut() {
        *moon.position.get_mut(dim) += moon.velocity.get(dim);
    }
    new_moons
}

fn orbit(moons: &[Moon], dim: Dimension) -> usize {
    for (count, state) in std::iter::successors(Some(moons.to_owned()), |moons| {
        Some(update_along_dimension(moons, dim))
    })
    .enumerate()
    .skip(1)
    {
        if state == moons {
            return count;
        }
    }
    unreachable!()
}

fn part1(input: &[Moon], count: usize) -> isize {
    std::iter::successors(Some(input.to_owned()), |moons| Some(update(moons)))
        .nth(count)
        .unwrap()
        .into_iter()
        .map(|moon| moon.potential() * moon.kinetic())
        .sum()
}

fn part2(input: &[Moon]) -> usize {
    use num::integer::Integer;
    let x = orbit(input, X);
    let y = orbit(input, Y);
    let z = orbit(input, Z);
    x.lcm(&y).lcm(&z)
}

fn main() {
    let input: Vec<_> =
        parse(&std::fs::read_to_string(Opt::from_args().input).expect("error reading file"));
    println!("Part 1: {}", part1(&input, 1000));
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
        assert_eq!(part1(&parse(include_str!("../examples/1.txt")), 10), 179);
        assert_eq!(part1(&parse(include_str!("../examples/2.txt")), 100), 1940);
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2(&parse(include_str!("../examples/1.txt"))), 2772);
        assert_eq!(part2(&parse(include_str!("../examples/2.txt"))), 4686774924);
    }
}
