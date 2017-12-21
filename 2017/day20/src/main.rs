#[macro_use]
extern crate nom;

extern crate structopt;
#[macro_use]
extern crate structopt_derive;

use structopt::StructOpt;
use std::collections::HashSet;
use std::fs::File;
use std::io::Read;
use std::path::PathBuf;

#[derive(Debug, Clone, PartialEq, Eq)]
struct Vec3 {
    x: isize,
    y: isize,
    z: isize,
}

impl Vec3 {
    fn manhattan_magnitude(&self) -> isize {
        self.x.abs() + self.y.abs() + self.z.abs()
    }
    fn manhattan_distance(&self, other: &Vec3) -> isize {
        (self.x - other.x).abs() + (self.y - other.y).abs()
            + (self.z - other.z).abs()
    }
}

impl std::ops::Add for Vec3 {
    type Output = Self;
    fn add(self, rhs: Self) -> Self::Output {
        Vec3 {
            x: self.x + rhs.x,
            y: self.y + rhs.y,
            z: self.z + rhs.z,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Particle {
    position: Vec3,
    velocity: Vec3,
    acceleration: Vec3,
}

impl Particle {
    fn step(&self) -> Particle {
        let new_velocity = self.velocity.clone() + self.acceleration.clone();
        let new_position = self.position.clone() + new_velocity.clone();
        Particle {
            position: new_position,
            velocity: new_velocity,
            acceleration: self.acceleration.clone(),
        }
    }
    fn done_turning(&self) -> bool {
        (self.acceleration.x == 0
            || self.acceleration.x.signum() == self.velocity.x.signum())
            && (self.acceleration.y == 0
                || self.acceleration.y.signum() == self.velocity.y.signum())
            && (self.acceleration.z == 0
                || self.acceleration.z.signum() == self.velocity.z.signum())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum TrackingStatus {
    Tracking(Particle, Particle),
    Safe,
    Collided,
}

fn track(a: &Particle, b: &Particle) -> TrackingStatus {
    // parallel particles are an edge case here
    if a.velocity == b.velocity && a.acceleration == b.acceleration
        && a.position != b.position
    {
        return TrackingStatus::Safe;
    }
    let new_a = a.step();
    let new_b = b.step();
    if a.position == b.position {
        TrackingStatus::Collided
    } else if a.done_turning() && b.done_turning()
        && new_a.position.manhattan_distance(&new_b.position)
            > a.position.manhattan_distance(&b.position)
    {
        TrackingStatus::Safe
    } else {
        TrackingStatus::Tracking(new_a, new_b)
    }
}

fn part1(particles: &[Particle]) -> usize {
    particles
        .iter()
        .enumerate()
        .min_by(|&(_, a), &(_, b)| {
            a.acceleration
                .manhattan_magnitude()
                .cmp(&b.acceleration.manhattan_magnitude())
                .then(
                    a.velocity
                        .manhattan_magnitude()
                        .cmp(&b.velocity.manhattan_magnitude()),
                )
                .then(
                    a.position
                        .manhattan_magnitude()
                        .cmp(&b.position.manhattan_magnitude()),
                )
        })
        .expect("Need more than one particle")
        .0
}

fn part2(particles: &[Particle]) -> usize {
    let mut tracking_pairs =
        Vec::with_capacity(particles.len() * particles.len() / 2);
    for (idx_a, a) in particles.iter().enumerate() {
        tracking_pairs.extend(
            particles
                .iter()
                .enumerate()
                .skip(idx_a + 1)
                .map(|(idx_b, b)| ((idx_a, idx_b), (a.clone(), b.clone()))),
        );
    }
    let mut collided = HashSet::new();
    while !tracking_pairs.is_empty() {
        let mut new_tracking_pairs = Vec::with_capacity(tracking_pairs.len());
        for ((idx_a, idx_b), (a, b)) in tracking_pairs.drain(..) {
            match track(&a, &b) {
                TrackingStatus::Safe => {}
                TrackingStatus::Collided => {
                    collided.insert(idx_a);
                    collided.insert(idx_b);
                }
                TrackingStatus::Tracking(new_a, new_b) => {
                    new_tracking_pairs.push(((idx_a, idx_b), (new_a, new_b)));
                }
            }
        }
        tracking_pairs = new_tracking_pairs;
    }
    particles.len() - collided.len()
}

fn main() {
    let opt = Opt::from_args();
    let mut contents = String::new();
    if opt.input.to_str() == Some("-") {
        std::io::stdin()
            .read_to_string(&mut contents)
            .expect("could not read stdin");
    } else {
        let mut file = File::open(&opt.input)
            .expect(&format!("file {} not found", opt.input.display()));
        file.read_to_string(&mut contents)
            .expect(&format!("could not read file {}", opt.input.display()));
    }
    let particles = parsing::particles(contents.as_bytes())
        .to_result()
        .expect("Could not parse input");
    println!("Part 1: {}", part1(&particles));
    println!("Part 2: {}", part2(&particles));
}

#[derive(StructOpt, Debug)]
#[structopt(name = "day20", about = "Advent of code 2017 day 20")]
struct Opt {
    #[structopt(help = "Input file", parse(from_os_str))] input: PathBuf,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn part1_test() {
        let a = parsing::particle(b"p=<3,0,0>, v=<2,0,0>, a=<-1,0,0>")
            .to_result()
            .unwrap();
        let b = parsing::particle(b"p=<4,0,0>, v=<0,0,0>, a=<-2,0,0>")
            .to_result()
            .unwrap();
        assert_eq!(part1(&[a, b]), 0);
    }

    #[test]
    fn part2_test() {
        let particles = [
            parsing::particle(b"p=<-6,0,0>, v=<3,0,0>, a=<0,0,0>")
                .to_result()
                .unwrap(),
            parsing::particle(b"p=<-4,0,0>, v=<2,0,0>, a=<0,0,0>")
                .to_result()
                .unwrap(),
            parsing::particle(b"p=<-2,0,0>, v=<1,0,0>, a=<0,0,0>")
                .to_result()
                .unwrap(),
            parsing::particle(b"p=<3,0,0>, v=<-1,0,0>, a=<0,0,0>")
                .to_result()
                .unwrap(),
        ];
        assert_eq!(part2(&particles), 1);
    }
}

mod parsing {
    use super::{Particle, Vec3};
    use nom::*;

    named!(
        pub particles <Vec<Particle> >,
        separated_list_complete!(newline, particle)
    );

    named!(
        pub particle <Particle>,
        do_parse!(
            tag!("p=") >>
                p: vec3 >>
                tag!(", v=") >>
                v: vec3 >>
                tag!(", a=") >>
                a: vec3 >>
                (Particle {position: p, velocity: v, acceleration: a})
        )
    );

    named!(
        vec3<Vec3>,
        delimited!(
            tag!("<"),
            do_parse!(
                x: integer >> tag!(",") >> y: integer >> tag!(",") >> z: integer
                    >> (Vec3 { x: x, y: y, z: z })
            ),
            tag!(">")
        )
    );

    named!(
        integer<isize>,
        map_res!(
            map_res!(
                take_while!(|b| is_digit(b) || b == b'-'),
                ::std::str::from_utf8
            ),
            ::std::str::FromStr::from_str
        )
    );

    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn integer_test() {
            assert!(integer(b"123").is_done());
            assert!(integer(b"-123").is_done());
        }

        #[test]
        fn vec3_test() {
            assert!(vec3(b"<123,456,789>").is_done());
            assert!(vec3(b"<-1,1,-1>").is_done());
        }

        #[test]
        fn particle_test() {
            assert!(particle(b"p=<3,0,0>, v=<2,0,0>, a=<-1,0,0>").is_done());
            assert!(particle(b"p=<4,0,0>, v=<0,0,0>, a=<-2,0,0>").is_done());
        }
    }
}
