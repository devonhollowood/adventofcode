#[macro_use]
extern crate nom;

extern crate structopt;
#[macro_use]
extern crate structopt_derive;

use structopt::StructOpt;
use std::fs::File;
use std::io::Read;

#[derive(Debug, PartialEq, Eq)]
struct Group {
    subgroups: Vec<GroupContents>,
}

impl Group {
    fn score(&self) -> usize {
        self.score_with_init(1)
    }
    fn score_with_init(&self, init: usize) -> usize {
        self.subgroups
            .iter()
            .map(|contents| match *contents {
                GroupContents::SubGroup(ref sg) => sg.score_with_init(init + 1),
                GroupContents::Garbage(_) => 0,
            })
            .sum::<usize>() + init
    }
    #[cfg(test)]
    fn count(&self) -> usize {
        self.subgroups
            .iter()
            .map(|contents| match *contents {
                GroupContents::SubGroup(ref sg) => sg.count(),
                GroupContents::Garbage(_) => 0,
            })
            .sum::<usize>() + 1
    }
    fn garbage_len(&self) -> usize {
        self.subgroups
            .iter()
            .map(|contents| match *contents {
                GroupContents::SubGroup(ref sg) => sg.garbage_len(),
                GroupContents::Garbage(count) => count,
            })
            .sum::<usize>()
    }
}

#[derive(Debug, PartialEq, Eq)]
enum GroupContents {
    SubGroup(Group),
    Garbage(usize),
}

fn main() {
    let opt = Opt::from_args();
    let mut contents = String::new();
    if opt.input == "-" {
        std::io::stdin()
            .read_to_string(&mut contents)
            .expect("could not read stdin");
    } else {
        let mut file = File::open(&opt.input)
            .expect(&format!("file {} not found", opt.input));
        file.read_to_string(&mut contents)
            .expect(&format!("could not read file {}", opt.input));
    }
    let parsed = group(contents.as_bytes())
        .to_full_result()
        .expect("Could not parse input");
    println!("Part 1: {}", parsed.score());
    println!("Part 2: {}", parsed.garbage_len());
}

named!(
    group<&[u8], Group>,
    map!(
        delimited!(
            char!('{'),
            separated_list_complete!(
                char!(','),
                alt_complete!(
                    map!(garbage, GroupContents::Garbage) |
                    map!(group, GroupContents::SubGroup)
                )
            ),
            char!('}')
        ),
        |v| Group { subgroups: v }
    )
);

named!(
    garbage<usize>,
    delimited!(
        char!('<'),
        map!(
            many0!(alt_complete!(
                map!(escaped_char, |_| 0) | map!(is_not!("!>"), |s| s.len())
            )),
            |v| v.iter().sum()
        ),
        char!('>')
    )
);

named!(escaped_char, preceded!(char!('!'), take!(1)));

#[derive(StructOpt, Debug)]
#[structopt(name = "day09", about = "Advent of code 2017 day 09")]
struct Opt {
    #[structopt(help = "Input file")] input: String,
}

#[cfg(test)]
mod tests {
    use nom::IResult;
    use super::*;

    #[test]
    fn count_test() {
        assert_eq!(group(&b"{}"[..]).to_full_result().unwrap().count(), 1);
        assert_eq!(group(&b"{{{}}}"[..]).to_full_result().unwrap().count(), 3);
        assert_eq!(group(&b"{{},{}}"[..]).to_full_result().unwrap().count(), 3);
        assert_eq!(
            group(&b"{{{},{},{{}}}}"[..])
                .to_full_result()
                .unwrap()
                .count(),
            6
        );
        assert_eq!(
            group(&b"{<{},{},{{}}>}"[..])
                .to_full_result()
                .unwrap()
                .count(),
            1
        );
        assert_eq!(
            group(&b"{<a>,<a>,<a>,<a>}"[..])
                .to_full_result()
                .unwrap()
                .count(),
            1
        );
        assert_eq!(
            group(&b"{{<a>},{<a>},{<a>},{<a>}}"[..])
                .to_full_result()
                .unwrap()
                .count(),
            5
        );
        assert_eq!(
            group(&b"{{<!>},{<!>},{<!>},{<a>}}"[..])
                .to_full_result()
                .unwrap()
                .count(),
            2
        );
    }

    #[test]
    fn score_test() {
        assert_eq!(group(&b"{}"[..]).to_full_result().unwrap().score(), 1);
        assert_eq!(group(&b"{{{}}}"[..]).to_full_result().unwrap().score(), 6);
        assert_eq!(group(&b"{{},{}}"[..]).to_full_result().unwrap().score(), 5);
        assert_eq!(
            group(&b"{{{},{},{{}}}}"[..])
                .to_full_result()
                .unwrap()
                .score(),
            16
        );
        assert_eq!(
            group(&b"{<a>,<a>,<a>,<a>}"[..])
                .to_full_result()
                .unwrap()
                .score(),
            1
        );
        assert_eq!(
            group(&b"{{<ab>},{<ab>},{<ab>},{<ab>}}"[..])
                .to_full_result()
                .unwrap()
                .score(),
            9
        );
        assert_eq!(
            group(&b"{{<!!>},{<!!>},{<!!>},{<!!>}}"[..])
                .to_full_result()
                .unwrap()
                .score(),
            9
        );
        assert_eq!(
            group(&b"{{<a!>},{<a!>},{<a!>},{<ab>}}"[..])
                .to_full_result()
                .unwrap()
                .score(),
            3
        );
    }

    #[test]
    fn garbage_test() {
        assert_eq!(garbage(&b"<>"[..]), IResult::Done(&b""[..], 0));
        assert_eq!(
            garbage(&b"<random characters>"[..]),
            IResult::Done(&b""[..], 17)
        );
        assert_eq!(garbage(&b"<<<<>"[..]), IResult::Done(&b""[..], 3));
        assert_eq!(garbage(&b"<{!>}>"[..]), IResult::Done(&b""[..], 2));
        assert_eq!(garbage(&b"<!!>"[..]), IResult::Done(&b""[..], 0));
        assert_eq!(garbage(&b"<!!!>>"[..]), IResult::Done(&b""[..], 0));
        assert_eq!(
            garbage(&b"<{o\"i!a,<{i<a>"[..]),
            IResult::Done(&b""[..], 10)
        );
        assert_eq!(garbage(&b"<!!>>"[..]), IResult::Done(&b">"[..], 0));
    }
}
