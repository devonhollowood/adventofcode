extern crate clap;

use std::io::prelude::*;
use std::fs::File;
use std::str::from_utf8;
use std::ascii::AsciiExt;

#[derive(Debug)]
struct Abbas<'a> {
    inner: &'a [u8],
    idx: usize, // idx must never be left in a [] block
}

fn is_abba(candidate: &[u8]) -> bool {
    candidate.len() == 4
        && candidate[0] == candidate[3]
        && candidate[1] == candidate[2]
        && candidate[0] != candidate[1]
}

impl<'a> Iterator for Abbas<'a> {
    type Item = &'a str;
    fn next(&mut self) -> Option<Self::Item> {
        let inner_end = self.inner.len();
        while self.idx < inner_end {
            // stop = next '[' or end of string
            let stop = self.inner[self.idx..]
                .iter()
                .position(|ch| *ch == '[' as u8) // position relative to idx
                .map(|pos| pos + self.idx) // position relative to start
                .unwrap_or(inner_end);
            // continue until would run into stop
            while self.idx <= stop - 4 {
                let window = &self.inner[self.idx .. self.idx + 4];
                if is_abba(window) {
                    self.idx += 1;
                    return Some(from_utf8(window).unwrap());
                }
                self.idx += 1;
            }
            // skip to stop
            self.idx = stop;
            // skip to next ']'
            while self.idx < inner_end && self.inner[self.idx] != ']' as u8 {
                self.idx += 1;
            }
            // go past ']'
            if self.idx < inner_end {
                self.idx += 1;
            }
        }
        return None;
    }
}

#[derive(Debug)]
struct AntiAbbas<'a> {
    inner: &'a [u8],
    idx: usize, // idx must always be left in a [] block, or at end
}

impl<'a> Iterator for AntiAbbas<'a> {
    type Item = &'a str;
    fn next(&mut self) -> Option<Self::Item> {
        let inner_end = self.inner.len();
        while self.idx < inner_end {
            // stop = next ']' or end of string
            let stop = self.inner[self.idx..]
                .iter()
                .position(|ch| *ch == ']' as u8) // position relative to idx
                .map(|pos| pos + self.idx) // position relative to start
                .unwrap_or(inner_end);
            // continue until would run into stop
            while self.idx <= stop - 4 {
                let window = &self.inner[self.idx .. self.idx + 4];
                if is_abba(window) {
                    self.idx += 1;
                    return Some(from_utf8(window).unwrap());
                }
                self.idx += 1;
            }
            // skip to stop
            self.idx = stop;
            // skip to next '['
            while self.idx < inner_end && self.inner[self.idx] != '[' as u8 {
                self.idx += 1;
            }
            // go into [] block
            if self.idx < inner_end {
                self.idx += 1;
            }
        }
        return None;
    }
}

fn abbas(s: &str) -> Abbas {
    if !s.is_ascii() {
        panic!("Only ascii strings are supported");
    }
    Abbas { inner: s.as_bytes(), idx: 0 }
}

fn anti_abbas(s: &str) -> AntiAbbas {
    if !s.is_ascii() {
        panic!("Only ascii strings are supported");
    }
    AntiAbbas {
        inner: s.as_bytes(),
        idx: s.find('[').map(|pos| pos + 1).unwrap_or(s.len())
    }
}

fn is_valid_ip(candidate: &str) -> bool {
    anti_abbas(candidate).next().is_none()
        && abbas(candidate).next().is_some()
}

fn parse_args() -> std::io::Result<Box<Read>> {
    let source = clap::App::new("Day 07")
        .author("Devon Hollowood")
        .arg(clap::Arg::with_name("ips")
             .index(1)
             .short("f")
             .long("ips")
             .help("file to read IPs from. Reads from stdin otherwise")
             .takes_value(true)
        )
        .get_matches()
        .value_of_os("ips")
        .map(|str| str.to_owned());
    match source {
        Some(filename) => Ok(Box::new(File::open(filename)?)),
        None => Ok(Box::new(std::io::stdin())),
    }
}

fn read_ips<R: Read>(source: &mut R) -> std::io::Result<Vec<String>> {
    let mut contents = String::new();
    source.read_to_string(&mut contents)?;
    Ok(contents.lines().map(|line| line.trim().to_owned()).collect())
}

fn main() {
    let mut source = parse_args()
        .unwrap_or_else(|err| panic!("Error reading file: {}", err));
    let ips = read_ips(&mut source)
        .unwrap_or_else(|err| panic!("Error reading ips: {}", err));
    let n_valid_ips = ips.iter().filter(|ip| is_valid_ip(&ip)).count();
    println!("# of valid IPs: {}", n_valid_ips);
}
