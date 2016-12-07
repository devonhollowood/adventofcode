extern crate clap;
extern crate octavo;

use octavo::digest::prelude::*;
use std::fmt::Write;

fn md5(input: &str, mut output: &mut [u8]) {
    let mut digest = Md5::default();
    digest.update(input);
    digest.result(&mut output);
}

#[inline(always)]
fn to_char(x: u8) -> char {
    if x < 10 {
        (x + '0' as u8) as char
    } else if x < 16 {
        (x - 10 + 'a' as u8) as char
    } else {
        panic!("bad hex value: {}", x)
    }
}

fn crack_password_1(door_id: &str) -> String {
    let mut password = String::new();
    let mut password_buffer = door_id.to_owned();
    password.reserve(format!("{}", u64::max_value()).len());
    let mut buf = vec![0u8; Md5::output_bytes()];
    for index in 0u64.. {
        if password.len() >= 8 {
            break;
        }
        password_buffer.truncate(door_id.len());
        write!(password_buffer, "{}", index)
            .expect("Could not write to buffer");
        md5(&password_buffer, &mut buf);
        if buf[0] == 0 && buf[1] == 0 && (buf[2] & 0xF0) == 0 {
            password.push(to_char(buf[2] & 0xF));
        }
    }
    password
}

fn crack_password_2(door_id: &str) -> String {
    let mut password = vec![None; 8];
    let mut password_buffer = door_id.to_owned();
    password.reserve(format!("{}", u64::max_value()).len());
    let mut buf = vec![0u8; Md5::output_bytes()];
    for index in 0u64.. {
        if password.iter().all(|opt| opt.is_some()) {
            break;
        }
        password_buffer.truncate(door_id.len());
        write!(password_buffer, "{}", index)
            .expect("Could not write to buffer");
        md5(&password_buffer, &mut buf);
        if !(buf[0] == 0 && buf[1] == 0 && (buf[2] & 0xF0) == 0) {
            continue;
        }
        let password_index = (buf[2] & 0xF) as usize;
        if password_index >= 8 {
            continue;
        }
        if password[password_index].is_some() {
            continue;
        }
        password[password_index] = Some(to_char((buf[3] & 0xF0) >> 4));
    }
    password.into_iter().map(|opt| opt.unwrap()).collect()
}

fn parse_args() -> String {
    clap::App::new("Day 04")
        .author("Devon Hollowood")
        .arg(clap::Arg::with_name("door-id")
             .help("file to read rooms from. Reads from stdin otherwise")
             .takes_value(true)
             .required(true)
        )
        .get_matches()
        .value_of("door-id")
        .unwrap()
        .to_owned()
}

fn main() {
    let door_id = parse_args();
    println!("first password: {}", crack_password_1(&door_id));
    println!("second password: {}", crack_password_2(&door_id));
}
