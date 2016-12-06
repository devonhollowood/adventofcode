extern crate clap;
extern crate octavo;

use octavo::digest::prelude::*;
use std::fmt::Write;

fn md5_hex(input: &str) -> String {
    let mut digest = Md5::default();
    let mut result = vec![0u8; Md5::output_bytes()];
    digest.update(input);
    digest.result(&mut result);
    let mut hex_str = String::new();
    for num in result {
        write!(hex_str, "{:02x}", num)
            .expect(&format!("Could not write value to hex: {}", num));
    }
    hex_str
}

fn crack_password(door_id: &str) -> String {
    let mut password = String::new();
    let mut index = 0u64;
    while password.len() < 8 {
        let hex = md5_hex(&format!("{}{}", door_id, index));
        if hex.starts_with("00000") {
            password.push(
                hex.chars().nth(5).expect(&format!("invalid md5: {}", hex))
            );
        }
        index += 1;
    }
    password
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
    let password = crack_password(&door_id);
    println!("password: {}", password);
}
