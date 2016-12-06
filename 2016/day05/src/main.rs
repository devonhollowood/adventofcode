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

fn crack_password_1(door_id: &str) -> String {
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

fn crack_password_2(door_id: &str) -> String {
    let mut password = vec![None; 8];
    for index in 0u64.. {
        if password.iter().all(|opt| opt.is_some()) {
            break;
        }
        let hex = md5_hex(&format!("{}{}", door_id, index));
        if !hex.starts_with("00000") {
            continue;
        }
        let password_index_char =
            hex.chars().nth(5).expect(&format!("invalid md5: {}", hex));
        let password_index = match password_index_char.to_digit(8) {
            Some(idx) => idx as usize,
            None => continue,
        };
        if password[password_index].is_some() {
            continue;
        }
        password[password_index] = Some(
            hex.chars().nth(6).expect(&format!("invalid md5: {}", hex))
        );
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
