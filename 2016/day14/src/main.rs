extern crate clap;
extern crate octavo;

use octavo::digest::prelude::*;
use std::collections::{VecDeque, HashSet};

type Idx = usize;
type Key = String;

fn write_hex(input: &[u8], output: &mut String) {
    use std::fmt::Write;
    output.clear();
    for byte in input {
        write!(output, "{:02x}", byte).expect("Could not write to buffer");
    }
}

fn md5(input: &[u8], mut output: &mut String) {
    let mut output_buf = vec![0u8; Md5::output_bytes()];
    let mut digest = Md5::default();
    digest.update(input);
    digest.result(&mut output_buf);
    write_hex(&output_buf, &mut output);
}

fn runs(input: &str, len: usize) -> Vec<char> {
    let mut current = None;
    let mut run = 1;
    let mut runs = Vec::new();
    for ch in input.chars() {
        if current == Some(ch) {
            run += 1;
            if run == len {
                runs.push(ch);
                run = 0;
                current = None;
            }
        } else {
            run = 1;
            current = Some(ch);
        }
    }
    runs
}

fn generate_keys(salt: &[u8], n_keys: usize) -> Vec<(Idx, Key)> {
    use std::io::Write;
    let mut keys = Vec::new();
    let mut candidates: VecDeque<(Idx, char, Key)> = VecDeque::new();
    let mut validated = HashSet::new();
    let mut input_buf: Vec<u8> = salt.to_vec();
    let mut key_buf = String::with_capacity(Md5::output_bytes() * 2);
    for idx in 0.. {
        // dump front candidate if invalidated
        if candidates.front().map(|&(cand_idx, _, _)| idx - cand_idx > 1000).unwrap_or_default() {
            candidates.pop_front();
        }
        // pop validated candidates into `keys`
        while candidates.front()
            .map(|&(cand_idx, _, _)| validated.contains(&cand_idx))
            .unwrap_or_default() {
            let (cand_idx, _, key) = candidates.pop_front().expect("`candidates` was empty");
            keys.push((cand_idx, key));
            if keys.len() >= n_keys {
                return keys;
            }
        }
        // create new key
        input_buf.truncate(salt.len());
        write!(input_buf, "{}", idx).expect("Could not write to buffer");
        md5(&input_buf, &mut key_buf);
        // handle runs of 5
        for ch in runs(&key_buf, 5) {
            // mark validated candidates
            for &(cand_idx, cand_ch, _) in &candidates {
                if cand_ch == ch {
                    validated.insert(cand_idx);
                }
            }
        }
        // handle runs of 3
        if let Some(&ch) = runs(&key_buf, 3).first() {
            candidates.push_back((idx, ch, key_buf.clone()));
        }
    }
    unreachable!()
}

fn parse_args() -> String {
    let matches = clap::App::new("Day 14")
        .author("Devon Hollowood")
        .arg(clap::Arg::with_name("salt")
            .index(1)
            .short("s")
            .long("salt")
            .help("salt for generating keys")
            .required(true)
            .takes_value(true))
        .get_matches();
    matches.value_of("salt").unwrap().to_owned()
}

fn main() {
    let salt = parse_args();
    println!("Generating keys...");
    for (idx, key) in generate_keys(salt.as_bytes(), 64) {
        println!("Key at idx {}: {}", idx, key);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn example_keyset() {
        let keys = generate_keys(b"abc", 2);
        println!("Got keys: {:?}", keys);
        assert_eq!(keys[0].0, 39);
        assert_eq!(keys[1].0, 92);
        // assert_eq!(keys[63].0, 22728);
    }
}
