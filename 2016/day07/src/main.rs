extern crate clap;

use std::io::prelude::*;
use std::fs::File;

#[derive(Debug)]
/// Iterator over parts of str in delimiters
struct Delimited<'a> {
    inner: &'a str,
    start_char: char,
    end_char: char,
}

impl<'a> Iterator for Delimited<'a> {
    type Item = &'a str;
    fn next(&mut self) -> Option<Self::Item> {
        if self.inner.is_empty() {
            return None;
        }
        let end = self.inner.len();
        let next_end = self.inner
            .find(self.end_char)
            .unwrap_or(end);
        let (delimited, rest) = self.inner.split_at(next_end);
        let next_start = rest.find(self.start_char)
            .map(|p| p + 1)
            .unwrap_or(end - delimited.len());
        self.inner = &rest[next_start..];
        Some(delimited)
    }
}

fn exteriors(s: &str) -> Delimited {
    Delimited {
        inner: &s,
        start_char: ']',
        end_char: '[',
    }
}

fn interiors(s: &str) -> Delimited {
    let start = s.find('[').map(|p| p + 1).unwrap_or(s.len());
    Delimited {
        inner: &s[start..],
        start_char: '[',
        end_char: ']',
    }
}

struct SearchWindows<'a> {
    current: Option<&'a str>,
    inner: Delimited<'a>,
    get_window: Box<Fn(&str) -> Option<&str>>,
    predicate: Box<Fn(&str) -> bool>,
}

impl<'a> SearchWindows<'a> {
    fn new<F, P>(mut delimiters: Delimited<'a>,
                 get: F,
                 pred: P)
                 -> SearchWindows<'a>
        where F: 'static + Fn(&str) -> Option<&str>,
              P: 'static + Fn(&str) -> bool
    {
        SearchWindows {
            current: delimiters.next(),
            inner: delimiters,
            get_window: Box::new(get),
            predicate: Box::new(pred),
        }
    }
}

impl<'a> Iterator for SearchWindows<'a> {
    type Item = &'a str;
    fn next(&mut self) -> Option<Self::Item> {
        while self.current.is_some() {
            while let Some(win) = (self.get_window)(self.current.unwrap()) {
                self.current = Some(advance(self.current.unwrap()));
                if (self.predicate)(win) {
                    return Some(win);
                }
            }
            self.current = self.inner.next();
        }
        None
    }
}

fn abbas(s: &str) -> SearchWindows {
    SearchWindows::new(exteriors(s), |st| window(st, 4), is_abba)
}

fn anti_abbas(s: &str) -> SearchWindows {
    SearchWindows::new(interiors(s), |st| window(st, 4), is_abba)
}

fn abas(s: &str) -> SearchWindows {
    SearchWindows::new(exteriors(s), |st| window(st, 3), is_aba)
}

fn anti_abas(s: &str) -> SearchWindows {
    SearchWindows::new(interiors(s), |st| window(st, 3), is_aba)
}

fn advance(s: &str) -> &str {
    let mut iter = s.chars();
    iter.next();
    iter.as_str()
}

fn window(s: &str, len: usize) -> Option<&str> {
    if len == 0 {
        return None;
    }
    s.char_indices().nth(len - 1).map(|(idx, ch)| &s[..idx + ch.len_utf8()])
}

fn is_abba(candidate: &str) -> bool {
    if candidate.len() != 4 {
        return false;
    }
    let mut iter = candidate.chars();
    let a = iter.next().unwrap();
    let b = iter.next().unwrap();
    let c = iter.next().unwrap();
    let d = iter.next().unwrap();
    a == d && b == c && a != b
}

fn is_aba(candidate: &str) -> bool {
    if candidate.len() != 3 {
        return false;
    }
    let mut iter = candidate.chars();
    let a = iter.next().unwrap();
    let b = iter.next().unwrap();
    let c = iter.next().unwrap();
    a == c && a != b
}

fn is_corresponding_bab(aba: &str, candidate: &str) -> bool {
    if aba.len() != 3 || candidate.len() != 3 {
        return false;
    }
    let mut cand_iter = candidate.chars();
    let a = cand_iter.next().unwrap();
    let b = cand_iter.next().unwrap();
    let c = cand_iter.next().unwrap();
    let mut aba_iter = aba.chars();
    aba_iter.next() == Some(b) &&
        aba_iter.next() == Some(a) &&
        aba_iter.next() == Some(b) &&
        a == c
}

fn supports_tls(candidate: &str) -> bool {
    anti_abbas(candidate).next().is_none() && abbas(candidate).next().is_some()
}

fn supports_ssl(candidate: &str) -> bool {
    abas(candidate).any(|aba| {
        anti_abas(candidate).any(|bab| is_corresponding_bab(aba, bab))
    })
}

fn parse_args() -> std::io::Result<Box<Read>> {
    let source = clap::App::new("Day 07")
        .author("Devon Hollowood")
        .arg(clap::Arg::with_name("ips")
            .index(1)
            .short("f")
            .long("ips")
            .help("file to read IPs from. Reads from stdin otherwise")
            .takes_value(true))
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
    let n_support_tls = ips.iter().filter(|ip| supports_tls(&ip)).count();
    let n_support_ssl = ips.iter().filter(|ip| supports_ssl(&ip)).count();
    println!("# of IPs supporting TLS: {}", n_support_tls);
    println!("# of IPs supporting SSL: {}", n_support_ssl);
}
