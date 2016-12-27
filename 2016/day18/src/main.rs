extern crate clap;
extern crate itertools;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Tile {
    Safe,
    Trap,
}

impl Tile {
    fn is_safe(&self) -> bool {
        match *self {
            Tile::Safe => true,
            Tile::Trap => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Row {
    tiles: Vec<Tile>,
}

impl Row {
    fn next_row(&self) -> Row {
        use Tile::*;
        let mut new_tiles = Vec::with_capacity(self.tiles.len());
        for idx in 0..self.tiles.len() {
            let left = if idx == 0 { Safe } else { self.tiles[idx - 1] };
            let mid = self.tiles[idx];
            let right = if idx == self.tiles.len() - 1 {
                Safe
            } else {
                self.tiles[idx + 1]
            };
            match (left, mid, right) {
                (Trap, Trap, Safe) |
                (Safe, Trap, Trap) |
                (Trap, Safe, Safe) |
                (Safe, Safe, Trap) => new_tiles.push(Trap),
                _ => new_tiles.push(Safe),
            }
        }
        Row { tiles: new_tiles }
    }
    fn generate_rows(self, len: usize) -> Vec<Row> {
        itertools::iterate(self, |r| r.next_row()).take(len).collect()
    }
}

impl std::str::FromStr for Row {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut tiles = Vec::with_capacity(s.len());
        for ch in s.chars() {
            match ch {
                '.' => tiles.push(Tile::Safe),
                '^' => tiles.push(Tile::Trap),
                _ => return Err(format!("Bad char '{}' in row: {}", ch, s)),
            }
        }
        Ok(Row { tiles: tiles })
    }
}

impl std::fmt::Display for Row {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        use std::fmt::Write;
        for tile in &self.tiles {
            match *tile {
                Tile::Safe => f.write_char('.')?,
                Tile::Trap => f.write_char('^')?,
            }
        }
        Ok(())
    }
}

fn parse_args() -> Result<(Row, bool), String> {
    let matches = clap::App::new("Day 18")
        .author("Devon Hollowood")
        .arg(clap::Arg::with_name("initial")
            .index(1)
            .help("initial row for generating room")
            .required(true))
        .arg(clap::Arg::with_name("show")
            .short("s")
            .long("show"))
        .get_matches();
    Ok((matches.value_of("initial").unwrap().parse()?, matches.is_present("show")))
}

fn main() {
    let (initial, show) = parse_args()
        .unwrap_or_else(|e| panic!("Error parsing initial row: {}", e));
    let room = initial.clone().generate_rows(40);
    if show {
        for row in &room {
            println!("{}", row);
        }
    }
    let n_safe_40 = room.iter().flat_map(|row| row.tiles.iter()).filter(|t| t.is_safe()).count();
    println!("Number of safe tiles in 40 rows: {}", n_safe_40);
    let n_safe_400000 = itertools::iterate(initial, |row| row.next_row())
        .take(400000)
        .fold(0, |count, row| {
            count +
            row.tiles
                .iter()
                .filter(|tile| tile.is_safe())
                .count()
        });
    println!("Number of safe tiles in 400000 rows: {}", n_safe_400000);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn example_room() {
        assert_eq!("..^^.".parse::<Row>().unwrap().generate_rows(3),
                   vec!["..^^.".parse().unwrap(),
                        ".^^^^".parse().unwrap(),
                        "^^..^".parse().unwrap()])
    }

    #[test]
    fn example_count() {
        let room = ".^^.^.^^^^".parse::<Row>().unwrap().generate_rows(10);
        assert_eq!(room.iter().flat_map(|row| row.tiles.iter()).filter(|t| t.is_safe()).count(),
                   38);
    }
}
