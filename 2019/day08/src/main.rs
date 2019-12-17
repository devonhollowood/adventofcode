use std::path::PathBuf;
use structopt::StructOpt;

type Color = isize;

#[derive(Debug)]
struct Image {
    width: usize,
    height: usize,
    data: Vec<Color>,
}

impl Image {
    fn new<T>(width: usize, height: usize, data: T) -> Self
    where
        T: IntoIterator<Item = Color>,
    {
        let data: Vec<_> = data.into_iter().collect();
        assert_eq!(data.len() % (width * height), 0);
        Self {
            width,
            height,
            data,
        }
    }

    fn layers(&self) -> impl Iterator<Item = Layer> {
        self.data
            .chunks(self.width * self.height)
            .map(move |chunk| Layer {
                width: self.width,
                data: chunk,
            })
    }
}

#[derive(Debug)]
struct Layer<'a> {
    width: usize,
    data: &'a [Color],
}

impl<'a> Layer<'a> {
    fn rows(&self) -> impl Iterator<Item = Row> {
        self.data
            .chunks(self.width)
            .map(move |chunk| Row { data: chunk })
    }
}

#[derive(Debug)]
struct Row<'a> {
    data: &'a [Color],
}

impl<'a> Row<'a> {
    fn pixels(&self) -> impl Iterator<Item = &'a Color> {
        self.data.iter()
    }
}

fn part1(image: &Image) -> usize {
    let (ones, twos) = image
        .layers()
        .min_by_key(|layer| {
            layer
                .rows()
                .map(|row| row.pixels().filter(|pix| **pix == 0).count())
                .sum::<usize>()
        })
        .expect("Expected at least one layer")
        .rows()
        .flat_map(|row| row.pixels())
        .fold((0, 0), |(ones, twos), pix| match pix {
            1 => (ones + 1, twos),
            2 => (ones, twos + 1),
            _ => (ones, twos),
        });
    ones * twos
}

fn main() {
    let input = Image::new(
        25,
        6,
        std::fs::read_to_string(Opt::from_args().input)
            .expect("error reading file")
            .chars()
            .filter_map(|c| c.to_digit(10))
            .map(|n| n as Color),
    );
    println!("Part 1: {}", part1(&input));
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
        let input = Image::new(3, 2, vec![1, 1, 2, 3, 1, 2, 3, 4, 5, 0, 1, 2]);
        assert_eq!(part1(&input), 6);
    }
}
