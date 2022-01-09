use anyhow::{anyhow, Result};
use nalgebra::DMatrix;

pub fn parse(input: &str) -> Result<DMatrix<u32>> {
    let mut ncols = 0;
    let mut nrows = 0;
    let mut data = Vec::new();
    for line in input.lines().filter(|line| !line.trim().is_empty()) {
        let digits: Vec<u32> = line
            .trim()
            .chars()
            .filter(|c| c.is_digit(2))
            .map(|c| c.to_digit(2).unwrap())
            .collect();
        if ncols == 0 {
            ncols = digits.len();
        }
        if ncols != digits.len() {
            return Err(anyhow!(
                "line {} is of length {}, but expected {}",
                line,
                digits.len(),
                ncols
            ));
        }
        data.extend(digits);
        nrows += 1;
    }

    // this takes data in column-major order, but we built it in row-major order, so we swap nrows
    // and ncols and then transpose
    Ok(DMatrix::from_vec(ncols, nrows, data).transpose())
}

fn from_binary_digits_be(digits: &[u32]) -> u32 {
    digits
        .iter()
        .inspect(|d| assert!(**d == 0 || **d == 1))
        .fold(0, |acc, digit| (acc << 1) + digit)
}

pub fn part1(input: &DMatrix<u32>) -> u32 {
    let digits = input.row_sum().map(|count| {
        if count * 2 > input.nrows() as u32 {
            1
        } else {
            0
        }
    });
    let gamma = from_binary_digits_be(digits.data.as_slice());

    let epsilon = from_binary_digits_be(
        digits
            .map(|digit| if digit == 1 { 0 } else { 1 })
            .data
            .as_slice(),
    );

    gamma * epsilon
}

fn oxygen(input: &DMatrix<u32>) -> u32 {
    let mut rows = input.clone();
    let mut col = 0;
    while rows.nrows() > 1 {
        let ones = rows.column(col).sum();
        let most = if ones * 2 >= rows.nrows() as u32 {
            1
        } else {
            0
        };
        let keep: Vec<usize> = rows
            .row_iter()
            .enumerate()
            .filter_map(|(idx, row)| if row[col] == most { Some(idx) } else { None })
            .collect();
        rows = rows.select_rows(&keep);
        col += 1;
    }
    from_binary_digits_be(rows.data.as_slice())
}

fn co2(input: &DMatrix<u32>) -> u32 {
    let mut rows = input.clone();
    let mut col = 0;
    while rows.nrows() > 1 {
        let ones = rows.column(col).sum();
        let least = if ones * 2 < rows.nrows() as u32 { 1 } else { 0 };
        let keep: Vec<usize> = rows
            .row_iter()
            .enumerate()
            .filter_map(|(idx, row)| if row[col] == least { Some(idx) } else { None })
            .collect();
        rows = rows.select_rows(&keep);
        col += 1;
    }
    from_binary_digits_be(rows.data.as_slice())
}

pub fn part2(input: &DMatrix<u32>) -> u32 {
    oxygen(input) * co2(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse() {
        let parsed = parse(
            r"
1,1
1,0
0,1
0,0",
        );
        assert_eq!(parsed.unwrap(), nalgebra::dmatrix!(1, 1; 1, 0; 0, 1; 0, 0));
    }

    const INPUT: &str = r"
00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010";

    #[test]
    fn test_part1() {
        let parsed = parse(INPUT).unwrap();
        assert_eq!(part1(&parsed), 198);
    }

    #[test]
    fn test_part2() {
        let parsed = parse(INPUT).unwrap();
        assert_eq!(part2(&parsed), 230);
    }
}
