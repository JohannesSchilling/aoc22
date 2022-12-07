use std::fs;
use std::error::Error;
use std::collections::HashSet;
use std::iter::FromIterator;


fn main() -> Result<(), Box<dyn Error>> {
    let inp = fs::read_to_string("input.txt")?;

    for l in inp.lines() {
        let (first, _) = l.split_at(5);
        println!("1: {}: {}", first, solve(&l, 4)?);
        println!("2: {}: {}", first, solve(&l, 14)?);
    }

    Ok(())
}

fn solve(inp: &str, winsize: usize) -> Result<usize, Box<dyn Error>> {
    for (i, win) in inp.as_bytes().windows(winsize).enumerate() {
        if HashSet::<u8>::from_iter(win.iter().copied()).len() == winsize {
            return Ok(i + winsize);
        }
    }

    Err("waaah".into())
}
