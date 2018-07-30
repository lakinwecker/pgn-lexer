extern crate pgn_lexer;
extern crate clap;
extern crate flate2;
extern crate rayon;

use pgn_lexer::parser;

use flate2::bufread::GzDecoder;
use clap::{Arg, App};
use std::fs::File;
use std::io::BufReader;
use rayon::prelude::*;


pub struct CountGames {
    games: u32
}
impl CountGames {
    pub fn new() -> CountGames {
        CountGames{games: 0}
    }
}

impl parser::PGNTokenVisitor for CountGames {
    fn visit_token(&mut self, token: &parser::Token) {
        if let parser::Token::Result(_) = token {
            self.games += 1;
        }
    }
}

fn main() -> std::io::Result<()> {
    let matches = App::new("count-gzip")
                      .version("0.1")
                      .author("Lakin Wecker <lakin@structuredabstraction.com>")
                      .about("Counts games in a PGN file")
                      .arg(Arg::with_name("INPUT")
                           .help("Sets the input file to use")
                           .required(true)
                           .multiple(true))
                      .get_matches();

    let input_files: Vec<&str> = matches.values_of("INPUT").unwrap().collect();
    let total: u32 = input_files.par_iter().map(|filename| {
        let res = File::open(filename).and_then(|f| {
            GzDecoder::new(BufReader::new(f))
        });
        match res {
            Ok(mut reader) => {
                let mut visitor = CountGames::new();
                {
                    let mut parser = parser::PGNTokenVisitorParser::new(&mut reader, &mut visitor);
                    parser.parse();
                }
                visitor.games
            },
            Err(_) => {
                println!("ERROR!");
                0
            }
        }
    }).sum();

    println!("Parsed: {}", total);

    Ok(())

}
