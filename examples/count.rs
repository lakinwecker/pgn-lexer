extern crate memmap;
extern crate pgn_lexer;
extern crate clap;

use pgn_lexer::parser;

use memmap::{Mmap, Protection};
use clap::{Arg, App};


fn main() {
    let matches = App::new("count")
                      .version("0.1")
                      .author("Lakin Wecker <lakin@structuredabstraction.com>")
                      .about("Counts games in a PGN file")
                      .arg(Arg::with_name("INPUT")
                           .help("Sets the input file to use")
                           .required(true)
                           .index(1))
                      .get_matches();

    let input_file = matches.value_of("INPUT").unwrap(); // Required above, unwrap is fine.
    let file_mmap = Mmap::open_path(input_file, Protection::Read).unwrap(); // Dangerous unwrap. :P
    let mut bytes: &[u8] = unsafe { file_mmap.as_slice() };
    if bytes[0..3] == [239u8, 187u8, 191u8] {
        bytes = &bytes[3..];
    }
    let results = parser::PGNTokenIterator::new(bytes);
    let mut game_count = 0;
    for x in results {
        if let parser::Token::Result(_) = x {
            game_count += 1;
        }
    }
    println!("Games: {}", game_count);
}
