# PGN Tokenizer
## Goals
- Use zero-copy byte slices all the way through.
- No heap allocations

## API
- A token based iterator that takes a byte slices and returns tokens that reference sub-slices


## Example

```
extern crate memmap;
extern crate rust_pgn_tokenizer;

pub mod parser;

use memmap::{Mmap, Protection};


fn main() {
    let file_mmap = Mmap::open_path("./lichess_db_standard_rated_2017-01.pgn", Protection::Read).unwrap();
    let mut bytes: &[u8] = unsafe { file_mmap.as_slice() };
    if bytes[0..3] == [239u8, 187u8, 191u8] {
        bytes = &bytes[3..];
    }
    let results = parser::PGNTokenIterator{bytes: bytes};
    let mut game_count = 0;
    for x in results {
        if let parser::Token::Result(_) = x {
            game_count += 1;
        }
    }
    println!("Games: {}", game_count);
}
```
