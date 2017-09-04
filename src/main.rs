// This file is part of the samson library.
//
// Copyright (C) 2017 Lakin Wecker <lakin@wecker.ca>
// 
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
// 
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU General Public License for more details.
// 
// You should have received a copy of the GNU General Public License
// along with this program. If not, see <http://www.gnu.org/licenses/>.
#![feature(test)]
extern crate test;

extern crate memmap;

extern crate nom;
pub mod parser;
use memmap::{Mmap, Protection};


fn main() {
    let file_mmap = Mmap::open_path("/home/lakin/Downloads/160118 to 170513 Lichess Update.pgn", Protection::Read).unwrap();
    let mut bytes: &[u8] = unsafe { file_mmap.as_slice() };
    if bytes[0..3] == [239u8, 187u8, 191u8] {
        bytes = &bytes[3..];
    }
    let results = parser::PGNTokenIterator{bytes: bytes};
    //let results = results.map(|x| println!("[{}]", x));
    println!("{}", results.count());

}
