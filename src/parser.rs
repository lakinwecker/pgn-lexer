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

//------------------------------------------------------------------------------
// Parsers for the SAN specification
//------------------------------------------------------------------------------
//
use nom::*;

///-----------------------------------------------------------------------------
#[derive(Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Debug, Hash)]
pub enum Token <'a> {
    Move(&'a [u8])
}

///-----------------------------------------------------------------------------
/*named!(pub san_pawn_move_bare,  re_bytes_match_static!(r"[abcdefgh]{1}[12345678]{1}"));
named!(pub san_pawn_capture_bare,  re_bytes_match_static!(r"[abcdefgh]{1}x[abcdefgh]{1}[12345678]{1}"));*/


///-----------------------------------------------------------------------------
/*named!(pub san_explicit_move,
    alt_complete!(
        san_pawn_move_bare |
        san_pawn_capture_bare //|
        //san_piece_move_bare |
        //san_pawn_move |
        //san_piece_move |
        //san_pawn_capture
    )
);*/

///-----------------------------------------------------------------------------
/*named!(pub san_move<Token>,
    map!(
        alt_complete!(
            san_explicit_move |
            san_explicit_move
            //san_castle_queen_side |
            //san_castle_king_side |
            //san_null_move
        ),
        |m| Token::Move(m)
    )
);*/

pub const ROOK: u8 = 'R' as u8;
pub const KNIGHT: u8 = 'N' as u8;
pub const BISHOP: u8 = 'B' as u8;
pub const QUEEN: u8 = 'Q' as u8;
pub const KING: u8 = 'K' as u8;
pub const A: u8 = 'a' as u8;
pub const B: u8 = 'b' as u8;
pub const C: u8 = 'c' as u8;
pub const D: u8 = 'd' as u8;
pub const E: u8 = 'e' as u8;
pub const F: u8 = 'f' as u8;
pub const G: u8 = 'g' as u8;
pub const H: u8 = 'h' as u8;
pub const _1: u8 = '1' as u8;
pub const _2: u8 = '2' as u8;
pub const _3: u8 = '3' as u8;
pub const _4: u8 = '4' as u8;
pub const _5: u8 = '5' as u8;
pub const _6: u8 = '6' as u8;
pub const _7: u8 = '7' as u8;
pub const _8: u8 = '8' as u8;
pub const X: u8 = 'x' as u8;
pub const EQUALS: u8 = '=' as u8;
pub const PLUS: u8 = '+' as u8;
pub const O: u8 = 'O' as u8;
pub const DASH: u8 = '-' as u8;
pub const HASH: u8 = '#' as u8;
pub const Z: u8 = 'Z' as u8;
pub const _0: u8 = '0' as u8;

// TODO: Figure out better error handling
pub const MOVE_PARSING_ERROR: u32 = 32;

pub fn is_file(i:u8) -> bool {
    return i >= A && i <= H;
}
pub fn is_rank(i:u8) -> bool {
    return i >= _1 && i <= _8;
}
pub fn is_capture(i:u8) -> bool {
    return i == X;
}
pub fn is_equals(i:u8) -> bool {
    return i == EQUALS;
}
pub fn is_plus(i:u8) -> bool {
    return i == PLUS;
}
pub fn is_piece(i:u8) -> bool {
    return i == ROOK || i == KNIGHT || i == BISHOP || i == QUEEN || i == KING;
}

pub fn is_o(i:u8) -> bool {
    return i == O;
}

pub fn is_dash(i:u8) -> bool {
    return i == DASH;
}

pub fn is_hash(i:u8) -> bool {
    return i == HASH;
}
pub fn is_zed(i:u8) -> bool {
    return i == Z;
}
pub fn is_zero(i:u8) -> bool {
    return i == _0;
}

// e4 dxe4 e8=Q dxe8=Q
pub fn san_pawn_move(i:&[u8]) -> IResult<&[u8], Token>{
    let mut length = 0;
    if i.len() > 3 && is_file(i[0]) && is_capture(i[1]) && is_file(i[2]) && is_rank(i[3]) {
        length = 4;
    } else if i.len() > 1 && is_file(i[0]) && is_rank(i[1]) {
        length = 2;
    } else {
        length = 0;
    }
    if length == 0 {
        return IResult::Error(ErrorKind::Custom(MOVE_PARSING_ERROR))
    }
    if i.len() > length + 1 && is_equals(i[length]) && is_piece(i[length+1]) {
        length = length + 2;
    }
    if i.len() > length && (is_plus(i[length]) || is_hash(i[length])) {
        length = length + 1;
    }
    IResult::Done(&i[length..], Token::Move(&i[0..length]))
}

// Nf3 Bb5 Nef3 N2f3 Nxf3 N2xf3+
pub fn san_piece_move(i:&[u8]) -> IResult<&[u8], Token>{
    let mut length = 0;
    if i.len() > 5 && is_piece(i[0]) && is_file(i[1]) && is_rank(i[2]) && is_capture(i[3]) && is_file(i[4]) && is_rank(i[5]) {
        length = 6;
    } else if i.len() > 4 && is_piece(i[0]) && is_file(i[1]) && is_rank(i[2]) && is_file(i[3]) && is_rank(i[4]) {
        length = 5;
    } else if i.len() > 4 && is_piece(i[0]) && (is_file(i[1]) || is_rank(i[1])) && is_capture(i[2]) && is_file(i[3]) && is_rank(i[4]) {
        length = 5;
    } else if i.len() > 3 && is_piece(i[0]) && (is_file(i[1]) || is_rank(i[1]) || is_capture(i[1])) && is_file(i[2]) && is_rank(i[3]) {
        length = 4;
    } else if i.len() > 2 && is_piece(i[0]) && is_file(i[1]) && is_rank(i[2]) {
        length = 3;
    } else {
        length = 0;
    }
    if length == 0 {
        return IResult::Error(ErrorKind::Custom(MOVE_PARSING_ERROR))
    }
    if i.len() > length && (is_plus(i[length]) || is_hash(i[length])) {
        length = length + 1;
    }
    IResult::Done(&i[length..], Token::Move(&i[0..length]))
}

// O-O O-O-O O-O+ O-O-O+
pub fn san_castles(i:&[u8]) -> IResult<&[u8], Token>{
    let mut length = 0;
    if i.len() > 4 && is_o(i[0]) && is_dash(i[1]) && is_o(i[2]) && is_dash(i[3]) && is_o(i[4]) {
        length = 5;
    } else if i.len() > 2 && is_o(i[0]) && is_dash(i[1]) && is_o(i[2]) {
        length = 3;
    } else {
        length = 0;
    }
    if length == 0 {
        return IResult::Error(ErrorKind::Custom(MOVE_PARSING_ERROR))
    }
    if i.len() > length && (is_plus(i[length]) || is_hash(i[length])) {
        length = length + 1;
    }
    IResult::Done(&i[length..], Token::Move(&i[0..length]))
}

// -- Z0
pub fn san_null_move(i:&[u8]) -> IResult<&[u8], Token>{
    let mut length = 0;
    if i.len() > 1 && is_dash(i[0]) && is_dash(i[1]) {
        length = 2;
    } else if i.len() > 1 && is_zed(i[0]) && is_zero(i[1]) {
        length = 2;
    } else {
        length = 0;
    }
    if length == 0 {
        return IResult::Error(ErrorKind::Custom(MOVE_PARSING_ERROR))
    }
    if i.len() > length && (is_plus(i[length]) || is_hash(i[length])) {
        length = length + 1;
    }
    IResult::Done(&i[length..], Token::Move(&i[0..length]))
}


pub fn san_move(i:&[u8]) -> IResult<&[u8], Token>{
    // e4 Nf3 dxe4 dxe8=Q N2f3 Ndf3
  match i[0] {
      ROOK | KNIGHT | BISHOP | QUEEN | KING => san_piece_move(i),
      A...H => san_pawn_move(i),
      O => san_castles(i),
      DASH | Z => san_null_move(i),
      _ => IResult::Error(ErrorKind::Custom(MOVE_PARSING_ERROR))
  }
  
}


#[cfg(test)]
mod tests {

    use super::*;
    use nom::IResult::*;
    use test::Bencher;

    #[test]
    fn test_san_move_pawn_single_square() {
        assert_eq!(Done(&b""[..], Token::Move(&b"a1"[..])), san_move(&b"a1"[..]));
        assert_eq!(Done(&b""[..], Token::Move(&b"a8"[..])), san_move(&b"a8"[..]));
        assert_eq!(Done(&b""[..], Token::Move(&b"h1"[..])), san_move(&b"h1"[..]));
        assert_eq!(Done(&b""[..], Token::Move(&b"h8"[..])), san_move(&b"h8"[..]));
        assert_eq!(Done(&b""[..], Token::Move(&b"e4"[..])), san_move(&b"e4"[..]));
        assert_eq!(Done(&b" e5"[..], Token::Move(&b"e4"[..])), san_move(&b"e4 e5"[..]));
        assert_eq!(Done(&b"!! e5"[..], Token::Move(&b"e4"[..])), san_move(&b"e4!! e5"[..]));
        assert_eq!(Done(&b"!? e5"[..], Token::Move(&b"e4"[..])), san_move(&b"e4!? e5"[..]));
        assert_eq!(Done(&b"!? e5"[..], Token::Move(&b"e4"[..])), san_move(&b"e4!? e5"[..]));
        assert_eq!(Done(&b""[..], Token::Move(&b"e4=N"[..])), san_move(&b"e4=N"[..]));
        assert_eq!(Done(&b""[..], Token::Move(&b"e4=N#"[..])), san_move(&b"e4=N#"[..]));
    }

    #[test]
    fn test_san_pawn_capture() {
        assert_eq!(Done(&b""[..], Token::Move(&b"bxc1"[..])), san_move(&b"bxc1"[..]));
        assert_eq!(Done(&b""[..], Token::Move(&b"axe4"[..])), san_move(&b"axe4"[..]));
        assert_eq!(Done(&b""[..], Token::Move(&b"bxc1+"[..])), san_move(&b"bxc1+"[..]));
        assert_eq!(Done(&b""[..], Token::Move(&b"bxc1=R+"[..])), san_move(&b"bxc1=R+"[..]));
        assert_eq!(Done(&b""[..], Token::Move(&b"bxc1=R#"[..])), san_move(&b"bxc1=R#"[..]));
    }

    #[test]
    fn test_san_move_piece_move() {
        assert_eq!(Done(&b""[..], Token::Move(&b"Nf3"[..])), san_move(&b"Nf3"[..]));
        assert_eq!(Done(&b""[..], Token::Move(&b"Nf6"[..])), san_move(&b"Nf6"[..]));
        assert_eq!(Done(&b""[..], Token::Move(&b"Nf6+"[..])), san_move(&b"Nf6+"[..]));
        assert_eq!(Done(&b""[..], Token::Move(&b"N2f6"[..])), san_move(&b"N2f6"[..]));
        assert_eq!(Done(&b""[..], Token::Move(&b"N2f6+"[..])), san_move(&b"N2f6+"[..]));
        assert_eq!(Done(&b""[..], Token::Move(&b"Ng1f3"[..])), san_move(&b"Ng1f3"[..]));
        assert_eq!(Done(&b""[..], Token::Move(&b"Ng1f3+"[..])), san_move(&b"Ng1f3+"[..]));
        assert_eq!(Done(&b""[..], Token::Move(&b"Rd3d1"[..])), san_move(&b"Rd3d1"[..]));
        assert_eq!(Done(&b""[..], Token::Move(&b"Rd3d1+"[..])), san_move(&b"Rd3d1+"[..]));
        assert_eq!(Done(&b""[..], Token::Move(&b"Rd3d1#"[..])), san_move(&b"Rd3d1#"[..]));
    }
    #[test]
    fn test_san_move_piece_capture() {
        assert_eq!(Done(&b""[..], Token::Move(&b"Nxf3"[..])), san_move(&b"Nxf3"[..]));
        assert_eq!(Done(&b""[..], Token::Move(&b"Nxf6"[..])), san_move(&b"Nxf6"[..]));
        assert_eq!(Done(&b""[..], Token::Move(&b"Nxf6+"[..])), san_move(&b"Nxf6+"[..]));
        assert_eq!(Done(&b""[..], Token::Move(&b"N2xf6"[..])), san_move(&b"N2xf6"[..]));
        assert_eq!(Done(&b""[..], Token::Move(&b"N2xf6+"[..])), san_move(&b"N2xf6+"[..]));
        assert_eq!(Done(&b""[..], Token::Move(&b"Ng1xf3"[..])), san_move(&b"Ng1xf3"[..]));
        assert_eq!(Done(&b""[..], Token::Move(&b"Ng1xf3+"[..])), san_move(&b"Ng1xf3+"[..]));
        assert_eq!(Done(&b""[..], Token::Move(&b"Rd3xd1"[..])), san_move(&b"Rd3xd1"[..]));
        assert_eq!(Done(&b""[..], Token::Move(&b"Rd3xd1+"[..])), san_move(&b"Rd3xd1+"[..]));
        assert_eq!(Done(&b""[..], Token::Move(&b"Rd3xd1#"[..])), san_move(&b"Rd3xd1#"[..]));
    }

    #[test]
    fn test_castles() {
        assert_eq!(Done(&b""[..], Token::Move(&b"O-O"[..])), san_move(&b"O-O"[..]));
        assert_eq!(Done(&b""[..], Token::Move(&b"O-O-O"[..])), san_move(&b"O-O-O"[..]));
        assert_eq!(Done(&b""[..], Token::Move(&b"O-O+"[..])), san_move(&b"O-O+"[..]));
        assert_eq!(Done(&b""[..], Token::Move(&b"O-O-O+"[..])), san_move(&b"O-O-O+"[..]));
        assert_eq!(Done(&b""[..], Token::Move(&b"O-O-O#"[..])), san_move(&b"O-O-O#"[..]));
    }
    #[test]
    fn test_null_move() {
        assert_eq!(Done(&b""[..], Token::Move(&b"--"[..])), san_move(&b"--"[..]));
        assert_eq!(Done(&b""[..], Token::Move(&b"--+"[..])), san_move(&b"--+"[..]));
        assert_eq!(Done(&b""[..], Token::Move(&b"--#"[..])), san_move(&b"--#"[..]));
        assert_eq!(Done(&b""[..], Token::Move(&b"Z0"[..])), san_move(&b"Z0"[..]));
        assert_eq!(Done(&b""[..], Token::Move(&b"Z0+"[..])), san_move(&b"Z0+"[..]));
        assert_eq!(Done(&b""[..], Token::Move(&b"Z0#"[..])), san_move(&b"Z0#"[..]));
    }
    #[bench]
    fn bench_parse_san_move_null(b: &mut Bencher) {
        b.iter(|| {
            assert_eq!(Done(&b""[..], Token::Move(&b"--#"[..])), san_move(&b"--#"[..]));
        });
    }
    #[bench]
    fn bench_parse_san_move_castle_queen_side(b: &mut Bencher) {
        b.iter(|| {
            assert_eq!(Done(&b""[..], Token::Move(&b"O-O-O"[..])), san_move(&b"O-O-O"[..]));
        });
    }
    #[bench]
    fn bench_parse_san_move_castle_king_side(b: &mut Bencher) {
        b.iter(|| {
            assert_eq!(Done(&b""[..], Token::Move(&b"O-O"[..])), san_move(&b"O-O"[..]));
        });
    }
    #[bench]
    fn bench_parse_san_move_simple_capture(b: &mut Bencher) {
        b.iter(|| {
            assert_eq!(Done(&b""[..], Token::Move(&b"bxc2"[..])), san_move(&b"bxc2"[..]));
        });
    }
    #[bench]
    fn bench_parse_san_move_simple(b: &mut Bencher) {
        b.iter(|| {
            assert_eq!(Done(&b""[..], Token::Move(&b"e4"[..])), san_move(&b"e4"[..]));
        });
    }
    #[bench]
    fn bench_parse_san_capture_promotion(b: &mut Bencher) {
        b.iter(|| {
            assert_eq!(Done(&b""[..], Token::Move(&b"bxc1=R"[..])), san_move(&b"bxc1=R"[..]));
        });
    }
    #[bench]
    fn bench_parse_san_move_complicated(b: &mut Bencher) {
        b.iter(|| {
            assert_eq!(Done(&b""[..], Token::Move(&b"bxc1=R+"[..])), san_move(&b"bxc1=R+"[..]));
        });
    }
}
