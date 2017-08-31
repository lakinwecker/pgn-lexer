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

// TODO: Figure out better error handling
const SAN_PARSING_ERROR: u32 = 32;

fn is_file(i:u8) -> bool {
    return i >= b'a' && i <= b'h';
}
fn is_rank(i:u8) -> bool {
    return i >= b'1' && i <= b'8';
}
fn is_capture(i:u8) -> bool {
    return i == b'x';
}
fn is_equals(i:u8) -> bool {
    return i == b'=';
}
fn is_piece(i:u8) -> bool {
    return i == b'R' || i == b'N' || i == b'B' || i == b'Q' || i == b'K';
}

fn is_o(i:u8) -> bool {
    return i == b'O';
}

fn is_dash(i:u8) -> bool {
    return i == b'-';
}

fn is_zed(i:u8) -> bool {
    return i == b'Z';
}
fn is_zero(i:u8) -> bool {
    return i == b'0';
}
fn is_plus_or_hash(i:u8) -> bool {
    return i == b'#' || i == b'+';
}

macro_rules! match_character {
    ($name:ident, $($matcher:ident),+) => {
        fn $name (incoming:&[u8]) -> Option<usize> {
            let mut i: usize = 0;
            $(
                {
                    if incoming.len() <= i || !$matcher(incoming[i]) {
                        return None
                    }
                    i += 1
                }
            )*;
            Some(i)
        }
    };
}

match_character![check, is_plus_or_hash];
match_character![pawn_capture, is_file, is_capture, is_file, is_rank];
match_character![pawn_move, is_file, is_rank];
match_character![promotion, is_equals, is_piece];

// e4 dxe4 e8=Q dxe8=Q
fn san_pawn_move(i:&[u8]) -> IResult<&[u8], Token>{
    let result = pawn_capture(i)
    .or_else(|| pawn_move(i))
    .and_then(|length| {
        promotion(&i[length..])
        .or_else(|| Some(0))
        .and_then(|l2| {
            let length = length + l2;
            check(&i[length..])
            .or_else(|| Some(0))
            .and_then(|l3| Some(length + l3))
        })
    });
    match result {
        Some(length) => return IResult::Done(&i[length..], Token::Move(&i[0..length])),
        None => return IResult::Error(ErrorKind::Custom(SAN_PARSING_ERROR))
    }
}


// Ng1xf3
match_character![piece_capture_with_rank_and_file, is_piece, is_file, is_rank, is_capture, is_file, is_rank];
// N1xf3
match_character![piece_capture_with_rank, is_piece, is_rank, is_capture, is_file, is_rank];
// Ngxf3
match_character![piece_capture_with_file, is_piece, is_file, is_capture, is_file, is_rank];
// Nxf3
match_character![piece_capture, is_piece, is_capture, is_file, is_rank];
// Ng1f3
match_character![piece_move_with_rank_and_file, is_piece, is_file, is_rank, is_file, is_rank];
// N1f3
match_character![piece_move_with_rank, is_piece, is_rank, is_file, is_rank];
// Ngf3
match_character![piece_move_with_file, is_piece, is_file, is_file, is_rank];
// Nf3
match_character![piece_move, is_piece, is_file, is_rank];

fn san_piece_move(i:&[u8]) -> IResult<&[u8], Token>{
    let result = piece_capture_with_rank_and_file(i)
    .or_else(|| piece_capture_with_rank(i))
    .or_else(|| piece_capture_with_file(i))
    .or_else(|| piece_capture(i))
    .or_else(|| piece_move_with_rank_and_file(i))
    .or_else(|| piece_move_with_rank(i))
    .or_else(|| piece_move_with_file(i))
    .or_else(|| piece_move(i))
    .and_then(|length| {
        check(&i[length..])
        .or_else(|| Some(0))
        .and_then(|l2| Some(length + l2))
    });
    match result {
        Some(length) => return IResult::Done(&i[length..], Token::Move(&i[0..length])),
        None => return IResult::Error(ErrorKind::Custom(SAN_PARSING_ERROR))
    }
}

match_character![king_side_castles, is_o, is_dash, is_o];
match_character![queen_side_castles, is_o, is_dash, is_o, is_dash, is_o];

fn san_castles(i:&[u8]) -> IResult<&[u8], Token>{
    let result = queen_side_castles(i)
    .or_else(|| king_side_castles(i))
    .and_then(|length| {
        return check(&i[length..])
        .or_else(|| Some(0))
        .and_then(|extra_length| Some(length+extra_length));
    });
    match result {
        Some(length) => return IResult::Done(&i[length..], Token::Move(&i[0..length])),
        None => return IResult::Error(ErrorKind::Custom(SAN_PARSING_ERROR))
    }
}

// Z0
match_character![null_move_z0, is_zed, is_zero];
// --
match_character![null_move_dash_dash, is_dash, is_dash];

fn san_null_move(i:&[u8]) -> IResult<&[u8], Token>{
    let result = null_move_dash_dash(i)
    .or_else(|| null_move_z0(i))
    .and_then(|length| {
        return check(&i[length..])
        .or_else(|| Some(0))
        .and_then(|extra_length| Some(length+extra_length));
    });
    match result {
        Some(length) => return IResult::Done(&i[length..], Token::Move(&i[0..length])),
        None => return IResult::Error(ErrorKind::Custom(SAN_PARSING_ERROR))
    }
}


pub fn san_move(i:&[u8]) -> IResult<&[u8], Token>{
    if i.len() < 1 {
      return IResult::Error(ErrorKind::Custom(SAN_PARSING_ERROR));
    }
    match i[0] {
        b'R' | b'N' | b'B' | b'Q' | b'K' => san_piece_move(i),
        b'a'...b'h' => san_pawn_move(i),
        b'O' => san_castles(i),
        b'-' | b'Z' => san_null_move(i),
        _ => IResult::Error(ErrorKind::Custom(SAN_PARSING_ERROR))
    }
}


#[cfg(test)]
mod tests {

    use super::*;
    use nom::IResult::*;
    use test::Bencher;

    #[test]
    fn test_san_move_pawn_single_square() {
        assert_eq!(Done(&b""[..], Token::Move(b"a1")), san_move(b"a1"));
        assert_eq!(Done(&b""[..], Token::Move(b"a8")), san_move(b"a8"));
        assert_eq!(Done(&b""[..], Token::Move(b"h1")), san_move(b"h1"));
        assert_eq!(Done(&b""[..], Token::Move(b"h8")), san_move(b"h8"));
        assert_eq!(Done(&b""[..], Token::Move(b"e4")), san_move(b"e4"));
        assert_eq!(Done(&b" e5"[..], Token::Move(b"e4")), san_move(b"e4 e5"));
        assert_eq!(Done(&b"!! e5"[..], Token::Move(b"e4")), san_move(b"e4!! e5"));
        assert_eq!(Done(&b"!? e5"[..], Token::Move(b"e4")), san_move(b"e4!? e5"));
        assert_eq!(Done(&b"!? e5"[..], Token::Move(b"e4")), san_move(b"e4!? e5"));
        assert_eq!(Done(&b""[..], Token::Move(b"e4=N")), san_move(b"e4=N"));
        assert_eq!(Done(&b""[..], Token::Move(b"e4=N#")), san_move(b"e4=N#"));
    }

    #[test]
    fn test_san_pawn_capture() {
        assert_eq!(Done(&b""[..], Token::Move(b"bxc1")), san_move(b"bxc1"));
        assert_eq!(Done(&b""[..], Token::Move(b"axe4")), san_move(b"axe4"));
        assert_eq!(Done(&b""[..], Token::Move(b"bxc1+")), san_move(b"bxc1+"));
        assert_eq!(Done(&b""[..], Token::Move(b"bxc1=R+")), san_move(b"bxc1=R+"));
        assert_eq!(Done(&b""[..], Token::Move(b"bxc1=R#")), san_move(b"bxc1=R#"));
    }

    #[test]
    fn test_san_move_piece_move() {
        assert_eq!(Done(&b""[..], Token::Move(b"Nf3")), san_move(b"Nf3"));
        assert_eq!(Done(&b""[..], Token::Move(b"Nf6")), san_move(b"Nf6"));
        assert_eq!(Done(&b""[..], Token::Move(b"Nf6+")), san_move(b"Nf6+"));
        assert_eq!(Done(&b""[..], Token::Move(b"N2f6")), san_move(b"N2f6"));
        assert_eq!(Done(&b""[..], Token::Move(b"N2f6+")), san_move(b"N2f6+"));
        assert_eq!(Done(&b""[..], Token::Move(b"Ng1f3")), san_move(b"Ng1f3"));
        assert_eq!(Done(&b""[..], Token::Move(b"Ng1f3+")), san_move(b"Ng1f3+"));
        assert_eq!(Done(&b""[..], Token::Move(b"Rd3d1")), san_move(b"Rd3d1"));
        assert_eq!(Done(&b""[..], Token::Move(b"Rd3d1+")), san_move(b"Rd3d1+"));
        assert_eq!(Done(&b""[..], Token::Move(b"Rd3d1#")), san_move(b"Rd3d1#"));
    }
    #[test]
    fn test_san_move_piece_capture() {
        assert_eq!(Done(&b""[..], Token::Move(b"Nxf3")), san_move(b"Nxf3"));
        assert_eq!(Done(&b""[..], Token::Move(b"Nxf6")), san_move(b"Nxf6"));
        assert_eq!(Done(&b""[..], Token::Move(b"Nxf6+")), san_move(b"Nxf6+"));
        assert_eq!(Done(&b""[..], Token::Move(b"N2xf6")), san_move(b"N2xf6"));
        assert_eq!(Done(&b""[..], Token::Move(b"N2xf6+")), san_move(b"N2xf6+"));
        assert_eq!(Done(&b""[..], Token::Move(b"Ng1xf3")), san_move(b"Ng1xf3"));
        assert_eq!(Done(&b""[..], Token::Move(b"Ng1xf3+")), san_move(b"Ng1xf3+"));
        assert_eq!(Done(&b""[..], Token::Move(b"Rd3xd1")), san_move(b"Rd3xd1"));
        assert_eq!(Done(&b""[..], Token::Move(b"Rd3xd1+")), san_move(b"Rd3xd1+"));
        assert_eq!(Done(&b""[..], Token::Move(b"Rd3xd1#")), san_move(b"Rd3xd1#"));
    }

    #[test]
    fn test_castles() {
        assert_eq!(Done(&b""[..], Token::Move(b"O-O")), san_move(b"O-O"));
        assert_eq!(Done(&b""[..], Token::Move(b"O-O-O")), san_move(b"O-O-O"));
        assert_eq!(Done(&b""[..], Token::Move(b"O-O+")), san_move(b"O-O+"));
        assert_eq!(Done(&b""[..], Token::Move(b"O-O#")), san_move(b"O-O#"));
        assert_eq!(Done(&b""[..], Token::Move(b"O-O-O+")), san_move(b"O-O-O+"));
        assert_eq!(Done(&b""[..], Token::Move(b"O-O-O#")), san_move(b"O-O-O#"));
    }
    #[test]
    fn test_null_move() {
        assert_eq!(Done(&b""[..], Token::Move(b"--")), san_move(b"--"));
        assert_eq!(Done(&b""[..], Token::Move(b"--+")), san_move(b"--+"));
        assert_eq!(Done(&b""[..], Token::Move(b"--#")), san_move(b"--#"));
        assert_eq!(Done(&b""[..], Token::Move(b"Z0")), san_move(b"Z0"));
        assert_eq!(Done(&b""[..], Token::Move(b"Z0+")), san_move(b"Z0+"));
        assert_eq!(Done(&b""[..], Token::Move(b"Z0#")), san_move(b"Z0#"));
    }
    #[bench]
    fn bench_parse_san_move_null(b: &mut Bencher) {
        b.iter(|| {
            assert_eq!(Done(&b""[..], Token::Move(b"--#")), san_move(b"--#"));
        });
    }
    #[bench]
    fn bench_parse_san_move_castle_queen_side(b: &mut Bencher) {
        b.iter(|| {
            assert_eq!(Done(&b""[..], Token::Move(b"O-O-O")), san_move(b"O-O-O"));
        });
    }
    #[bench]
    fn bench_parse_san_move_castle_king_side(b: &mut Bencher) {
        b.iter(|| {
            assert_eq!(Done(&b""[..], Token::Move(b"O-O")), san_move(b"O-O"));
        });
    }
    #[bench]
    fn bench_parse_san_move_simple_capture(b: &mut Bencher) {
        b.iter(|| {
            assert_eq!(Done(&b""[..], Token::Move(b"bxc2")), san_move(b"bxc2"));
        });
    }
    #[bench]
    fn bench_parse_san_move_simple(b: &mut Bencher) {
        b.iter(|| {
            assert_eq!(Done(&b""[..], Token::Move(b"e4")), san_move(b"e4"));
        });
    }
    #[bench]
    fn bench_parse_san_capture_promotion(b: &mut Bencher) {
        b.iter(|| {
            assert_eq!(Done(&b""[..], Token::Move(b"bxc1=R")), san_move(b"bxc1=R"));
        });
    }
    #[bench]
    fn bench_parse_san_move_complicated(b: &mut Bencher) {
        b.iter(|| {
            assert_eq!(Done(&b""[..], Token::Move(b"bxc1=R+")), san_move(b"bxc1=R+"));
        });
    }
}
