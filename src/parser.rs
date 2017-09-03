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

use nom::*;
use std::fmt;

///-----------------------------------------------------------------------------
#[derive(Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Debug, Hash)]
pub enum Token <'a> {
    Move(&'a [u8]),
    NullMove(&'a [u8]),
    EscapeComment(&'a [u8]),
    NAG(&'a [u8]),
    Result(&'a [u8]),
    Commentary(&'a [u8]),
    TagSymbol(&'a [u8]),
    TagString(&'a [u8]),
    StartVariation(&'a [u8]),
    EndVariation(&'a [u8]),
}

impl<'a> fmt::Display for Token<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // The `f` value implements the `Write` trait, which is what the
        // write! macro is expecting. Note that this formatting ignores the
        // various flags provided to format strings.
        match *self {
            Token::Move(x) => write!(f, "Move({})", String::from_utf8_lossy(x)),
            Token::NullMove(x) => write!(f, "NullMove({})", String::from_utf8_lossy(x)),
            Token::EscapeComment(x) => write!(f, "EscapeComment({})", String::from_utf8_lossy(x)),
            Token::NAG(x) => write!(f, "NAG({})", String::from_utf8_lossy(x)),
            Token::Result(x) => write!(f, "Result({})", String::from_utf8_lossy(x)),
            Token::Commentary(x) => write!(f, "Commentary({})", String::from_utf8_lossy(x)),
            Token::TagSymbol(x) => write!(f, "TagSymbol({})", String::from_utf8_lossy(x)),
            Token::TagString(x) => write!(f, "TagString({})", String::from_utf8_lossy(x)),
            Token::StartVariation(x) => write!(f, "StartVariation({})", String::from_utf8_lossy(x)),
            Token::EndVariation(x) => write!(f, "EndVariation({})", String::from_utf8_lossy(x)),
        }
    }
}

// TODO: Figure out better error handling
const SAN_PAWN_MOVE_INVALID: u32 = 1;
const SAN_PIECE_MOVE_INVALID: u32 = 2;
const SAN_CASTLES_INVALID: u32 = 3;
const SAN_NULL_MOVE_INVALID: u32 = 4;
const SAN_EMPTY_INPUT: u32 = 5;
const SAN_INVALID_CHARACTER: u32 = 6;
const PGN_INTEGER_EMPTY: u32 = 1001;
const PGN_INTEGER_INVALID: u32 = 1002;
const PGN_STRING_EMPTY: u32 = 1010;
const PGN_STRING_INVALID: u32 = 1011;
const PGN_STRING_INVALID_ESCAPE_SEQUENCE: u32 = 1012;
const PGN_STRING_TOO_LARGE: u32 = 1013;
const PGN_ESCAPE_COMMENT_EMPTY: u32 = 1020;
const PGN_ESCAPE_COMMENT_INVALID: u32 = 1021;
const PGN_NAG_EMPTY: u32 = 1030;
const PGN_NAG_INVALID: u32 = 1031;
const PGN_SYMBOL_EMPTY: u32 = 1040;
const PGN_SYMBOL_INVALID: u32 = 1041;
const PGN_GAME_RESULT_EMPTY: u32 = 1050;
const PGN_GAME_RESULT_INVALID: u32 = 1051;
const PGN_COMMENTARY_EMPTY: u32 = 1060;
const PGN_COMMENTARY_INVALID: u32 = 1061;
const PGN_COMMENTARY_TOO_LARGE: u32 = 1062;
const PGN_TAG_PAIR_EMPTY: u32 = 1070;
const PGN_TAG_PAIR_INVALID: u32 = 1071;
const PGN_TAG_PAIR_INVALID_SYMBOL: u32 = 1072;
const PGN_TAG_PAIR_INVALID_STRING: u32 = 1073;
const PGN_MOVE_NUMBER_EMPTY: u32 = 1080;
const PGN_MOVE_NUMBER_INVALID: u32 = 1081;
const PGN_START_VARIATION_EMPTY: u32 = 1090;
const PGN_START_VARIATION_INVALID: u32 = 1091;
const PGN_END_VARIATION_EMPTY: u32 = 1100;
const PGN_END_VARIATION_INVALID: u32 = 1101;


fn is_0(i:u8) -> bool { i == b'0' }
fn is_1(i:u8) -> bool { i == b'1' }
fn is_2(i:u8) -> bool { i == b'2' }
fn is_capture(i:u8) -> bool { i == b'x' }
fn is_dash(i:u8) -> bool { i == b'-' }
fn is_digit(i:u8) -> bool { i >= b'0' && i <= b'9' }
fn is_equals(i:u8) -> bool { i == b'=' }
fn is_file(i:u8) -> bool { i >= b'a' && i <= b'h' }
fn is_letter(i:u8) -> bool { is_lowercase_letter(i) || is_uppercase_letter(i) }
fn is_lowercase_letter(i:u8) -> bool { i >= b'a' && i <= b'z' }
fn is_o(i:u8) -> bool { i == b'O' }
fn is_period(i:u8) -> bool { i == b'.' }
fn is_piece(i:u8) -> bool { i == b'R' || i == b'N' || i == b'B' || i == b'Q' || i == b'K' }
fn is_plus_or_hash(i:u8) -> bool { i == b'#' || i == b'+' }
fn is_rank(i:u8) -> bool { i >= b'1' && i <= b'8' }
fn is_slash(i:u8) -> bool { i == b'/' }
fn is_space(i:u8) -> bool { i == b' ' }
fn is_star(i:u8) -> bool { i == b'*' }
fn is_uppercase_letter(i:u8) -> bool { i >= b'A' && i <= b'Z' }
fn is_whitespace(i:u8) -> bool { i == b' ' || i == b'\n' || i == b'\r' || i == b'\t'  }
fn is_zed(i:u8) -> bool { i == b'Z' }
fn is_zero(i:u8) -> bool { i == b'0' }

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
        None => return IResult::Error(ErrorKind::Custom(SAN_PAWN_MOVE_INVALID))
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
        None => return IResult::Error(ErrorKind::Custom(SAN_PIECE_MOVE_INVALID))
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
        None => return IResult::Error(ErrorKind::Custom(SAN_CASTLES_INVALID))
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
        Some(length) => return IResult::Done(&i[length..], Token::NullMove(&i[0..length])),
        None => return IResult::Error(ErrorKind::Custom(SAN_NULL_MOVE_INVALID))
    }
}


fn san_move_token(i:&[u8]) -> IResult<&[u8], Token>{
    if i.len() < 1 {
        return IResult::Error(ErrorKind::Custom(SAN_EMPTY_INPUT));
    }
    match i[0] {
        b'R' | b'N' | b'B' | b'Q' | b'K' => san_piece_move(i),
        b'a'...b'h' => san_pawn_move(i),
        b'O' => san_castles(i),
        b'-' | b'Z' => san_null_move(i),
        _ => IResult::Error(ErrorKind::Custom(SAN_INVALID_CHARACTER))
    }
}

// Delimited by quote: ASCII 34
// \\ -> \
// \" -> "
// \t and \n not allowed
// max of 255 length
// printable characters, ASCII [32-126]
//
// Results still include \" and \\
//
// TODO: This does _not_ deal with utf-8
const MAX_LENGTH: usize = 255;
fn pgn_string(i:&[u8]) -> IResult<&[u8], &[u8]>{
    if i.len() < 1 {
        return IResult::Error(ErrorKind::Custom(PGN_STRING_EMPTY));
    }
    let mut prev = i[0];
    if prev != b'"' {
        return IResult::Error(ErrorKind::Custom(PGN_STRING_INVALID));
    }
    let mut length = 1;
    while length < i.len() {
        let cur = i[length];
        if cur == b'"' && prev != b'\\' {
            break;
        } else if prev == b'\\' && (cur != b'\\' && cur != b'"') {
            return IResult::Error(ErrorKind::Custom(PGN_STRING_INVALID_ESCAPE_SEQUENCE));
        }
        prev = cur;
        length += 1;
        if length > MAX_LENGTH {
            return IResult::Error(ErrorKind::Custom(PGN_STRING_TOO_LARGE));
        }
        
    }
    // Ensure we skip over the quotes
    IResult::Done(&i[length+1..], &i[1..length])
}

fn pgn_integer(i:&[u8]) -> IResult<&[u8], &[u8]>{
    if i.len() < 1 {
        return IResult::Error(ErrorKind::Custom(PGN_INTEGER_EMPTY));
    }
    let mut length = 0;
    while length < i.len() && i[length] >= b'0' && i[length] <= b'9' {
        length += 1
    }
    if length == 0 {
        return IResult::Error(ErrorKind::Custom(PGN_INTEGER_INVALID));
    } else {
        IResult::Done(&i[length..], &i[0..length])
    }
}

fn pgn_escape_comment_token(i:&[u8]) -> IResult<&[u8], Token>{
    if i.len() < 1 {
        return IResult::Error(ErrorKind::Custom(PGN_ESCAPE_COMMENT_EMPTY));
    }
    if i[0] != b'%' {
        return IResult::Error(ErrorKind::Custom(PGN_ESCAPE_COMMENT_INVALID));
    }
    let mut length = 1;
    while length < i.len() && i[length] != b'\r' && i[length] != b'\n' {
        length += 1
    }
    IResult::Done(&i[length..], Token::EscapeComment(&i[1..length]))
}

fn pgn_nag_token(i:&[u8]) -> IResult<&[u8], Token>{
    if i.len() < 1 {
        return IResult::Error(ErrorKind::Custom(PGN_NAG_EMPTY));
    }
    if i[0] != b'$' {
        return IResult::Error(ErrorKind::Custom(PGN_NAG_INVALID));
    }
    match pgn_integer(&i[1..]) {
        IResult::Done(_left, integer) => IResult::Done(&i[integer.len()+1..], Token::NAG(&i[1..integer.len()+1])),
        IResult::Incomplete(x) => IResult::Incomplete(x),
        _ => IResult::Error(ErrorKind::Custom(PGN_NAG_INVALID))
        
    }
}

fn pgn_symbol(i:&[u8]) -> IResult<&[u8], &[u8]>{
    if i.len() < 1 {
        return IResult::Error(ErrorKind::Custom(PGN_SYMBOL_EMPTY));
    }
    if !(is_digit(i[0]) || is_letter(i[0])) {
        return IResult::Error(ErrorKind::Custom(PGN_SYMBOL_INVALID));
    }
    let mut length = 1;
    while length < i.len() && (
        is_digit(i[length])
        || is_letter(i[length])
        || is_plus_or_hash(i[length])
        || is_equals(i[length])
        || i[length] == b':'
        || i[length] == b'_'
        || i[length] == b'-'
    ) {
        length += 1
    }
    IResult::Done(&i[length..], &i[0..length])
}

match_character![game_ongoing, is_star];
match_character![game_white_win, is_1, is_dash, is_0];
match_character![game_black_win, is_0, is_dash, is_1];
match_character![game_draw, is_1, is_slash, is_2, is_dash, is_1, is_slash, is_2];

fn pgn_game_result_token(i:&[u8]) -> IResult<&[u8], Token>{
    if i.len() < 1 {
        return IResult::Error(ErrorKind::Custom(PGN_GAME_RESULT_EMPTY));
    }

    let result = game_ongoing(i)
    .or_else(|| game_white_win(i))
    .or_else(|| game_black_win(i))
    .or_else(|| game_draw(i));
    match result {
        Some(length) => return IResult::Done(&i[length..], Token::Result(&i[0..length])),
        None => return IResult::Error(ErrorKind::Custom(PGN_GAME_RESULT_INVALID))
    }
}

// TODO: ; type comments

// This is somewhat arbitrarily picked to be 2MB. A single commentary token
// can't exceed that. We need to set _some_ sort of limit, this seems reasonable
const MAX_COMMENTARY_LENGTH:usize = 2097152;
fn pgn_commentary_token(i:&[u8]) -> IResult<&[u8], Token>{
    if i.len() < 1 {
        return IResult::Error(ErrorKind::Custom(PGN_COMMENTARY_EMPTY));
    }
    if i[0] != b'{' {
        return IResult::Error(ErrorKind::Custom(PGN_COMMENTARY_INVALID));
    }
    let mut length = 1;
    while length < i.len() && i[length] != b'}' {
        length += 1;
        if length > MAX_COMMENTARY_LENGTH {
            return IResult::Error(ErrorKind::Custom(PGN_COMMENTARY_TOO_LARGE));
        }
    }
    // Ensure we skip over the braces.
    IResult::Done(&i[length+1..], Token::Commentary(&i[1..length]))
}

fn remove_whitespace(i:&[u8]) -> &[u8] {
    let mut length = 0;
    while length < i.len() && is_whitespace(i[length]) {
        length += 1;
    }
    &i[length..]
}

fn get_spaces(i:&[u8]) -> &[u8] {
    let mut length = 0;
    while length < i.len() && is_space(i[length]) {
        length += 1;
    }
    &i[0..length]
}

fn pgn_tag_symbol_token(i:&[u8]) -> IResult<&[u8], Token> {
    if i.len() < 1 {
        return IResult::Error(ErrorKind::Custom(PGN_TAG_PAIR_EMPTY));
    }
    if i[0] != b'[' {
        return IResult::Error(ErrorKind::Custom(PGN_TAG_PAIR_INVALID));
    }
    let i = remove_whitespace(&i[1..]);
    match pgn_symbol(i) {
        IResult::Done(i, symbol) => {
            return IResult::Done(&i[0..], Token::TagSymbol(symbol));
        },
        IResult::Incomplete(x) => IResult::Incomplete(x),
        _ => IResult::Error(ErrorKind::Custom(PGN_TAG_PAIR_INVALID_SYMBOL))
    }
}

fn pgn_tag_string_token(i:&[u8]) -> IResult<&[u8], Token> {
    if i.len() < 1 {
        return IResult::Error(ErrorKind::Custom(PGN_TAG_PAIR_EMPTY));
    }
    let i = remove_whitespace(&i[0..]);
    match pgn_string(i) {
        IResult::Done(i, string) => {
            let i = remove_whitespace(i);
            let x = 0;
            if x < i.len() && i[x] == b']' {
                return IResult::Done(&i[1..], Token::TagString(string));
            } else {
                return IResult::Error(ErrorKind::Custom(PGN_TAG_PAIR_INVALID));
            }
        },
        IResult::Incomplete(x) => IResult::Incomplete(x),
        _ => IResult::Error(ErrorKind::Custom(PGN_TAG_PAIR_INVALID_STRING))
    }
}

match_character![one_period, is_period];
match_character![two_periods, is_period, is_period];
match_character![three_periods, is_period, is_period, is_period];

fn pgn_move_number(i:&[u8]) -> IResult<&[u8], &[u8]>{
    if i.len() < 1 {
        return IResult::Error(ErrorKind::Custom(PGN_MOVE_NUMBER_EMPTY));
    }
    let result = pgn_integer(i);
    match result {
        IResult::Done(left, integer) => {
            let ws = get_spaces(left);
            let left = &left[ws.len()..];
            let result = three_periods(left)
                .or_else(|| two_periods(left))
                .or_else(|| one_period(left))
                .or_else(|| Some(0)).
                and_then(|periods_length| Some(integer.len() + ws.len() + periods_length));
            match result {
                Some(length) => return IResult::Done(&i[length..], &i[0..length]),
                None => return IResult::Done(&i[integer.len()..], &i[0..integer.len()]),
            }
        },
        IResult::Incomplete(x) => IResult::Incomplete(x),
        _ => IResult::Error(ErrorKind::Custom(PGN_MOVE_NUMBER_INVALID))
    }
}

fn pgn_start_variation_token(i:&[u8]) -> IResult<&[u8], Token>{
    if i.len() < 1 {
        return IResult::Error(ErrorKind::Custom(PGN_START_VARIATION_EMPTY));
    }
    if i[0] == b'(' {
        return IResult::Done(&i[1..], Token::StartVariation(&i[0..1]));
    } else {
        return IResult::Error(ErrorKind::Custom(PGN_START_VARIATION_INVALID));
    }
}

fn pgn_end_variation_token(i:&[u8]) -> IResult<&[u8], Token>{
    if i.len() < 1 {
        return IResult::Error(ErrorKind::Custom(PGN_END_VARIATION_EMPTY));
    }
    if i[0] == b')' {
        return IResult::Done(&i[1..], Token::EndVariation(&i[0..1]));
    } else {
        return IResult::Error(ErrorKind::Custom(PGN_END_VARIATION_INVALID));
    }
}

fn or_else<I, O, E, Op>(res: IResult<I, O, E>, op: Op) -> IResult<I, O, E>
    where
        Op: FnOnce() -> IResult<I, O, E>, 
{
    match res {
        IResult::Done(i, o) => IResult::Done(i, o),
        IResult::Incomplete(_) => op(),
        IResult::Error(_) => op(),
    }
}

// A simple PGN token stream. Operates on a byte slice, and streams
// byte slices of the form Token::
pub struct PGNTokenIterator<'a> {
    pub bytes: &'a [u8],
}

// Implement `Iterator` for `Fibonacci`.
// The `Iterator` trait only requires a method to be defined for the `next` element.
impl<'a> Iterator for PGNTokenIterator<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Token<'a>> {
        let i = self.bytes;
        let i = remove_whitespace(i);
        let mut result = pgn_escape_comment_token(i);
        result = or_else(result, || {
            match pgn_move_number(i) {
                IResult::Done(left, _) => {
                    let left = remove_whitespace(left);
                    san_move_token(left)
                },
                IResult::Incomplete(x) => IResult::Incomplete(x),
                IResult::Error(e) => IResult::Error(e)
            }
        });
        result = or_else(result, || pgn_game_result_token(i));
        result = or_else(result, || pgn_tag_symbol_token(i));
        result = or_else(result, || pgn_tag_string_token(i));
        result = or_else(result, || pgn_start_variation_token(i));
        result = or_else(result, || pgn_end_variation_token(i));
        result = or_else(result, || pgn_commentary_token(i));
        result = or_else(result, || pgn_nag_token(i));
        result = or_else(result, || san_move_token(i));
        match result {
            IResult::Done(i, token) => {
                self.bytes = i;
                Some(token.clone())
            },
            _ => None
        }
    }
}



#[cfg(test)]
mod tests {

    use super::*;
    use nom::IResult::*;
    use test::Bencher;

    #[test]
    fn test_san_move_pawn_single_square() {
        assert_eq!(Done(&b""[..], Token::Move(b"a1")), san_move_token(b"a1"));
        assert_eq!(Done(&b""[..], Token::Move(b"a8")), san_move_token(b"a8"));
        assert_eq!(Done(&b""[..], Token::Move(b"h1")), san_move_token(b"h1"));
        assert_eq!(Done(&b""[..], Token::Move(b"h8")), san_move_token(b"h8"));
        assert_eq!(Done(&b""[..], Token::Move(b"e4")), san_move_token(b"e4"));
        assert_eq!(Done(&b" e5"[..], Token::Move(b"e4")), san_move_token(b"e4 e5"));
        assert_eq!(Done(&b"!! e5"[..], Token::Move(b"e4")), san_move_token(b"e4!! e5"));
        assert_eq!(Done(&b"!? e5"[..], Token::Move(b"e4")), san_move_token(b"e4!? e5"));
        assert_eq!(Done(&b"!? e5"[..], Token::Move(b"e4")), san_move_token(b"e4!? e5"));
        assert_eq!(Done(&b""[..], Token::Move(b"e4=N")), san_move_token(b"e4=N"));
        assert_eq!(Done(&b""[..], Token::Move(b"e4=N#")), san_move_token(b"e4=N#"));
    }

    #[test]
    fn test_san_pawn_capture() {
        assert_eq!(Done(&b""[..], Token::Move(b"bxc1")), san_move_token(b"bxc1"));
        assert_eq!(Done(&b""[..], Token::Move(b"axe4")), san_move_token(b"axe4"));
        assert_eq!(Done(&b""[..], Token::Move(b"bxc1+")), san_move_token(b"bxc1+"));
        assert_eq!(Done(&b""[..], Token::Move(b"bxc1=R+")), san_move_token(b"bxc1=R+"));
        assert_eq!(Done(&b""[..], Token::Move(b"bxc1=R#")), san_move_token(b"bxc1=R#"));
    }

    #[test]
    fn test_san_move_piece_move() {
        assert_eq!(Done(&b""[..], Token::Move(b"Nf3")), san_move_token(b"Nf3"));
        assert_eq!(Done(&b""[..], Token::Move(b"Nf6")), san_move_token(b"Nf6"));
        assert_eq!(Done(&b""[..], Token::Move(b"Nf6+")), san_move_token(b"Nf6+"));
        assert_eq!(Done(&b""[..], Token::Move(b"N2f6")), san_move_token(b"N2f6"));
        assert_eq!(Done(&b""[..], Token::Move(b"N2f6+")), san_move_token(b"N2f6+"));
        assert_eq!(Done(&b""[..], Token::Move(b"Ng1f3")), san_move_token(b"Ng1f3"));
        assert_eq!(Done(&b""[..], Token::Move(b"Ng1f3+")), san_move_token(b"Ng1f3+"));
        assert_eq!(Done(&b""[..], Token::Move(b"Rd3d1")), san_move_token(b"Rd3d1"));
        assert_eq!(Done(&b""[..], Token::Move(b"Rd3d1+")), san_move_token(b"Rd3d1+"));
        assert_eq!(Done(&b""[..], Token::Move(b"Rd3d1#")), san_move_token(b"Rd3d1#"));
    }
    #[test]
    fn test_san_move_piece_capture() {
        assert_eq!(Done(&b""[..], Token::Move(b"Nxf3")), san_move_token(b"Nxf3"));
        assert_eq!(Done(&b""[..], Token::Move(b"Nxf6")), san_move_token(b"Nxf6"));
        assert_eq!(Done(&b""[..], Token::Move(b"Nxf6+")), san_move_token(b"Nxf6+"));
        assert_eq!(Done(&b""[..], Token::Move(b"N2xf6")), san_move_token(b"N2xf6"));
        assert_eq!(Done(&b""[..], Token::Move(b"N2xf6+")), san_move_token(b"N2xf6+"));
        assert_eq!(Done(&b""[..], Token::Move(b"Ng1xf3")), san_move_token(b"Ng1xf3"));
        assert_eq!(Done(&b""[..], Token::Move(b"Ng1xf3+")), san_move_token(b"Ng1xf3+"));
        assert_eq!(Done(&b""[..], Token::Move(b"Rd3xd1")), san_move_token(b"Rd3xd1"));
        assert_eq!(Done(&b""[..], Token::Move(b"Rd3xd1+")), san_move_token(b"Rd3xd1+"));
        assert_eq!(Done(&b""[..], Token::Move(b"Rd3xd1#")), san_move_token(b"Rd3xd1#"));
    }

    #[test]
    fn test_castles() {
        assert_eq!(Done(&b""[..], Token::Move(b"O-O")), san_move_token(b"O-O"));
        assert_eq!(Done(&b""[..], Token::Move(b"O-O-O")), san_move_token(b"O-O-O"));
        assert_eq!(Done(&b""[..], Token::Move(b"O-O+")), san_move_token(b"O-O+"));
        assert_eq!(Done(&b""[..], Token::Move(b"O-O#")), san_move_token(b"O-O#"));
        assert_eq!(Done(&b""[..], Token::Move(b"O-O-O+")), san_move_token(b"O-O-O+"));
        assert_eq!(Done(&b""[..], Token::Move(b"O-O-O#")), san_move_token(b"O-O-O#"));
    }
    #[test]
    fn test_null_move() {
        assert_eq!(Done(&b""[..], Token::NullMove(b"--")), san_move_token(b"--"));
        assert_eq!(Done(&b""[..], Token::NullMove(b"--+")), san_move_token(b"--+"));
        assert_eq!(Done(&b""[..], Token::NullMove(b"--#")), san_move_token(b"--#"));
        assert_eq!(Done(&b""[..], Token::NullMove(b"Z0")), san_move_token(b"Z0"));
        assert_eq!(Done(&b""[..], Token::NullMove(b"Z0+")), san_move_token(b"Z0+"));
        assert_eq!(Done(&b""[..], Token::NullMove(b"Z0#")), san_move_token(b"Z0#"));
    }
    #[bench]
    fn bench_parse_san_move_null(b: &mut Bencher) {
        b.iter(|| {
            assert_eq!(Done(&b""[..], Token::NullMove(b"--#")), san_move_token(b"--#"));
        });
    }
    #[bench]
    fn bench_parse_san_move_castle_queen_side(b: &mut Bencher) {
        b.iter(|| {
            assert_eq!(Done(&b""[..], Token::Move(b"O-O-O")), san_move_token(b"O-O-O"));
        });
    }
    #[bench]
    fn bench_parse_san_move_castle_king_side(b: &mut Bencher) {
        b.iter(|| {
            assert_eq!(Done(&b""[..], Token::Move(b"O-O")), san_move_token(b"O-O"));
        });
    }
    #[bench]
    fn bench_parse_san_move_simple_capture(b: &mut Bencher) {
        b.iter(|| {
            assert_eq!(Done(&b""[..], Token::Move(b"bxc2")), san_move_token(b"bxc2"));
        });
    }
    #[bench]
    fn bench_parse_san_move_simple(b: &mut Bencher) {
        b.iter(|| {
            assert_eq!(Done(&b""[..], Token::Move(b"e4")), san_move_token(b"e4"));
        });
    }
    #[bench]
    fn bench_parse_san_capture_promotion(b: &mut Bencher) {
        b.iter(|| {
            assert_eq!(Done(&b""[..], Token::Move(b"bxc1=R")), san_move_token(b"bxc1=R"));
        });
    }
    #[bench]
    fn bench_parse_san_move_complicated(b: &mut Bencher) {
        b.iter(|| {
            assert_eq!(Done(&b""[..], Token::Move(b"bxc1=R+")), san_move_token(b"bxc1=R+"));
        });
    }

    #[test]
    fn test_pgn_integer() {
        assert_eq!(Done(&b""[..], &b"99"[..]), pgn_integer(b"99"));
        assert_eq!(Done(&b" e4"[..], &b"99"[..]), pgn_integer(b"99 e4"));
        assert_eq!(Done(&b"..."[..], &b"99"[..]), pgn_integer(b"99..."));
        assert_eq!(IResult::Error(ErrorKind::Custom(PGN_INTEGER_EMPTY)), pgn_integer(b""));
    }

    #[test]
    fn test_pgn_string_token() {
        assert_eq!(IResult::Error(ErrorKind::Custom(PGN_STRING_EMPTY)), pgn_string(b""));
        assert_eq!(Done(&b""[..], &b"aaaaaaa"[..]), pgn_string(b"\"aaaaaaa\""));
        assert_eq!(Done(&b""[..], &b"aaaaaaa \\\" aaaaaaa"[..]), pgn_string(b"\"aaaaaaa \\\" aaaaaaa\""));
        assert_eq!(Done(&b""[..], &b"GER/CCM-E/01-C (GER)"[..]), pgn_string(b"\"GER/CCM-E/01-C (GER)\""));
    }

    #[test]
    fn test_pgn_escape_comment_token() {
        assert_eq!(IResult::Error(ErrorKind::Custom(PGN_ESCAPE_COMMENT_EMPTY)), pgn_escape_comment_token(b""));
        assert_eq!(Done(&b"\n"[..], Token::EscapeComment(b"1234")), pgn_escape_comment_token(b"%1234\n"));
        assert_eq!(Done(&b"\n"[..], Token::EscapeComment(b"%234")), pgn_escape_comment_token(b"%%234\n"));
        assert_eq!(Done(&b"\r"[..], Token::EscapeComment(b"% 234")), pgn_escape_comment_token(b"%% 234\r"));
    }

    #[test]
    fn test_pgn_nag() {
        assert_eq!(IResult::Error(ErrorKind::Custom(PGN_NAG_EMPTY)), pgn_nag_token(b""));
        assert_eq!(Done(&b""[..], Token::NAG(b"1234")), pgn_nag_token(b"$1234"));
        assert_eq!(Done(&b""[..], Token::NAG(b"234")), pgn_nag_token(b"$234"));
        assert_eq!(Done(&b" e4"[..], Token::NAG(b"234")), pgn_nag_token(b"$234 e4"));
    }

    #[test]
    fn test_pgn_symbol_token() {
        assert_eq!(IResult::Error(ErrorKind::Custom(PGN_SYMBOL_EMPTY)), pgn_symbol(b""));
        assert_eq!(Done(&b""[..], &b"sasd#_+#=:-"[..]), pgn_symbol(b"sasd#_+#=:-"));
        assert_eq!(Done(&b"!()~{}[]"[..], &b"sasd#_+#=:-"[..]), pgn_symbol(b"sasd#_+#=:-!()~{}[]"));
    }
    #[test]
    fn test_pgn_game_result_token() {
        assert_eq!(IResult::Error(ErrorKind::Custom(PGN_GAME_RESULT_EMPTY)), pgn_game_result_token(b""));
        assert_eq!(Done(&b""[..], Token::Result(b"1-0")), pgn_game_result_token(b"1-0"));
        assert_eq!(Done(&b""[..], Token::Result(b"0-1")), pgn_game_result_token(b"0-1"));
        assert_eq!(Done(&b""[..], Token::Result(b"1/2-1/2")), pgn_game_result_token(b"1/2-1/2"));
        assert_eq!(Done(&b""[..], Token::Result(b"*")), pgn_game_result_token(b"*"));
    }
    #[test]
    fn test_pgn_commentary_token() {
        assert_eq!(IResult::Error(ErrorKind::Custom(PGN_COMMENTARY_EMPTY)), pgn_commentary_token(b""));
        assert_eq!(Done(&b""[..], Token::Commentary(b"this is a comment")), pgn_commentary_token(b"{this is a comment}"));
        assert_eq!(Done(&b""[..], Token::Commentary(b"this is a\n comment")), pgn_commentary_token(b"{this is a\n comment}"));
    }
    #[test]
    fn test_pgn_tag_symbol_token() {
        assert_eq!(IResult::Error(ErrorKind::Custom(PGN_TAG_PAIR_EMPTY)), pgn_tag_symbol_token(b""));
        assert_eq!(Done(&b" \"?\"]"[..], Token::TagSymbol(b"Event")), pgn_tag_symbol_token(b"[Event \"?\"]"));
        assert_eq!(Done(&b" \"Tony Rotella\"]"[..], Token::TagSymbol(b"Event")), pgn_tag_symbol_token(b"[Event \"Tony Rotella\"]"));
    }
    #[test]
    fn test_pgn_tag_string_token() {
        assert_eq!(IResult::Error(ErrorKind::Custom(PGN_TAG_PAIR_EMPTY)), pgn_tag_string_token(b""));
        assert_eq!(Done(&b""[..], Token::TagString(b"?")), pgn_tag_string_token(b"\"?\"]"));
        assert_eq!(Done(&b""[..], Token::TagString(b"Tony Rotella")), pgn_tag_string_token(b"\"Tony Rotella\"]"));
        assert_eq!(Done(&b""[..], Token::TagString(b"2016.05.22")), pgn_tag_string_token(b"\"2016.05.22\"]"));
    }
    #[test]
    fn test_pgn_move_number_token() {
        assert_eq!(IResult::Error(ErrorKind::Custom(PGN_MOVE_NUMBER_EMPTY)), pgn_move_number(b""));
        assert_eq!(Done(&b""[..], &b"1"[..]), pgn_move_number(b"1"));
        assert_eq!(Done(&b""[..], &b"2."[..]), pgn_move_number(b"2."));
        assert_eq!(Done(&b""[..], &b"49..."[..]), pgn_move_number(b"49..."));
        assert_eq!(Done(&b"."[..], &b"3..."[..]), pgn_move_number(b"3...."));
        assert_eq!(Done(&b"."[..], &b"3 ..."[..]), pgn_move_number(b"3 ...."));
    }
    #[test]
    fn test_pgn_start_variation_token() {
        assert_eq!(IResult::Error(ErrorKind::Custom(PGN_START_VARIATION_EMPTY)), pgn_start_variation_token(b""));
        assert_eq!(Done(&b""[..], Token::StartVariation(b"(")), pgn_start_variation_token(b"("));
        assert_eq!(Done(&b" 1."[..], Token::StartVariation(b"(")), pgn_start_variation_token(b"( 1."));
    }
    #[test]
    fn test_pgn_end_variation() {
        assert_eq!(IResult::Error(ErrorKind::Custom(PGN_END_VARIATION_EMPTY)), pgn_end_variation_token(b""));
        assert_eq!(Done(&b""[..], Token::EndVariation(b")")), pgn_end_variation_token(b")"));
        assert_eq!(Done(&b" 1."[..], Token::EndVariation(b")")), pgn_end_variation_token(b") 1."));
    }
    #[test]
    fn test_pgn_game_parser() {
        let results = PGNTokenIterator{bytes: &b"[Event \"World Senior Teams +50\"]
[Site \"Radebeul GER\"]
[Date \"2016.07.03\"]
[Round \"8.2\"]
[White \"Anastasian, A.\"]
[Black \"Lewis, An\"]
[Result \"1-0\"]
[ECO \"E90\"]
[WhiteElo \"2532\"]
[BlackElo \"2269\"]
[PlyCount \"84\"]
[EventDate \"2016.06.26\"]

1. d4 Nf6 2. c4 g6 3. Nc3 Bg7 4. e4 d6 5. Nf3 O-O 6. h3 e5 7. d5 Na6 8. Be3 Nh5
9. Nh2 Qe8 10. Be2 Nf4 11. Bf3 f5 12. a3 Nc5 13. Bxc5 dxc5 14. O-O Qe7 15. Re1
a6 16. Ne2 Qd6 17. Nf1 Bd7 18. Rb1 b6 19. Nd2 Bh6 20. Nxf4 Bxf4 21. b4 Rae8 22.
Qc2 Rf6 23. Qc3 Qf8 24. Nb3 cxb4 25. axb4 Bg5 26. Rb2 Rf7 27. Nc1 Qh6 28. Nd3
fxe4 29. Bxe4 Bxh3 30. gxh3 Qxh3 31. Bg2 Qh4 32. Re4 Qh5 33. Rbe2 Ref8 34. c5
Bf4 35. Nxe5 Qh2+ 36. Kf1 Rf5 37. Nf3 Qh5 38. Re7 Bh6 39. R2e5 bxc5 40. bxc5
Rxf3 41. Bxf3 Z0 42. Ke1 Qh1+ 1-0"[..]};
        let results: Vec<Token> = results.collect();
        println!("[{:?}]", results[results.len()-1]);
        println!("[{:?}]", results[results.len()-2]);
        // 24 tag tokens
        // 42 full moves (84 tokens)
        // 1 result
        assert_eq!(results.len(), 24+84+1);
        assert_eq!(results[0], Token::TagSymbol(b"Event"));
        assert_eq!(results[1], Token::TagString(b"World Senior Teams +50"));
        assert_eq!(results[2], Token::TagSymbol(b"Site"));
        assert_eq!(results[3], Token::TagString(b"Radebeul GER"));
        assert_eq!(results[4], Token::TagSymbol(b"Date"));
        assert_eq!(results[5], Token::TagString(b"2016.07.03"));
        assert_eq!(results[6], Token::TagSymbol(b"Round"));
        assert_eq!(results[7], Token::TagString(b"8.2"));

        let last = results.len()-1;
        assert_eq!(results[last], Token::Result(b"1-0"));
        assert_eq!(results[last-1], Token::Move(b"Qh1+"));
        assert_eq!(results[last-2], Token::Move(b"Ke1"));
        assert_eq!(results[last-3], Token::NullMove(b"Z0"));
    }
    #[bench]
    fn bench_parse_game(b: &mut Bencher) {
        b.iter(|| {
            let results = PGNTokenIterator{bytes: &b"[Event \"World Senior Teams +50\"]
    [Site \"Radebeul GER\"]
    [Date \"2016.07.03\"]
    [Round \"8.2\"]
    [White \"Anastasian, A.\"]
    [Black \"Lewis, An\"]
    [Result \"1-0\"]
    [ECO \"E90\"]
    [WhiteElo \"2532\"]
    [BlackElo \"2269\"]
    [PlyCount \"84\"]
    [EventDate \"2016.06.26\"]

    1. d4 Nf6 2. c4 g6 3. Nc3 Bg7 4. e4 d6 5. Nf3 O-O 6. h3 e5 7. d5 Na6 8. Be3 Nh5
    9. Nh2 Qe8 10. Be2 Nf4 11. Bf3 f5 12. a3 Nc5 13. Bxc5 dxc5 14. O-O Qe7 15. Re1
    a6 16. Ne2 Qd6 17. Nf1 Bd7 18. Rb1 b6 19. Nd2 Bh6 20. Nxf4 Bxf4 21. b4 Rae8 22.
    Qc2 Rf6 23. Qc3 Qf8 24. Nb3 cxb4 25. axb4 Bg5 26. Rb2 Rf7 27. Nc1 Qh6 28. Nd3
    fxe4 29. Bxe4 Bxh3 30. gxh3 Qxh3 31. Bg2 Qh4 32. Re4 Qh5 33. Rbe2 Ref8 34. c5
    Bf4 35. Nxe5 Qh2+ 36. Kf1 Rf5 37. Nf3 Qh5 38. Re7 Bh6 39. R2e5 bxc5 40. bxc5
    Rxf3 41. Bxf3 Z0 42. Ke1 Qh1+ 1-0"[..]};
            // 24 tag tokens
            // 42 full moves (84 tokens)
            // 1 result
            assert_eq!(results.count(), 24+84+1);
        });
    }
}
