# PGN Tokenizer
## Goals
- Use zero-copy byte slices all the way through.
- No heap allocations

## API
- A token based iterator that takes a byte slices and returns tokens that reference sub-slices


## Example

This [example](https://github.com/lakinwecker/pgn-lexer/blob/master/examples/count.rs) counts the number of games in a PGN file.

