# PGN Tokenizer

An attempt to make a PGN file tokenizer. This library will do no parsing. It will simply stream a file and produce a stream of tokens. Each token will be labelled as specifically as possible, but will contain only a byte-slice for the incoming data.

What you do with that data is up to you.
