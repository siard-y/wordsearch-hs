# Word Searcher

Find words in a grid of letters.

## Description

Haskell program that takes a grid and a list of words, and outputs the location of the found words.

## Getting Started

### Dependencies

- Just Cabal, no Stack
- `optparse-applicative` is the only external library

### Build

```
cabal build
```

### Run

```
cabal run wordsearch-hs -- -g <grid_file.txt> -w <words_file.txt>
```

or find the executable with `cabal list-bin wordsearch-hs` and

```
./wordsearch-hs -g <grid_file.txt> -w <words_file.txt>
```

## License

This project is licensed under the GPL-3.0 license.