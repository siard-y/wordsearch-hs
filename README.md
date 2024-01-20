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

### Input
Two files:
<table>
<tr><td> <code>grid.txt</code> </td> <td> <code>words.txt</code> </td><td>Should output</td></tr>
<tr>
<td>

```
KVTZOE
NIRBRL
IPRLAP
PEWANR
DIZCGU
VCPKEP
```

</td>
<td>

```
PURPLE
PINK
BLACK
ORANGE
RED
```

</td>
<td>

```
PURPLE at [(5,5),(5,4),(5,3),(5,2),(5,1),(5,0)]
PINK at [(0,3),(0,2),(0,1),(0,0)]
BLACK at [(3,1),(3,2),(3,3),(3,4),(3,5)]
ORANGE at [(4,0),(4,1),(4,2),(4,3),(4,4),(4,5)]
RED at [(2,2),(1,3),(0,4)]
```

</td>
</table>

Note that the coordinates are in `(x,y)` format, and start from 0. So `(second letter, third row)` = `(1,2)`.

## License

This project is licensed under the GPL-3.0 license.