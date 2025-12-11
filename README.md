# Advent of Code 2025 in Haskell

This repository has my solutions to the 2025 Advent of Code in Haskell. It
is based on Luis Morillo's [template-advent-of-code][template-aoc], which
provides a simple CLI tool and VSCode tasks to streamline your Advent of Code
experience in Haskell, without the need to learn `cabal` and with a sensible
set of packages.

## Template Features

- A sensible set of dependencies for AoC.
- A bash command-line interface (CLI) that allows you to:
  - Create Haskell files for each new Advent of Code day.
  - Run solutions for each day with ease.
  - Automatically manage Cabal files, reducing setup hassles.
- VSCode tasks for seamless interaction with the CLI.

## CLI Usage

Here's how to use the CLI for common tasks:

- Create a new solution for a specific day:

  ```bash
  ./aoc-hs new -d <day>
  ```

- Run a solution for a specific day and part:

  ```bash
  ./aoc-hs run -d <day> -p <part> --input
  ```

Below there is the complete description of the CLI (you can run `./aoc-hs --help` to get it in you local machine):

```bash
Usage: aoc-hs [new -d <day> [--no-curl] | run -d <day> -p <part> [-f <file-name> | --example | -e | --input | -i]]

Description:
  This tool simplifies Advent of Code solutions in Haskell by creating templates and handling input files. No need to learn Cabal!

Subcommand: new

Create a new Advent of Code solution for the specified day. It creates a main module, modifies the .cabal file, and downloads the input data.

Usage: aoc-hs new -d <day>
Example: aoc-hs new -d 3
         aoc-hs new -d 3 --no-curl
Options:
  -d <day>       Specify the day for the Advent of Code puzzle (1-12).
  --no-curl      It wont download your personal AoC input file. You don't have you provide a cookie with this option

Subcommand: run

Run an Advent of Code solution for the specified day and part. The input data is read from a file which can be supplied via -f or you can
use shortcuts --example and --input. Default --input

Usage: aoc-hs run -d <day> -p <part> [-f <file-name> | --example | -e | --input | -i]
Example: aoc-hs run -d 3 -p 2 --example
         aoc-hs run -d 3 -p 3 -e
         aoc-hs run -d 3 -p 2 --input
         aoc-hs run -d 3 -p 2 -i
         aoc-hs run -d 3 -p 2 -f my-input-file.txt
Options:
  -d <day>       Specify the day for the Advent of Code puzzle (1-25).
  -p <part>      Specify the part of the puzzle (1 or 2).
  -f <file-name> Specify a custom input file to use.
  --example, -e  Use the example input file (./inputs/day-<day>.example) as input.
  --input, -i    (Default) Use the puzzle input file (./inputs/day-<day>.input) as input.
```

## Haskell details

After running `./aoc-hs new -d x`, you'll find files `day-x.hs` in the
`solutions` folder. Such files should contain the Haskell code for that day.
Each day is built as an stand alone executable, so code isn't shared between
two days. In practise, this is not a problem since it is rare that AoC days
require code sharing. Each executable depends on these libraries:

- [attoparsec][attoparsec]: for parsing inputs
- [base >=4.7 && <5][base]: the base package
- [bytestring][bytestring]: bytestring is the string format used by attoparsec
- [containers][containers]: containers for generic containers like Map, Set, etc...
- [matrix][matrix]: because AoC love two-dimensional discrete problems
- [vector][vector]: `Int` based array. Oftenly a replacement for Haskell's Lists
- [split][split]: algorithms to split lists
- [search-algorithms][search-algorithms]: a blessed interface to bfs, dfs, dijkstra, etc...
- [mtl][mtl]: Just in case you need the state monad
- [pointedlist][pointedlist]: because AoC loves circual arrays.
- [bytestring-trie][bytestring-trie]: bytestring based Trie for searching string problems.

The dependencies are chosen to match a typical AoC season.

With Respect to `hls`, there is an explicit `hie.yaml` just in case. I've had
problems in the past with [`common` stanzas][common-stanzas].

## License

This project is licensed under the GPL v3 license - see the [LICENSE](LICENSE)
file for details.

<!-- Markdown links and images -->
[template-aoc]: https://github.com/lsmor/template-advent-of-code
[attoparsec]: https://hackage.haskell.org/package/attoparsec
[base]: https://hackage.haskell.org/package/base
[bytestring]: https://hackage.haskell.org/package/bytestring
[containers]: https://hackage.haskell.org/package/containers
[matrix]: https://hackage.haskell.org/package/matrix
[vector]: https://hackage.haskell.org/package/vector
[split]: https://hackage.haskell.org/package/split
[search-algorithms]: https://hackage.haskell.org/package/search-algorithms
[mtl]: https://hackage.haskell.org/package/mtl
[pointedlist]: https://hackage.haskell.org/package/pointedlist
[bytestring-trie]: https://hackage.haskell.org/package/bytestring-trie-0.2.7.2
[common-stanzas]: https://github.com/Avi-D-coder/implicit-hie/issues/1
