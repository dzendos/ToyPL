## Info

This repository contains:

1. Translator from Toy Programming Language to TPL-VM.
2. Interpreter for resulting program on TPL-VM.

For simplicity these two parts are connected in one program.

## Building project

To build the project you can use `stack` or `cabal`.

## How to use

You need to provide path to program written on TPL language (some examples you can find [here](https://github.com/dzendos/ToyPL/tree/main/examples))

After that if the given program is correct, it will print translated into TPL-VM program.

Execute it using `step` and `run` to execute one line of code and program till the end respectively.
