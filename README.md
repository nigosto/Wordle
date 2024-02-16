# Wordle
Console app written in Haskell that simulates the famous game Wordle and extending it with different game modes. This is my course project for the Functional Programming course at Sofia University.

## Table of contents
- [Prerequisites](#prerequisites)
- [Running the project](#running-the-project)
- [Installing the executable manually](#installing-the-executable-manually)
- [Documentation](#documentation)
- [License](#license)
- [Contacts](#contacts)

## Prerequisites
- ghc 9.4.7 or newer
- cabal 3.6.2.1 or newer

## Running the project
Running the project using `cabal`:
```shell
cabal run
```

If you want to use custom word list, you need to change the contents of `assets/wordlist.txt`.

## Installing the executable manually
If you want to install the executable yourselves, then you need to run the following command:
```shell
cabal install
```

Now the executable should be located in the configured `installdir` of `cabal` (usually `~/.local/bin` or `~/.cabal/bin`).

## Documentation
TODO

## License
MIT License

Copyright (c) 2024 Georgi Atanasov

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

## Contacts
- GitHub: https://github.com/nigosto
- LinkedIn: https://www.linkedin.com/in/georgi-n-atanasov
- email: nigosto@gmail.com