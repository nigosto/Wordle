# Wordle
Console app written in Haskell that simulates the famous game Wordle and extends it with different game modes.

## Table of contents
- [Prerequisites](#prerequisites)
- [Running the project](#running-the-project)
- [Installing the executable manually](#installing-the-executable-manually)
- [How to Play](#how-to-play)
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

Or if you want to run it in REPL:
```shell
cabal repl
```

## Installing the executable manually
If you want to install the executable yourselves, then you need to run the following command:
```shell
cabal install
```

Now the executable should be located in the configured `installdir` of `cabal` (usually `~/.local/bin` or `~/.cabal/bin`).

## How to Play
### Game Setup
The game begins by asking the player for the path to the file, containing the word list. There is not specific format for the word list - it should contain the words separated by any whitespaces or new-line characters. The game is case sensitive, so beware when creating your word list.

After providing the path to the wordlist, the user is prompted to choose the game mode and the corresponding difficulty. They must be all lower case, except the first letter, which may be capital as well.

Finally, the game asks for the length of words with which the user wants to play. There is no limit for the word length, as long as there are words in the list with the provided length (compared to the original version of **Wordle**, which is played with only 5-letter words).

### Game Modes
There are currently 2 available game modes - **Classic** and **Guesser**. The **Classic** mode represents the original version of **Wordle**, where the player tries to guess the secret word. The **Guesser** mode swaps the roles of the player and the game - the player thinks of a secret word and the game tries to guess it, while the player responds with the corresponding letter colors after each guess.

#### Classic Mode
In Classic mode, the player is asked to provide its guess every turn. After providing it, the game responds with the list of the colors of the letters, where **Green** means that the letter is in its correct position, **Yellow** means that the letter is part of the word, but is not in its correct position and **Gray** means that the letter is not part of the word. Classic mode has 3 difficulties - **Easy**, **Normal** and **Hard**:
- **Easy mode** provides the player with helpfull messages after every guess. Their intent is to help the player to take better guesses and remind him of what has been found so far. The messages are displayed for every of the following situations:
    - if the player has used a letter, that was found as gray;
    - if the player has not used a letter that was found as yellow;
    - if the player has not used a letter that was found as green or has used it, but not in the correct position;
- **Normal mode** is the original version of **Wordle** - there are no helping messages, nor any additional rules; 
- **Hard mode** extends the original version by allowing the game to **lie** a single time during the session. There is 20% chance the colors, diplayed after the player's guess, to be incorrect untill the game lies to the player. If the player uses letters that have been used already and their color is already known, when the game lies, their colors will be unchanged so that it is harder to notice when it happened. The rest of the colors are chosen at random;

#### Guesser Mode
In Guesser mode, the player thinks of a secret word and has to provide the colors of the letters after each of the game's guesses. The format of the colors is the following:
- the colors should be separated by any number of white spaces;
- the colors should be all lower case, except for the first letter, which may be capital. For convenience, in order not to type the whole names of the colors the player may uses abbreviations: `gn` for green, `y` for yellow and `gy` for gray;

If the player's answers are ambiguous or the secret word is not part of the word list, the game will end automatically. Guesser mode has 2 diffuculties - **Normal** and **Hard**:
- **Normal mode** does not introduce any new rules and the game is played by the above rules only;
- **Hard mode** allows the player to **lie** once to the game about the colors of the letters of its guess. When the player lies, it is not required to provide the same colors for the letters, that have been identified already, compared to the Classic hard mode. If the player lies more than once, then the answer is considered ambiguous and the game ends.

## License
The project is licensed under the MIT License. See the [LICENSE](./LICENSE) file for more information.

## Contacts
- GitHub: https://github.com/nigosto
- LinkedIn: https://www.linkedin.com/in/georgi-n-atanasov
- email: georgi.n.atanasov02@gmail.com