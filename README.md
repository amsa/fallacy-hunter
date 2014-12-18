# About

TODO


# Installation

### 1. Install Cabal

In case you didn't already. All infos can be found here: https://www.haskell.org/cabal/


### 2. Install Packages
```
cabal install hatt-1.5.0.3
cabal install stemmer-0.5
cabal install chatter-0.5.0.0

```


# Running

1. Change into the `src` directory of this repo

2. Start GHCI:
 ```
 $ ghci
 ```

3. Load our entry file:
 ```haskell
 Prelude> :l Fallacy/TextToFOL.hs
 ```

4. Start the fallacy detection loop:
 ```haskell
 *Main> main
 ```

5. Enter any sentence and hit `Enter` to start fallacy detectoin. The result will be printed on the console.