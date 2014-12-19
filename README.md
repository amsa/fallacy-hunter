# Fallacy Hunter

This program aims to detect the fallacies in a given set of sentences. It is assumed that these sentences comprise of few premises and a conclusion.

## External Packages
- [hatt](http://hackage.haskell.org/package/hatt-1.5.0.3)
- [chatter](http://hackage.haskell.org/package/chatter-0.5.0.0)
- [stemmer](https://hackage.haskell.org/package/stemmer-0.5/docs/NLP-Stemmer.html)

## Installation

#### 1. Install Cabal

Cabal is package installer for Haskell. https://www.haskell.org/cabal/

#### 2. Install Packages
```
cabal install hatt-1.5.0.3
cabal install stemmer-0.5
cabal install chatter-0.5.0.0
```

## Running

1. Change into the `src` directory of this repo

2. Start GHCI:
	```
	$ ghci
	```

3. Load the entry module:
	```haskell
	Prelude> :l Fallacy/Main.hs
	```

4. Start the fallacy detection loop:
	```haskell
	*Main> main
	```

5. You will see the following output:
	```
	Instructions:
	1. Enter sentences without quotes.
	2. Hit Enter to start fallacy detection.
	3. Repeat step 2 and 3 as you wish.
	4. Press Ctrl+C to exit.
	
	> 
	```


## Testing

1. Follow steps 1 and 2 from _Running_

2. Load the unit tests:
	```haskell
	Prelude> :l Fallacy/DetectorTest.hs
	```

3. Run all tests:
	```haskell
	Prelude> runTestTT tests
	```


## Examples

### Affirming a disjunct
```
> Max is a cat or Max is a mammal. Max is a cat. Therefore, Max is not a mammal.


Input in logical form:
(((a ∨ c) ∧ (c ∧ b)) → ¬a)

Found fallacies:
     Type => Affirming a Disjunct
     Logical Form => (((c ∨ a) ∧ c) → ¬a)
```
### Affirming the consequent
```
> kkk

### Denying the antecedent
