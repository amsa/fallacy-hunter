# Fallacy Hunter

This program aims to detect the fallacies in a given set of sentences. Fallacies always require premises and conclusions in the given sentences, see [Examples](https://github.com/gekonwi/brandeis.semantics.final_project#examples) below.

## Installation

#### 1. Install Cabal

Cabal is package installer for Haskell. https://www.haskell.org/cabal/

#### 2. Install Packages
```
cd fallacy-hunter
cabal sandbox init  (optional) 
cabal install 
```

## Running in GHCI
1. Start GHCI:
	```
	$ ghci
	```

2. Load the entry module:
	```haskell
	Prelude> :l Fallacy/Main.hs
	```

3. Start the fallacy detection loop:
	```haskell
	*Main> main
	```

4. You will see the following output:
	```
	Instructions:
	1. Enter sentences without quotes.
	2. Hit Enter to start fallacy detection.
	3. Repeat step 2 and 3 as you wish.
	4. Press Ctrl+C to exit.
	
	> 
	```

## Running Web Application
In your terminal try:
```
$ fallacy-hunter
```
or if you installed it in a sandbox try running:
```
$ ./.cabal-sandbox/bin/fallacy-hunter
```
Now go to [http://localhost:8000](http://localhost:8000) from your browser.

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
(((b ∨ a) ∧ b) → ¬a)

Found fallacies:
         Type => Affirming the disjunct
         Logical Form => (((b ∨ a) ∧ b) → ¬a)


---------------------------------------------------
```
### Affirming the consequent
```
> If Bill Gates owns Fort Knox, then Bill Gates is rich. Bill Gates is rich. Therefore, Bill Gates owns Fort Knox.

Input in logical form:
(((a → b) ∧ b) → a)

Found fallacies:
     Type => Affirming the consequent
     Logical Form => (((a → b) ∧ b) → a)


--------------------------------------------------
```
### Denying the antecedent
```
> If Queen Elizabeth is an American Citizen, then Queen Elizabeth is a human being. Queen Elizabeth is not an American Citizen. Therefore, Queen Elizabeth is not a human being.

Input in logical form:
(((b → a) ∧ ¬b) → ¬a)

Found fallacies:
     Type => Denying the antecedent
     Logical Form => (((b → a) ∧ ¬b) → ¬a)


---------------------------------------------------
                  
