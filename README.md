# Fallacy Hunter

TODO: short description


## Installing

#### 1. Install Cabal

In case you didn't already. All infos can be found here: https://www.haskell.org/cabal/


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

3. Load our entry file:
	```haskell
	Prelude> :l Fallacy/TextToFOL.hs
	```

4. Start the fallacy detection loop:
	```haskell
	*Fallacy.TextToFOL> main
	```

5. You should see the following:
	```
	*Fallacy.TextToFOL> main
	
	Instructions:
	1. Enter sentences without quotes.
	2. Hit Enter to start fallacy detection.
	3. Repeat step 2 and 3 as you wish.
	4. Press Ctrl+C to exit.
	> 
	```

6. Follow the instructions printed in the console.