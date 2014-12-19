-- our module converting free-text into FOL expressions
import Fallacy.TextToLogical

-- our module detecting fallacies given FOL expressions
import Fallacy.Detector

import Control.Monad (forever)

import qualified NLP.POS as POS

main = forever $ do  
	putStrLn $ "\nInstructions:\n" ++
		"1. Enter sentences without quotes.\n" ++
		"2. Hit Enter to start fallacy detection.\n"++
		"3. Repeat step 2 and 3 as you wish.\n"++
		"4. Press Ctrl+C to exit.\n"
	putStr "> "
	
	input <- getLine
	
	tagger <- POS.defaultTagger
	
	let
		inputAsFOL = toLogicalForm input tagger
		result = findFallacies inputAsFOL

	putStrLn $ "\nInput in logical form:\n" ++ (show inputAsFOL)

	putStrLn "\nFound fallacies:"
	mapM_ print result

	putStrLn $ "\n---------------------------------------------------\n"
