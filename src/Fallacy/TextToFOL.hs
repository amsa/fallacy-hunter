module Fallacy.TextToFOL where

-- our module detecting fallacies given FOL expressions
import Fallacy.DetectInFOL

import Data.Logic.Propositional

import Control.Monad  
import qualified Data.Map as M
import qualified Data.List.Split as Split

import qualified Data.Text as T
import qualified Data.Char as Char 
import qualified NLP.POS as POS
import qualified NLP.Types.Tree as Types
import NLP.Corpora.Conll (Tag)
import NLP.Stemmer

data Formula = Nil 
			| Sentence String
			| Neg Formula
			| Conj Formula Formula
			| Disj Formula Formula
			| Cond Formula Formula
			deriving (Eq)

instance Show Formula where 
		show (Sentence s) = "\"" ++ s ++ "\""
		show (Neg f) = "NOT " ++ show(f)
		show (Conj l r) = "(" ++ show(l) ++ ") AND (" ++ show(r) ++ ")"
		show (Disj l r) = "(" ++ show(l) ++ ") OR (" ++ show(r) ++ ")"
		show (Cond l r) = "(" ++ show(l) ++ ") => (" ++ show(r)  ++ ")"
		show _ = ""

sentStr (Sentence x) = x
sentWords (Sentence x) = words x

puncList, conclusionWordList, stopWords :: [String]
puncList = [",", ".", ";", ":"]
conclusionWordList = ["therefore", "so", "hence"]
stopWords = ["do", "does", "a", "an", "the", "of", "to"]

main = forever $ do  
	putStrLn $ "\nInstructions:\n" ++
		"1. Enter sentences without quotes.\n" ++
		"2. Hit Enter to start fallacy detection.\n"++
		"3. Repeat step 2 and 3 as you wish.\n"++
		"4. Press Ctrl+C to exit."
	putStr "> "
	l <- getLine
	res <- findFallaciesInSentence l
	putStrLn $ "Found fallacies:\n" ++ show(res)

{- tagString
- parses the input string, and handles POS tagging
-
- parameters:
-   String: the string that we are trying to detect containing fallacies
-   (if any)
-}

tagString :: String -> IO [Types.TaggedSentence NLP.Corpora.Conll.Tag]
tagString input = do 
	tagger <- POS.defaultTagger
	return (POS.tag tagger (T.pack input))

tagStringTuple :: String -> IO [[(String, String)]]
tagStringTuple input = do
	taggedSents <- tagString input
	let
		posList = [Types.unTS x | x <- taggedSents] 
		tags = map (\s -> 
					 foldl (\acc p -> 
						 let
						 	tok = T.unpack $ Types.showPOStok p
							pos = T.unpack $ Types.showPOStag p
							in acc ++ [(toLower tok, pos)]) [] s
					) posList
		in return (tags)

toLower :: String -> String
toLower word = map (\c -> Char.toLower c) word

setSentBoundaries :: String -> String
setSentBoundaries sentence = unwords $ map (\w -> case w of 
							 "and" -> "."
							 "," -> "."
							 _ -> w) $ words sentence


stemString :: String -> String
stemString input = 
		let filtered = filter (\s -> (toLower s) `notElem` stopWords) $ words input 
			in unwords $ map (stem English) $ filtered

removePunc :: [(String, b)] -> [(String, b)]
removePunc = foldr (\tuple acc -> if (fst tuple) `elem` puncList then acc else tuple:acc) []

removeConclusionWords :: [(String, b)] -> [(String, b)]
removeConclusionWords = foldr (\tuple acc -> if (fst tuple) `elem` conclusionWordList then acc else tuple:acc) []

extractPremiseConclusion :: [([Char], b)] -> ([([Char], b)], [([Char], b)])
extractPremiseConclusion = span (\e -> (fst e) `notElem` conclusionWordList) 

extractPremiseConclusionAll :: String -> IO ([[(String, String)]], [[(String, String)]])
extractPremiseConclusionAll sentence = do 
	tagged <- tagStringTuple (setSentBoundaries $ stemString sentence)
	let
		t = map (\s -> 
			let (p, q) = extractPremiseConclusion s 
				in (removePunc p, removeConclusionWords $ removePunc q)
				) tagged 
		(premise, conclusion) = foldr (\(p, q) acc ->
			if length p > 0 
			then (p:fst acc, snd acc) 
			else (fst acc, q:snd acc)
			) ([], []) t
		in return (premise, conclusion)

toString :: [[(String, b)]] -> [String]
toString sentList = foldr (\l acc -> unwords [fst x | x <- l]:acc) [] sentList

removeWord :: String -> String -> String
removeWord word sent = unwords $ foldr (\w acc -> if w == word then acc else w:acc) [] (words sent)

getVarName :: Maybe Char -> Char
getVarName (Just x) = x
getVarName _ = '_'

reduceDisj :: [Formula] -> Formula
reduceDisj [] = error "Empty expression list is given"
reduceDisj [x] = x
reduceDisj (s:sx) = Disj s (reduceDisj sx)

reduceAllDisj :: [[Formula]] -> [Formula]
reduceAllDisj frm = foldr (\l acc -> if length l > 1 then (reduceDisj $ reverse l):acc else (head l):acc) [] frm

reduceConj :: [Formula] -> Formula
reduceConj [] = error "Empty expression list is given"
reduceConj [x] = x
reduceConj (s:sx) = Conj s (reduceConj sx)

reduceAllConj :: [Formula] -> Formula
reduceAllConj frm = if length frm > 0 then reduceConj frm else Nil

extractIf :: [Formula] -> [Formula]
extractIf sentList = foldr (\s acc -> 
						 let sw = sentWords s
							 in if "if" `elem` sw && "then" `elem` sw 
								then let
										removedIf = removeWord "if" (sentStr s)
										tmp = Split.splitOn " then " removedIf 
										in Cond (Sentence $ removeWord "if" $ head tmp) (Sentence $ last tmp):acc
								else s:acc
								) [] sentList

extractDisj :: Formula -> [Formula]
extractDisj (Cond l r) = extractDisj l ++ extractDisj r
extractDisj (Conj l r) = extractDisj l ++ extractDisj r
extractDisj (Neg p) = extractDisj p
extractDisj (Sentence s) = 
	if "or" `elem` (words s)
	then map (Sentence) (Split.splitOn " or " s)
	else [Sentence s]

extractNot :: Formula -> Formula
extractNot t@(Sentence s) = if "not" `elem` (words s) then (Neg $ Sentence (removeWord "not" s)) else t
extractNot (Cond l r) = Cond (extractNot l) (extractNot r)
extractNot (Conj l r) = Conj (extractNot l) (extractNot r)
extractNot (Disj l r) = Disj (extractNot l) (extractNot r)
extractNot (Neg p) = extractNot p

extractSent :: Formula -> [String]
extractSent (Sentence s) = [s]
extractSent (Neg s) = extractSent s
extractSent (Cond l r) = extractSent l ++ extractSent r
extractSent (Conj l r) = extractSent l ++ extractSent r
extractSent (Disj l r) = extractSent l ++ extractSent r
extractSent _ = []

toSentList :: [String] -> [Formula]
toSentList = map (Sentence)

makeVarMap :: (Formula, Formula) -> M.Map [Char] Char
makeVarMap (premise, conclusion) = 
	let
		lst = premise:[conclusion]
		sentList = foldr (\s acc -> (extractSent s) ++ acc ) [] lst
		varMap = foldr (\sent acc -> if M.member sent acc 
									then acc 
									else M.insert sent (Char.chr(M.size acc+96)) acc
						) (M.fromList [("", '_')]) sentList
		in M.delete "" varMap


reduce :: M.Map String Char -> Formula -> Expr
reduce varMap (Sentence s) = var $ getVarName $ M.lookup s varMap
reduce varMap (Neg f) = neg $ reduce varMap f
reduce varMap (Conj l r) = conj (reduce varMap l) (reduce varMap r)
reduce varMap (Disj l r) = disj (reduce varMap l) (reduce varMap r)
reduce varMap (Cond l r) = cond (reduce varMap l) (reduce varMap r)
reduce _ _ = var '_'

parseKeywords :: ([[(String, b)]], [[(String, b1)]]) -> (Expr, Expr)
parseKeywords (premise, conclusion) = 
	let
		(psents, qsents) = (toString premise, toString conclusion)
		(psentList, qsentList) = (toSentList psents, toSentList qsents)
		(pIfExtracted, qIfExtracted) = (extractIf psentList, extractIf qsentList)
		(pDisjExtracted, qDisjExtracted) = (map (extractDisj) pIfExtracted, map (extractDisj) qIfExtracted)
		(pDisjReduced, qDisjReduced) = (reduceAllDisj pDisjExtracted, reduceAllDisj qDisjExtracted)
		(pNotRemoved, qNotRemoved) = (map (extractNot) pDisjReduced, map (extractNot) qDisjReduced)
		t@(p, q) = (reduceAllConj pNotRemoved, reduceAllConj qNotRemoved)
		varMap = makeVarMap t
		in (reduce varMap p, reduce varMap q)


toLogicalForm :: String -> Expr
toLogicalForm sentence = do
	t <- extractPremiseConclusionAll sentence
	let (p, q) = parseKeywords t
		in Conditional p q


findFallaciesInSentence :: String -> IO [FoundFallacy]
findFallaciesInSentence sentence = do
	t <- extractPremiseConclusionAll sentence
	let
		(p, q) = parseKeywords t
		result = findFallacies $ Conditional p q
		in return (result)