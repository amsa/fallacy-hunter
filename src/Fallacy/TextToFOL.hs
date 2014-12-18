module Fallacy.TextToFOL where

import Fallacy.LogicShortcuts

import Data.Logic.Propositional

import qualified Data.Map as M
import qualified Data.List.Split as Split

import qualified Data.Text as T
import qualified Data.Char as Char 

import qualified NLP.POS as POS
import qualified NLP.Types.Tree as Types
import NLP.Types (POSTagger)
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

punctuations, conclusionWords, stopWords :: [String]
punctuations = [",", ".", ";", ":"]
conclusionWords = ["therefore", "so", "hence", "thus"]
stopWords = ["do", "does", "a", "an", "the", "of", "to"]


type TaggedWords = [Types.TaggedSentence NLP.Corpora.Conll.Tag]
type ConllPOSTagger = POSTagger NLP.Corpora.Conll.Tag

{- 
==========================
sentStr
==========================

Returns the string value of a formula if it is a Sentence, or throws an error otherwise
-}

sentStr :: Formula -> String
sentStr (Sentence x) = x
sentStr _ = error "The given parameter is not a Sentence."

{- 
==========================
sentWords
==========================

Returns a list of all the words of a formula if it is a Sentence, or throws an error otherwise
-}
sentWords :: Formula -> [String]
sentWords (Sentence x) = words x
sentWords _ = error "The given parameter is not a Sentence."


{- 
==========================
tagString
==========================

Parses the input string, and handles POS tagging

parameters:
  String: the string that we are trying to detect containing fallacies (if any)
-}
tagString :: String -> ConllPOSTagger -> TaggedWords
tagString input tagger = POS.tag tagger $ T.pack input

{- 
==========================
tagStringTuple
==========================

For each word returns a pair with the word and its string value of POS

parameters:
  TaggedWords
-}
tagStringTuple :: TaggedWords -> [[(String, String)]]
tagStringTuple taggedWords = tags
	where
		posList = [Types.unTS x | x <- taggedWords] 
		tags = map (\s -> 
					 foldl (\acc p -> 
						 let
						 	tok = T.unpack $ Types.showPOStok p
							pos = T.unpack $ Types.showPOStag p
							in acc ++ [(toLower tok, pos)]) [] s
					) posList

{- 
==========================
toLower
==========================

Convert the case of the characters of a given word to lower case
-}
toLower :: String -> String
toLower word = map (\c -> Char.toLower c) word

{- 
==========================
setSentBoundaries
==========================

Sets the boundies of sentences before turning into logical form
-}
setSentBoundaries :: String -> String
setSentBoundaries sentence = unwords $ map (\w -> case w of 
							 "and" -> "."
							 "," -> "."
							 _ -> w) $ words sentence

{- 
==========================
stemString
==========================

Handles stemming of the given sentence
-}
stemString :: String -> String
stemString input = 
		let filtered = filter (\s -> (toLower s) `notElem` stopWords) $ words input 
			in unwords $ map (stem English) $ filtered

{- 
==========================
removePunc
==========================

Removes the punctuations from a list of tuples containing the words and their POS
-}
removePunc :: [(String, b)] -> [(String, b)]
removePunc = foldr (\tuple acc -> if (fst tuple) `elem` punctuations then acc else tuple:acc) []

{- 
==========================
removeConclusionWords
==========================

Removes the conclusion words such as therefore from the sentence
-}
removeConclusionWords :: [(String, b)] -> [(String, b)]
removeConclusionWords = foldr (\tuple acc -> if (fst tuple) `elem` conclusionWords then acc else tuple:acc) []

{- 
==========================
extractPremiseConclusion
==========================

Extracts premise and conclusion from the sentence
-}
extractPremiseConclusion :: [([Char], b)] -> ([([Char], b)], [([Char], b)])
extractPremiseConclusion = span (\e -> (fst e) `notElem` conclusionWords) 

{- 
==========================
extractPremiseConclusionAll
==========================

Extract all the premises and conclusions inside a given text
-}
extractPremiseConclusionAll :: TaggedWords -> ([[(String, String)]], [[(String, String)]])
extractPremiseConclusionAll taggedWords = (premise, conclusion)
	where
		tagged = tagStringTuple taggedWords
		
		t = map (\s -> 
			let (p, q) = extractPremiseConclusion s 
				in (removePunc p, removeConclusionWords $ removePunc q)
				) tagged 
		
		(premise, conclusion) = foldr (\(p, q) acc ->
			if length p > 0 
			then (p:fst acc, snd acc) 
			else (fst acc, q:snd acc)
			) ([], []) t

{- 
==========================
toString
==========================

Converts the list of tuples with words and POS to a list of words of the sentence
-}
toString :: [[(String, b)]] -> [String]
toString sentList = foldr (\l acc -> unwords [fst x | x <- l]:acc) [] sentList

{- 
==========================
removeWord
==========================

Removes a given word from a given sentence
-}
removeWord :: String -> String -> String
removeWord word sent = unwords $ foldr (\w acc -> if w == word then acc else w:acc) [] (words sent)

{- 
==========================
reduceDisj
==========================

Reduces disjunctions recursively and return a formula
-}
reduceDisj :: [Formula] -> Formula
reduceDisj [] = error "Empty expression list is given"
reduceDisj [x] = x
reduceDisj (s:sx) = Disj s (reduceDisj sx)

{- 
==========================
reduceAllDisj
==========================

Reduces list of disjunctions
-}
reduceAllDisj :: [[Formula]] -> [Formula]
reduceAllDisj frm = foldr (\l acc -> if length l > 1 then (reduceDisj $ reverse l):acc else (head l):acc) [] frm

{- 
==========================
reduceConj
==========================

Reduces conjunctions recursively and return a formula
-}
reduceConj :: [Formula] -> Formula
reduceConj [] = error "Empty expression list is given"
reduceConj [x] = x
reduceConj (s:sx) = Conj s (reduceConj sx)

reduceAllConj :: [Formula] -> Formula
reduceAllConj frm = if length frm > 0 then reduceConj frm else Nil

{- 
==========================
extractIf
==========================

Extracts all the implications (conditions) from a list of formula
-}
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

{- 
==========================
extractDisj
==========================

Extracts disjunctions from a formula
-}
extractDisj :: Formula -> [Formula]
extractDisj (Cond l r) = extractDisj l ++ extractDisj r
extractDisj (Conj l r) = extractDisj l ++ extractDisj r
extractDisj (Neg p) = extractDisj p
extractDisj (Sentence s) = 
	if "or" `elem` (words s)
	then map (Sentence) (Split.splitOn " or " s)
	else [Sentence s]

{- 
==========================
extractNot
==========================

Extracts negations from a formula
-}
extractNot :: Formula -> Formula
extractNot t@(Sentence s) = if "not" `elem` (words s) then (Neg $ Sentence (removeWord "not" s)) else t
extractNot (Cond l r) = Cond (extractNot l) (extractNot r)
extractNot (Conj l r) = Conj (extractNot l) (extractNot r)
extractNot (Disj l r) = Disj (extractNot l) (extractNot r)
extractNot (Neg p) = extractNot p

{- 
==========================
extractNot
==========================

Extracts all the containing sentences form a given formula
-}
extractSent :: Formula -> [String]
extractSent (Sentence s) = [s]
extractSent (Neg s) = extractSent s
extractSent (Cond l r) = extractSent l ++ extractSent r
extractSent (Conj l r) = extractSent l ++ extractSent r
extractSent (Disj l r) = extractSent l ++ extractSent r
extractSent _ = []

{- 
==========================
toSentList
==========================

Converts a list of sentences into a a list of formulas (of type Sentence)
-}
toSentList :: [String] -> [Formula]
toSentList = map (Sentence)

{- 
==========================
makeVarMap
==========================

Creates variable map using all the formulas in premise and conclusion
-}
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

{- 
==========================
getVarName
==========================

Extracts variable name from variable map
-}
getVarName :: Maybe Char -> Char
getVarName (Just x) = x
getVarName _ = '_'

{- 
==========================
reduce
==========================

Reduces a formula recursively and turn it into a well-formed logical expression
-}
reduce :: M.Map String Char -> Formula -> Expr
reduce varMap (Sentence s) = var $ getVarName $ M.lookup s varMap
reduce varMap (Neg f) = neg $ reduce varMap f
reduce varMap (Conj l r) = conj (reduce varMap l) (reduce varMap r)
reduce varMap (Disj l r) = disj (reduce varMap l) (reduce varMap r)
reduce varMap (Cond l r) = cond (reduce varMap l) (reduce varMap r)
reduce _ _ = var '_'

{- 
==========================
parseKeywords
==========================

Parses keywords in premise and conclusion, and returns well-formed logical expression 
for each side
-}
parseKeywords :: ([[(String, b)]], [[(String, b1)]]) -> (Expr, Expr)
parseKeywords (premise, conclusion) = 
	let
		parse :: [[(String, c)]] -> Formula
		parse x = conjReduced
			where
				sents = toString x
				sentList = toSentList sents
				ifExtracted = extractIf sentList
				disjExtracted = map extractDisj ifExtracted
				disjReduced = reduceAllDisj disjExtracted
				notRemoved = map extractNot disjReduced
				conjReduced = reduceAllConj notRemoved

		p = parse premise
		q = parse conclusion

		varMap = makeVarMap (p, q)
		in (reduce varMap p, reduce varMap q)

{- 
==========================
toLogicalForm
==========================

Converts a given string into a well-formed logical expression
-}
toLogicalForm :: String -> ConllPOSTagger -> Expr
toLogicalForm input tagger = Conditional p q
	where
		stemmedInput = stemString input
		boundInput = setSentBoundaries stemmedInput
		taggedInput = tagString boundInput tagger
		t = extractPremiseConclusionAll taggedInput
		(p, q) = parseKeywords t
