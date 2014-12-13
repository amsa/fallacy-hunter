module Fallacy where

import Data.Logic.Propositional
import qualified Data.Text as T
import qualified Data.Char as Char 
import qualified NLP.POS as POS
import qualified NLP.Types.Tree as Types
import NLP.Corpora.Conll (Tag)
import NLP.Stemmer


type Subj = String
type Pred = String
type Obj = String

data UnaryOp = Not | None deriving (Show)

data TripleTree = Triple Subj Pred Obj
          | Neg TripleTree
          | Conj TripleTree TripleTree
          | Disj TripleTree TripleTree
          | Cond TripleTree TripleTree
          deriving (Eq, Show)

var = Variable . Var
neg = Negation
conj = Conjunction
disj = Disjunction
cond = Conditional
iff = Biconditional

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
        let posList = [Types.unTS x | x <- taggedSents] 
            tags = map (\s -> 
                       foldl (\acc p -> 
                             let tok = T.unpack $ Types.showPOStok p
                                 pos = T.unpack $ Types.showPOStag p
                                 in acc ++ [(tok, pos)]) [] s
                        ) posList
            in return (tags)


{-extractKeywords :: String -> IO [(UnaryOp, String)]-}
extractKeywords sentence = do 
                  tagged <- tagStringTuple (setSentBoundaries $ stemString sentence)
                  let conclusionWordList = ["therefore", "so", "hence"]
                      t = map (\s -> 
                              let 
                              (premise, conclusion) = foldr (\w acc -> 
                                let sec = snd acc in
                                  if (length sec > 0 && head sec `elem` conclusionWordList) then (fst w:fst acc, snd acc) 
                                      else (fst acc, fst w:snd acc)
                                      ) ([], []) s 

                              {-(notList, sentWords) = foldr (\w acc -> -}
                                {-if fst w == "not" then ("not":fst acc, snd acc) else (fst acc, fst w:snd acc)) ([], []) s-}

                              {-sent = unwords $ foldr (\w acc -> if length w > 0 then w:acc else acc) [] sentWords-}
                              {-notVal = if even $ length notList then None else Not-}
                              {-in (notVal, sent)-}
                              in (premise, conclusion)
                              ) tagged
                      in return (t)


toLower :: String -> String
toLower word = map (\c -> Char.toLower c) word

setSentBoundaries :: String -> String
setSentBoundaries sentence = unwords $ map (\w -> case w of 
                             "and" -> "."
                             "," -> "."
                             _ -> toLower w) $ words sentence


stemString :: String -> String
stemString input = 
        let stopWords = ["do", "does", "a", "an", "the", "of", "to"]
            filtered = filter (\s -> (toLower s) `notElem` stopWords) $ words input 
            in unwords $ map (stem English) $ filtered


{-
========================================================================
affirmDisjunct
========================================================================

The pattern for 'Affirming a Disjunct' fallacy is
(a OR b) AND a => NOT b

parameters:	
	Var: variable to be mapped as a in the formula above
	Var: variable to be mapped as b in the formula above

returns:
	Expr: the expression `(a OR b) AND a => NOT b` with `a` and `b` being
		replaced by the two variables given as parameters	
-}

affirmDisjunct :: Var -> Var -> Expr
affirmDisjunct a b = affirmDisjunct_left `cond` affirmDisjunct_right
	where
		aExp = Variable a
		bExp = Variable b
		affirmDisjunct_left = (aExp `disj` bExp) `conj` aExp
		affirmDisjunct_right = (neg bExp)


{-
========================================================================
denyAntecedent
========================================================================

The pattern for 'Denying the antecedent' fallacy is
(a => b) AND (NOT a) => NOT b

parameters:	
	Var: variable to be mapped as a in the formula above
	Var: variable to be mapped as b in the formula above

returns:
	Expr: the expression `(a => b) AND (NOT a) => NOT b` with `a` and `b` 
		being replaced by the two variables given as parameters	
-}

denyAntecedent :: Var -> Var -> Expr
denyAntecedent a b = denyAntecedent_left `cond` denyAntecedent_right
	where
		aExp = Variable a
		bExp = Variable b
		denyAntecedent_left = (aExp `cond` bExp) `conj` (neg aExp)
		denyAntecedent_right = (neg bExp)


{-
========================================================================
isFallacy
========================================================================

Checks if the given expression contains one of the fallacies we implemented.

parameters:	
	Expr: the expression to be checked for contained fallacies

returns
	Bool: True if the given expression contains one of the fallacies we 
		detect here, otherwise False
-}
isFallacy :: Expr -> Bool
isFallacy (Conditional left right) = any isFallacyMapping varPairs
	where

		fallacies = [affirmDisjunct, denyAntecedent]
		
		varPairs = [(a, b) | a <- variables left, b <- variables left]		

		isFallacyMapping :: (Var, Var) -> Bool
		isFallacyMapping varPair = any (impliesFallacy varPair) fallacies

		impliesFallacy :: (Var, Var) -> (Var -> Var -> Expr) -> Bool
		impliesFallacy (a, b) fallacyFunc = 
			isTautology $ (left `cond` fal_left) `conj` (right `cond` fal_right)
			where

				(Conditional fal_left fal_right) = fallacyFunc a b

isFallacy _ = False
