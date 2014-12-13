module TextToFOL where

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

data TripleTree = Triple Subj Pred Obj
          | Neg TripleTree
          | Conj TripleTree TripleTree
          | Disj TripleTree TripleTree
          | Cond TripleTree TripleTree
          deriving (Eq, Show)

data UnaryOp = Not | None deriving (Show)


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
