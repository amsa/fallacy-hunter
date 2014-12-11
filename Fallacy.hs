module Fallacy where

import Data.Logic.Propositional


type Subj = String
type Pred = String
type Obj = String

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

p = var 'p'
q = var 'q'
r = var 'r'

fallacy1 = isTautology $ ((p `disj` q) `conj` p) `cond` (neg q)
