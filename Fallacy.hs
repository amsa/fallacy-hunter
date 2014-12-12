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

a = var 'a'
b = var 'b'
c = var 'c'
d = var 'd'

affirmingDisjunct_left = (a `disj` b) `conj` a
affirmingDisjunct_right = (neg b)
affirmingDisjunct = affirmingDisjunct_left `cond` affirmingDisjunct_right

isFallacy :: Expr -> Bool
isFallacy (Conditional left right) =
	isTautology (left `cond` affirmingDisjunct_left) &&
	isTautology (right `cond` affirmingDisjunct_right)