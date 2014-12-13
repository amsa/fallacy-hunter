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

{-
========================================================================
affirmDisj
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

affirmDisj :: Var -> Var -> Expr
affirmDisj a b = affirmDisj_left `cond` affirmDisj_right
	where
		aExp = Variable a
		bExp = Variable b
		affirmDisj_left = (aExp `disj` bExp) `conj` aExp
		affirmDisj_right = (neg bExp)


{-
========================================================================
isFallacy
========================================================================

Checks if the given expression is a fallacy. All detectable fallacies have
	the form `expression1 => expression2`. If the given expression does not
	have this form, it is not a fallacy (although it might contain a 
	contradiction).

parameters:	
	Expr: the expression to be checked for contained fallacies

returns
	Bool: True if the given expression contains one of the fallacies we 
		detect here, otherwise False
-}
isFallacy :: Expr -> Bool

isFallacy (Conditional left right) = any isFallacyMapping varPairs
	where

		isFallacyMapping :: (Var, Var) -> Bool
		isFallacyMapping varPair = 
			isTautology (left `cond` fallacy_left) && 
			isTautology (right `cond` fallacy_right)

			where
				(Conditional fallacy_left fallacy_right) =
					affirmDisj (fst varPair) (snd varPair)

		varPairs = [(a, b) | a <- variables left, b <- variables left]

isFallacy _ = False
