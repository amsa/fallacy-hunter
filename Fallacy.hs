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