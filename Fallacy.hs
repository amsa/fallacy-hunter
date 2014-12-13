module Fallacy where

import Data.Logic.Propositional

var = Variable . Var
neg = Negation
conj = Conjunction
disj = Disjunction
cond = Conditional
iff = Biconditional


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
	Expr: the expression as defined above with `a` and `b` being replaced 
		by the two variables given as parameters	
-}

affirmDisjunct :: Var -> Var -> Expr
affirmDisjunct a b = left `cond` right
	where
		aExp = Variable a
		bExp = Variable b
		left = (aExp `disj` bExp) `conj` aExp
		right = (neg bExp)


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
	Expr: the expression as defined above with `a` and `b` being replaced 
		by the two variables given as parameters	
-}

denyAntecedent :: Var -> Var -> Expr
denyAntecedent a b = left `cond` right
	where
		aExp = Variable a
		bExp = Variable b
		left = (aExp `cond` bExp) `conj` (neg aExp)
		right = (neg bExp)


{-
========================================================================
affirmConsequent
========================================================================

The pattern for 'Affirming the Consequent' fallacy is
(a => b) AND b => a

parameters:	
	Var: variable to be mapped as a in the formula above
	Var: variable to be mapped as b in the formula above

returns:
	Expr: the expression as defined above with `a` and `b` being replaced 
		by the two variables given as parameters	
-}

affirmConsequent :: Var -> Var -> Expr
affirmConsequent a b = left `cond` right
	where
		aExp = Variable a
		bExp = Variable b
		left = (aExp `cond` bExp) `conj` bExp
		right = aExp


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

		fallacies = [affirmDisjunct, denyAntecedent, affirmConsequent]
		
		varPairs = [(a, b) | a <- variables left, b <- variables left]		

		isFallacyMapping :: (Var, Var) -> Bool
		isFallacyMapping varPair = any (impliesFallacy varPair) fallacies

		impliesFallacy :: (Var, Var) -> (Var -> Var -> Expr) -> Bool
		impliesFallacy (a, b) fallacyFunc = 
			isTautology $ (left `cond` fal_left) `conj` (right `cond` fal_right)
			where

				(Conditional fal_left fal_right) = fallacyFunc a b

isFallacy _ = False
