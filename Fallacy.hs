module Fallacy where

import Data.Logic.Propositional

var = Variable . Var
neg = Negation
conj = Conjunction
disj = Disjunction
cond = Conditional
iff = Biconditional

data FallacyType = AffirmDisjunct | DenyAntecedent | AffirmConsequent
	deriving (Show, Eq)

fallacyName :: FallacyType -> String
fallacyName AffirmDisjunct = "Affirming a Disjunct"
fallacyName DenyAntecedent = "Denying the antecedent"
fallacyName AffirmConsequent = "Affirming the Consequent"

data FoundFallacy = FoundFallacy {
	fallacyType :: FallacyType,
	fallacyExpr :: Expr,
	origExpr :: Expr
	} deriving (Show, Eq)

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
	FallacyType: the fallacy type
	Expr: the expression as defined above with `a` and `b` being replaced 
		by the two variables given as parameters	
-}

affirmDisjunct :: Var -> Var -> (FallacyType, Expr)
affirmDisjunct a b = (AffirmDisjunct, left `cond` right)
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
	FallacyType: the fallacy type
	Expr: the expression as defined above with `a` and `b` being replaced 
		by the two variables given as parameters	
-}

denyAntecedent :: Var -> Var -> (FallacyType, Expr)
denyAntecedent a b = (DenyAntecedent, left `cond` right)
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
	FallacyType: the fallacy type
	Expr: the expression as defined above with `a` and `b` being replaced 
		by the two variables given as parameters	
-}

affirmConsequent :: Var -> Var -> (FallacyType, Expr)
affirmConsequent a b = (AffirmConsequent, left `cond` right)
	where
		aExp = Variable a
		bExp = Variable b
		left = (aExp `cond` bExp) `conj` bExp
		right = aExp


{-
========================================================================
findFallacies
========================================================================

Lists all fallacies (which we implemented so far and) which are contained
in the input expression.

parameters:	
	Expr: the expression to be checked for contained fallacies

returns
	[FoundFallacy]: a list of all found fallacies, each with its type, the pure 
	fallacy pattern with matching variables, and the original expression from 
	the input which contains the fallacy 
		
-}


findFallacies :: Expr -> [FoundFallacy]
findFallacies input@(Conditional left right) =
	[FoundFallacy falType expr input | (falType, expr) <- foundFallacies]
	where
		fallacyFunctions = [affirmDisjunct, denyAntecedent, affirmConsequent]
		
		fallacies = [func a b | 
			func <- fallacyFunctions, a <- variables left, b <- variables left]

		inputImpliesFallacy :: (FallacyType, Expr) -> Bool
		inputImpliesFallacy (_, (Conditional fal_left fal_right)) =
			isTautology $ (left `cond` fal_left) `conj` (right `cond` fal_right)

		foundFallacies = filter inputImpliesFallacy fallacies

findFallacies _ = []


isFallacy :: Expr -> Bool
isFallacy expr = length (findFallacies expr) > 0