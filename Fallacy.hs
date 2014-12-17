module Fallacy where

import Data.Logic.Propositional
import Data.Maybe

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
	
	-- the fallacy pattern with potentially replaced variables to match 
	-- original expression
	fallacyExpr :: Expr,
	
	-- the input expression in which this fallacy was detected
	origExpr :: Expr
	} deriving (Show, Eq)


{-
================================================================================
eachImplies
================================================================================

Checks if each expression in the list implies the target expression

parameters:	
	[Expr]: a list of expressions which potentially imply the target expression
	Expr: the target expression

returns:
	Bool: True iff each expression in the list implies the target expression
-}
eachImplies :: [Expr] -> Expr -> Bool
eachImplies lefts right = all (\x -> isTautology $ x `cond` right) lefts


{-
================================================================================
checkFallacy
================================================================================

Helper function for all fallacy functions.

parameters:	
	Expr: the input expression to be analyzed for this fallacy
	(Expr -> Bool): fallacy matching function 
	FallacyType: the type of the fallacy to be checked
	Expr: the fallacy pattern with potentially replaced variables

returns:
	FoundFallacy: if one was found
	Nothing: otherwise
-}
checkFallacy :: Expr -> (Expr -> Bool) -> FallacyType -> Expr -> Maybe FoundFallacy
checkFallacy input matchesFunc falType pattern
	| matchesFunc input = Just $ FoundFallacy falType pattern input
	| otherwise  		= Nothing



{-
================================================================================
affirmDisjunct
================================================================================

The pattern for 'Affirming a Disjunct' fallacy is
((a | b) & a) -> ~b

parameters:	
	Expr: the input expression to be analyzed for this fallacy
	Expr: expression / variable to be mapped as 'a' in the fallacy pattern
	Expr: expression / variable to be mapped as 'b' in the fallacy pattern

returns:
	FoundFallacy: if one was found
	Nothing: otherwise
-}
affirmDisjunct :: Expr -> Expr -> Expr -> Maybe FoundFallacy
affirmDisjunct input a b = checkFallacy input matches AffirmDisjunct pattern
	where
		pattern = ((a `disj` b) `conj` a) `cond` (neg b)

		matches (Conditional 
			(Conjunction (Disjunction inA1 inB1) inA2) 
			inNegB
			) = (eachImplies [inA1, inA2] a) &&
				(eachImplies [inB1] b) &&
				(eachImplies [inNegB] (neg b))
		matches _ = False



{-
================================================================================
denyAntecedent
================================================================================

The pattern for 'Denying the antecedent' fallacy is
((a -> b) & ~a) -> ~b

parameters:	
	Expr: the input expression to be analyzed for this fallacy
	Expr: expression / variable to be mapped as 'a' in the fallacy pattern
	Expr: expression / variable to be mapped as 'b' in the fallacy pattern

returns:
	FoundFallacy: if one was found
	Nothing: otherwise
-}
denyAntecedent :: Expr -> Expr -> Expr -> Maybe FoundFallacy
denyAntecedent input a b = checkFallacy input matches DenyAntecedent pattern
	where
		pattern = ((a `cond` b) `conj` (neg a)) `cond` (neg b)

		matches (Conditional 
			(Conjunction (Conditional inA inB) inNegA) 
			inNegB
			) = (eachImplies [inA] a) &&
				(eachImplies [inB] b) &&
				(eachImplies [inNegA] (neg a)) &&
				(eachImplies [inNegB] (neg b))
		matches _ = False


{-
================================================================================
affirmConsequent
================================================================================

The pattern for 'Affirming the Consequent' fallacy is
((a -> b) & b) -> a

parameters:	
	Expr: the input expression to be analyzed for this fallacy
	Expr: expression / variable to be mapped as 'a' in the fallacy pattern
	Expr: expression / variable to be mapped as 'b' in the fallacy pattern

returns:
	FoundFallacy: if one was found
	Nothing: otherwise
-}
affirmConsequent :: Expr -> Expr -> Expr -> Maybe FoundFallacy
affirmConsequent input a b = checkFallacy input matches AffirmConsequent pattern
	where
		pattern = ((a `cond` b) `conj` b) `cond` a

		matches (Conditional 
			(Conjunction (Conditional inA1 inB1) inB2) 
			inA2
			) = (eachImplies [inA1, inA2] a) &&
				(eachImplies [inB1, inB2] b)
		matches _ = False




{-
================================================================================
findFallacies
================================================================================

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
findFallacies (Variable _) = []

findFallacies (Negation a) = findFallacies a

findFallacies (Conjunction a b) = (findFallacies a) ++ (findFallacies b)

findFallacies (Disjunction a b) = (findFallacies a) ++ (findFallacies b)

findFallacies (Biconditional a b) = (findFallacies a) ++ (findFallacies b)

findFallacies input@(Conditional inputLeft inputRight) =
	result ++ (findFallacies inputLeft) ++ (findFallacies inputRight)
	
	where
		fallacyFunctions = [affirmDisjunct, denyAntecedent, affirmConsequent]
		
		vars = map Variable $ variables inputLeft

		maybeFallacies = [func input a b | func <- fallacyFunctions, 
			a <- vars, b <- vars, a /= b]

		result = catMaybes maybeFallacies