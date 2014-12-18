module Fallacy.DetectInFOL where

import Fallacy.LogicShortcuts

import Data.Logic.Propositional


data FoundFallacy = FoundFallacy {
	fallacyType :: FallacyType,
	
	-- the fallacy pattern with potentially replaced variables to match 
	-- input expression
	fallacyExpr :: Expr,
	
	-- the input expression in which this fallacy was detected
	input :: Expr
	} deriving (Eq)

instance Show FoundFallacy where
        show ff = "\t Type => " ++ show (fallacyType ff) ++ "\n" ++ 
                  "\t Logical Form => " ++ show (fallacyExpr ff) ++ "\n"

{-
================================================================================
all fallacies we can recognize
================================================================================

-}

data FallacyType = AffirmDisjunct | DenyAntecedent | AffirmConsequent deriving (Eq)

instance Show FallacyType where
        show f = fallacyName f

allFallacyTypes :: [FallacyType]
allFallacyTypes = [AffirmDisjunct, DenyAntecedent, AffirmConsequent]

fallacyName :: FallacyType -> String
fallacyName AffirmDisjunct = "Affirming a Disjunct"
fallacyName DenyAntecedent = "Denying the antecedent"
fallacyName AffirmConsequent = "Affirming the Consequent"

fallacyPattern :: FallacyType -> Expr
fallacyPattern falType = parse pattern
	where pattern = case falType of
		AffirmDisjunct -> "((a | b) & a) -> ~b"
		DenyAntecedent -> "((a -> b) & ~a) -> ~b"
		AffirmConsequent -> "((a -> b) & b) -> a"


{-
================================================================================
parse
================================================================================

Shortcut for parsing expressions.

parameters:
	String:	input, parsable by Data.Logic.Propositional.parseExpr
		
returns:
	Expr:	the parsed expression from the input
-}

parse :: String -> Expr
parse input = case parseExpr "" input of
	Left ex -> error $ "cannot parse: " ++ input
	Right val -> val



{-
================================================================================
replaceAB
================================================================================

Replaces the variables 'a' and 'b' in the given expression by the given
variables / expressions.

parameters:	
	Expr: input expression containing only variables 'a' and 'b'
	Expr: expression / variable to replace 'a' in the input
	Expr: expression / variable to replace 'b' in the input

returns:
	Expr: input expression with 'a' and 'b' replaced by the given substitutes 
-}

replaceAB :: Expr -> Expr -> Expr -> Expr
replaceAB input aSubst bSubst = case input of
	(Variable (Var 'a')) -> aSubst
	(Variable (Var 'b')) -> bSubst
	(Negation a) -> Negation $ replaceAB a aSubst bSubst
	(Conjunction a b) -> replace2 Conjunction a b
	(Disjunction a b) -> replace2 Disjunction a b
	(Conditional a b) -> replace2 Conditional a b
	(Biconditional a b) -> replace2 Biconditional a b
	a -> a
	where
		replace2 :: (Expr -> Expr -> Expr) -> Expr -> Expr -> Expr
		replace2 construcror a b = construcror (replaceAB a aSubst bSubst)
											   (replaceAB b aSubst bSubst)



{-
================================================================================
matchesPattern
================================================================================

Checks if at least one commutation of the given input expression matches the 
given pattern. This is true if the expression trees only differ at places where 
the pattern has leaves (variables). In those cases however the corresponding 
part in the input expression tree has to imply the variable in the pattern.

parameters:	
	Expr: input expression
	Expr: pattern

returns:
	Bool: True if input matches pattern, otherwise false 
-}

matchesPattern :: Expr -> Expr -> Bool
matchesPattern input pattern = case (input, pattern) of
	-- remove double negations from input and pattern
	(Negation (Negation i), p) -> matchesPattern i p
	(i, Negation (Negation p)) -> matchesPattern i p
	
	(i, p@(Variable _)) -> isTautology $ i `cond` p
	(i, p@(Negation (Variable _))) -> isTautology $ i `cond` p
	
	-- commutative operators
	(Conjunction i1 i2, Conjunction p1 p2) -> matches2Com i1 i2 p1 p2
	(Disjunction i1 i2, Disjunction p1 p2) -> matches2Com i1 i2 p1 p2
	(Biconditional i1 i2, Biconditional p1 p2) -> matches2Com i1 i2 p1 p2
	
	(Conditional i1 i2, Conditional p1 p2) -> matches2 i1 i2 p1 p2
	
	_ -> False
	where
		matches2 :: Expr -> Expr -> Expr -> Expr -> Bool
		matches2 i1 i2 p1 p2 = (matchesPattern i1 p1) &&
							   (matchesPattern i2 p2)
		
		matches2Com :: Expr -> Expr -> Expr -> Expr -> Bool
		matches2Com i1 i2 p1 p2 = (matches2 i1 i2 p1 p2) ||
							 	  (matches2 i2 i1 p1 p2)



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
		vars = map Variable $ variables inputLeft

		inputMatchesPattern :: (FallacyType, Expr) -> Bool
		inputMatchesPattern (_, pattern) = matchesPattern input pattern


		falTypesAndPatterns = [
			(falType, replaceAB (fallacyPattern falType) a b) |
			falType <- allFallacyTypes, a <- vars, b <- vars, a /= b
			]

		foundFallacies = filter inputMatchesPattern falTypesAndPatterns

		result = [FoundFallacy falType pattern input | 
			(falType, pattern) <- foundFallacies]
