import Test.HUnit
import Fallacy

import Data.Logic.Propositional
--tests = test [ 
--             "test triple" ~: "Triple(Max,is,cat)" ~: "Triple \"Max\" \"is\" \"cat\"" ~=? show(Triple "Max" "is" "cat")
--             ]

a = var 'a'
b = var 'b'
c = var 'c'
d = var 'd'

{-
================================================================================
assertEqualTest
================================================================================

a shortcut for creating TestCase elements of the form
testName = TestCase $ assertEqual "" True (someExpression)

parameters:
	a:	expected value
	a:	actual value

returns:
	the test resulting from comparing expected with actual value
-}
assertEqualTest :: (Eq a, Show a) => a -> a -> Test
assertEqualTest expected actual = 
	TestCase $ assertEqual "" expected actual



{-
================================================================================
hasFallaciesTest
================================================================================

Creates a test which succeeds iff the given input expression contains each 
given FoundFallacy.

parameters:
	
	Expr:	the expression to be given as input to findFallacies
	
	[FoundFallacy]: the FoundFallacy instances to look for in the findFallacies 
		output
	
returns:

	Test:	the test as described above
-}
hasFallaciesTest :: Expr -> [FoundFallacy] -> Test
hasFallaciesTest inputExpr expFallacies = assertEqualTest True $ actual
	where
		foundFallacies = findFallacies inputExpr

		isFound :: FoundFallacy -> Bool
		isFound expFallacy = expFallacy `elem` foundFallacies

		actual = all isFound expFallacies


{-
================================================================================
hasNotFallacyTest
================================================================================

Creates a test which succeeds iff the given input expression does not contain 
the given FoundFallacy.

parameters:
	
	Expr:	the expression to be given as input to findFallacies
	
	FoundFallacy: the fallacy which should not be contained in the findFallacies
		output
	
returns:

	Test:	the test as described above
-}
hasNotFallacyTest :: Expr -> FoundFallacy -> Test
hasNotFallacyTest inputExpr unwantedFallacy = 
	assertEqualTest False $ elem unwantedFallacy $ findFallacies inputExpr

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
affirmDisjunctPosTest
================================================================================

The pattern for 'Affirming a Disjunct' fallacy is
(a | b) & a -> ~b

there is asertBool which has the functionality of 'assertTrue'
but since it is called so confusingly, I preferred the unambiguous
assertEquel True ...
-}
affirmDisjunctPosTest = hasFallaciesTest expr [expFallacy]
	where
		complexA = parse "(a & c) & d"			-- reducable to a
		complexA2 = parse "a & (c -> a)" 		-- reducable to a
		complexB = parse "b & d"				-- reducable to b
		complexNegB = parse "c & (c -> ~b)" 	-- reducable to ~b
		
		exprLeft = (complexA `disj` complexB) `conj` complexA2
		exprRight = complexNegB
		expr = exprLeft `cond` exprRight

		expFalExpr = parse "((a | b) & a) -> ~b"
		expFallacy = FoundFallacy AffirmDisjunct expFalExpr expr


{-
================================================================================
affirmDisjunctNegTest
================================================================================
-}
affirmDisjunctNegTest = hasNotFallacyTest expr unwantedFallacy
	where
		complexA = parse "(a & c) & d" 				-- reducable to a
		complexA2 = parse "a & (c -> a)" 			-- reducable to a
		complexB = parse "b & d" 					-- reducable to b
		complexSomething = parse "d & (c -> b)" 	-- NOT reducable to b

		exprLeft = (complexA `disj` complexB) `conj` complexA2
		exprRight = neg complexSomething
		expr = exprLeft `cond` exprRight

		unwantedFalExpr = parse "((a | b) & a) -> ~b"
		unwantedFallacy = FoundFallacy AffirmDisjunct unwantedFalExpr expr


{-
================================================================================
affirmDisjunctChangedVarsTest
================================================================================
The pattern for 'Affirming a Disjunct' fallacy is
(a | b) & a -> ~b

This tests if the fallacy detection also works with
(c | d) & c -> ~d
-}
affirmDisjunctChangedVarsTest = hasFallaciesTest expr [expFallacy]
	where
		complexC = parse "(c & c) & a"		-- reducable to c
		complexC2 = parse "c & (c | a)" 	-- reducable to c
		complexD = parse "a & (a -> d)"		-- reducable to d
		complexNegD = parse "~d & (a | b)" 	-- reducable to ~d
		
		exprLeft = (complexC `disj` complexD) `conj` complexC2
		exprRight = complexNegD
		expr = exprLeft `cond` exprRight

		expFalExpr = parse "((c | d) & c) -> ~d"
		expFallacy = FoundFallacy AffirmDisjunct expFalExpr expr



{-
================================================================================
wrongFormatTest
================================================================================
A fallacy pattern always has the form (expr1 -> expr2).
This tests if the function can handle expressions of a different format
(which are therefore no fallacies) withour raising errors.
-}
wrongFormatTest = assertEqualTest True $ null $ findFallacies $ parse "a & ~a"


{-
================================================================================
denyAntecedentPosTest
================================================================================

The pattern for 'Denying the antecedent' fallacy is
(a -> b) & ~a -> ~b
-}

denyAntecedentPosTest = hasFallaciesTest expr [expFallacy]
	where
		complexA = parse "(a & a) & a" 	-- reducable to a
		complexNegA = parse "~(c | a)" 	-- reducable to ~a
		complexB = parse "~(~b | d)"	-- reducable to b
		complexNegB = parse "~ ~ ~b"	-- reducable to ~b

		exprLeft = (complexA `cond` complexB) `conj` complexNegA
		exprRight = complexNegB
		expr = exprLeft `cond` exprRight

		expFalExpr = parse "((a -> b) & ~a) -> ~b"
		expFallacy = FoundFallacy DenyAntecedent expFalExpr expr




{-
================================================================================
affirmConseqPosTest
================================================================================

The pattern for 'Affirming the consequent' fallacy is
(a -> b) & b -> a
-}

affirmConseqPosTest = hasFallaciesTest expr [expFallacy]
	where
		complexA = parse "~ ~a" 		 -- reducable to a
		complexA2 = parse "~(c | ~a)" 	 -- reducable to a
		complexB = parse "~(~b | d)" 	 -- reducable to b
		complexB2 = parse "(b | c) & ~c" -- reducable to b

		exprLeft = (complexA `cond` complexB) `conj` complexB2
		exprRight = complexA2 `conj` complexB
		expr = exprLeft `cond` exprRight

		expFalExpr = parse "((a -> b) & b) -> a"
		expFallacy = FoundFallacy AffirmConsequent expFalExpr expr


{-
================================================================================
affirmConseqPosTest2
================================================================================

The pattern for 'Affirming the consequent' fallacy is
(a -> b) & b -> a

Since some expressions, like the one in affirmConseqPosTest, contain both, 
'Affirming the consequent' fallacy and 'Denying the antecedent' fallacy,
this tests "pure" 'Affirming the consequent' fallacy (which is not 'Denying 
the antecedent' fallacy)
-}

affirmConseqPosTest2 = hasFallaciesTest expr [expFallacy]
	where
		expr = parse "((a -> b) & b) -> a"
		expFallacy = FoundFallacy AffirmConsequent expr expr

{-
================================================================================
noFallacyTest1
================================================================================

In one of our experiments the following expression was wrongly classified 
as fallacy:
~a & b -> ~a
-}
--noFallacyTest1 = assertEqualTest True $ null $ findFallacies expr
--	where
--		expr = parse "(~a & b) -> ~a"



{-
================================================================================
affirmDisjunctRecursAndTest
================================================================================

The pattern for 'Affirming a Disjunct' fallacy is
(a | b) & a -> ~b

This tests if it can be found recusrively if the whole input expression is of
type (expression1 & expression2) with (expression2 == ((d | c) & d -> ~c)). 
-}

affirmDisjunctRecursAndTest = hasFallaciesTest input [expFallacy]
	where
		-- random expression not containining any fallacies
		andExpr1 = parse "(~a & b) & c"

		-- the 'Affirming a Disjunct' pattern with 'a, b' replaced by 'd, c':
		andExpr2 = parse "(d | c) & d -> ~c"

		input = andExpr1 `conj` andExpr2

		expFallacy = FoundFallacy AffirmDisjunct andExpr2 andExpr2



{-
================================================================================
denyAntecedentRecursOrTest
================================================================================

The pattern for 'Denying the antecedent' fallacy is
(a -> b) & ~a -> ~b

The pattern for 'Affirming the Consequent' fallacy is
(a -> b) & b -> a

This tests if both can be found recusrively if the whole input expression is of
type (expression1 | expression2) with expression1 containging 'Denying the 
antecedent' and expression 2 containing 'Affirming the Consequent'. 
-}

denyAntecedentRecursOrTest = hasFallaciesTest input expFallacies
	where
		-- the 'Denying the antecedent' pattern with 'b' replaced by 'c':
		orExpr1 = parse "((a -> c) & ~a) -> ~c)"

		-- the 'Affirming the Consequent' pattern with 'a, b' inverted:
		orExpr2 = parse "((b -> a) & a) -> b"

		input = orExpr1 `disj` orExpr2

		expFallacies = [
			FoundFallacy DenyAntecedent orExpr1 orExpr1,
			FoundFallacy AffirmConsequent orExpr2 orExpr2
			]


{-
================================================================================
denyAntecedentRecursOrTest
================================================================================

The pattern for 'Denying the antecedent' fallacy is
(a -> b) & ~a -> ~b

This tests if it can be found three levels deep in the recursive input 
expression structure. The latter has the form (expr1 & ~expr2), with 
(expr2 == expr3 -> expr4). expr3 contains the 'Denying the antecedent' fallacy 
pattern.   
-}

denyAntecedent3LevelRecursTest = hasFallaciesTest input [expFallacy]
	where
		-- the Denying the antecedent' fallacy pattern
		expr3 = parse "((a -> b) & ~a) -> ~b"

		-- random expression without any fallacies
		expr4 = parse "~(a | ~a) & b"

		-- building up the tree ...
		expr2 = expr3 `cond` expr4

		-- another random expression without any fallacies
		expr1 = parse "c <-> d"

		-- building up the tree ...
		input = expr1 `conj` (neg expr2)

		expFallacy = FoundFallacy DenyAntecedent expr3 expr3


{-
================================================================================
collection of all tests
================================================================================
-}
tests = TestList [
	TestLabel "affirmDisjunctPosTest" affirmDisjunctPosTest,
	TestLabel "affirmDisjunctNegTest" affirmDisjunctNegTest,
	TestLabel "affirmDisjunctChangedVarsTest" affirmDisjunctChangedVarsTest,
	TestLabel "wrongFormatTest" wrongFormatTest,
	TestLabel "denyAntecedentPosTest" denyAntecedentPosTest,
	TestLabel "affirmConseqPosTest" affirmConseqPosTest,
	--TestLabel "noFallacyTest1" noFallacyTest1,
	TestLabel "affirmConseqPosTest2" affirmConseqPosTest2,
	TestLabel "affirmDisjunctRecursAndTest" affirmDisjunctRecursAndTest,
	TestLabel "denyAntecedentRecursOrTest" denyAntecedentRecursOrTest,
	TestLabel "denyAntecedent3LevelRecursTest" denyAntecedent3LevelRecursTest
	]
