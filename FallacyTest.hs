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
========================================================================
assertEqualTest
========================================================================

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
========================================================================
containsFallacyTest
========================================================================

Creates a test which expects the given input expression to contain the 
given fallacy with the given fallacy pattern (expressing the variable 
mapping)

parameters:
	
	Expr:	the expression to be given as input to findFallacies
	
	FoundFallacy: the expected FoundFallacy to be contained in the 
		findFallacies output
	
returns:

	Test:	a test which succeeds if isFallacy output contains the expected
		FoundFallacy
-}
containsFallacyTest :: Expr -> FoundFallacy -> Test
containsFallacyTest inputExpr expFoundFallacy = 
	assertEqualTest True $ elem expFoundFallacy $ findFallacies inputExpr


{-
========================================================================
parse
========================================================================

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
========================================================================
affirmDisjunct_posTest
========================================================================

The pattern for 'Affirming a Disjunct' fallacy is
(a OR b) AND a => NOT b

there is asertBool which has the functionality of 'assertTrue'
but since it is called so confusingly, I preferred the unambiguous
assertEquel True ...
-}
affirmDisjunct_posTest = containsFallacyTest expr expFoundFallacy
	where
		complexA = parse "(a & c) & d"			-- reducable to a
		complexA2 = parse "a & (c -> a)" 		-- reducable to a
		complexB = parse "b & d"				-- reducable to b
		complexNegB = parse "c & (c -> ~b)" 	-- reducable to (NOT b)
		
		expr_left = (complexA `disj` complexB) `conj` complexA2
		expr_right = complexNegB
		expr = expr_left `cond` expr_right

		expFalExpr = parse "((a | b) & a) -> ~b"

		expFoundFallacy = FoundFallacy AffirmDisjunct expFalExpr expr


{-
========================================================================
affirmDisjunct_negTest
========================================================================
-}
affirmDisjunct_negTest = assertEqualTest False (isFallacy expr)
	where
		complexA = parse "(a & c) & d" 				-- reducable to a
		complexA2 = parse "a & (c -> a)" 			-- reducable to a
		complexB = parse "b & d" 					-- reducable to b
		complexSomething = parse "d & (c -> b)" 	-- NOT reducable to b

		expr_left = (complexA `disj` complexB) `conj` complexA2
		expr_right = neg complexSomething
		expr = expr_left `cond` expr_right



{-
========================================================================
affirmDisjunct_changedVars_posTest
========================================================================
The pattern for 'Affirming a Disjunct' fallacy is
(a OR b) AND a => NOT b

This tests if the fallacy detection also works with
(c OR d) AND c => NOT d
-}
affirmDisjunct_changedVars_posTest = assertEqualTest True (isFallacy expr)
	where
		complexC = parse "(c & c) & a"		-- reducable to c
		complexC2 = parse "c & (c | a)" 	-- reducable to c
		complexD = parse "a & (a -> d)"		-- reducable to d
		complexNegD = parse "~d & (a | b)" 	-- reducable to (NOT d)
		
		expr_left = (complexC `disj` complexD) `conj` complexC2
		expr_right = complexNegD
		expr = expr_left `cond` expr_right


{-
========================================================================
wrongFormat_Test
========================================================================
A fallacy pattern always has the form (expr1 => expr2).
This tests if the function can handle expressions of a different format
(which are therefore no fallacies) withour raising errors.
-}
wrongFormat_Test = assertEqualTest False $ isFallacy $ parse "a & ~a"


{-
========================================================================
denyAntecedent_posTest
========================================================================

The pattern for 'Denying the antecedent' fallacy is
(a => b) AND (NOT a) => NOT b
-}

denyAntecedent_posTest = assertEqualTest True (isFallacy expr)
	where
		complexA = parse "(a & a) & a" 	-- reducable to a
		complexNegA = parse "~(c | a)" 	-- reducable to (NOT a)
		complexB = parse "~(~b | d)"	-- reducable to b
		complexNegB = parse "~ ~ ~b"	-- reducable to (NOT b)

		expr_left = (complexA `cond` complexB) `conj` complexNegA
		expr_right = complexNegB
		expr = expr_left `cond` expr_right



{-
========================================================================
affirmConseq_posTest
========================================================================

The pattern for 'Affirming the consequent' fallacy is
(a => b) AND b => a
-}

affirmConseq_posTest = assertEqualTest True (isFallacy expr)
	where
		complexA = parse "~ ~a" 		 -- reducable to a
		complexA2 = parse "~(c | ~a)" 	 -- reducable to a
		complexB = parse "~(~b | d)" 	 -- reducable to b
		complexB2 = parse "(b | c) & ~c" -- reducable to b

		expr_left = (complexA `cond` complexB) `conj` complexB2
		expr_right = complexA2 `conj` complexB
		expr = expr_left `cond` expr_right


{-
========================================================================
affirmConseq_posTest2
========================================================================

The pattern for 'Affirming the consequent' fallacy is
(a => b) AND b => a

Since some expressions, like the one in affirmConseq_posTest, contain both, 
'Affirming the consequent' fallacy and 'Denying the antecedent' fallacy,
this tests "pure" 'Affirming the consequent' fallacy (which is not 'Denying 
the antecedent' fallacy)
-}

affirmConseq_posTest2 = assertEqualTest True (isFallacy expr)
	where
		expr = parse "((a -> b) & b) -> a"


{-
========================================================================
noFallacyTest1
========================================================================

In one of our experiments the following expression was wrongly classified 
as fallacy:
(NOT a) AND b => not a
-}
noFallacyTest1 = assertEqualTest False (isFallacy expr)
	where
		expr = parse "(~a & b) -> ~a"



{-
========================================================================
collection of all tests
========================================================================
-}
tests = TestList [
	TestLabel "affirmDisjunct_posTest" affirmDisjunct_posTest,
	TestLabel "affirmDisjunct_negTest" affirmDisjunct_negTest,
	TestLabel "affirmDisjunct_changedVars_posTest" affirmDisjunct_changedVars_posTest,
	TestLabel "wrongFormat_Test" wrongFormat_Test,
	TestLabel "denyAntecedent_posTest" denyAntecedent_posTest,
	TestLabel "affirmConseq_posTest" affirmConseq_posTest,
	TestLabel "affirmConseq_posTest2" affirmConseq_posTest2,
	TestLabel "noFallacyTest1" noFallacyTest1
	]
