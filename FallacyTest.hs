import Test.HUnit
import Fallacy

import Data.Logic.Propositional


--tests = test [ 
--             "test triple" ~: "Triple(Max,is,cat)" ~: "Triple \"Max\" \"is\" \"cat\"" ~=? show(Triple "Max" "is" "cat")
--             ]


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
affirmDisjunct_posTest
========================================================================

The pattern for 'Affirming a Disjunct' fallacy is
(a OR b) AND a => NOT b

there is asertBool which has the functionality of 'assertTrue'
but since it is called so confusingly, I preferred the unambiguous
assertEquel True ...
-}
affirmDisjunct_posTest = assertEqualTest True (isFallacy expr)
	where
		complexA = a `conj` c `conj` d 				-- reducable to a
		complexA2 = a `conj` (c `cond` a) 			-- reducable to a
		complexB = b `conj` d 						-- reducable to b
		complexNegB = c `conj` (c `cond` (neg b)) 	-- reducable to (NOT b)
		
		expr_left = (complexA `disj` complexB) `conj` complexA2
		expr_right = complexNegB
		expr = expr_left `cond` expr_right


{-
========================================================================
affirmDisjunct_negTest
========================================================================
-}
affirmDisjunct_negTest = assertEqualTest False (isFallacy expr)
	where
		complexA = a `conj` c `conj` d 				-- reducable to a
		complexA2 = a `conj` (c `cond` a) 			-- reducable to a
		complexB = b `conj` d 						-- reducable to b
		complexSomething = d `conj` (c `cond` b) 	-- NOT reducable to b

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
		complexC = c `conj` c `conj` a 				-- reducable to c
		complexC2 = c `conj` (c `disj` a) 			-- reducable to c
		complexD = a `conj` (a `cond` d)			-- reducable to d
		complexNegD = (neg d) `conj` (a `disj` b) 	-- reducable to (NOT d)
		
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
wrongFormat_Test = assertEqualTest False (isFallacy expr)
	where
		expr = (a `conj` (neg a))


{-
========================================================================
denyAntecedent_posTest
========================================================================

The pattern for 'Denying the antecedent' fallacy is
(a => b) AND (NOT a) => NOT b
-}

denyAntecedent_posTest = assertEqualTest True (isFallacy expr)
	where
		complexA = a `conj` a `conj` a 		-- reducable to a
		complexNegA = neg (c `disj` a) 		-- reducable to (NOT a)
		complexB = neg ((neg b) `disj` d) 	-- reducable to b
		complexNegB = neg $ neg $ neg b 	-- reducable to (NOT b)

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
		complexA = neg $ neg $ a 					-- reducable to a
		complexA2 = neg (c `disj` (neg a)) 			-- reducable to a
		complexB = neg ((neg b) `disj` d) 			-- reducable to b
		complexB2 = (b `disj` c) `conj` (neg c) 	-- reducable to b

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
		expr_left = (a `cond` b) `conj` b
		expr_right = a
		expr = expr_left `cond` expr_right





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
	TestLabel "affirmConseq_posTest2" affirmConseq_posTest2
	]