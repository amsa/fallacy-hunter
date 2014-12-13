import Test.HUnit
import Fallacy

import Data.Logic.Propositional


--tests = test [ 
--             "test triple" ~: "Triple(Max,is,cat)" ~: "Triple \"Max\" \"is\" \"cat\"" ~=? show(Triple "Max" "is" "cat")
--             ]



{-
========================================================================
affirmDisj_posTest
========================================================================

The pattern for 'Affirming a Disjunct' fallacy is
(a OR b) AND a => NOT b
-}
complexA = a `conj` c `conj` d 				-- reducable to a
complexA2 = a `conj` (c `cond` a) 			-- reducable to a
complexB = b `conj` d 						-- reducable to b
complexNegB = c `conj` (c `cond` (neg b)) 	-- reducable to (NOT b)

fol1_left = (complexA `disj` complexB) `conj` complexA2
fol1_right = complexNegB
fol1 = fol1_left `cond` fol1_right

{-
there is asertBool which has the functionality of 'assertTrue'
but since it is called so confusingly, I preferred the unambiguous
assertEquel True ...
-}
affirmDisj_posTest = TestCase $ assertEqual "" True (isFallacy fol1)


{-
========================================================================
affirmDisj_negTest
========================================================================
-}
complexSomething = d `conj` (c `cond` b) 	-- NOT reducable to b

fol2_left = (complexA `disj` complexB) `conj` complexA2
fol2_right = neg complexSomething
fol2 = fol2_left `cond` fol2_right

affirmDisj_negTest = TestCase $ assertEqual "" False (isFallacy fol2)



{-
========================================================================
affirmDisj_changedVariables_posTest
========================================================================
-}
complexC = c `conj` c `conj` a 				-- reducable to c
complexC2 = c `conj` (c `disj` a) 			-- reducable to c
complexD = a `conj` (a `cond` d)			-- reducable to d
complexNegD = (neg d) `conj` (a `disj` b) 	-- reducable to (NOT d)

fol3_left = (complexC `disj` complexD) `conj` complexC2
fol3_right = complexNegD
fol3 = fol3_left `cond` fol3_right

affirmDisj_changedVariables_posTest = 
	TestCase $ assertEqual "" True (isFallacy fol3)


{-
========================================================================
wrongFormat_Test
========================================================================
A fallacy pattern always has the form (expr1 => expr2).
This tests if the function can handle expressions of a different format
(which are therefore no fallacies) withour raised errors.
-}
fol4 = (a `conj` (neg a))
wrongFormat_Test = TestCase $ assertEqual "" False (isFallacy fol4)




tests = TestList [
	TestLabel "affirmDisj_posTest" affirmDisj_posTest,
	TestLabel "affirmDisj_negTest" affirmDisj_negTest,
	TestLabel "affirmDisj_changedVariables_posTest" affirmDisj_changedVariables_posTest,
	TestLabel "wrongFormat_Test" wrongFormat_Test
    ]
