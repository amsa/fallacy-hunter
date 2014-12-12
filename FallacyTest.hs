import Test.HUnit
import Fallacy

import Data.Logic.Propositional


--tests = test [ 
--             "test triple" ~: "Triple(Max,is,cat)" ~: "Triple \"Max\" \"is\" \"cat\"" ~=? show(Triple "Max" "is" "cat")
--             ]



{-
========================================================================
affirmingDisjunct_pos_test
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
affirmingDisjunct_pos_test = TestCase $ assertEqual "" True (isFallacy fol1)


{-
========================================================================
affirmingDisjunct_neg_test
========================================================================
-}
complexNotB = d `conj` (c `cond` b) 	-- NOT reducable to b

fol2_left = (complexA `disj` complexB) `conj` complexA2
fol2_right = neg complexNotB
fol2 = fol2_left `cond` fol2_right

affirmingDisjunct_neg_test = TestCase $ assertEqual "" False (isFallacy fol2)




tests = TestList [
	TestLabel "affirmingDisjunct_pos_test" affirmingDisjunct_pos_test,
	TestLabel "affirmingDisjunct_neg_test" affirmingDisjunct_neg_test
	]