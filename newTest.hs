affirmFalseFromOurExperiment = assertEqualTest False (isFallacy expr)
	where
		expr_left = (neg a) `conj` b 
		expr_right = neg a 
		expr = expr_left `cond` expr_right


