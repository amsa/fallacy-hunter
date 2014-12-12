import Data.Logic.Propositional

var = Variable . Var
neg = Negation
conj = Conjunction
disj = Disjunction
cond = Conditional
iff = Biconditional

a = var 'a'
p = var 'p'
q = var 'q'
r = var 'r'

affirmingDisjunctLeft = ((p `disj` q) `conj` p)
affirmingDisjunctRight = (neg q)
affirmingDisjunct = affirmingDisjunctLeft `cond` affirmingDisjunctRight

fol1Left = (((p `conj` a) `disj` (q `conj` a)) `conj` (p `conj` a))
fol1Right = ((neg q) `conj` a)
fol1 = fol1Left `cond` fol1Right


fol2 = fol1Left `cond` a

isFallacy :: Expr -> Bool
isFallacy (Conditional left right) =
	isTautology (left `cond` affirmingDisjunctLeft) &&
	isTautology (right `cond` affirmingDisjunctRight)

test1 = isFallacy fol1 -- expected: true
test2 = isFallacy fol2 -- expected: false