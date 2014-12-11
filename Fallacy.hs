import Data.Logic.Propositional

var = Variable . Var
neg = Negation
conj = Conjunction
disj = Disjunction
cond = Conditional
iff = Biconditional

p = var 'p'
q = var 'q'
r = var 'r'

fallacy1 = isTautology $ ((p `disj` q) `conj` p) `cond` (neg q)
