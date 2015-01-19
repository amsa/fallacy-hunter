module LogicalShortcuts where

import Data.Logic.Propositional
import Prelude

var = Variable . Var
neg = Negation
conj = Conjunction
disj = Disjunction
cond = Conditional
iff = Biconditional
