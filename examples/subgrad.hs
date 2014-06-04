import HVX
import Numeric.LinearAlgebra

-- declare a symbolic variable
x = EVar "x"
-- give x a value (a 4-element column vector)
vars = [("x", (4><1) [0,-3,1,2])]
-- define the expression to sudifferentiate: max(abs(x))
myexpr = hmax(habs(x))
-- compute the subgradient
mysubgrad = jacobianWrtVar myexpr vars "x"
