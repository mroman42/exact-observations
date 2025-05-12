import sympy as sp

# Example: Define a symbolic variable
x = sp.Symbol('x')

# Example: Perform a symbolic computation
def expression(u): return sp.sin(u)**2 - sp.cos(u)**2
simplified_expression = sp.simplify(expression(x))
integral = sp.integrate(simplified_expression, x)

print(f"Simplified Expression: {simplified_expression}")
print(f"Integral: {integral}")

# Example: Solve an equation
equation = sp.Eq(sp.sin(x), 0)
solution = sp.solve(equation, x)
print(f"Solution to the equation sin(x) = 0: {solution}")


def f(u1,u2,u3,u4): return sp.sin(u1) + sp.cos(u3)

x = sp.Symbol('x')
y = sp.Symbol('y')
z = sp.Symbol('z')
v = sp.Symbol('v')

f0 = sp.simplify(f(y,2.1,v,x))
w0 = sp.integrate(f(y,2.1,v,x), v)
print( w0 )