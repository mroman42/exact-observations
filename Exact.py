import sympy as sp
import numpy as np
import matplotlib.pyplot as plt

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

def normal_distribution(x, mu, sigma):
    return (1/(sigma * sp.sqrt(2 * sp.pi))) * sp.exp(-0.5 * ((x - mu)/sigma)**2)


def f(u1,u2,u3,u4): return sp.sin(u1) + sp.cos(u3)

x = sp.Symbol('x')
y = sp.Symbol('y')
z = sp.Symbol('z')
v = sp.Symbol('v')
m = sp.Symbol('m')

f0 = sp.simplify(f(y,2.1,v,x))
w0 = sp.integrate(f(1,2.1,v,x), v)
w1 = (normal_distribution(2.1,m,1.0))/( sp.integrate(normal_distribution(v,m,1.0), (v, -10, 10)))

print( w0 )

# Convert the symbolic expression w1 into a numerical function
f_numeric = sp.lambdify(m, w1, modules="numpy")
f_numeric_vectorized = np.vectorize(f_numeric)
m_values = np.linspace(-10, 10, 500)  
f_values = f_numeric_vectorized(m_values)

# Plot the results
plt.figure(figsize=(8, 6))
plt.plot(m_values, f_values, label='w0(m)')
plt.title('Plot of w0(m)')
plt.xlabel('m')
plt.ylabel('w0(m)')
plt.legend()
plt.grid(True)
plt.show()