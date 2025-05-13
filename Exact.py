import sympy as sp
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.colors import LogNorm  # Import LogNorm from matplotlib.colors


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

# Define the symbolic variables
x = sp.Symbol('x')
t = sp.Symbol('t')

# Define the normal distribution function
def normal_distribution(x, mu, sigma):
    return (1 / (sigma * sp.sqrt(2 * sp.pi))) * sp.exp(-0.5 * ((x - mu) / sigma) ** 2)

# Define the function w0 depending on x and t
w0 = normal_distribution(x, 0, t)

# Convert the symbolic expression w0 into a numerical function
w0_numeric = sp.lambdify((x, t), w0, modules="numpy")

# Define ranges for x and t
x_values = np.linspace(-10, 10, 500)  # Range for x
t_values = np.linspace(0.1, 5, 500)  # Range for t (avoid t=0 to prevent division by zero)

# Create a meshgrid for x and t
X, T = np.meshgrid(x_values, t_values)

# Evaluate w0 over the grid
W0_values = w0_numeric(X, T)

# Plot the heat map
# plt.figure(figsize=(10, 8))
# plt.contourf(X, T, W0_values, levels=50, cmap="viridis")
# plt.colorbar(label="w0(x, t)")
# plt.title("Heat Map of w0(x, t)")
# plt.xlabel("x")
# plt.ylabel("t")
# plt.show()

# Plot the heat map with swapped axes and logarithmic color scale
plt.figure(figsize=(10, 8))
# Swap axes by passing T as the x-axis and X as the y-axis
plt.contourf(T, X, W0_values, levels=50, cmap="viridis")
plt.colorbar(label="log(w0(x, t))")
plt.title("Heat Map of w0(x, t)")
plt.xlabel("t")
plt.ylabel("x")
plt.show()