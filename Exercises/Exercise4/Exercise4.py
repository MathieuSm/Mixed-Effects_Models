import numpy as np
import matplotlib.pyplot as plt

# Define parameters
c = 0
a = 1
b = 1

a2 = 2
b2 = 2
c2 = 1

# Define function
x = np.linspace(0,21,1000)
y = c + a * (b * x)**3 / (np.exp(b * x) - 0.999)
y2 = c + a2 * (b * x)**3 / (np.exp(b * x) - 0.999)
y3 = c + a * (b2 * x)**3 / (np.exp(b2 * x) - 0.999)
y4 = c2 + a * (b * x)**3 / (np.exp(b * x) - 0.999)

# Plot the resulting curve
Figure, Axis = plt.subplots(1,1)
Axis.plot(x, y, color=(0,0,0), label='a=1 b=1 c=0')
Axis.plot(x, y2, color=(1,0,0), label='a=2 b=1 c=0')
Axis.plot(x, y3, color=(0,1,0), label='a=1 b=2 c=0')
Axis.plot(x, y4, color=(0,0,1), label='a=1 b=1 c=1')
plt.legend()
plt.show()
plt.close(Figure)