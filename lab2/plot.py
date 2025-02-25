import numpy as np
import matplotlib.pyplot as plt

# ініціалізація інтервалів x та y
x = np.arange(-500, 500)
y = np.arange(-500, 500)
x, y = np.meshgrid(x, y)

# обчислення функції для кожного x та y
z = -x*np.sin(np.sqrt(abs(x))) - y*np.sin(np.sqrt(abs(y)))

# побудова графіка
fig = plt.figure()
ax = fig.add_subplot(projection='3d')
ax.plot_surface(x, y, z, cmap=plt.cm.viridis)

ax.set_xlabel('x')
ax.set_ylabel('y')
ax.set_zlabel('z')

plt.show()


