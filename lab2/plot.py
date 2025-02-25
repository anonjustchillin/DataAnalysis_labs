import numpy as np
import matplotlib.pyplot as plt
import pandas as pd

fig = plt.figure()
ax = fig.add_subplot(projection='3d')


# dangerous code
# x = np.arange(-500, 500)
# y = np.arange(-500, 500)
# 
# x2 = np.append(0,x.flatten())
# y2 = np.append(0,y.flatten())
# 
# x2,y2 = np.meshgrid(x2,y2)
# 
# z = -x2*np.sin(np.sqrt(abs(x2))) - y2*np.sin(np.sqrt(abs(y2)))
# 
# ax.plot_trisurf(x2.flatten(), y2.flatten(), z.flatten(),
#                 linewidth=0.2, antialiased=True)

x = np.arange(-500, 500)
y = np.arange(-500, 500)
x, y = np.meshgrid(x, y)

z = -x*np.sin(np.sqrt(abs(x))) - y*np.sin(np.sqrt(abs(y)))

# ax.plot(x, y, z)
ax.plot_surface(x, y, z, cmap=plt.cm.viridis)

ax.set_xlabel('x')
ax.set_ylabel('y')
ax.set_zlabel('z')

plt.show()