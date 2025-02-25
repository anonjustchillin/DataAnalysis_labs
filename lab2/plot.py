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



# Load the data
dataset_size = np.size(x)

# Extract the target and the feature
xy = z
np.random.shuffle(xy)  # Shuffle the data
xy = xy[:dataset_size]

# Normalize the data
mean = np.mean(xy, axis=0)
std = np.std(xy, axis=0)
xy_normalized = (xy - mean) / std

# Split the data
train_size = int(0.8 * dataset_size)
train_xy, test_xy = xy_normalized[:train_size], xy_normalized[train_size:]


def model(m, x, b):
    """Simple linear model"""
    return m * x + b


def loss(y_true, y_pred):
    """Mean squared error"""
    return np.mean((y_true - y_pred) ** 2)


def stochastic_gradient_descent(
        x, y, epochs=100, learning_rate=0.01, batch_size=32, stopping_threshold=1e-6
):
    """
    SGD with support for mini-batches and gradient clipping.
    """
    # Initialize the model parameters randomly
    m = np.random.randn()
    b = np.random.randn()

    n = len(x)
    previous_loss = np.inf

    for i in range(epochs):
        # Shuffle the data
        indices = np.random.permutation(n)
        x = x[indices]
        y = y[indices]

        for j in range(0, n, batch_size):
            x_batch = x[j : j + batch_size]
            y_batch = y[j : j + batch_size]

            # Compute the gradients
            y_pred = model(m, x_batch, b)
            m_gradient = 2 * np.mean(x_batch * (y_batch - y_pred))
            b_gradient = 2 * np.mean(y_batch - y_pred)

            # Gradient clipping
            clip_value = 1.0
            m_gradient = np.clip(m_gradient, -clip_value, clip_value)
            b_gradient = np.clip(b_gradient, -clip_value, clip_value)

            # Update the model parameters
            m -= learning_rate * m_gradient
            b -= learning_rate * b_gradient

        # Compute the loss
        y_pred = model(m, x, b)
        current_loss = loss(y, y_pred)

        if abs(previous_loss - current_loss) < stopping_threshold:
            break

        previous_loss = current_loss

    return m, b


# Find the optimal parameters to m and b with SGD
m, b = stochastic_gradient_descent(
    train_xy[:, 0], train_xy[:, 1], learning_rate=0.1, epochs=10000, batch_size=1024
)

# Make predictions
y_preds = model(m, test_xy[:, 0], b)

# Compute and print the loss
mean_squared_error = loss(test_xy[:, 1], y_preds)

print(f"Normalized RMSE: {mean_squared_error**0.5}")

# Denormalize the predictions and compute the actual RMSE
y_preds_denormalized = y_preds * std[1] + mean[1]
y_true_denormalized = test_xy[:, 1] * std[1] + mean[1]
actual_mse = np.mean((y_true_denormalized - y_preds_denormalized) ** 2)

print(f"Actual RMSE: {actual_mse**0.5}")

