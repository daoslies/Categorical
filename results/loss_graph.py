import matplotlib.pyplot as plt

# Read losses
with open('results/XOR_losses.txt', 'r') as f:
    losses = [float(line.strip()) for line in f]

# Plot
plt.figure(figsize=(10, 6))
plt.plot(losses, linewidth=2)
plt.xlabel('Epoch', fontsize=12)
plt.ylabel('Loss (MSE)', fontsize=12)
plt.title('XOR Training Loss - Categorical Neural Network', fontsize=14)
plt.grid(True, alpha=0.3)
plt.yscale('log')  # Log scale shows convergence better
plt.tight_layout()
plt.savefig('results/loss_curve.png', dpi=300)
plt.show()

print(f"Final loss: {losses[-1]:.6f}")
print(f"Starting loss: {losses[0]:.6f}")