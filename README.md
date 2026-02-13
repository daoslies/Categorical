# Categorical

A minimal neural network library in Haskell that makes the categorical structure of deep learning explicit in the type system.

## To Run

'''
cabal build all && cabal run all
'''

## Motivation
- Layers are morphisms between vector spaces
- Networks compose categorically
- Training and transformations are functorial
- Pedagogical: connects category theory to working neural networks

## Features
- Dense layers with biases
- ReLU and sigmoid activations
- Backpropagation and gradient descent
- Explicit parameter handling (no hidden closures)
- XOR classification demo

## Example network construction
```haskell
network =
  dense 2 4 w1
  >>> relu
  >>> dense 4 1 w2
  >>> sigmoid
```

## Project Structure
- Category.hs   – Core categorical abstractions
- Layer.hs      – Layers as morphisms
- Functor.hs    – Structure-preserving transformations
- Training.hs   – Gradient descent loop
- Example.hs    – XOR demo

## Result
The example network learns XOR from scratch:


![XOR training loss curve](results/loss_curve.png)

```

Final loss: 1.117670549365512e-5

[0.0,0.0] -> [4.871061614493543e-3]  → ~0 (expected: [0.0])
[0.0,1.0] -> [0.9973112784063706]    → ~1 (expected: [1.0])
[1.0,0.0] -> [0.9973146064307177]    → ~1 (expected: [1.0])
[1.0,1.0] -> [2.554644052148444e-3]  → ~0 (expected: [0.0])

```

Demonstrates correct gradient flow through composed morphisms.
