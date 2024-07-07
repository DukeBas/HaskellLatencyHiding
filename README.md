# Haskell MPC latency hiding
This project contains three modules designed to hide latency of reduce operations where each operator application introduces delay in Haskell. This could be used in secure multi-party computation for example.

## Modules
### `hiding`
The module that implements the latency hiding functions using `Async`. These functions rearrange operations such that communication delays are overlapped in time.

### `delay`
In this module a wrapper is implemented to easily wrap a normal operator (like `+`) to have a specified delay. The delay is passed using a generator function so the delay can be distributed according to any distribution.

### `main`
The main module sets up the benchmarking to verify the implementations.

## Install
This project uses Cabal. To run the code, clone the repository and build using `cabal build` which will resolve dependencies. Alternatively use `cabal repl` to interactively try different parts.

## Results
A jupyter notebook with a simple analysis of the results can be found in the root of the repository.