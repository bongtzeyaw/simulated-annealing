# Overview
A project of Ocaml Programming to implement Taboo Research (or Simulated Annealing), one of the fundamental meta-heuristic methods. The goal is to find the solution to a classic problem of optimisation, Airport Gate Allocation problem.

# Modelisation
A gate is only compatible with certain types of aircraft. Suppose that we have n gates, a solution of Airport Gate Allocation Problem is characterised by n sequences of flights, separated in time with at least a time minimum separation. 

# Simulated annealing
A new solution can be created by either swapping two sub-sequences of flight of fixed length (while assuring gate compatibility) or doing the same for variable length. The evaluation of solution can be either the sum of all time gaps or the minimum of the minimum time gaps for each sequence of flight. These different options have different impacts on the quality of the solution.
