# Haskell-Practice
Practicing some Haskell ideas

# Queue.hs
Implements deque in Haskell with amortised constant time operation  
Idea from https://rafal.io/posts/haskell-queues.html  

# NumStruc.hs/NumStrucExamples.hs
Practising the usage of State Monad, lazy IO and strict IO  
Computes the infinite list of any accumulative integral list, where we can determine if n is an element of the list when we all the elements smaller than n  
As an example, prints out infinite list of primes and Ulam Numbers using a custom filter function with accumulators  
* The list of Ulam Number starts with 1, 2 and all other Ulam numbers can be expressed as the sum of two distinct ulam numbers in exactly one way  
