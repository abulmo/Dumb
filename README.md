# Dumb
A simple &amp; (not so) stupid UCI chess engine

Dumb is based on the following algorithms:

 - Bitboard/mailbox legal move generator based on hyperbola quintessence
 - PVS/negascout search with aspiration windows
 - simple quiescence search with see pruning
 - simple 4-bucket hash table (no ageing)
 - simple evaluation function with knowledge limited to material, positional and tempo (no pawn structure, mobility, king safety, imbalance, trapped pieces, ...) tuned to probably suboptimal coefficients.
 - simple (insertion) move sorting: hash move + mvvlva ordered good capture/promotions + killer moves + quiet moves (with history) + bad captures
 - null move pruning, razoring and eval pruning, LMR, IIR
 - checking move extension (no singular move extension)
 - can do perft with bulk count and divide
 - include a basic bench(mark) to estimate its search speed.

Strength:

version 1.9
 - CCRL rating 40/2: 2698
 - CEGT rating 40/5: 2524
  
version 1.8
 - CCRL rating 40/2: 2680
 - CEGT rating 40/5: 2516

version 1.5
 - CCRL rating 40/2: 2592
 
version 1.4
 - CCRL rating 40/2: 2357
 - CGET rating 40/4: 2146

version 1.3:
 - CCRL rating 40/2: 2271
 - CGET rating 40/4: 2087
 
