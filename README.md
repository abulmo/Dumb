# Dumb
A simple &amp; stupid UCI chess engine

Dumb is based on the following algorithms:

 - Bitboard/mailbox legal move generator based on magic bitboard
 - PVS/negascout search with aspiration window
 - simple quiescence search with see pruning
 - simple 4-bucket hash table (no ageing)
 - simple evaluation function with knowledge limited to material, positional and tempo (no pawn structure, mobility, king safety, imbalance, trapped pieces, ...) tuned to probably suboptimal coefficients.
 - simple (insertion) move sorting: hash move + mvvlva ordered capture/promotions + killer moves + quiet moves (no history)
 - null move pruning, razoring and eval pruning (no lmr, lmp, probcut, ...)
 - checking move extension (no singular move extension)
 - can do perft with bulk count and divide
 - include a basic test for the move generator and a bench(mark) to estimate search speed.

Strength:
version 1.5
 expected about +50 elo vs 1.4
version 1.4
 - CCRL rating 40/2: 2359
 - CGET rating 40/4: 2155
version 1.3:
 - CCRL rating 40/2: 2272
 - CGET rating 40/4: 2087
 
