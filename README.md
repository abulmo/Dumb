# Dumb
A simple &amp; stupid UCI chess engine

Dumb is based on the following algorithms:

 - Bitboard/mailbox legal move generator based on hyperbola quintessence
 - PVS/negascout search with aspiration window
 - simple quiescence search (no see pruning)
 - simple 4-bucket hash table (no ageing)
 - simple evaluation function with knowledge limited to material, positional and tempo (no pawn structure, mobility, king safety, imbalance, trapped pieces, ...) tuned to probably suboptimal coefficients.
 - simple (insertion) move sorting: hash move + mvvlva ordered capture/promotions + killer moves + quiet moves (no history)
 - null move pruning, razoring and eval pruning (no lmr, lmp, probcut, ...)
 - checking move extension (no singular move extension)

Strength:
version 1.3:
 - CCRL rating 40/4: 2241
 - CGET rating 40/4: 2053
 
