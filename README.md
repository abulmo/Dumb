# Dumb
A simple &amp; (not so) stupid UCI chess engine

Dumb is based on the following algorithms:

 - Bitboard/mailbox pseudo-legal move generator based on hyperbola quintessence
 - PVS/negascout search with aspiration windows
 - simple quiescence search with see pruning
 - simple 4-bucket hash table (no ageing)
 - simple evaluation function with knowledge limited to material, positional and tempo (no pawn structure, mobility, king safety, imbalance, trapped pieces, ...) tuned to probably suboptimal coefficients.
 - simple (insertion) move sorting: hash move + mvvlva ordered good capture/promotions + killer moves + quiet moves (with history) + bad captures
 - null move pruning, razoring and eval pruning, LMR, IIR
 - checking move extension (no singular move extension)
 - can do perft and divide
 - include a basic bench(mark) to estimate its search speed.

Strength:

version 2.0
 - CCRL rating Blitz 2+1: 2763
 - CEGT rating 40/4: 2607

version 1.9-1.11
 - CCRL rating Blitz 2+1: 2617-2618
 - CEGT rating 40/4: 2525

