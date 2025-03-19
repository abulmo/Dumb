# Dumb
A simple &amp; (not so) stupid UCI chess engine

Dumb is based on the following algorithms:

 - Bitboard/mailbox pseudo-legal move generator based on magic bitboards
 - PVS/negascout search with aspiration windows
 - simple quiescence search with see pruning
 - simple 4-bucket hash table (with ageing)
 - simple evaluation function with knowledge limited to material, positional and tempo (no pawn structure, mobility, king safety, imbalance, trapped pieces, ...) tuned to probably suboptimal coefficients.
 - simple (insertion) move sorting: hash move + mvvlva ordered good capture/promotions + killer moves + quiet moves (with history) + bad captures
 - null move pruning, razoring and eval pruning, LMR, IIR, probcut
 - checking move extension (no singular move extension)
 - can do perft and divide
 - include a basic bench(mark) to estimate its search speed.
 - can show the board and/or the eval

Strength:

version  2.1
 - CCRL rating Blitz 2+1: 2829
 - CEGT rating 40/4: 2677

version 2.0
 - CCRL rating Blitz 2+1: 2743
 - CEGT rating 40/4: 2609

version 1.9-1.11
 - CCRL rating Blitz 2+1: 2694
 - CEGT rating 40/4: 2526

