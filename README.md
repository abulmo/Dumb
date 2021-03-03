# Dumber
A simple &amp; stupid UCI chess engine

Dumb is based on the following algorithms:

 - Bitboard/mailbox legal move generator based on hyperbola quintessence
 - plain alphabeta
 - simple quiescence search
 - can do perft with bulk count and divide
 - include a basic test for the move generator and a bench(mark) to estimate search speed.
 - No hashtable, No sophisticated pruning, No sophisticated move sorting.
 

Strength:

version 1.0
 expected -1000 elo vs dumb 1.8 so around 1650 Elo on CCRL.


