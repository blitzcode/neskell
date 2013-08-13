
# Simple test checking if we correctly reach branch targets with negative
# offsets. We had that bug, and the other branch tests don't cover it
#
# Expected Results: X = $FF

LDX #$05

loop:
DEX
BPL loop

