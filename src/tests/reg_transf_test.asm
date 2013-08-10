
# Test taken from hmc-6502
# http://code.google.com/p/hmc-6502/source/browse/trunk/emu/testvectors/TestAllInstructions/

# Tests instructions for transfering values between registers
# (TAX, TXA, TYA, TAY, DEX, INX, DEY, INY, TXS, TSX).
# Assumes that loads & stores work with all addressing modes.
#
# Expected Results: $40 = 0x33

start:
LDA #$35

TAX
DEX
DEX
INX
TXA

TAY
DEY
DEY
INY
TYA

TAX
LDA #$20
TXS
LDX #$10
TSX
TXA

STA $40
