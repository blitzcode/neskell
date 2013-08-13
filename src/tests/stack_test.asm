
# Test taken from hmc-6502
# http://code.google.com/p/hmc-6502/source/browse/trunk/emu/testvectors/TestAllInstructions/

# Tests stack instructions (PHA & PLA & PHP & PLP).
# Assumes that loads & stores (all addressing modes).
# Also assumes ADC (all addressing modes) and all flag instructions work.
#
# Expected Results: $30 = 0x29

LDA #$27
ADC #$01
SEC
PHP
CLC
PLP
ADC #$00
PHA
LDA #$00
PLA
STA $30

