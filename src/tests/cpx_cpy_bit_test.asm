
# Test taken from hmc-6502
# http://code.google.com/p/hmc-6502/source/browse/trunk/emu/testvectors/TestAllInstructions/

# Tests instructions CPX & CPY & BIT for all addressing modes.
# Assumes that loads & stores (with all addressing modes) & BEQ & BNE work.
# Also assumes that AND & ORA & EOR work with all addressing modes.
#
# Expected Results: $42 = 0xA5

start:
; prepare memory
LDA #$A5
STA $20
STA $0120
LDA #$5A
STA $21

; cpx imm...
LDX #$A5
CPX #$A5
BEQ b1 ; taken
LDX #$01 ; not done
b1:
; cpx zpg...
CPX $20
BEQ b2 ; taken
LDX #$02 ; not done
b2:
; cpx abs...
CPX $0120
BEQ b3 ; taken
LDX #$03 ; not done
b3:
; cpy imm...
STX $30
LDY $30
CPY #$A5
BEQ b4 ; taken
LDY #$04 ; not done
b4:
; cpy zpg...
CPY $20
BEQ b5 ; taken
LDY #$05 ; not done
b5:
; cpy abs...
CPY $0120
BEQ b6 ; taken
LDY #$06 ; not done
b6:
; bit zpg...
STY $31
LDA $31
BIT $20
BNE b7 ; taken
LDA #$07 ; not done
b7:
; bit abs...
BIT $0120
BNE b8 ; taken
LDA #$08 ; not done
b8:
BIT $21
BNE b9 ; not taken
STA $42
b9:

