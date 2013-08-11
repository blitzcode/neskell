
# Test taken from hmc-6502
# http://code.google.com/p/hmc-6502/source/browse/trunk/emu/testvectors/TestAllInstructions/

# Tests instructions ADC & SBC with all addressing modes.
# Assumes that loads & stores work with all addressing modes.
#
# Expected Results: $30=0x9D

start:
LDA #$6A
STA $50
LDA #$6B
STA $51
LDA #$A1
STA $60
LDA #$A2
STA $61

LDA #$FF
ADC #$FF
ADC #$FF
SBC #$AE

STA $40
LDX $40
ADC $00,X
SBC $01,X

ADC $60
SBC $61

STA $0120
LDA #$4D
STA $0121
LDA #$23
ADC $0120
SBC $0121

STA $F0
LDX $F0
LDA #$64
STA $0124
LDA #$62
STA $0125
LDA #$26
ADC $0100,X
SBC $0101,X

STA $F1
LDY $F1
LDA #$E5
STA $0128
LDA #$E9
STA $0129
LDA #$34
ADC $0100,Y
SBC $0101,Y

STA $F2
LDX $F2
LDA #$20
STA $70
LDA #$01
STA $71
LDA #$24
STA $72
LDA #$01
STA $73
ADC ($41,X)
SBC ($3F,X)

STA $F3
LDY $F3
LDA #$DA
STA $80
LDA #$00
STA $81
LDA #$DC
STA $82
LDA #$00
STA $83
LDA #$AA
ADC ($80),Y
SBC ($82),Y
STA $30
