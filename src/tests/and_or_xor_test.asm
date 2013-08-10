
# Test taken from hmc-6502
# http://code.google.com/p/hmc-6502/source/browse/trunk/emu/testvectors/TestAllInstructions/

# Tests instructions AND & EOR & ORA with all addressing modes.
# Assumes that LDA/LDX/LDY & STA/STX/STY work with all addressing modes.
#
# Expected Results:
#
# $A9 = 0xAA

start:
; imm
LDA #85
AND #83
ORA #56
EOR #17

; zpg
STA $99
LDA #185
STA $10
LDA #231
STA $11
LDA #57
STA $12
LDA $99
AND $10
ORA $11
EOR $12

; zpx
LDX #16
STA $99
LDA #188
STA $20
LDA #49
STA $21
LDA #23
STA $22
LDA $99
AND $10,X
ORA $11,X
EOR $12,X

; abs
STA $99
LDA #111
STA $0110
LDA #60
STA $0111
LDA #39
STA $0112
LDA $99
AND $0110
ORA $0111
EOR $0112

; abx
STA $99
LDA #138
STA $0120
LDA #71
STA $0121
LDA #143
STA $0122
LDA $99
AND $0110,X
ORA $0111,X
EOR $0112,X

; aby
LDY #32
STA $99
LDA #115
STA $0130
LDA #42
STA $0131
LDA #241
STA $0132
LDA $99
AND $0110,Y
ORA $0111,Y
EOR $0112,Y

; idx
STA $99
LDA #112
STA $30
LDA #$01
STA $31
LDA #113
STA $32
LDA #$01
STA $33
LDA #114
STA $34
LDA #$01
STA $35
LDA #197
STA $0170
LDA #124
STA $0171
LDA #161
STA $0172
LDA $99
AND ($20,X)
ORA ($22,X)
EOR ($24,X)

; idy
STA $99
LDA #96
STA $40
LDA #$01
STA $41
LDA #97
STA $42
LDA #$01
STA $43
LDA #98
STA $44
LDA #$01
STA $45
LDA #55
STA $0250
LDA #35
STA $0251
LDA #157
STA $0252
LDA $99
LDY #$F0
AND ($40),Y
ORA ($42),Y
EOR ($44),Y

STA $A9
