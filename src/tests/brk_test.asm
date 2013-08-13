
# BRK interrupt test
#
# Expected Value: $FF = $44

JMP start ; Keep ISR at a fixed address when we change the program

ISR: ; $0603
DEC $FF
RTI

start:

; Setup interrupt vector
LDA #$03
STA $FFFE
LDA #$06
STA $FFFF

LDA #$45
STA $FF

; -1
BRK
NOP
INC $FF
BRK
NOP

; Check if carry is preserved across interrupt
LDA #$00
SEC
BRK
NOP
ADC $FF
STA $FF

NOP ; Can't use the usual BRK/0x00 for the stopping condition

