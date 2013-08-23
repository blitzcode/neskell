
; Tests from http://www.6502.org/tutorials/decimal_mode.html
;
; After the test, 0x87 0x91 0x29 0x27 0x34 0x73 0x41 0x46 0x05 should be on the stack
;
; Some CLC/SEC instructions are commented out to test that the carry is
; correctly set by the predecessor, comment back in to make all tests standalone

SED

; BCD addition: 58 + 46 + 1 = 105
SEC      ; Note: carry is set, not clear!
LDA #$58
ADC #$46 ; After this instruction, C = 1, A = $05
PHA

; BCD addition: 12 + 34 = 46
CLC
LDA #$12
ADC #$34 ; After this instruction, C = 0, A = $46
PHA

; BCD addition: 15 + 26 = 41
;CLC
LDA #$15
ADC #$26 ; After this instruction, C = 0, A = $41
PHA

; BCD addition: 81 + 92 = 173
;CLC
LDA #$81
ADC #$92 ; After this instruction, C = 1, A = $73
PHA

; BCD subtraction: 46 - 12 = 34
;SEC
LDA #$46
SBC #$12 ; After this instruction, C = 1, A = $34
PHA

; BCD subtraction: 40 - 13 = 27
;SEC
LDA #$40
SBC #$13 ; After this instruction, C = 1, A = $27
PHA

; BCD subtraction: 32 - 2 - 1 = 29
CLC      ; Note: carry is clear, not set!
LDA #$32
SBC #$02 ; After this instruction, C = 1, A = $29
PHA

; BCD subtraction: 12 - 21
;SEC
LDA #$12
SBC #$21 ; After this instruction, C = 0, A = $91
PHA

; BCD subtraction: 21 - 34
SEC
LDA #$21
SBC #$34 ; After this instruction, C = 0, A = $87
PHA

