
; Test the decimal mode of the illegal ARR opcode. The operand for all test
; cases is FF as we're not interested in testing the AND / ZN flag behavior,
; only the unusual VC flags and BCD fixup that happens after the AND+ROL.
;
; Expected Result: SP = $E7
;
; After the test the stack should look like this:
;
;      0  1  2  3  4  5  6  7  8  9  A  B  C  D  E  F
; 01E0                         3C 0D 3D D8 3D D5 7D 80
; 01F0 3E 00 3D D5 BC 8D BD 58 BD 55 FD 00 BC 80 BD 55
;
; Reference from VICE. Test also works in Visual 6502 - it doesn't model the
; AND correctly, but since the operand is always 0xFF it's a no-op anyway

LDX #$FF
TXS

SED

SEC
LDA #$FF
;ARR #$FF
 DCB $6B
 DCB $FF
PHA ; $01FF
PHP ; $01FE

SEC
LDA #$00
;ARR #$FF
 DCB $6B
 DCB $FF
PHA ; $01FD
PHP ; $01FC

SEC
LDA #$55
;ARR #$FF
 DCB $6B
 DCB $FF
PHA ; $01FB
PHP ; $01FA

SEC
LDA #$FE
;ARR #$FF
 DCB $6B
 DCB $FF
PHA ; $01F9
PHP ; $01F8

SEC
LDA #$F0
;ARR #$FF
 DCB $6B
 DCB $FF
PHA ; $01F7
PHP ; $01F6

SEC
LDA #$0F
;ARR #$FF
 DCB $6B
 DCB $FF
PHA ; $01F5
PHP ; $01F4

CLC
LDA #$FF
;ARR #$FF
 DCB $6B
 DCB $FF
PHA ; $01F3
PHP ; $01F2

CLC
LDA #$00
;ARR #$FF
 DCB $6B
 DCB $FF
PHA ; $01F1
PHP ; $01F0

CLC
LDA #$55
;ARR #$FF
 DCB $6B
 DCB $FF
PHA ; $01EF
PHP ; $01EE

CLC
LDA #$FE
;ARR #$FF
 DCB $6B
 DCB $FF
PHA ; $01ED
PHP ; $01EC

CLC
LDA #$F0
;ARR #$FF
 DCB $6B
 DCB $FF
PHA ; $01EB
PHP ; $01EA

CLC
LDA #$0F
;ARR #$FF
 DCB $6B
 DCB $FF
PHA ; $01E9
PHP ; $01E8

