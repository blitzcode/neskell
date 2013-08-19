
# Test illegal / unofficial LAX opcode
# Skip 0xAB / LAX Immediate, it is not stable
#
# Stack should look like this in the end
# 01F0: 00 00 00 00 DB DB 55 55 FF FF 11 11 C3 C3 21 21

; Setup memory locations

LDA #$21
STA $01

LDA #$C3
STA $15

LDA #$11
STA $2F12

LDA #$FF
STA $2C19

LDA #$CF
STA $20
LDA #$00
STA $21
LDA #$55
STA $CF

LDA #$3C
STA $30
LDA #$30
STA $31
LDA #$DB
STA $303F

; Test all six stable LAX addressing modes, push both registers on the stack

;LAX $01     ; A = X = 0x21
 DCB #$A7
 DCB #$01
PHA
TXA
PHA

LDY #$05
;LAX $10,Y   ; A = X = 0xC3
 DCB #$B7
 DCB #$10
PHA
TXA
PHA

;LAX $2F12   ; A = X = 0x11
 DCB #$AF
 DCB #$12
 DCB #$2F
PHA
TXA
PHA

LDY #$05
;LAX $2C14,Y ; A = X = 0xFF
 DCB #$BF
 DCB #$14
 DCB #$2C
PHA
TXA
PHA

LDX #$01
;LAX ($1F,X) ; A = X = 0x55
 DCB #$A3
 DCB #$1F
PHA
TXA
PHA

LDY #$03
;LAX ($30),Y ; A = X = 0xDB
 DCB #$B3
 DCB #$30
PHA
TXA
PHA

