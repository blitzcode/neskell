
; Test AHX,TAS,SHX,SHY (the & ADDR_HI instructions) in all addressing modes
;
; Expected Result: SP = $F2
;
; After the test the stack should look like this:
;
;      0  1  2  3  4  5  6  7  8  9  A  B  C  D  E  F
; 01F0          01 C9 01 80 C0 E0 01 55 80 80 01 34 10
;
; (Verified against Visual 6502 & VICE)

LDX #$FF
TXS

; AHX - Store A & X & (ADDR_HI + 1) into M
; ----------------------------------------

LDA #$09
STA $10
LDA #$D0
STA $11
LDA #$BA
LDX #$76
LDY #$01
;AHX ($10),Y
 DCB $93
 DCB $10
LDA $D00A
PHA ; $01FF

LDA #$BB
STA $10
LDA #$00
STA $11
LDA #$55
LDX #$FF
LDY #$01
;AHX ($10),Y
 DCB $93
 DCB $10
; Push the flags once for good measure, they shouldn't be affected
PHP ; $01FE
LDA $BC
PHA ; $01FD

LDA #$AA
LDX #$FF
LDY #$01
;AHX $C000,Y
 DCB $9F
 DCB $00
 DCB $C0
LDA $C001
PHA ; $01FC

LDA #$F0
LDX #$AA
LDY #$04
;AHX $C001,Y
 DCB $9F
 DCB $01
 DCB $C0
LDA $C005
PHA ; $01FB

; TAS - AND A, X, SP, ADDR_HI
; ---------------------------

TSX     ; Save SP
STX $10

LDA #$55
LDX #$FF
LDY #$01

;TAS $0010,Y
 DCB $9B
 DCB $10
 DCB $00

TSX     ; Save SP in A through X - we want to push both SP and M on the stack
TXA

LDX $10 ; Restore SP
TXS

PHA ; $01FA
LDA $11
PHA ; $01F9

TSX     ; Save SP
STX $10

LDA #$E5
LDX #$F0
LDY #$D0

;TAS $C000,Y
 DCB $9B
 DCB $00
 DCB $C0

TSX     ; Save SP in A through X - we want to push both SP and M on the stack
TXA

LDX $10 ; Restore SP
TXS

PHA ; $01F8
LDA $C0D0
PHA ; $01F7

; SHX - X AND ADDR_HI
; -------------------

LDY #$30
LDX #$AA
;SHX $C000,Y
 DCB $9E
 DCB $00
 DCB $C0
LDA $C030
PHA ; $01F6

LDY #$20
LDX #$FF
;SHX $0001,Y
 DCB $9E
 DCB $01
 DCB $00
LDA $0021
PHA ; $01F5

; SHY - Y AND ADDR_HI
; -------------------

LDX #$FF
LDY #$EB
;SHY $C800,X
 DCB $9C
 DCB $00
 DCB $C8
LDA $C8FF
PHA ; $01F4

LDX #$F0
LDY #$55
;SHY $0000,X
 DCB $9C
 DCB $00
 DCB $00
LDA $F0
PHA ; $01F3

