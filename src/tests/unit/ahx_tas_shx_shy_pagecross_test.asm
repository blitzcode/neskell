
; Test the pagecrossing behavior of AHX/TAS/SHX/SHY. The result to be written
; is used as MSB of the storage address if we cross a page boundary.
;
; Expected Result: SP = $F5
;
; After the test the stack should look like this:
;
;      0  1  2  3  4  5  6  7  8  9  A  B  C  D  E  F
; 01F0                   00 42 00 41 00 01 00 CF C0 D0 - VICE
; 01F0                   42 42 41 41 00 01 CE CF C0 D0 - V6502
;                        *     *     *     *     *
; * = Pagecross
;
; Note that 01FC, 01F8 and 01F6 differ between VICE and Visual 6502. I'm not sure
; what the reason is, maybe something with the C64 memory map etc. interfering.
; This is a big TODO, figure out what the real 6502 does. For now we're taking
; Visual 6502 as the ground truth.

LDX #$FF
TXS

; AHX - Store A & X & (ADDR_HI + 1) into M
; ----------------------------------------

LDA #$01 ; Indirect address
STA $10
LDA #$CF
STA $11

LDA #$FF
TAX
LDY #$00 ; No pagecross
;AHX ($10),Y
 DCB $93
 DCB $10
LDA $CF01
PHA ; $01FF = $FF & ($CF + 1) = $D0

LDA #$FF
LDX #$EF ; Mask out bit 4
LDY #$FF ; Pagecross
;AHX ($10),Y
 DCB $93
 DCB $10
LDA $C000 ; AddrHI = $FF & $EF & ($CF + 1) = $C0, AddrLO = $01 + $FF = 0
PHA ; $01FE = $C0

LDA #$FF
TAX
LDY #$00 ; No pagecross
;AHX $CEFF,Y
 DCB $9F
 DCB $FF
 DCB $CE
LDA $CEFF
PHA ; $01FD = $FF & ($CE + 1) = $CF 

LDA #$FE
TAX
LDY #$01 ; Pagecross
;AHX $CEFF,Y
 DCB $9F
 DCB $FF
 DCB $CE
LDA $CE00 ; = AddrHI = $FE & ($CE + 1) = $CE, AddrLO = $01 + $FF = 0
PHA ; $01FC = $CE

; TAS - AND A, X, SP, ADDR_HI
; ---------------------------

TSX     ; Save SP
STX $10

LDX #$55
LDA #$AB
LDY #$00 ; No pagecross
;TAS $C001,Y
 DCB $9B
 DCB $01
 DCB $C0
LDA $C001 ; $55 & $AB & ($C0 + 1) = $01

LDX $10 ; Restore SP
TXS
PHA ; $01FB

TSX     ; Save SP
STX $10

LDA #$FF
STA $14
LDA #$00
TAX
LDY #$FF ; Pagecross
;TAS $C015,Y
 DCB $9B
 DCB $15
 DCB $C0
LDA $14 ; AddrHI = $00 & ($C0 + 1) = $00, AddrLO = $15 + $FF = $14

LDX $10 ; Restore SP
TXS
PHA ; $01FA

; SHX - X AND ADDR_HI
; -------------------

LDX #$7F
LDY #$01 ; No pagecross
;SHX $C020,Y
 DCB $9E
 DCB $20
 DCB $C0
LDA $C021
PHA ; $01F9 = $41

LDX #$7F
LDY #$FF ; Pagecross
;SHX $C020,Y
 DCB $9E
 DCB $20
 DCB $C0
LDA $411F
PHA ; $01F8 = $41

; SHY - Y AND ADDR_HI
; -------------------

LDY #$7F
LDX #$01 ; No pagecross
;SHY $C130,X
 DCB $9C
 DCB $30
 DCB $C1
LDA $C131
PHA ; $01F7 = $42

LDY #$7F
LDX #$FF ; Pagecross
;SHY $C130,X
 DCB $9C
 DCB $30
 DCB $C1
LDA $422F
PHA ; $01F6 = $42

