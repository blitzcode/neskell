
; Test ANC,ALR,ARR,XAA,AHX,TAS,SHX,SHY,LAS,AXS in all addressing modes
;
; Expected Result:
;
; After the test the stack should look like this:
;
;      0  1  2  3  4  5  6  7  8  9  A  B  C  D  E  F
; 01D0 00 00 00 00 00 00 00 00 00 00 FF B4 4C 35 A0 A0
; 01E0 A0 00 00 01 80 01 55 80 01 34 09 B4 80 B5 FF 36
; 01F0 00 75 55 F5 D5 35 7F 37 00 35 40 B5 36 B5 34 36
;
; Reference here is VICE (the code will work correctly with the memory map of
; a C64). Visual 6502 does not seem to produce the correct result, here's how
; the stack it produces looks like:
;
;      0  1  2  3  4  5  6  7  8  9  A  B  C  D  E  F
; 01D0 00 00 00 00 00 00 00 00 00 00 FF F4 4C 75 E0 E0
; 01E0 A0 00 00 01 80 01 55 80 01 75 09 F5 D5 B5 FF 35
; 01F0 7F 75 55 F5 D5 35 7F 35 55 35 40 B5 36 B5 B5 B5
;
; Basically, Visual 6502 does not seem to perform the AND operation common to
; many of the illegal opcodes. This might be because it is not able to model the
; analog effects responsible for it happening. Note that in the stack a lot of
; the SR values are simply wrong because V gets set erroneously early on and
; never cleared.
;
; Also see this NesDev forum thread: http://forums.nesdev.com/viewtopic.php?f=3&t=10417

LDX #$FF
TXS

; ANC
; ---

LDA #$FF
;ANC($0B) #$00
 DCB $0B
 DCB $00
PHP ; $01FF

LDA #$81
;ANC($0B) #$01
 DCB $0B
 DCB $01
PHP ; $01FE

LDA #$AA
;ANC($0B) #$FF
 DCB $0B
 DCB $FF
PHP ; $01FD

LDA #$00
;ANC($2B) #$FF
 DCB $2B
 DCB $FF
PHP ; $01FC

LDA #$CD
;ANC($2B) #$FF
 DCB $2B
 DCB $FF
PHP ; $01FB

; ALR
; ---

LDA #$81
;ALR #$FF
 DCB $4B
 DCB $FF
PHA ; $01FA
PHP ; $01F9

LDA #$AB
;ALR #$55
 DCB $4B
 DCB $55
PHA ; $01F8
PHP ; $01F7

LDA #$FF
;ALR #$FF
 DCB $4B
 DCB $FF
PHA ; $01F6
PHP ; $01F5

; ARR
; ---

SEC
LDA #$AB
;ARR #$FF
 DCB $6B
 DCB $FF
PHA ; $01F4
PHP ; $01F3

CLC
LDA #$AB
;ARR #$FF
 DCB $6B
 DCB $FF
PHA ; $01F2
PHP ; $01F1

CLC
LDA #$FF
;ARR #$00
 DCB $6B
 DCB $00
PHA ; $01F0
PHP ; $01EF

SEC
LDA #$FF
;ARR #$FF
 DCB $6B
 DCB $FF
PHA ; $01EE
PHP ; $01ED

SEC
LDA #$AA
;ARR #$55
 DCB $6B
 DCB $55
PHA ; $01EC
PHP ; $01EB

; TODO - Test in decimal mode

; XAA
; ---

PHP

;XAA #$FF
 DCB $8B
 DCB $FF

; We'll only execute the instruction and test the time it takes before
; returning the registers to well defined states
PLP
LDA #$00

; AHX
; ---

LDA #$09
STA $10
LDA #$00
STA $11
LDA #$AA
LDX #$55
LDY #$01
;AHX ($10),Y
 DCB $93
 DCB $10
LDA $10
PHA ; $01EA

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
PHP ; $01E9
LDA $BC
PHA ; $01E8

LDA #$AA
LDX #$FF
LDY #$01
;AHX $C000,Y
 DCB $9F
 DCB $00
 DCB $C0
LDA $C001
PHA ; $01E7

; TAS
; ---

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

PHA ; $01E6
LDA $11
PHA ; $01E5

; SHX
; ---

LDY #$30
LDX #$AA
;SHX $C000,Y
 DCB $9E
 DCB $00
 DCB $C0
LDA $C030
PHA ; $01E4

LDY #$20
LDX #$FF
;SHX $0001,Y
 DCB $9E
 DCB $01
 DCB $00
LDA $0021
PHA ; $01E3

; SHY
; ---

LDX #$FF
;SHY $C800,X
 DCB $9C
 DCB $00
 DCB $C8
LDA $C8FF
PHA ; $01E2

LDX #$F0
;SHY $0000,X
 DCB $9C
 DCB $00
 DCB $00
LDA $F0
PHA ; $01E1

; LAS
; ---

TSX     ; Save SP
STX $10

LDA #$AA
STA $05
LDY #$04
;LAS $0001,Y
 DCB $BB
 DCB $01
 DCB $00

; There's unfortunately no easy way to preserve SR as many of the register
; transfer / load instructions alter it and we can only push it to the stack,
; which is not valid due to LAS modifying the SP

STX $11 ; Move X and SP from after LAS out of the way
TSX
STX $12

LDX $10 ; Restore SP
TXS

PHA ; $01E0
LDA $11
PHA ; $01DF
LDA $12
PHA ; $01DE

; AXS
; ---

LDA #$FF
LDX #$55
;AXS #$09
 DCB $CB
 DCB $09
PHP ; $01DD
TXA
PHA ; $01DC

LDA #$55
LDX #$00
;AXS #$01
 DCB $CB
 DCB $01
PHP ; $01DB
TXA
PHA ; $01DA

