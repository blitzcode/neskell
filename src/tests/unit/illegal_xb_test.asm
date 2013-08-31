
; Test ANC,ALR,ARR,XAA,LAS,AXS (0x*B opcodes, all with AND) in all addressing
; modes
;
; Expected Result: SP = $DE
;
; After the test the stack should look like this:
;
;      0  1  2  3  4  5  6  7  8  9  A  B  C  D  E  F
; 01D0                                              FF
; 01E0 B4 4C 35 A0 A0 A0 B4 80 B5 FF 36 00 75 55 F5 D5
; 01F0 35 7F 37 00 35 40 B5 CD 36 00 B5 AA 34 01 36 00
;
; Reference here is VICE (the code will work correctly with the memory map of
; a C64). Visual 6502 does not seem to produce the correct result, here's how
; the stack it produces looks like:
;
;      0  1  2  3  4  5  6  7  8  9  A  B  C  D  E  F
; 01D0                                              FF
; 01E0 B4 4C 35 E5 E5 A1 F5 D5 B5 FF 35 7F 75 55 F5 D5
; 01F0 35 7F 35 55 35 40 B5 CD 36 00 B5 AA B5 81 B5 FF
;
; Basically, Visual 6502 does not seem to perform the AND operation common to
; all of the illegal opcodes. The AND is frequently explained to be resulting
; from two values being put on the bus at the same time. Maybe Visual 6502 is not
; able to model the analog effects responsible for it happening.
;
; Also see this NesDev forum thread: http://forums.nesdev.com/viewtopic.php?f=3&t=10417

CLD
SEI

LDX #$FF
TXS

; ANC - A AND M
; -------------

; Clear all normal flags
LDA #$01
CLC
CLV

LDA #$FF
;ANC($0B) #$00
 DCB $0B
 DCB $00
PHA ; $01FF
PHP ; $01FE

LDA #$81
;ANC($0B) #$01
 DCB $0B
 DCB $01
PHA ; $01FD
PHP ; $01FC

LDA #$AA
;ANC($0B) #$FF
 DCB $0B
 DCB $FF
PHA ; $01FB
PHP ; $01FA

LDA #$00
;ANC($2B) #$FF
 DCB $2B
 DCB $FF
PHA ; $01F9
PHP ; $01F8

LDA #$CD
;ANC($2B) #$FF
 DCB $2B
 DCB $FF
PHA ; $01F7
PHP ; $01F6

; ALR - Combined AND + LSR
; ------------------------

; Clear all normal flags
LDA #$01
CLC
CLV

LDA #$81
;ALR #$FF
 DCB $4B
 DCB $FF
PHA ; $01F5
PHP ; $01F4

LDA #$AB
;ALR #$55
 DCB $4B
 DCB $55
PHA ; $01F3
PHP ; $01F2

LDA #$FF
;ALR #$FF
 DCB $4B
 DCB $FF
PHA ; $01F1
PHP ; $01F0

; ARR - Combined AND + ROR with different SR effects
; --------------------------------------------------

; Clear all normal flags
LDA #$01
CLC
CLV

SEC
LDA #$AB
;ARR #$FF
 DCB $6B
 DCB $FF
PHA ; $01EF
PHP ; $01EE

CLC
LDA #$AB
;ARR #$FF
 DCB $6B
 DCB $FF
PHA ; $01ED
PHP ; $01EC

CLC
LDA #$FF
;ARR #$00
 DCB $6B
 DCB $00
PHA ; $01EB
PHP ; $01EA

SEC
LDA #$FF
;ARR #$FF
 DCB $6B
 DCB $FF
PHA ; $01E9
PHP ; $01E8

SEC
LDA #$AA
;ARR #$55
 DCB $6B
 DCB $55
PHA ; $01E7
PHP ; $01E6

; XAA - Unstable opcode, like a three register AND
; ------------------------------------------------

; Clear all normal flags
LDA #$01
CLC
CLV

PHP

;XAA #$FF
 DCB $8B
 DCB $FF

; We'll only execute the instruction and test the time it takes before
; returning the registers to well defined states
PLP
LDA #$00

; LAS - Load A, X and SP with SP AND M
; ------------------------------------

; Clear all normal flags
LDA #$01
CLC
CLV

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

PHA ; $01E5
LDA $11
PHA ; $01E4
LDA $12
PHA ; $01E3

; AXS - Store A AND X minus M into X
; ----------------------------------

; Clear all normal flags
LDA #$01
CLC
CLV

LDA #$FF
LDX #$55
;AXS #$09
 DCB $CB
 DCB $09
PHP ; $01E2
TXA
PHA ; $01E1

LDA #$55
LDX #$00
;AXS #$01
 DCB $CB
 DCB $01
PHP ; $01E0
TXA
PHA ; $01DF

