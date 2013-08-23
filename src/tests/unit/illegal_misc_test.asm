
; Test ANC,ALR,ARR,XAA,AHX,TAS,SHX,SHY,LAS,AXS in all addressing modes
;
; The result is a checksum based on the memory, accumulator and flag results of
; the tested opcodes. Comparing the stack to the reference hardware / emulator
; is the way to debug if the checksum is wrong.
;
; Expected Result:

; ANC - Sets SR based on A AND M 
; ------------------------------

LDA #$FF
;ANC($0B) #$00
 DCB $0B
 DCB $00
PHA
PHP

LDA #$AA
;ANC($0B) #$FF
 DCB $0B
 DCB $FF
PHA
PHP

LDA #$00
;ANC($2B) #$FF
 DCB $2B
 DCB $FF
PHA
PHP

LDA #$CD
;ANC($2B) #$FF
 DCB $2B
 DCB $FF
PHA
PHP

; ALR - Combined AND + LSR
; ------------------------

LDA #$81
;ALR #$FF
 DCB $4B
 DCB $FF
PHA
PHP

LDA #$AB
;ALR #$55
 DCB $4B
 DCB $55
PHA
PHP

LDA #$FF
;ALR #$FF
 DCB $4B
 DCB $FF
PHA
PHP

; ARR - Combined AND + ROR with different SR effects
; --------------------------------------------------

SEC
LDA #$AB
;ARR #$FF
 DCB $6B
 DCB $FF
PHA
PHP

CLC
LDA #$AB
;ARR #$FF
 DCB $6B
 DCB $FF
PHA
PHP

CLC
LDA #$FF
;ARR #$00
 DCB $6B
 DCB $00
PHA
PHP

SEC
LDA #$FF
;ARR #$FF
 DCB $6B
 DCB $FF
PHA
PHP

; TODO - Test in decimal mode

; XAA - Unstable opcode, like a three register AND
; ------------------------------------------------

PHP

;XAA #$FF
 DCB $8B
 DCB $FF

; We'll only execute the instruction and test the time it takes before
; returning the registers to well defined states
PLP
LDA #$00

; AHX - Store A & X & (ADDR_HI + 1) into M
; ----------------------------------------

; There are some conflicting description of what this opcode does, but this
; version seems to match the reference best.
;
;  A & X & (ADDR_HI + 1) -> M         N Z C I D V
;                                     - - - - - -

LDA #$AA
LDX #$55
LDY #$00
;AHX ($00),Y
 DCB $93
 DCB $00
LDA $00
PHA

LDA #$55
LDX #$FF
LDY #$01
;AHX ($01),Y
 DCB $93
 DCB $01
LDA $02
PHA

LDA #$AA
LDX #$FF
LDY $01
;AHX $FE00,Y
 DCB $9F
 DCB $00
 DCB $FE
LDA $FE01
PHA

; TAS - AND A, X, SP, ADDR_HI
; ---------------------------

TSX     ; Save SP
STX $00

LDA #$55
LDX #$FF
LDY #$01

;TAS $0000,Y
 DCB $9B
 DCB $00
 DCB $00

TXA     ; Save X in A - we want to push both X and M on the stack

LDX $00 ; Restore SP
TXS

PHA
LDA $01
PHA

; SHX - X AND ADDR_HI
; -------------------

LDY #$30
LDX #$AA

;SHX $FE00,Y
 DCB $9E
 DCB $00
 DCB $FE

LDA $FF30
PHA

; SHY - Y AND ADDR_HI
; -------------------

LDY #$FF

;SHY $A900,Y
 DCB $9C
 DCB $00
 DCB $A9

LDA $A9FF
PHA

; LAS - Load A, X and SP with SP AND M
; ------------------------------------

TSX     ; Save SP
STX $00

LDA #$FF
STA $03
LDY #$03
;LAS $0000,Y
 DCB $BB
 DCB $00
 DCB $00

; There's unfortunately no easy way to preserve SR as many of the register
; transfer / load instructions alter it and we can only push it to the stack,
; which is not valid due to LAS modifying the SP

STX $01 ; Move X and SP from after LAS out of the way
TSX
STX $02

LDX $00 ; Restore SP
TXS

PHA
LDA $01
PHA
LDA $02
PHA

; AXS - Store A AND X minus M into X
; ----------------------------------

; Stores to X the value of (A & X) - Immediate. This instruction does not have
; any decimal mode, and it does not affect the V flag. Also Carry will be
; ignored in the subtraction, but set according to the result.
;
;  X <- (A & X) - M                   N Z C I D V
;                                     + + + - - -
;
; Z Zero Flag         Set if A = 0
; N Negative Flag     Set if bit 7 set
; C Carry Flag        Clear if overflow in bit 7

LDA #$FF
LDX #$55
;AXS #$01
 DCB $CB
 DCB $01
PHP
TXA
PHA

LDA #$55
LDX #$00
;AXS #$01
 DCB $CB
 DCB $01
PHP
TXA
PHA

; Final checksum
;---------------

LDA #$00    ; A=$00
TSX         ; X=SP
INX         ; X = X + 1
loop:
EOR $0100,X ; A = A ^ ($0100 + X)
INX         ; X = X + 1
CPX #$FE    ; Z = X == $FE
BNE loop    ; if Z then goto loop

