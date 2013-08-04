
; Test file containing all possible instruction and addressing mode
; combinations, including a description of their operation, binary
; representation, effect on CPU registers and execution timing.
;
; This file is basically a carefully merged version of the references below
; and used as ground truth during emulator development, also suitable as an
; assembler / disassembler test. I just wanted to have it all in one place.
;
; References / Sources / Originals:
;
; http://www.6502.org/tutorials/6502opcodes.html
; http://e-tradition.net/bytes/6502/6502_instruction_set.html
; http://www.obelisk.demon.co.uk/6502/reference.html

; Instructions with all addressing modes in alphabetical order
; ------------------------------------------------------------

lbl:

; ADC - Add with Carry
;
; This instruction adds the contents of a memory location to the accumulator
; together with the carry bit. If overflow occurs the carry bit is set, this
; enables multiple byte addition to be performed.
;
;    A + M + C -> A, C                N Z C I D V
;                                     + + + - - +
;
; C  Carry Flag         Set if overflow in bit 7
; Z  Zero Flag          Set if A = 0
; I  Interrupt Disable  Not affected
; D  Decimal Mode Flag  Not affected
; B  Break Command      Not affected
; V  Overflow Flag      Set if sign bit is incorrect
; N  Negative Flag      Set if bit 7 set
;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
ADC #$44     ;Immediate     $69  2   2
ADC $44      ;Zero Page     $65  2   3
ADC $44,X    ;Zero Page,X   $75  2   4
ADC $4400    ;Absolute      $6D  3   4
ADC $4400,X  ;Absolute,X    $7D  3   4+
ADC $4400,Y  ;Absolute,Y    $79  3   4+
ADC ($44,X)  ;Indirect,X    $61  2   6
ADC ($44),Y  ;Indirect,Y    $71  2   5+

; AND - Bitwise AND with Accumulator
;
; A logical AND is performed, bit by bit, on the accumulator contents using
; the contents of a byte of memory.
;
;    A AND M -> A                     N Z C I D V
;                                     + + - - - -
;
; C Carry Flag        Not affected
; Z Zero Flag         Set if A = 0
; I Interrupt Disable Not affected
; D Decimal Mode Flag Not affected
; B Break Command     Not affected
; V Overflow Flag     Not affected
; N Negative Flag     Set if bit 7 set
;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
AND #$44     ;Immediate     $29  2   2
AND $44      ;Zero Page     $25  2   3
AND $44,X    ;Zero Page,X   $35  2   4
AND $4400    ;Absolute      $2D  3   4
AND $4400,X  ;Absolute,X    $3D  3   4+
AND $4400,Y  ;Absolute,Y    $39  3   4+
AND ($44,X)  ;Indirect,X    $21  2   6
AND ($44),Y  ;Indirect,Y    $31  2   5+

; ASL - Arithmetic Shift Left
;
; This operation shifts all the bits of the accumulator or memory contents one
; bit left. Bit 0 is set to 0 and bit 7 is placed in the carry flag. The effect
; of this operation is to multiply the memory contents by 2 (ignoring 2's
; complement considerations), setting the carry if the result will not fit in
; 8 bits.
;
;    C <- [76543210] <- 0             N Z C I D V
;                                     + + + - - -
;
; C Carry Flag        Set to contents of old bit 7
; Z Zero Flag         Set if A = 0
; I Interrupt Disable Not affected
; D Decimal Mode Flag Not affected
; B Break Command     Not affected
; V Overflow Flag     Not affected
; N Negative Flag     Set if bit 7 of the result is set
;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
ASL A        ;Accumulator   $0A  1   2
ASL $44      ;Zero Page     $06  2   5
ASL $44,X    ;Zero Page,X   $16  2   6
ASL $4400    ;Absolute      $0E  3   6
ASL $4400,X  ;Absolute,X    $1E  3   7

; BCC - Branch on Carry Clear

;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
BCC lbl      ;Relative      $90  2   2++

; BCS - Branch on carry set

;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
BCS lbl      ;Relative      $B0  2   2++

; BEQ - Branch on Result Zero

;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
BEQ lbl      ;Relative      $F0  2   2++

; BIT - Test Bits in Memory with Accumulator

;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
BIT $44      ;Zero Page     $24  2   3
BIT $4400    ;Absolute      $2C  3   4

; BMI - Branch on Result Minus

;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
BMI lbl      ;Relative      $30  2   2++

; BNE - Branch on Result not Zero

;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
BNE lbl      ;Relative      $D0  2   2++

; BPL - Branch on Result Plus

;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
BPL lbl      ;Relative      $10  2   2++

; BRK - Force Break

;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
BRK          ;Implied       $00  1   7

; BVC - Branch on Overflow Clear

;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
BVC lbl      ;Relative      $50  2   2++

; BVS - Branch on Overflow Set

;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
BVS lbl      ;Relative      $70  2   2++

; CLC - Clear Carry Flag

;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
CLC          ;Implied       $18  1   2

; CLD - Clear Decimal Mode

;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
CLD          ;Implied       $D8  1   2

; CLI - Clear Interrupt Disable Bit

;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
CLI          ;Implied       $58  1   2

; CLV - Clear Overflow Flag

;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
CLV          ;Implied       $B8  1   2

; CMP - Compare Memory with Accumulator

;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
CMP #$44     ;Immediate     $C9  2   2
CMP $44      ;Zero Page     $C5  2   3
CMP $44,X    ;Zero Page,X   $D5  2   4
CMP $4400    ;Absolute      $CD  3   4
CMP $4400,X  ;Absolute,X    $DD  3   4+
CMP $4400,Y  ;Absolute,Y    $D9  3   4+
CMP ($44,X)  ;Indirect,X    $C1  2   6
CMP ($44),Y  ;Indirect,Y    $D1  2   5+

; CPX - Compare Memory and Index X

;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
CPX #$44     ;Immediate     $E0  2   2
CPX $44      ;Zero Page     $E4  2   3
CPX $4400    ;Absolute      $EC  3   4

; CPY - Compare Memory and Index Y

;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
CPY #$44     ;Immediate     $C0  2   2
CPY $44      ;Zero Page     $C4  2   3
CPY $4400    ;Absolute      $CC  3   4

; DEC - Decrement Memory by One

;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
DEC $44      ;Zero Page     $C6  2   5
DEC $44,X    ;Zero Page,X   $D6  2   6
DEC $4400    ;Absolute      $CE  3   6
DEC $4400,X  ;Absolute,X    $DE  3   7

; DEX - Decrement Index X by One

;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
DEX          ;Implied       $CA  1   2

; DEY - Decrement Index Y by One

;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
DEY          ;Implied       $88  1   2

; EOR - Exclusive-OR Memory with Accumulator

;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
EOR #$44     ;Immediate     $49  2   2
EOR $44      ;Zero Page     $45  2   3
EOR $44,X    ;Zero Page,X   $55  2   4
EOR $4400    ;Absolute      $4D  3   4
EOR $4400,X  ;Absolute,X    $5D  3   4+
EOR $4400,Y  ;Absolute,Y    $59  3   4+
EOR ($44,X)  ;Indirect,X    $41  2   6
EOR ($44),Y  ;Indirect,Y    $51  2   5+

; INC - Increment Memory by One

;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
INC $44      ;Zero Page     $E6  2   5
INC $44,X    ;Zero Page,X   $F6  2   6
INC $4400    ;Absolute      $EE  3   6
INC $4400,X  ;Absolute,X    $FE  3   7

; INX - Increment Index X by One

;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
INX          ;Implied       $E8  1   2

; INY - Increment Index Y by One

;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
INY          ;Implied       $C8  1   2

; JMP - Jump to New Location

;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
JMP $5597    ;Absolute      $4C  3   3
JMP ($5597)  ;Indirect      $6C  3   5

; JSR - Jump to New Location Saving Return Address

;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
JSR $5597    ;Absolute      $20  3   6

; LDA - Load Accumulator with Memory

;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
LDA #$44     ;Immediate     $A9  2   2
LDA $44      ;Zero Page     $A5  2   3
LDA $44,X    ;Zero Page,X   $B5  2   4
LDA $4400    ;Absolute      $AD  3   4
LDA $4400,X  ;Absolute,X    $BD  3   4+
LDA $4400,Y  ;Absolute,Y    $B9  3   4+
LDA ($44,X)  ;Indirect,X    $A1  2   6
LDA ($44),Y  ;Indirect,Y    $B1  2   5+

; LDX - Load Index X with Memory

;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
LDX #$44     ;Immediate     $A2  2   2
LDX $44      ;Zero Page     $A6  2   3
LDX $44,Y    ;Zero Page,Y   $B6  2   4
LDX $4400    ;Absolute      $AE  3   4
LDX $4400,Y  ;Absolute,Y    $BE  3   4+

; LDY - Load Index Y with Memory

;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
LDY #$44     ;Immediate     $A0  2   2
LDY $44      ;Zero Page     $A4  2   3
LDY $44,X    ;Zero Page,X   $B4  2   4
LDY $4400    ;Absolute      $AC  3   4
LDY $4400,X  ;Absolute,X    $BC  3   4+

; LSR - Shift One Bit Right (Memory or Accumulator)

;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
LSR A        ;Accumulator   $4A  1   2
LSR $44      ;Zero Page     $46  2   5
LSR $44,X    ;Zero Page,X   $56  2   6
LSR $4400    ;Absolute      $4E  3   6
LSR $4400,X  ;Absolute,X    $5E  3   7

; NOP - No Operation

;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
NOP          ;Implied       $EA  1   2

; ORA - OR Memory with Accumulator

;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
ORA #$44     ;Immediate     $09  2   2
ORA $44      ;Zero Page     $05  2   3
ORA $44,X    ;Zero Page,X   $15  2   4
ORA $4400    ;Absolute      $0D  3   4
ORA $4400,X  ;Absolute,X    $1D  3   4+
ORA $4400,Y  ;Absolute,Y    $19  3   4+
ORA ($44,X)  ;Indirect,X    $01  2   6
ORA ($44),Y  ;Indirect,Y    $11  2   5+

; PHA - Push Accumulator on Stack

;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
PHA          ;Implied       $48  1   3

; PHP - Push Processor Status on Stack

;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
PHP          ;Implied       $08  1   3

; PLA - Pull Accumulator from Stack

;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
PLA          ;Implied       $68  1   4

; PLP - Pull Processor Status from Stack

;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
PLP          ;Implied       $28  1   4

; ROL - Rotate One Bit Left (Memory or Accumulator)

;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
ROL A        ;Accumulator   $2A  1   2
ROL $44      ;Zero Page     $26  2   5
ROL $44,X    ;Zero Page,X   $36  2   6
ROL $4400    ;Absolute      $2E  3   6
ROL $4400,X  ;Absolute,X    $3E  3   7

; ROR - Rotate One Bit Right (Memory or Accumulator)

;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
ROR A        ;Accumulator   $6A  1   2
ROR $44      ;Zero Page     $66  2   5
ROR $44,X    ;Zero Page,X   $76  2   6
ROR $4400    ;Absolute      $6E  3   6
ROR $4400,X  ;Absolute,X    $7E  3   7

; RTI - Return from Interrupt

;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
RTI          ;Implied       $40  1   6

; RTS - Return from Subroutine

;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
RTS          ;Implied       $60  1   6

; SBC - Subtract Memory from Accumulator with Borrow

;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
SBC #$44     ;Immediate     $E9  2   2
SBC $44      ;Zero Page     $E5  2   3
SBC $44,X    ;Zero Page,X   $F5  2   4
SBC $4400    ;Absolute      $ED  3   4
SBC $4400,X  ;Absolute,X    $FD  3   4+
SBC $4400,Y  ;Absolute,Y    $F9  3   4+
SBC ($44,X)  ;Indirect,X    $E1  2   6
SBC ($44),Y  ;Indirect,Y    $F1  2   5+

; SEC - Set Carry Flag

;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
SEC          ;Implied       $38  1   2

; SED - Set Decimal Flag

;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
SED          ;Implied       $F8  1   2

; SEI - Set Interrupt Disable Status

;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
SEI          ;Implied       $78  1   2

; STA - Store Accumulator in Memory

;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
STA $44      ;Zero Page     $85  2   3
STA $44,X    ;Zero Page,X   $95  2   4
STA $4400    ;Absolute      $8D  3   4
STA $4400,X  ;Absolute,X    $9D  3   5
STA $4400,Y  ;Absolute,Y    $99  3   5
STA ($44,X)  ;Indirect,X    $81  2   6
STA ($44),Y  ;Indirect,Y    $91  2   6

; STX - Store Index X in Memory

;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
STX $44      ;Zero Page     $86  2   3
STX $44,Y    ;Zero Page,Y   $96  2   4
STX $4400    ;Absolute      $8E  3   4

; STY - Store Index Y in Memory

;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
STY $44      ;Zero Page     $84  2   3
STY $44,X    ;Zero Page,X   $94  2   4
STY $4400    ;Absolute      $8C  3   4

; TAX - Transfer Accumulator to Index X

;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
TAX          ;Implied       $AA  1   2

; TAY - Transfer Accumulator to Index Y

;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
TAY          ;Implied       $A8  1   2

; TSX - Transfer Stack Pointer to Index X

;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
TSX          ;Implied       $BA  1   2

; TXA - Transfer Index X to Accumulator

;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
TXA          ;Implied       $8A  1   2

; TXS - Transfer Index X to Stack Register

;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
TXS          ;Implied       $9A  1   2

; TYA - Transfer Index Y to Accumulator

;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
TYA          ;Implied       $98  1   2

; +  = Add 1 to cycles if page boundary is crossed
; ++ = Add 1 to cycles if branch is taken, one more if branch occurs to different page

