
; Test file containing all possible instruction and addressing mode
; combinations, including a description of their operation, binary
; representation, effect on CPU registers and execution timing.
;
; This file is basically a carefully merged version of the references below
; and used as ground truth during emulator development, also suitable as an
; assembler / disassembler test. Nice to have it all in one place, plus each
; of these documents has typos, omissions and errors which were discovered
; during the N-way merge.
;
; For testing disassembly / instruction decoding, the file 'instr_test.bin'
; is an assembled version of this file, and its disassembly should match
; 'instr_test_ref_disasm.asm'. Those two files were generated using the tools
; of 6502js (https://github.com/skilldrick/6502js). Note that all instruction
; arguments are sequential numbers from $00 to $AB. A bug in 6502js prevents
; the relative addressing in branch instructions to assemble, only labels can
; be targeted. As a workaround, all branch instructions target 'lbl' and were
; later manually fixed in the binary / disassembly to their correct targets.
; We also handle 'DCB' slightly different, see end of the file.
;
; References / Sources / Originals:
;
; http://www.6502.org/tutorials/6502opcodes.html
; http://e-tradition.net/bytes/6502/6502_instruction_set.html
; http://www.obelisk.demon.co.uk/6502/reference.html
; http://www.atariarchives.org/alp/appendix_1.php

; Instructions with all addressing modes in alphabetical order
; ------------------------------------------------------------

lbl:

; ADC - Add with Carry
;
; This instruction adds the contents of a memory location to the accumulator
; together with the carry bit. If overflow occurs the carry bit is set, this
; enables multiple byte addition to be performed.
;
; ADC results are dependant on the setting of the decimal flag. In decimal
; mode, addition is carried out on the assumption that the values involved are
; packed BCD (Binary Coded Decimal). 
;
;    A + M + C -> A, C                N Z C I D V
;                                     + + + - - +
;
; C  Carry Flag         Set if overflow in bit 7
; Z  Zero Flag          Set if A = 0
; V  Overflow Flag      Set if sign bit is incorrect
; N  Negative Flag      Set if bit 7 set
;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
ADC #$01     ;Immediate     $69  2   2
ADC $02      ;Zero Page     $65  2   3
ADC $03,X    ;Zero Page,X   $75  2   4
ADC $0405    ;Absolute      $6D  3   4
ADC $0607,X  ;Absolute,X    $7D  3   4+
ADC $0809,Y  ;Absolute,Y    $79  3   4+
ADC ($0A,X)  ;Indirect,X    $61  2   6
ADC ($0B),Y  ;Indirect,Y    $71  2   5+

; AND - Bitwise AND with Accumulator
;
; A logical AND is performed, bit by bit, on the accumulator contents using
; the contents of a byte of memory.
;
;    A AND M -> A                     N Z C I D V
;                                     + + - - - -
;
; Z Zero Flag         Set if A = 0
; N Negative Flag     Set if bit 7 set
;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
AND #$0C     ;Immediate     $29  2   2
AND $0D      ;Zero Page     $25  2   3
AND $0E,X    ;Zero Page,X   $35  2   4
AND $0F10    ;Absolute      $2D  3   4
AND $1112,X  ;Absolute,X    $3D  3   4+
AND $1314,Y  ;Absolute,Y    $39  3   4+
AND ($15,X)  ;Indirect,X    $21  2   6
AND ($16),Y  ;Indirect,Y    $31  2   5+

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
; N Negative Flag     Set if bit 7 of the result is set
;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
ASL A        ;Accumulator   $0A  1   2
ASL $17      ;Zero Page     $06  2   5
ASL $18,X    ;Zero Page,X   $16  2   6
ASL $191A    ;Absolute      $0E  3   6
ASL $1B1C,X  ;Absolute,X    $1E  3   7

; BCC - Branch on Carry Clear
;
; If the carry flag is clear then add the relative displacement to the program
; counter to cause a branch to a new location.
;
;    branch on C = 0                  N Z C I D V
;                                     - - - - - -
;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
BCC lbl      ;Relative      $90  2   2++

; BCS - Branch on Carry Set
;
; If the carry flag is set then add the relative displacement to the program
; counter to cause a branch to a new location.
;
;    branch on C = 1                  N Z C I D V
;                                     - - - - - -
;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
BCS lbl      ;Relative      $B0  2   2++

; BEQ - Branch on Result Zero
;
; If the zero flag is set then add the relative displacement to the program
; counter to cause a branch to a new location.
;
;    branch on Z = 1                  N Z C I D V
;                                     - - - - - -
;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
BEQ lbl      ;Relative      $F0  2   2++

; BIT - Test Bits in Memory with Accumulator
;
; This instructions is used to test if one or more bits are set in a target
; memory location. The mask pattern in A is ANDed with the value in memory to
; set or clear the zero flag, but the result is not kept. Bits 7 and 6 of the
; value from memory are copied into the N and V flags.
;
;    A AND M, M7 -> N, M6 -> V        N Z C I D V
;                                    M7 + - - - M6
;
; Z Zero Flag         Set if the result if the AND is zero
; V Overflow Flag     Set to bit 6 of the memory value
; N Negative Flag     Set to bit 7 of the memory value
;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
BIT $20      ;Zero Page     $24  2   3
BIT $2122    ;Absolute      $2C  3   4

; BMI - Branch on Result Negative
;
; If the negative flag is set then add the relative displacement to the
; program counter to cause a branch to a new location.
;
;    branch on N = 1                  N Z C I D V
;                                     - - - - - -
;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
BMI lbl      ;Relative      $30  2   2++

; BNE - Branch on Result not Zero
;
; If the zero flag is clear then add the relative displacement to the program
; counter to cause a branch to a new location.
;
;    branch on Z = 0                  N Z C I D V
;                                     - - - - - -
;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
BNE lbl      ;Relative      $D0  2   2++

; BPL - Branch on Result Positive
;
; If the negative flag is clear then add the relative displacement to the
; program counter to cause a branch to a new location.
;
;    branch on N = 0                  N Z C I D V
;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
BPL lbl      ;Relative      $10  2   2++

; BRK - Force Break
;
; The BRK instruction forces the generation of an interrupt request. The
; program counter and processor status are pushed on the stack (with the B bit
; set), then the interrupt-disable I flag is set and the IRQ interrupt vector
; at $FFFE/F is loaded into the PC.
;
;    interrupt,                       N Z C I D V
;    push PC+2, push SR               - - - 1 - -
;
; I Interrupt Disable Set to 1
;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
BRK          ;Implied       $00  1   7

; BVC - Branch on Overflow Clear
;
; If the overflow flag is clear then add the relative displacement to the
; program counter to cause a branch to a new location.
;
;    branch on V = 0                  N Z C I D V
;                                     - - - - - -
;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
BVC lbl      ;Relative      $50  2   2++

; BVS - Branch on Overflow Set
;
; If the overflow flag is set then add the relative displacement to the
; program counter to cause a branch to a new location.
;
;    branch on V = 1                  N Z C I D V
;                                     - - - - - -
;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
BVS lbl      ;Relative      $70  2   2++

; CLC - Clear Carry Flag
;
; Set the carry flag to zero.
;
;    0 -> C                           N Z C I D V
;                                     - - 0 - - -
;
; C Carry Flag        Set to 0
;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
CLC          ;Implied       $18  1   2

; CLD - Clear Decimal Mode
;
; Set the decimal mode flag to zero.
;
;    0 -> D                           N Z C I D V
;                                     - - - - 0 -
;
; D Decimal Mode Flag Set to 0
;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
CLD          ;Implied       $D8  1   2

; CLI - Clear Interrupt Disable Bit
;
; Clear the interrupt disable flag allowing normal interrupt requests to be
; serviced.
;
;    0 -> I                           N Z C I D V
;                                     - - - 0 - -
;
; I Interrupt Disable Set to 0
;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
CLI          ;Implied       $58  1   2

; CLV - Clear Overflow Flag
;
; Clear the overflow flag.
;
;    0 -> V                           N Z C I D V
;                                     - - - - - 0
;
; V Overflow Flag     Set to 0
;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
CLV          ;Implied       $B8  1   2

; CMP - Compare Memory with Accumulator
;
; Compare sets flags as if a subtraction had been carried out. If the value in
; the accumulator is equal or greater than the compared value, the Carry will
; be set. The equal (Z) and negative (N) flags will be set based on equality
; or lack thereof and the sign (i.e. A>=$80) of the accumulator. 
;
;    A - M                            N Z C I D V
;                                     + + + - - -
;
; C Carry Flag        Set if A >= M
; Z Zero Flag         Set if A = M
; N Negative Flag     Set if bit 7 of the result is set
;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
CMP #$28     ;Immediate     $C9  2   2
CMP $29      ;Zero Page     $C5  2   3
CMP $2A,X    ;Zero Page,X   $D5  2   4
CMP $2B2C    ;Absolute      $CD  3   4
CMP $2D2E,X  ;Absolute,X    $DD  3   4+
CMP $2F30,Y  ;Absolute,Y    $D9  3   4+
CMP ($31,X)  ;Indirect,X    $C1  2   6
CMP ($32),Y  ;Indirect,Y    $D1  2   5+

; CPX - Compare Memory and Index X
;
; This instruction compares the contents of the X register with another memory
; held value and sets the zero and carry flags as appropriate. Operation and
; flag results are identical to equivalent mode accumulator CMP ops. 
;
;    X - M                            N Z C I D V
;                                     + + + - - -
;
; C Carry Flag        Set if X >= M
; Z Zero Flag         Set if X = M
; N Negative Flag     Set if bit 7 of the result is set
;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
CPX #$33     ;Immediate     $E0  2   2
CPX $34      ;Zero Page     $E4  2   3
CPX $3536    ;Absolute      $EC  3   4

; CPY - Compare Memory and Index Y
;
; This instruction compares the contents of the Y register with another memory
; held value and sets the zero and carry flags as appropriate. Operation and
; flag results are identical to equivalent mode accumulator CMP ops. 
;
;    Y - M                            N Z C I D V
;                                     + + + - - -
;
; C Carry Flag        Set if Y >= M
; Z Zero Flag         Set if Y = M
; N Negative Flag     Set if bit 7 of the result is set
;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
CPY #$37     ;Immediate     $C0  2   2
CPY $38      ;Zero Page     $C4  2   3
CPY $393A    ;Absolute      $CC  3   4

; DEC - Decrement Memory by One
;
; Subtracts one from the value held at a specified memory location, setting
; the zero and negative flags as appropriate.
;
;    M - 1 -> M                       N Z C I D V
;                                     + + - - - -
;
; Z Zero Flag         Set if result is zero
; N Negative Flag     Set if bit 7 of the result is set
;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
DEC $3B      ;Zero Page     $C6  2   5
DEC $3C,X    ;Zero Page,X   $D6  2   6
DEC $3D3E    ;Absolute      $CE  3   6
DEC $3F40,X  ;Absolute,X    $DE  3   7

; DEX - Decrement Index X by One
;
; Subtracts one from the X register, setting the zero and negative flags as
; appropriate.
;
;    X - 1 -> X                       N Z C I D V
;                                     + + - - - -
;
; Z Zero Flag         Set if X is zero
; N Negative Flag     Set if bit 7 of X is set
;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
DEX          ;Implied       $CA  1   2

; DEY - Decrement Index Y by One
;
; Subtracts one from the Y register, setting the zero and negative flags as
; appropriate.
;
;    Y - 1 -> Y                       N Z C I D V
;                                     + + - - - -
;
; Z Zero Flag         Set if Y is zero
; N Negative Flag     Set if bit 7 of Y is set
;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
DEY          ;Implied       $88  1   2

; EOR - Exclusive-OR Memory with Accumulator
;
; An exclusive OR is performed, bit by bit, on the accumulator contents using
; the contents of a byte of memory.
;
;    A EOR M -> A                     N Z C I D V
;                                     + + - - - -
;
; Z Zero Flag         Set if A = 0
; N Negative Flag     Set if bit 7 set
;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
EOR #$41     ;Immediate     $49  2   2
EOR $42      ;Zero Page     $45  2   3
EOR $43,X    ;Zero Page,X   $55  2   4
EOR $4445    ;Absolute      $4D  3   4
EOR $4647,X  ;Absolute,X    $5D  3   4+
EOR $4849,Y  ;Absolute,Y    $59  3   4+
EOR ($4A,X)  ;Indirect,X    $41  2   6
EOR ($4B),Y  ;Indirect,Y    $51  2   5+

; INC - Increment Memory by One
;
; Adds one to the value held at a specified memory location, setting the zero
; and negative flags as appropriate.
;
;    M + 1 -> M                       N Z C I D V
;                                     + + - - - -
;
; Z Zero Flag         Set if result is zero
; N Negative Flag     Set if bit 7 of the result is set
;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
INC $4C      ;Zero Page     $E6  2   5
INC $4D,X    ;Zero Page,X   $F6  2   6
INC $4E4F    ;Absolute      $EE  3   6
INC $5051,X  ;Absolute,X    $FE  3   7

; INX - Increment Index X by One
;
; Adds one to the X register, setting the zero and negative flags as
; appropriate.
;
;    X + 1 -> X                       N Z C I D V
;                                     + + - - - -
;
; Z Zero Flag         Set if X is zero
; N Negative Flag     Set if bit 7 of X is set
;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
INX          ;Implied       $E8  1   2

; INY - Increment Index Y by One
;
; Adds one to the Y register, setting the zero and negative flags as
; appropriate.
;
;    Y + 1 -> Y                       N Z C I D V
;                                     + + - - - -
;
; Z Zero Flag         Set if Y is zero
; N Negative Flag     Set if bit 7 of Y is set
;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
INY          ;Implied       $C8  1   2

; JMP - Jump to New Location
;
; Sets the program counter to the address specified by the operand.
;
; An original 6502 has does not correctly fetch the target address if the
; indirect vector falls on a page boundary (e.g. $xxFF where xx is and value
; from $00 to $FF). In this case fetches the LSB from $xxFF as expected but;
; takes the MSB from $xx00. This is fixed in some later chips like the 65SC02
; so for compatibility always ensure the indirect vector is not at the end of
; the page.
;
;    (PC+1) -> PCL                    N Z C I D V
;    (PC+2) -> PCH                    - - - - - -
;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
JMP $5253    ;Absolute      $4C  3   3
JMP ($5455)  ;Indirect      $6C  3   5

; JSR - Jump to New Location Saving Return Address
;
; The JSR instruction pushes the address (minus one) of the return point on to
; the stack and then sets the program counter to the target memory address.
;
;    push (PC+2),                     N Z C I D V
;    (PC+1) -> PCL                    - - - - - -
;    (PC+2) -> PCH
;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
JSR $5657    ;Absolute      $20  3   6

; LDA - Load Accumulator with Memory
;
; Loads a byte of memory into the accumulator setting the zero and negative
; flags as appropriate.
;
;    M -> A                           N Z C I D V
;                                     + + - - - -
;
; Z Zero Flag         Set if A = 0
; N Negative Flag     Set if bit 7 of A is set
;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
LDA #$58     ;Immediate     $A9  2   2
LDA $59      ;Zero Page     $A5  2   3
LDA $5A,X    ;Zero Page,X   $B5  2   4
LDA $5B5C    ;Absolute      $AD  3   4
LDA $5D5E,X  ;Absolute,X    $BD  3   4+
LDA $5F60,Y  ;Absolute,Y    $B9  3   4+
LDA ($61,X)  ;Indirect,X    $A1  2   6
LDA ($62),Y  ;Indirect,Y    $B1  2   5+

; LDX - Load Index X with Memory
;
; Loads a byte of memory into the X register setting the zero and negative
; flags as appropriate.
;
;    M -> X                           N Z C I D V
;                                     + + - - - -
;
; Z Zero Flag         Set if X = 0
; N Negative Flag     Set if bit 7 of A is set
;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
LDX #$63     ;Immediate     $A2  2   2
LDX $64      ;Zero Page     $A6  2   3
LDX $65,Y    ;Zero Page,Y   $B6  2   4
LDX $6667    ;Absolute      $AE  3   4
LDX $6869,Y  ;Absolute,Y    $BE  3   4+

; LDY - Load Index Y with Memory
;
; Loads a byte of memory into the Y register setting the zero and negative
; flags as appropriate.
;
;    M -> Y                           N Z C I D V
;                                     + + - - - -
;
; Z Zero Flag         Set if Y = 0
; N Negative Flag     Set if bit 7 of A is set
;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
LDY #$6A     ;Immediate     $A0  2   2
LDY $6B      ;Zero Page     $A4  2   3
LDY $6C,X    ;Zero Page,X   $B4  2   4
LDY $6D6E    ;Absolute      $AC  3   4
LDY $6F70,X  ;Absolute,X    $BC  3   4+

; LSR - Shift One Bit Right (Memory or Accumulator)
;
; Each of the bits in A or M is shifted one place to the right. The bit that
; was in bit 0 is shifted into the carry flag. Bit 7 is set to zero. Since
; the high bit of the number being addressed is always forced to zero, the
; Negative flag is always reset by this operation.
; 
;    0 -> [76543210] -> C             N Z C I D V
;                                     0 + + - - -
;
; C Carry Flag        Set to contents of old bit 0
; Z Zero Flag         Set if result = 0
; N Negative Flag     Set to 0
;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
LSR A        ;Accumulator   $4A  1   2
LSR $71      ;Zero Page     $46  2   5
LSR $72,X    ;Zero Page,X   $56  2   6
LSR $7374    ;Absolute      $4E  3   6
LSR $7576,X  ;Absolute,X    $5E  3   7

; NOP - No Operation
;
; The NOP instruction causes no changes to the processor other than the normal
; incrementing of the program counter to the next instruction.
;
;    ---                              N Z C I D V
;                                     - - - - - -
;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
NOP          ;Implied       $EA  1   2

; ORA - OR Memory with Accumulator
;
; An inclusive OR is performed, bit by bit, on the accumulator contents using
; the contents of a byte of memory.
;
;    A OR M -> A                      N Z C I D V
;                                     + + - - - -
;
; Z Zero Flag         Set if A = 0
; N Negative Flag     Set if bit 7 set
;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
ORA #$77     ;Immediate     $09  2   2
ORA $78      ;Zero Page     $05  2   3
ORA $79,X    ;Zero Page,X   $15  2   4
ORA $7A7B    ;Absolute      $0D  3   4
ORA $7C7D,X  ;Absolute,X    $1D  3   4+
ORA $7E7F,Y  ;Absolute,Y    $19  3   4+
ORA ($80,X)  ;Indirect,X    $01  2   6
ORA ($81),Y  ;Indirect,Y    $11  2   5+

; PHA - Push Accumulator on Stack
;
; Pushes a copy of the accumulator on to the stack.
;
;    push A                           N Z C I D V
;                                     - - - - - -
;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
PHA          ;Implied       $48  1   3

; PHP - Push Processor Status on Stack
;
; Pushes a copy of the status flags on to the stack (B bit set).
;
;    push SR                          N Z C I D V
;                                     - - - - - -
;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
PHP          ;Implied       $08  1   3

; PLA - Pull Accumulator from Stack
;
; Pulls an 8 bit value from the stack and into the accumulator. The zero and
; negative flags are set as appropriate.
;
;    pull A                           N Z C I D V
;                                     + + - - - -
;
; Z Zero Flag         Set if A = 0
; N Negative Flag     Set if bit 7 of A is set
;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
PLA          ;Implied       $68  1   4

; PLP - Pull Processor Status from Stack
;
; Pulls an 8 bit value from the stack and into the processor flags. The flags
; will take on new states as determined by the value pulled.
;
;    pull SR                          N Z C I D V
;                                     from stack
;
; C Carry Flag        Set from stack
; Z Zero Flag         Set from stack
; I Interrupt Disable Set from stack
; D Decimal Mode Flag Set from stack
; V Overflow Flag     Set from stack
; N Negative Flag     Set from stack
;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
PLP          ;Implied       $28  1   4

; ROL - Rotate One Bit Left (Memory or Accumulator)
;
; Move each of the bits in either A or M one place to the left. Bit 0 is
; filled with the current value of the carry flag whilst the old bit 7
; becomes the new carry flag value.
;
;    C <- [76543210] <- C             N Z C I D V
;                                     + + + - - -
;
; C Carry Flag        Set to contents of old bit 7
; Z Zero Flag         Set if A = 0
; N Negative Flag     Set if bit 7 of the result is set
;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
ROL A        ;Accumulator   $2A  1   2
ROL $82      ;Zero Page     $26  2   5
ROL $83,X    ;Zero Page,X   $36  2   6
ROL $8485    ;Absolute      $2E  3   6
ROL $8687,X  ;Absolute,X    $3E  3   7

; ROR - Rotate One Bit Right (Memory or Accumulator)
;
; Move each of the bits in either A or M one place to the right. Bit 7 is
; filled with the current value of the carry flag whilst the old bit 0
; becomes the new carry flag value.
;
;    C -> [76543210] -> C             N Z C I D V
;                                     + + + - - -
;
; C Carry Flag        Set to contents of old bit 0
; Z Zero Flag         Set if A = 0
; N Negative Flag     Set if bit 7 of the result is set
;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
ROR A        ;Accumulator   $6A  1   2
ROR $88      ;Zero Page     $66  2   5
ROR $89,X    ;Zero Page,X   $76  2   6
ROR $8A8B    ;Absolute      $6E  3   6
ROR $8C8D,X  ;Absolute,X    $7E  3   7

; RTI - Return from Interrupt
;
; The RTI instruction is used at the end of an interrupt processing routine.
; It pulls the processor flags from the stack followed by the program counter.
; Note that unlike RTS, the return address on the stack is the actual address
; rather than the address - 1.
;
;    pull SR, pull PC                 N Z C I D V
;                                     from stack
;
; C Carry Flag        Set from stack
; Z Zero Flag         Set from stack
; I Interrupt Disable Set from stack
; D Decimal Mode Flag Set from stack
; V Overflow Flag     Set from stack
; N Negative Flag     Set from stack
;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
RTI          ;Implied       $40  1   6

; RTS - Return from Subroutine
;
; RTS pulls the top two bytes off the stack (low byte first) and transfers
; program control to that address+1. It is used, as expected, to exit a
; subroutine invoked via JSR which pushed the address - 1. 
;
;    pull PC, PC+1 -> PC              N Z C I D V
;                                     - - - - - -
;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
RTS          ;Implied       $60  1   6

; SBC - Subtract Memory from Accumulator with Borrow
;
; This instruction subtracts the contents of a memory location from the
; accumulator together with the not of the carry bit. If overflow occurs the
; carry bit is cleared, this enables multiple byte subtraction to be performed.
;
; SBC results are dependant on the setting of the decimal flag. In decimal
; mode, subtraction is carried out on the assumption that the values involved
; are packed BCD (Binary Coded Decimal). 
;
;    A - M - C -> A                   N Z C I D V
;                                     + + + - - +
;
; C Carry Flag        Clear if overflow in bit 7
; Z Zero Flag         Set if A = 0
; V Overflow Flag     Set if sign bit is incorrect
; N Negative Flag     Set if bit 7 set
;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
SBC #$8E     ;Immediate     $E9  2   2
SBC $8F      ;Zero Page     $E5  2   3
SBC $90,X    ;Zero Page,X   $F5  2   4
SBC $9192    ;Absolute      $ED  3   4
SBC $9394,X  ;Absolute,X    $FD  3   4+
SBC $9596,Y  ;Absolute,Y    $F9  3   4+
SBC ($97,X)  ;Indirect,X    $E1  2   6
SBC ($98),Y  ;Indirect,Y    $F1  2   5+

; SEC - Set Carry Flag
;
; Set the carry flag to one.
;
;    1 -> C                           N Z C I D V
;                                     - - 1 - - -
;
; C Carry Flag        Set to 1
;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
SEC          ;Implied       $38  1   2

; SED - Set Decimal Flag
;
; Set the decimal flag to one.
;
;    1 -> D                           N Z C I D V
;                                     - - - - 1 -
;
; D Decimal Mode Flag Set to 1
;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
SED          ;Implied       $F8  1   2

; SEI - Set Interrupt Disable Status
;
; Set the Interrupt disable flag to one.
;
;    1 -> I                           N Z C I D V
;                                     - - - 1 - -
;
; D Interrupt Disable Set to 1
;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
SEI          ;Implied       $78  1   2

; STA - Store Accumulator in Memory
;
; Stores the contents of the accumulator into memory.
;
;    A -> M                           N Z C I D V
;                                     - - - - - -
;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
STA $99      ;Zero Page     $85  2   3
STA $9A,X    ;Zero Page,X   $95  2   4
STA $9B9C    ;Absolute      $8D  3   4
STA $9D9E,X  ;Absolute,X    $9D  3   5
STA $9FA0,Y  ;Absolute,Y    $99  3   5
STA ($A1,X)  ;Indirect,X    $81  2   6
STA ($A2),Y  ;Indirect,Y    $91  2   6

; STX - Store Index X in Memory
;
; Stores the contents of the X register into memory.
;
;    X -> M                           N Z C I D V
;                                     - - - - - -
;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
STX $A3      ;Zero Page     $86  2   3
STX $A4,Y    ;Zero Page,Y   $96  2   4
STX $A5A6    ;Absolute      $8E  3   4

; STY - Store Index Y in Memory
;
; Stores the contents of the Y register into memory.
;
;    Y -> M                           N Z C I D V
;                                     - - - - - -
;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
STY $A7      ;Zero Page     $84  2   3
STY $A8,X    ;Zero Page,X   $94  2   4
STY $A9AA    ;Absolute      $8C  3   4

; TAX - Transfer Accumulator to Index X
;
; Copies the current contents of the accumulator into the X register and sets
; the zero and negative flags as appropriate.
;
;    A -> X                           N Z C I D V
;                                     + + - - - -
;
; Z Zero Flag         Set if X = 0
; N Negative Flag     Set if bit 7 of X is set
;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
TAX          ;Implied       $AA  1   2

; TAY - Transfer Accumulator to Index Y
;
; Copies the current contents of the accumulator into the Y register and sets
; the zero and negative flags as appropriate.
;
;    A -> Y                           N Z C I D V
;                                     + + - - - -
;
; Z Zero Flag         Set if Y = 0
; N Negative Flag     Set if bit 7 of Y is set
;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
TAY          ;Implied       $A8  1   2

; TSX - Transfer Stack Pointer to Index X
;
; Copies the current contents of the stack pointer into the X register.
;
;    SP -> X                          N Z C I D V
;                                     + + - - - -
; Z Zero Flag         Set if X = 0
; N Negative Flag     Set if bit 7 of X is set
;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
TSX          ;Implied       $BA  1   2

; TXA - Transfer Index X to Accumulator
;
; Copies the current contents of X register into the accumulator and sets the
; zero and negative flags as appropriate.
;
;    X -> A                           N Z C I D V
;                                     + + - - - -
;
; Z Zero Flag         Set if X = 0
; N Negative Flag     Set if bit 7 of X is set
;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
TXA          ;Implied       $8A  1   2

; TXS - Transfer Index X to Stack Register
;
; Copies the current contents of the X register into the stack register.
;
;    X -> SP                          N Z C I D V
;                                     - - - - - -
;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
TXS          ;Implied       $9A  1   2

; TYA - Transfer Index Y to Accumulator
;
; Copies the current contents of Y register into the accumulator and sets the
; zero and negative flags as appropriate.
;
;    Y -> A                           N Z C I D V
;                                     + + - - - -
;
; Z Zero Flag         Set if A = 0
; N Negative Flag     Set if bit 7 of A is set
;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
TYA          ;Implied       $98  1   2

; +  = Add 1 to cycles if page boundary is crossed
; ++ = Add 1 to cycles if branch is taken, one more if branch occurs to different page

; DCB -  Define Constant Byte
;
; Not an actual opcode, used for embedding data into the assembly, we map any
; illegal instructions to it

DCB #$AB

