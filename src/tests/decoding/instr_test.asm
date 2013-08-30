
; Test file containing all possible instruction and addressing mode
; combinations, including a description of their operation, binary
; representation, effect on CPU registers and execution timing.
;
; This file is basically a carefully merged version of the references below
; and used as ground truth during emulator development, also suitable as an
; assembler / disassembler test. Nice to have it all in one place, plus each
; of these documents has typos, omissions and errors which were discovered
; during the N-way merge and some Visual 6502 debugging sessions.
;
; For testing disassembly / instruction decoding, the file 'instr_test.bin' is
; an assembled version of this file, and its disassembly should match
; 'instr_test_ref_disasm.asm'. Those two files were generated using the tools
; of 6502js (https://github.com/skilldrick/6502js). The hexdump output
; can be converted to a binary with 'cat hexdmp | xxd -r -seek -1536 > bin'.
; Note that all instruction arguments are sequential numbers from $00. A
; bug in 6502js prevents the relative addressing in branch instructions to
; assemble, only labels can be targeted. As a workaround, all branch
; instructions target 'lbl' and were later manually fixed in the binary /
; disassembly to their correct targets (sequential numbers). At the end of the
; file are also all supported illegal / unofficial opcodes.
;
; References / Sources / Originals:
;
; http://www.6502.org/tutorials/6502opcodes.html
; http://e-tradition.net/bytes/6502/6502_instruction_set.html
; http://www.obelisk.demon.co.uk/6502/reference.html
; http://www.atariarchives.org/alp/appendix_1.php
; http://visual6502.org/wiki/index.php?title=6502_all_256_Opcodes
; http://wiki.nesdev.com/w/index.php/Programming_with_unofficial_opcodes
; http://www.oxyron.de/html/opcodes02.html
; http://www.viceteam.org/plain/64doc.txt
; http://www.ataripreservation.org/websites/freddy.offenga/illopc31.txt

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
ADC ($0A,X)  ;IdxInd        $61  2   6
ADC ($0B),Y  ;IndIdx        $71  2   5+

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
AND ($15,X)  ;IdxInd        $21  2   6
AND ($16),Y  ;IndIdx        $31  2   5+

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
;                                     - - - - - -
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
CMP ($31,X)  ;IdxInd        $C1  2   6
CMP ($32),Y  ;IndIdx        $D1  2   5+

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
EOR ($4A,X)  ;IdxInd        $41  2   6
EOR ($4B),Y  ;IndIdx        $51  2   5+

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
LDA ($61,X)  ;IdxInd        $A1  2   6
LDA ($62),Y  ;IndIdx        $B1  2   5+

; LDX - Load Index X with Memory
;
; Loads a byte of memory into the X register setting the zero and negative
; flags as appropriate.
;
;    M -> X                           N Z C I D V
;                                     + + - - - -
;
; Z Zero Flag         Set if X = 0
; N Negative Flag     Set if bit 7 of X is set
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
; N Negative Flag     Set if bit 7 of Y is set
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
ORA ($80,X)  ;IdxInd        $01  2   6
ORA ($81),Y  ;IndIdx        $11  2   5+

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
SBC ($97,X)  ;IdxInd        $E1  2   6
SBC ($98),Y  ;IndIdx        $F1  2   5+

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
STA ($A1,X)  ;IdxInd        $81  2   6
STA ($A2),Y  ;IndIdx        $91  2   6

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

; Illegal / Unofficial Opcodes Start Here
; ---------------------------------------

; We encode all illegal instructions using DCB

; KIL - Crash Processor
;
; Illegal opcode which puts the CPU in an unrecoverable state
;
;                                     N Z C I D V
;                                     - - - - - -
;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
;KIL($02)     Implied       $02  1   -
DCB $02
;KIL($12)     Implied       $12  1   -
DCB $12
;KIL($22)     Implied       $22  1   -
DCB $22
;KIL($32)     Implied       $32  1   -
DCB $32
;KIL($42)     Implied       $42  1   -
DCB $42
;KIL($52)     Implied       $52  1   -
DCB $52
;KIL($62)     Implied       $62  1   -
DCB $62
;KIL($72)     Implied       $72  1   -
DCB $72
;KIL($92)     Implied       $92  1   -
DCB $92
;KIL($B2)     Implied       $B2  1   -
DCB $B2
;KIL($D2)     Implied       $D2  1   -
DCB $D2
;KIL($F2)     Implied       $F2  1   -
DCB $F2

; NOP - Illegal NOP Variants
;
; The NOP instruction causes no changes to the processor other than the normal
; incrementing of the program counter to the next instruction.
;
;                                     N Z C I D V
;                                     - - - - - -
;
;SYNTAX           MODE          HEX LEN TIM
;-----------------------------------------
;NOP($7A)         Implied       $7A  1   2
DCB $7A)
;NOP($5A)         Implied       $5A  1   2
DCB $5A
;NOP($1A)         Implied       $1A  1   2
DCB $1A
;NOP($3A)         Implied       $3A  1   2
DCB $3A
;NOP($DA)         Implied       $DA  1   2
DCB $DA
;NOP($FA)         Implied       $FA  1   2
DCB $FA
;NOP($80) #$00    Immediate     $80  2   2
DCB $80
DCB $00
;NOP($82) #$00    Immediate     $82  2   2
DCB $82
DCB $00
;NOP($89) #$00    Immediate     $89  2   2
DCB $89
DCB $00
;NOP($C2) #$00    Immediate     $C2  2   2
DCB $C2
DCB $00
;NOP($E2) #$00    Immediate     $E2  2   2
DCB $E2
DCB $00
;NOP($04) $00     Zero Page     $04  2   3
DCB $04
DCB $00
;NOP($64) $00     Zero Page     $64  2   3
DCB $64
DCB $00
;NOP($44) $00     Zero Page     $44  2   3
DCB $44
DCB $00
;NOP($0C) $0000   Absolute      $0C  3   4
DCB $0C
DCB $00
DCB $00
;NOP($14) $00,X   Zero Page,X   $14  2   4
DCB $14
DCB $00
;NOP($34) $00,X   Zero Page,X   $34  2   4
DCB $34
DCB $00
;NOP($54) $00,X   Zero Page,X   $54  2   4
DCB $54
DCB $00
;NOP($74) $00,X   Zero Page,X   $74  2   4
DCB $74
DCB $00
;NOP($D4) $00,X   Zero Page,X   $D4  2   4
DCB $D4
DCB $00
;NOP($F4) $00,X   Zero Page,X   $F4  2   4
DCB $F4
DCB $00
;NOP($1C) $0000,X Absolute,X    $1C  3   4+
DCB $1C
DCB $00
DCB $00
;NOP($3C) $0000,X Absolute,X    $3C  3   4+
DCB $3C
DCB $00
DCB $00
;NOP($5C) $0000,X Absolute,X    $5C  3   4+
DCB $5C
DCB $00
DCB $00
;NOP($7C) $0000,X Absolute,X    $7C  3   4+
DCB $7C
DCB $00
DCB $00
;NOP($DC) $0000,X Absolute,X    $DC  3   4+
DCB $DC
DCB $00
DCB $00
;NOP($FC) $0000,X Absolute,X    $FC  3   4+
DCB $FC
DCB $00
DCB $00

; LAX - Load Accumulator and Index X with Memory
;
; Equivalent to LDX value then TXA, or LDA value then TAX.
; 
;
;    M -> X, M -> A                   N Z C I D V
;                                     + + - - - -
;
; Z Zero Flag         Set if X = 0
; N Negative Flag     Set if bit 7 of X is set
;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
;LAX #$00     Immediate     $AB  2   2  *Highly Unstable*
DCB $AB
DCB $00
;LAX $00      Zero Page     $A7  2   3
DCB $A7
DCB $00
;LAX $00,Y    Zero Page,Y   $B7  2   4
DCB $B7
DCB $00
;LAX $0000    Absolute      $AF  3   4
DCB $AF
DCB $00
DCB $00
;LAX $0000,Y  Absolute,Y    $BF  3   4+
DCB $BF
DCB $00
DCB $00
;LAX ($00,X)  IdxInd        $A3  2   6
DCB $A3
DCB $00
;LAX ($00),Y  IndIdx        $B3  2   5+
DCB $B3
DCB $00

; SAX - Stores the Bitwise AND of A and X
;
; Unlike AND, flags are unaffected.
;
;    A AND X -> M                     N Z C I D V
;                                     - - - - - -
;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
;SAX $00      Zero Page     $87  2   3
DCB $87
DCB $00
;SAX $00,Y    Zero Page,Y   $97  2   4
DCB $97
DCB $00
;SAX $0000    Absolute      $8F  3   4
DCB $8F
DCB $00
DCB $00
;SAX ($00,X)  IdxInd        $83  2   6
DCB $83
DCB $00

; SBC - Illegal SBC Variants
;
; Just an alias for the immediate SBC instruction, identical behavior.
;
;                                     N Z C I D V
;                                     + + + - - +
;
;SYNTAX        MODE          HEX LEN TIM
;---------------------------------------
;SBC($EB) #$00 Immediate     $EB  2   2
DCB $EB
DCB $00

; DCP - Combined DEC + CMP
;
; Equivalent to DEC value then CMP value, except supporting more addressing
; modes.
;
;                                     N Z C I D V
;                                     + + + - - -
;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
;DCP $00      Zero Page     $C7  2   5
DCB $C7
DCB $00
;DCP $00,X    Zero Page,X   $D7  2   6
DCB $D7
DCB $00
;DCP $0000    Absolute      $CF  3   6
DCB $CF
DCB $00
DCB $00
;DCP $0000,X  Absolute,X    $DF  3   7
DCB $DF
DCB $00
DCB $00
;DCP $0000,Y  Absolute,Y    $DB  3   7
DCB $DB
DCB $00
DCB $00
;DCP ($00,X)  IdxInd        $C3  2   8
DCB $C3
DCB $00
;DCP ($00),Y  IndIdx        $D3  2   8
DCB $D3
DCB $00

; ISC - Combined INC + SBC
;
; Equivalent to INC value then SBC value, except supporting more addressing
; modes.
;
;                                     N Z C I D V
;                                     + + + - - +
;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
;ISC $00      Zero Page     $E7  2   5
DCB $E7
DCB $00
;ISC $00,X    Zero Page,X   $F7  2   6
DCB $F7
DCB $00
;ISC $0000    Absolute      $EF  3   6
DCB $EF
DCB $00
DCB $00
;ISC $0000,X  Absolute,X    $FF  3   7
DCB $FF
DCB $00
DCB $00
;ISC $0000,Y  Absolute,Y    $FB  3   7
DCB $FB
DCB $00
DCB $00
;ISC ($00,X)  IdxInd        $E3  2   8
DCB $E3
DCB $00
;ISC ($00),Y  IndIdx        $F3  2   8
DCB $F3
DCB $00

; RLA - Combined ROL + AND
;
; Equivalent to ROL value then AND value, except supporting more addressing
; modes.
;
;                                     N Z C I D V
;                                     + + + - - -
;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
;RLA $00      Zero Page     $27  2   5
DCB $27
DCB $00
;RLA $00,X    Zero Page,X   $37  2   6
DCB $37
DCB $00
;RLA $0000    Absolute      $2F  3   6
DCB $2F
DCB $00
DCB $00
;RLA $0000,X  Absolute,X    $3F  3   7
DCB $3F
DCB $00
DCB $00
;RLA $0000,Y  Absolute,Y    $3B  3   7
DCB $3B
DCB $00
DCB $00
;RLA ($00,X)  IdxInd        $23  2   8
DCB $23
DCB $00
;RLA ($00),Y  IndIdx        $33  2   8
DCB $33
DCB $00

; RLA - Combined ROR + ADC
;
; Equivalent to ROR value then ADC value, except supporting more addressing
; modes. Essentially this computes A + value / 2, where value is 9-bit and the
; division is rounded up. 
;
;                                     N Z C I D V
;                                     + + + - - +
;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
;RRA $00      Zero Page     $67  2   5
DCB $67
DCB $00
;RRA $00,X    Zero Page,X   $77  2   6
DCB $77
DCB $00
;RRA $0000    Absolute      $6F  3   6
DCB $6F
DCB $00
DCB $00
;RRA $0000,X  Absolute,X    $7F  3   7
DCB $7F
DCB $00
DCB $00
;RRA $0000,Y  Absolute,Y    $7B  3   7
DCB $7B
DCB $00
DCB $00
;RRA ($00,X)  IdxInd        $63  2   8
DCB $63
DCB $00
;RRA ($00),Y  IndIdx        $73  2   8
DCB $73
DCB $00

; SLO - Combined ASL + ORA
;
; Equivalent to ASL value then ORA value, except supporting more addressing
; modes.
;
;                                     N Z C I D V
;                                     + + + - - -
;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
;SLO $00      Zero Page     $07  2   5
DCB $07
DCB $00
;SLO $00,X    Zero Page,X   $17  2   6
DCB $17
DCB $00
;SLO $0000    Absolute      $0F  3   6
DCB $0F
DCB $00
DCB $00
;SLO $0000,X  Absolute,X    $1F  3   7
DCB $1F
DCB $00
DCB $00
;SLO $0000,Y  Absolute,Y    $1B  3   7
DCB $1B
DCB $00
DCB $00
;SLO ($00,X)  IdxInd        $03  2   8
DCB $03
DCB $00
;SLO ($00),Y  IndIdx        $13  2   8
DCB $13
DCB $00

; SRE - Combined LSR + EOR
;
; Equivalent to LSR value then EOR value, except supporting more addressing
; modes.
;
;                                     N Z C I D V
;                                     + + + - - -
;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
;SRE $00      Zero Page     $47  2   5
DCB $47
DCB $00
;SRE $00,X    Zero Page,X   $57  2   6
DCB $57
DCB $00
;SRE $0000    Absolute      $4F  3   6
DCB $4F
DCB $00
DCB $00
;SRE $0000,X  Absolute,X    $5F  3   7
DCB $5F
DCB $00
DCB $00
;SRE $0000,Y  Absolute,Y    $5B  3   7
DCB $5B
DCB $00
DCB $00
;SRE ($00,X)  IdxInd        $43  2   8
DCB $43
DCB $00
;SRE ($00),Y  IndIdx        $53  2   8
DCB $53
DCB $00

; ANC - Sets SR based on A AND M
;
; Does A AND M, setting N and Z flags based on the result. Then it copies N
; (bit 7) to C. Stores result in A.
;
;    A = A AND M                      N Z C I D V
;                                     + + + - - -
;
; Z Zero Flag         Set if A = 0
; N Negative Flag     Set if bit 7 set
; C Carry Flag        Set if bit 7 set
;
;SYNTAX        MODE         HEX LEN TIM
;--------------------------------------
;ANC($0B) #$00 Immediate    $0B  2   2
DCB $0B
DCB $00
;ANC($2B) #$00 Immediate    $2B  2   2
DCB $2B
DCB $00

; ALR - Combined AND + LSR
;
; Does A AND M followed by LSR A.
;
;    A = A AND M                      N Z C I D V
;    0 -> [76543210] -> C             0 + + - - -
;
; C Carry Flag        Set to contents of old bit 0
; Z Zero Flag         Set if result = 0
; N Negative Flag     Set to 0
;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
;ALR #$00     Immediate     $4B  2   2
DCB $4B
DCB $00

; ARR - Combined AND + ROR with different SR effects
;
; Similar to doing AND Immediate followed by ROR A, but setting the flags
; C and V differently.
;
; In decimal mode, this instruction has some rather strange behavior,
; explained in detail in http://www.viceteam.org/plain/64doc.txt and
; http://sourceforge.net/p/vice-emu/code/27740/tree/trunk/vice/src/
; 6510core.c#l678
;
;                                     N Z C I D V
;                                     + + + - - +
; Z Zero Flag         Set if A = 0
; N Negative Flag     Set if bit 7 of the result is set (carry in)
; C Carry Flag        Set if bit 6 of the result is set
; V Overflow Flag     Set to Bit5 ^ Bit6
;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
;ARR #$00     Immediate     $6B  2   2
DCB $6B
DCB $00

; XAA - Unstable opcode, like a three register AND
;
; See http://visual6502.org/wiki/index.php?title=6502_Opcode_8B_%28XAA,_ANE%29
; for a good analysis. This opcode is not stable on a real 6502 and its
; result depends on analog effects and varies between different CPUs and
; operating conditions.
;
;  A = (A OR magic) AND X AND M       N Z C I D V
;                                     + + - - - -
;
; Z Zero Flag         Set if A = 0
; N Negative Flag     Set if bit 7 of A is set
;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
;XAA #$00     Immediate     $8B  2   2  *Highly Unstable*
DCB $8B
DCB $00

; AHX - Store A & X & (ADDR_HI + 1) into M
;
; There are some conflicting description of what this opcode does, but this
; version seems to match the reference best. The result to be written is used
; as MSB of the storage address if we cross a page boundary.
;
;  A & X & (ADDR_HI + 1) -> M         N Z C I D V
;                                     - - - - - -
;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
;AHX ($00),Y  IndIdx        $93  2   6
DCB $93
DCB $00
;AHX $0000,Y  Absolute,Y    $9F  3   5
DCB $9F
DCB $00
DCB $00

; TAS - AND A, X, SP, ADDR_HI
;
; AND X register with accumulator and store result in stack pointer, then AND
; stack pointer with the high byte of the target address of the argument + 1.
; Store result in memory. The result to be written is used as MSB of the
; storage address if we cross a page boundary.
;
;  X AND A -> SP                      N Z C I D V
;  SP AND (ADDR_HI + 1) -> M          - - - - - -
;
;SYNTAX      MODE           HEX LEN TIM
;--------------------------------------
;TAS $0000,Y Absolute,Y     $9B  3   5
DCB $9B
DCB $00
DCB $00

; SHX - X AND ADDR_HI
;
; AND X register with the high byte of the target address of the argument + 1.
; Store the result in memory. The result to be written is used as MSB of the
; storage address if we cross a page boundary.
;
;  X AND (ADDR_HI + 1) -> M           N Z C I D V
;                                     - - - - - -
;
;SYNTAX      MODE           HEX LEN TIM
;--------------------------------------
;SHX $0000,Y Absolute,Y     $9E  3   5
DCB $9E
DCB $00
DCB $00

; SHY - Y AND ADDR_HI
;
; AND Y register with the high byte of the target address of the argument + 1.
; Store the result in memory. The result to be written is used as MSB of the
; storage address if we cross a page boundary.
;
;  Y AND (ADDR_HI + 1) -> M           N Z C I D V
;                                     - - - - - -
;
;SYNTAX      MODE           HEX LEN TIM
;--------------------------------------
;SHY $0000,X Absolute,X     $9C  3   5
DCB $9C
DCB $00
DCB $00

; LAS - Load A, X and SP with SP AND M
;
; AND memory with stack pointer, transfer result to accumulator, X register
; and stack pointer.
;
;    SP AND M -> A, X, SP             N Z C I D V
;                                     + + - - - -
;
; Z Zero Flag         Set if A = 0
; N Negative Flag     Set if bit 7 set
;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
;LAS $0000,Y  Absolute,Y    $BB  3   4+
DCB $BB
DCB $00
DCB $00

; AXS - Store A AND X minus M into X
;
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
;
;SYNTAX       MODE          HEX LEN TIM
;--------------------------------------
;AXS #$00     Immediate     $CB  2   2
DCB $CB
DCB $00

; +  = Add 1 to cycles if page boundary is crossed during address computation
;      Store instructions have this extra cycle always build in as they can't
;      do a false write when a carry on the address LSB occurs
; ++ = Add 1 to cycles if branch is taken, one more if branch occurs to different page

