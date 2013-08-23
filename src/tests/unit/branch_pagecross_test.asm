
; Test extra cycle added for page crossing in branch instructions
;
; Load at address 0x02F9
;
; Result: A = FF, cycles = 14 (middle branch crossed page)

LDA #$01

BNE branch1
branch1:

BNE branch2
NOP
; page 0x03 starts here
branch2:

BNE branch3
branch3:

LDA #$FF

