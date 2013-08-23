
; Tests RTI instruction.
; Assumes lots of other instructions work already...
;
; Expected Results: $33 = 0x42

start:
CLC
LDA #$42
BCC runstuff
STA $33
BCS end
runstuff:
LDA #$06 ; Assumes our program is loaded at 0x0600, RTI should set the PC right
         ; after the first CLC instruction
PHA
LDA #$01
PHA
SEC
PHP
CLC
RTI
end:

