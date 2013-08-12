
# Test taken from hmc-6502
# http://code.google.com/p/hmc-6502/source/browse/trunk/emu/testvectors/TestAllInstructions/

# Tests all other branch instructions (BPL & BMI & BVC & BVS & BCC & BCS).
# Assumes that ADC & SBC & EOR work with all addressing modes.
#
# Expected Results: $80 = 0x1F

start:
; prepare memory
LDA #$54
STA $32
LDA #$B3
STA $A1
LDA #$87
STA $43

; BPL
LDX #$A1
BPL bpl1 ; not taken
LDX #$32
bpl1:
LDY $00,X
BPL bpl2 ; taken
LDA #$05 ; not done
LDX $A1 ; not done
bpl2:

; BMI
BMI bmi1 ; not taken
SBC #$03
bmi1:
BMI bmi2 ; taken
LDA #$41 ; not done
bmi2:

; BVC
EOR #$30
STA $32
ADC $00,X
BVC bvc1 ; not taken
LDA #$03
bvc1:
STA $54
LDX $00,Y
ADC $51,X
BVC bvc2 ; taken
LDA #$E5 ; not done
bvc2:

; BVS
ADC $40,X
BVS bvs1 ; not taken
STA $0001,Y
ADC $55
bvs1:
BVS bvs2 ; taken
LDA #$00
bvs2:

; BCC
ADC #$F0
BCC bcc1 ; not taken
STA $60
ADC $43
bcc1:
BCC bcc2 ; taken
LDA #$FF
bcc2:

; BCS
ADC $54
BCS bcs1 ; not taken
ADC #$87
LDX $60
bcs1:
BCS bcs2 ; taken
LDA #$00 ; not done
bcs2:
STA $73,X

