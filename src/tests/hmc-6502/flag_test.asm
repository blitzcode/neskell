
; Tests flag instructions (CLC & SEC & CLV & CLD & SED) & NOP.
; Assumes that loads & stores (all addressing modes) and all branches work.
; Also assumes ADC works with all addressing modes.
;
; Expected Results: $30 = 0xCE

start:
LDA #$99
ADC #$87
CLC
NOP
BCC bcc1 ; taken
ADC #$60 ; not done
ADC #$93 ; not done
bcc1:
SEC
NOP
BCC bcc2 ; not taken
CLV
bcc2:
BVC bvc1 ; taken
LDA #$00 ; not done
bvc1:
ADC #$AD
NOP
STA $30

