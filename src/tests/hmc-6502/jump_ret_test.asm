
; Tests instructions JMP (both addressing modes) & JSR & RTS.
; Assumes that loads & stores & ORA work with all addressing modes.
;
; NOTE: Depends on addresses of instructions... Specifically, the "final"
;       address is actually hard-coded at address $0020 (first 4 lines of code).
;       Additionally, a JMP and JSR specify specific addresses.
;
; Expected Results: $40=0x42

start:
LDA #$24
STA $20
LDA #$06
STA $21
LDA #$00
ORA #$03
JMP jump1
ORA #$FF ; not done

jump1:
ORA #$30
JSR subr
ORA #$42
JMP ($0020)
ORA #$FF ; not done

subr:
STA $30
LDX $30
LDA #$00
RTS

final:
STA $0D,X
