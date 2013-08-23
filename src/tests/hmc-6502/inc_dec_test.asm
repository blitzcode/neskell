
; Tests instructions INC & DEC with all addressing modes.
; Assumes that LDA/LDX/LDY & STA/STX/STY work with all addressing modes.
;
; Expected Results: $71=0xFF

start:
LDA #$FF
LDX #$00

STA $90
INC $90
INC $90
LDA $90
LDX $90

STA $90,X
INC $90,X
LDA $90,X
LDX $91

STA $0190,X
INC $0192
LDA $0190,X
LDX $0192

STA $0190,X
INC $0190,X
LDA $0190,X
LDX $0193

STA $0170,X
DEC $0170,X
LDA $0170,X
LDX $0174

STA $0170,X
DEC $0173
LDA $0170,X
LDX $0173

STA $70,X
DEC $70,X
LDA $70,X
LDX $72

STA $70,X
DEC $71
DEC $71
