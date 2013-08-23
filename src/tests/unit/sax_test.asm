
; Test unofficial SAX opcode
; Result:
; 0000: 80 80 80 80...

LDA #$03
STA $81
LDA #$00
STA $82

LDA #$AA
LDX #$81

;SAX $00
DCB $87
DCB $00

LDY #$01
;SAX $00,Y
DCB $97
DCB $00

;SAX $0002
DCB $8F
DCB $02
DCB $00

;SAX ($00,X)
DCB $83
DCB $00

