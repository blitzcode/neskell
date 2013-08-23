
; Test the page wrapping bug in the indirect addressing mode of the JMP instruction
;
; Expected Value: $00 = $65
; 
; 6502js fails this test, works fine in Visual 6502, of course

; Setup addresses
LDA #$16
STA $02FF ; LSB
LDA #$00
STA $0300 ; MSB it should read
LDA #$06
STA $0200 ; MSB it actually will read

; Put BRK at zp location of the 'correct' jump
LDA #$00
STA $16

JMP ($02FF)

target: ; $0616
LDA #$65
STA $00

