
; Tests CV flags as described in http://www.6502.org/tutorials/vflag.html
; Also tests CLC/SEC/PHP and the NZ flags
; Note that 6502js does not set the I flag by default, so stack will differ
; without an additional SEI

CLC      ; 1 + 1 = 2, returns C = 0
LDA #$01
ADC #$01
PHP ; 0x34 = --I-B1--

CLC      ; 1 + -1 = 0, returns C = 1
LDA #$01
ADC #$FF
PHP ; 0x37 = CZI-B1--

CLC      ; 127 + 1 = 128, returns C = 0
LDA #$7F
ADC #$01
PHP ; 0xF4 = --I-B1VN

CLC      ; -128 + -1 = -129, returns C = 1
LDA #$80
ADC #$FF
PHP ; 0x75 = C-I-B1V-

CLC      ; 1 + 1 = 2, returns V = 0
LDA #$01
ADC #$01
PHP ; 0x34 = --I-B1--

CLC      ; 1 + -1 = 0, returns V = 0
LDA #$01
ADC #$FF
PHP ; 0x37 = CZI-B1--

CLC      ; 127 + 1 = 128, returns V = 1
LDA #$7F
ADC #$01
PHP ; 0xF4 = --I-B1VN

CLC      ; -128 + -1 = -129, returns V = 1
LDA #$80
ADC #$FF
PHP ; 0x75 = C-I-B1V-

SEC      ; 0 - 1 = -1, returns V = 0
LDA #$00
SBC #$01
PHP ; 0xB4 = --I-B1-N

SEC      ; -128 - 1 = -129, returns V = 1
LDA #$80
SBC #$01
PHP ; 0x75 = C-I-B1V-

SEC      ; 127 - -1 = 128, returns V = 1
LDA #$7F
SBC #$FF
PHP ; 0xF4 = --I-B1VN

SEC      ; 0 - 128 = 128, returns V = 1
LDA #$00
SBC #$80
PHP ; 0xF4 = --I-B1VN

