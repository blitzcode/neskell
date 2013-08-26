
; Test the following unofficial / undocumented BCD behavior:
;
; - ADC/SBC with invalid BCD operands
; - Status of NVZ after BCD operations
;
; Expected Result: SP = $DD
;
; After the test the stack should look like this:
;
;      0  1  2  3  4  5  6  7  8  9  A  B  C  D  E  F
; 01D0                                           9A BD
; 01E0 9A BD 0A 3D 0A 3D 99 BC 00 3F 99 BC 76 3C 74 3C
; 01F0 E0 BD D0 7D 66 3F 65 3D 75 7D 80 FC 80 FC 00 3E
;
; (Visual 6502 & VICE used as reference)

LDX #$FF
TXS

SED

; Small BCD edge case / undocumented behavior test suite from
; http://visual6502.org/wiki/index.php?title=6502DecimalMode

; Tests for ADC

LDA #$00
CLC
ADC #$00 ; Gives 00 and N=0 V=0 Z=1 C=0
PHP ; $01FF
PHA ; $01FE

LDA #$79
SEC
ADC #$00 ; Gives 80 and N=1 V=1 Z=0 C=0
PHP ; $01FD
PHA ; $01FC

LDA #$24                               
CLC                                    
ADC #$56 ; Gives 80 and N=1 V=1 Z=0 C=0  
PHP ; $01FB
PHA ; $01FA

LDA #$93                               
CLC                                    
ADC #$82 ; Gives 75 and N=0 V=1 Z=0 C=1  
PHP ; $01F9
PHA ; $01F8

LDA #$89                               
CLC                                    
ADC #$76 ; Gives 65 and N=0 V=0 Z=0 C=1  
PHP ; $01F7
PHA ; $01F6

LDA #$89                               
SEC                                    
ADC #$76 ; Gives 66 and N=0 V=0 Z=1 C=1  
PHP ; $01F5
PHA ; $01F4

LDA #$80
CLC 
ADC #$F0 ; Gives D0 and N=0 V=1 Z=0 C=1
PHP ; $01F3
PHA ; $01F2

LDA #$80
CLC 
ADC #$FA ; Gives E0 and N=1 V=0 Z=0 C=1
PHP ; $01F1
PHA ; $01F0

LDA #$2F
CLC 
ADC #$4F ; Gives 74 and N=0 V=0 Z=0 C=0
PHP ; $01EF
PHA ; $01EE

LDA #$6F
SEC 
ADC #$00 ; Gives 76 and N=0 V=0 Z=0 C=0
PHP ; $01ED
PHA ; $01EC

; Tests for SBC

LDA #$00
CLC
SBC #$00 ; Gives 99 and N=1 V=0 Z=0 C=0
PHP ; $01EB
PHA ; $01EA

LDA #$00
SEC
SBC #$00 ; Gives 00 and N=0 V=0 Z=1 C=1
PHP ; $01E9
PHA ; $01E8

LDA #$00
SEC
SBC #$01 ; Gives 99 and N=1 V=0 Z=0 C=0
PHP ; $01E7
PHA ; $01E6

LDA #$0A
SEC
SBC #$00 ; Gives 0A and N=0 V=0 Z=0 C=1
PHP ; $01E5
PHA ; $01E4

LDA #$0B
CLC
SBC #$00 ; Gives 0A and N=0 V=0 Z=0 C=1
PHP ; $01E3
PHA ; $01E2

LDA #$9A
SEC
SBC #$00 ; Gives 9A and N=1 V=0 Z=0 C=1
PHP ; $01E1
PHA ; $01E0

LDA #$9B
CLC
SBC #$00 ; Gives 9A and N=1 V=0 Z=0 C=1
PHP ; $01DF
PHA ; $01DE

; TODO: http://www.6502.org/tutorials/decimal_mode.html

