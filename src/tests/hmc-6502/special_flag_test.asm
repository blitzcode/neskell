
; Tests SEI & CLI & SED & CLD.
; Assumes prior tests pass...
;
; Expected Result: $21 = 0x6E
; 
; Note that the expected result depends on the initial value of the flags. On
; Visual 6502 the Z flag is set on reset, js6502 doesn't have I set, etc.

SEI
SED
PHP
PLA
STA $20
CLI
CLD
PHP
PLA
ADC $20
STA $21

