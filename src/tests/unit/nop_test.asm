
; Test all variants of illegal NOP opcode

NOP($7A)
NOP($5A)
NOP($1A)
NOP($3A)
NOP($DA)
NOP($FA)
NOP($80) #$00
NOP($82) #$00
NOP($89) #$00
NOP($C2) #$00
NOP($E2) #$00
NOP($04) $00
NOP($64) $00
NOP($44) $00
NOP($0C) $0000
NOP($14) $00,X
NOP($34) $00,X
NOP($54) $00,X
NOP($74) $00,X
NOP($D4) $00,X
NOP($F4) $00,X
NOP($1C) $0000,X
NOP($3C) $0000,X
NOP($5C) $0000,X
NOP($7C) $0000,X
NOP($DC) $0000,X
LDX #$01 ; Test page crossing, next NOP should take 5 cycles
NOP($FC) $00FF,X
