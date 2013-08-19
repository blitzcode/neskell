
# Test all variants of illegal NOP opcode

NOI #$7A
NOI #$5A
NOI #$1A
NOI #$3A
NOI #$DA
NOI #$FA
NOI #$80 #$00
NOI #$82 #$00
NOI #$89 #$00
NOI #$C2 #$00
NOI #$E2 #$00
NOI #$04 $00
NOI #$64 $00
NOI #$44 $00
NOI #$0C $0000
NOI #$14 $00,X
NOI #$34 $00,X
NOI #$54 $00,X
NOI #$74 $00,X
NOI #$D4 $00,X
NOI #$F4 $00,X
NOI #$1C $0000,X
NOI #$3C $0000,X
NOI #$5C $0000,X
NOI #$7C $0000,X
NOI #$DC $0000,X
LDX #$01 ; Test page crossing, next NOI should take 5 cycles
NOI #$FC $00FF,X
