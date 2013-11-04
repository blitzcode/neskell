
# Neskell - A Haskell 6502 Emulator

![6502](https://raw.github.com/blitzcode/neskell/master/6502.png)

This repository contains the Haskell source code of an emulator for the [6502 CPU](http://en.wikipedia.org/wiki/6502). The goal of the project was to learn more about Haskell, the 6502 and CPU emulation as well as developing an important piece of a future [Nintendo Entertainment System (NES)](http://en.wikipedia.org/wiki/Nintendo_Entertainment_System) emulator. So far, only the emulation of the NMOS 6502 / 2A03 is largely complete, no work on the PPU / APU has been attempted yet.

# Building

Tested on OS X 10.6 with the [Haskell Platform](http://www.haskell.org/platform/) 2013.2.0.0, building should succeed with a simple

    $ make

The only dependency not already provided by the Haskell Platform is the `ansi-terminal` package, please install it from [Hackage](http://hackage.haskell.org/) using Cabal.

# Tests

### Running

Having comprehensive and easy to run tests is key for developing any emulator. Neskell features a test suite comprised of a number of custom and well-established tests from the community.

There's a nice progress display as the tests run using multiple CPU cores:

![testing](https://raw.github.com/blitzcode/neskell/master/screenshot.png)

The command line interface for running tests is as follows:

    Usage: neskell [OPTION...]
      -t[NAME]  --test[=NAME]        run full test suite / all tests containing NAME
      -q        --quick-test         run quick (<1s) test suite
      -l[NAME]  --list-tests[=NAME]  list all tests / all tests containing NAME
      -s        --no-verbose         less flashy test result display
      ...
    +RTS -? runtime system help, +RTS -N2 use two cores, +RTS -N use all cores

The Makefile will automatically run the quick test suite after each successful build:

    time ./neskell --quick-test --no-verbose +RTS -N -s
    All 31 tests OK

The `time` and `-s` RTS option should help detect performance regression during development as well.

### Tracing

The most powerful tool to debug why a given test fails is looking at the emulator's trace log. The build-in tests have various default levels of tracing, but those can be overridden with the following command line option:

      -r [nbe]  --trace=[nbe]        override test tracing to [n]one, [b]asic, full [e]xecution

Traces for each test go into a per-test log file in the `src/trace` directory. A basic trace looks as follows:

    Trace Log 2013-11-04 13:18:32.526098 CET
    
    --- Branch Pagecrossing Test ---
    
    Load Binary:
    
    02F9 A9 01 D0 00 D0 01 EA D0 00 A9 FF
    
    Setup: FF→SP 24→SR 0000→PC 02F9→PC
    
    Cycles  PC   AC IX IY Status Reg. SP Instr. Operand ILnCycl Op. Load  Stores
    Elapsed $PC  $A $X $Y $P:NV1BDIZC $S $I:Mne Data    [OIU]bC $Adr→$Val $Val→$Dst
    -------------------------------------------------------------------------------
    (Execution trace disabled)
    
    Zero Page:
    
    (All Zero)
    
    Stack:
    
    (All Zero)
    
    Succeeded:
        Stop Reason      [ OpCode(PC) == BRK ]
        Unmet Conditions [ ]
        Met Conditions   [ A == $FF
                         , Cycle ∈ [ 14 , 14 ]
                         ]
        CPU State        C:0000014 PC:$0304 A:$FF X:$00 Y:$00 SR:$A4:N·1··I·· SP:$FF
        Next Instruction BRK

The emulator APIs allow to set up the CPU and RAM state, set a termination criteria and verify a number of success / failure conditions to be met.

For further debugging, a full execution trace can be enabled with `--trace=e` (it's already the default for all tests with a short runtime):

    Trace Log 2013-11-04 13:28:10.597951 CET
    
    --- BRK Test ---
    
    Load Binary:
    
    0600 4C 06 06 C6 FF 40 A9 03 8D FE FF A9 06 8D FF FF
    0610 A9 45 85 FF 00 EA E6 FF 00 EA A9 00 38 00 EA 65
    0620 FF 85 FF EA
    
    Setup: FF→SP 24→SR 0000→PC 0600→PC 
    
    Cycles  PC   AC IX IY Status Reg. SP Instr. Operand ILnCycl Op. Load  Stores
    Elapsed $PC  $A $X $Y $P:NV1BDIZC $S $I:Mne Data    [OIU]bC $Adr→$Val $Val→$Dst
    -------------------------------------------------------------------------------
    0000000 0600 00 00 00 24:··1··I·· FF 4C:JMP $0606   O3b3C   -         0606→PC 
    0000003 0606 00 00 00 24:··1··I·· FF A9:LDA #$03    O2b2C   -         24→SR 03→A 0608→PC 
    0000005 0608 03 00 00 24:··1··I·· FF 8D:STA $FFFE   O3b4C   -         03→FFFE 060B→PC 
    0000009 060B 03 00 00 24:··1··I·· FF A9:LDA #$06    O2b2C   -         24→SR 06→A 060D→PC 
    0000011 060D 06 00 00 24:··1··I·· FF 8D:STA $FFFF   O3b4C   -         06→FFFF 0610→PC 
    0000015 0610 06 00 00 24:··1··I·· FF A9:LDA #$45    O2b2C   -         24→SR 45→A 0612→PC 
    0000017 0612 45 00 00 24:··1··I·· FF 85:STA $FF     O2b3C   -         45→00FF 0614→PC 
    0000020 0614 45 00 00 24:··1··I·· FF 00:BRK         O1b7C   -         06→01FF 16→01FE FD→SP 34→01FD FC→SP 24→SR 0603→PC 
    0000027 0603 45 00 00 24:··1··I·· FC C6:DEC $FF     O2b5C   00FF→45   24→SR 44→00FF 0605→PC 
    0000032 0605 45 00 00 24:··1··I·· FC 40:RTI         O1b6C   -         FD→SP 24→SR FF→SP 0616→PC 
    0000038 0616 45 00 00 24:··1··I·· FF E6:INC $FF     O2b5C   00FF→44   24→SR 45→00FF 0618→PC 
    0000043 0618 45 00 00 24:··1··I·· FF 00:BRK         O1b7C   -         06→01FF 1A→01FE FD→SP 34→01FD FC→SP 24→SR 0603→PC 
    0000050 0603 45 00 00 24:··1··I·· FC C6:DEC $FF     O2b5C   00FF→45   24→SR 44→00FF 0605→PC 
    0000055 0605 45 00 00 24:··1··I·· FC 40:RTI         O1b6C   -         FD→SP 24→SR FF→SP 061A→PC 
    0000061 061A 45 00 00 24:··1··I·· FF A9:LDA #$00    O2b2C   -         26→SR 00→A 061C→PC 
    0000063 061C 00 00 00 26:··1··IZ· FF 38:SEC         O1b2C   -         27→SR 061D→PC 
    0000065 061D 00 00 00 27:··1··IZC FF 00:BRK         O1b7C   -         06→01FF 1F→01FE FD→SP 37→01FD FC→SP 27→SR 0603→PC 
    0000072 0603 00 00 00 27:··1··IZC FC C6:DEC $FF     O2b5C   00FF→44   25→SR 43→00FF 0605→PC 
    0000077 0605 00 00 00 25:··1··I·C FC 40:RTI         O1b6C   -         FD→SP 27→SR FF→SP 061F→PC 
    0000083 061F 00 00 00 27:··1··IZC FF 65:ADC $FF     O2b3C   00FF→43   44→A 24→SR 0621→PC 
    0000086 0621 44 00 00 24:··1··I·· FF 85:STA $FF     O2b3C   -         44→00FF 0623→PC 
    
    Zero Page:
    
    00F0 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 44
    
    Stack:
    
    01F0 00 00 00 00 00 00 00 00 00 00 00 00 00 37 1F 06
    
    Succeeded:
        Stop Reason      [ OpCode(PC) == NOP ]
        Unmet Conditions [ ]
        Met Conditions   [ $00FF == $44
                         , SR == $24:··1··I··
                         , SP == $FF
                         , Cycle ∈ [ 89 , 89 ]
                         ]
        CPU State        C:0000089 PC:$0623 A:$44 X:$00 Y:$00 SR:$24:··1··I·· SP:$FF
        Next Instruction NOP

The execution trace has been carefully designed to be as compact as possible while still conveying all aspects of the CPU's state and the executed instructions acting upon it. Cycle data (included penalty cycles for page crossing etc.) as well as memory loads and stores are recorded, helping with comparison against a reference or figuring out when and for which reason execution diverged.

The trace is written to a fixed-size ringbuffer during execution (see `RingBufferLog.hs`), so the beginning may be truncated for very long sessions.

Note that the emulator recognizes [Blargg's test ROMs](http://blargg.8bitalley.com/nes-tests/) and will output extra diagnostic information into the trace:

    Blargg Test:
        Memory at 0x6001 indicates we are running one of Blargg's test ROMs
        Result Code: 0x00
        Status Text: \n01-basics\n\nPassed\n

### Test Suite

All tests are passing and located in `src/tests`.

* `6502_functional_tests` contains [Klaus Dormann's](http://2m5.de/6502_Emu/) well-known 6502 Functional Tests. It's a very comprehensive test for all of the documented behavior of the 6502. Here's the `readme.txt`, documenting how Neskell's version of the test was obtained / assembled:

```
6502 functional tests version 23-jul-2013
Taken from http://2m5.de/6502_Emu/6502_functional_tests.zip
-----------------------------------------------------------

6502_functional_test.bin is 6502_functional_test.a65 assembled with the
AS65 assembler:

as65 -z 6502_functional_test.a65

This gives a single binary image. I used the Linux version of AS65 from:

http://www.kingswood-consulting.co.uk/assemblers/

6502_functional_test.lst is a listing for debugging purposes generated by:

as65 -l -m -h0

The source needs to have to have the following flag set to produce
tighter object code (<16K):

load_data_direct = 0

The binary can then be loaded at 0x400. Set the PC to that address and go.

See here for a hacked version of Visual 6502 and instructions on how to
generate a reference trace log:

https://github.com/blitzcode/visual6502

The file v6502_first_235k_cycles.zip contains a trace of the first ~235K cycles
from Visual 6502.
```

* `decoding` contains the instruction decoding and disassembly test. The file `instr_test.asm` is not only a comprehensive test of all instructions, but contains documentation collected from various sources, see the preamble from the file:

```
Test file containing all possible instruction and addressing mode
combinations, including a description of their operation, binary
representation, effect on CPU registers and execution timing.

This file is basically a carefully merged version of the references below
and used as ground truth during emulator development, also suitable as an
assembler / disassembler test. Nice to have it all in one place, plus each
of these documents has typos, omissions and errors which were discovered
during the N-way merge and some Visual 6502 debugging sessions.

For testing disassembly / instruction decoding, the file 'instr_test.bin' is
an assembled version of this file, and its disassembly should match
'instr_test_ref_disasm.asm'. Those two files were generated using the tools
of 6502js (https://github.com/skilldrick/6502js). The hexdump output
can be converted to a binary with 'cat hexdmp | xxd -r -seek -1536 > bin'.
Note that all instruction arguments are sequential numbers from $00. A
bug in 6502js prevents the relative addressing in branch instructions to
assemble, only labels can be targeted. As a workaround, all branch
instructions target 'lbl' and were later manually fixed in the binary /
disassembly to their correct targets (sequential numbers). There's also a
fixed version available at http://biged.github.io/6502js/. At the end of the
file are also all supported illegal / unofficial opcodes.

References / Sources / Originals:

http://www.6502.org/tutorials/6502opcodes.html
http://e-tradition.net/bytes/6502/6502_instruction_set.html
http://www.obelisk.demon.co.uk/6502/reference.html
http://www.atariarchives.org/alp/appendix_1.php
http://visual6502.org/wiki/index.php?title=6502_all_256_Opcodes
http://wiki.nesdev.com/w/index.php/Programming_with_unofficial_opcodes
http://www.oxyron.de/html/opcodes02.html
http://www.viceteam.org/plain/64doc.txt
http://www.ataripreservation.org/websites/freddy.offenga/illopc31.txt
```

* `hmc-6502` contains the test suite of the [hmc-6502](http://code.google.com/p/hmc-6502/) project. It's not very in-depth and comprehensive, but provided a great first set of tests to run against during development. Tests have been extended and fixed, assembled using [6502js](https://github.com/skilldrick/6502js). Also see the notes in the .asm files and the 'readme.txt' from the directory:

```
All the tests in this directory have been taken from the hmc-6502 project, specifically

http://code.google.com/p/hmc-6502/source/browse/trunk/emu/testvectors/TestAllInstructions/

The code is licensed under BSD 3 / New BSD License. Many of the tests have been changed / extended.
```

* `instr_misc` contains [Blargg's](http://slack.net/~ant/) 'NES CPU Instruction Behavior Misc Tests'. The .bin files are extracted raw binaries of just the 6502 code from the test's original .nes files. See the 'readme.txt' (another one in the 'src' subdirectory) for more information on the tests. We only run the tests which can be verified / work on a pure 6502 platform (no other hardware connected).

* `instr_test-v4` contains [Blargg's](http://slack.net/~ant/) 'NES CPU Instruction Behavior Tests'. These tests verify most instruction behavior fairly thoroughly, including unofficial instructions. All tests passing.

* `nestest` contains [Kevin Horton's](http://kevtris.org/nes/) 'Ultimate NES CPU test ROM', a quick running series of CPU tests covering a large amount of documented and undocumented behavior.

* `unit` contains (mostly) new tests written just for Neskell. Specifically:
      - `add_sub_cvzn_flag_test.asm` Test CV flags as described in http://www.6502.org/tutorials/vflag.html
      - `ahx_tas_shx_shy_pagecross_test.asm` Test the pagecrossing behavior of AHX/TAS/SHX/SHY
      - `ahx_tas_shx_shy_test.asm` Test AHX,TAS,SHX,SHY (the & ADDR_HI instructions) in all addressing modes
      - `arr_bcd_test.asm` Test the decimal mode of the illegal ARR opcode
      - `bcd_add_sub_test.asm` Tests from http://www.6502.org/tutorials/decimal_mode.html
      - `branch_backwards_test.asm` Simple test checking if we correctly reach branch targets with negative offsets
      - `branch_pagecross_test.asm` Test extra cycle added for page crossing in branch instruction
      - `brk_test.asm` BRK interrupt test
      - `full_bcd_test.asm` Full BCD test from http://www.6502.org/tutorials/decimal_mode.html#B
      - `illegal_bcd_test.asm` Test undocumented BCD behavior of ADC/SBC with invalid BCD operands and status of NVZ after BCD operations
      - `illegal_rmw_test.asm` Test all illegal RMW opcodes (DCP,ISC,RLA,RRA,SLO,SRE) in all of their addressing modes
      - `illegal_xb_test.asm` Test ANC,ALR,ARR,XAA,LAS,AXS (0x*B opcodes, all with AND) in all addressing 3 modes
      - `jump_bug_test.asm` Test the page wrapping bug in the indirect addressing mode of the JMP instruction
      - `kil_test.asm` Test undocumented KIL opcode
      - `lax_test.asm` Test illegal / unofficial LAX opcode
      - `nop_test.asm` Test all variants of illegal NOP opcode
      - `sax_test.asm` Test unofficial SAX opcode

These references and tests have been invaluable during the development of Neskell, many thanks to all the individuals who spent the time and energy to make them!

# Accuracy

### Goal

Neskell aspires to emulate an NMOS 6502 or 2A03 (NES CPU) as accurately as possible. Every difference between the real hardware and the emulation observable by the program (or the user, once it is more than a CPU emulator) is considered a bug to be fixed. Currently, Neskell emulates all official and unofficial instructions with bit and cycle accuracy, including all known bugs and BCD behaviors. Certain aspects such as fake / double writes and sub-instruction timing have not been implemented yet, as they are only observable once extra hardware besides the CPU is emulated.

### Comparison

[Visual 6502](http://visual6502.org/) and [VICE](http://www.viceteam.org/) have been used extensively as references for the test cases used.

During the development of Neskell a few discrepancies between the two reference emulators and / or the actual hardware have been discovered. See `src/tests/unit/ahx_tas_shx_shy_pagecross_test.asm` / `src/tests/unit/illegal_xb_test.asm` and the discussion at [Illegal Instructions doing (A & Immediate), ANC/ALR/ARR/..](http://forums.nesdev.com/viewtopic.php?f=3&t=10417) for some interesting details.

Notes from `src/tests/vice.txt` on how to run a test with the VICE emulator:

    Basic steps to load a binary into the VICE emulator
    
    Open the Monitor and type
    
    f $0100 $01ff 0
    
    to zero out data on the stack, just for clarity
    
    Load the program at address $0200 with
    
    fill $0200 $0300 aa bb cc 11 22 433 ...
    
    Set the program counter to the starting address
    
    r pc=0200
    
    and then step with 'z', use 'r' to inspect registers
    
    Finally, type
    
    m $0100 $01ff
    
    to inspect the stack.

Notes from `src/tests/visual6502.txt` on how to run a test with Visual 6502:
    
    Basic steps to load a binary into the Visual 6502 simulator

    Convert a 6502 binary into an ASCII hex dump string
    
    mapM_ (\x -> Text.Printf.printf "%02x" x) . Data.ByteString.unpack =<< Data.ByteString.readFile "load_store_test.bin"
    
    Form URL: First part (r=0600) sets the PC, graphics=false hides the rendering of
    the simulation, a=0000&d=0... overwrites some pre-loaded code on the zero page.
    The a=0600&d=a9... pair is the hex dump of the binary generated above
    
    http://www.visual6502.org/JSSim/expert.html?r=0600&graphics=false&r=0600&a=0000&d=00000000000000000000000000000000000000000000000000000000000000000000&a=0600&d=a955a22aa0738581a9018561a97ea5818d1009a97ead10099556a97eb55684609160a97eb1609dff07a97ebdff0799ff07a97eb9ff078136a97ea1368650a660a4508e1309a222ae13098c1409a099ac1409942d9677a099b42da222b677a099bca008a222bea1089d0002
    
    The 'runv6502' script automates this process completely and opens the URL in the
    browser.
    
    Now trace with the 'Step' button and watch the zero page / stack writes and
    register changes. Be sure to click 'Log Up/Down' so that the trace doesn't
    immediately go off screen

A quick hack on the Visual 6502 source was required to speed up generation of reference traces. It disables all updates of the GUI/DOM and instead just dumps the instruction trace into the JS Debug Console:

    [16:44:25.988] "369 040d    ca  1   DEX 040d    41  2b  00  fd  nv-BdIzc"
    [16:44:26.011] "370 040e    10  1       040e    41  2b  00  fd  nv-BdIzc"
    [16:44:26.032] "371 040e    10  1   BPL     040e    41  2b  00  fd  nv-BdIzc"
    [16:44:26.054] "372 040f    f8  1       040f    41  2a  00  fd  nv-BdIzc"
    [16:44:26.076] "373 0410    a2  1       0410    41  2a  00  fd  nv-BdIzc"
    [16:44:26.097] "374 0408    bd  1   LDA Abs,X   0408    41  2a  00  fd
    nv-BdIzc"
    [16:44:26.120] "375 0409    5a  1       0409    41  2a  00  fd  nv-BdIzc"
    [16:44:26.178] "376 040a    36  1       040a    41  2a  00  fd  nv-BdIzc"
    [16:44:26.200] "377 3684    02  1       040b    41  2a  00  fd  nv-BdIzc"
    [16:44:26.221] "378 040b    95  1   STA zp,X    040b    02  2a  00  fd
    nv-BdIzc"
    [16:44:26.243] "379 040c    13  1       040c    02  2a  00  fd  nv-BdIzc"
    [16:44:26.264] "380 0013    00  1       040d    02  2a  00  fd  nv-BdIzc"
    [16:44:26.284] "381 003d    00  0       040d    02  2a  00  fd  nv-BdIzc"
    [16:44:26.294] "381 003d    02  0       040d    02  2a  00  fd  nv-BdIzc"
    [16:44:26.306] "382 040d    ca  1   DEX 040d    02  2a  00  fd  nv-BdIzc"
 
See [here](https://github.com/blitzcode/visual6502) for the fork.

The script at `src/tests/runv6502` can be used to quickly load the test binaries into Visual 6502.

# Disassembler

The build-in disassembler can be used through the `--dasm` flag like this:

    $ neskell --dasm tests/unit/nop_test.bin
    ...

All 256 instructions are supported and uniquely decoded. The folder `src/tests/decoding` contains a test for the disassembler. Here is how all possible instructions are represented in the disassembly:

    ADC #$01          SBC ($98),Y          INC $4C           LAX $00,Y         
    ADC $02           SEC                  INC $4D,X         LAX $0000         
    ADC $03,X         SED                  INC $4E4F         LAX $0000,Y       
    ADC $0405         SEI                  INC $5051,X       LAX ($00,X)       
    ADC $0607,X       STA $99              INX               LAX ($00),Y       
    ADC $0809,Y       STA $9A,X            INY               SAX $00           
    ADC ($0A,X)       STA $9B9C            JMP $5253         SAX $00,Y         
    ADC ($0B),Y       STA $9D9E,X          JMP ($5455)       SAX $0000         
    AND #$0C          STA $9FA0,Y          JSR $5657         SAX ($00,X)       
    AND $0D           STA ($A1,X)          LDA #$58          SBC($EB) #$00     
    AND $0E,X         STA ($A2),Y          LDA $59           DCP $00           
    AND $0F10         STX $A3              LDA $5A,X         DCP $00,X         
    AND $1112,X       STX $A4,Y            LDA $5B5C         DCP $0000         
    AND $1314,Y       STX $A5A6            LDA $5D5E,X       DCP $0000,X       
    AND ($15,X)       STY $A7              LDA $5F60,Y       DCP $0000,Y       
    AND ($16),Y       STY $A8,X            LDA ($61,X)       DCP ($00,X)       
    ASL A             STY $A9AA            LDA ($62),Y       DCP ($00),Y       
    ASL $17           TAX                  LDX #$63          ISC $00           
    ASL $18,X         TAY                  LDX $64           ISC $00,X         
    ASL $191A         TSX                  LDX $65,Y         ISC $0000         
    ASL $1B1C,X       TXA                  LDX $6667         ISC $0000,X       
    BCC $1D           TXS                  LDX $6869,Y       ISC $0000,Y       
    BCS $1E           TYA                  LDY #$6A          ISC ($00,X)       
    BEQ $1F           KIL($02)             LDY $6B           ISC ($00),Y       
    BIT $20           KIL($12)             LDY $6C,X         RLA $00           
    BIT $2122         KIL($22)             LDY $6D6E         RLA $00,X         
    BMI $23           KIL($32)             LDY $6F70,X       RLA $0000         
    BNE $24           KIL($42)             LSR A             RLA $0000,X       
    BPL $25           KIL($52)             LSR $71           RLA $0000,Y       
    BRK               KIL($62)             LSR $72,X         RLA ($00,X)       
    BVC $26           KIL($72)             LSR $7374         RLA ($00),Y       
    BVS $27           KIL($92)             LSR $7576,X       RRA $00           
    CLC               KIL($B2)             NOP               RRA $00,X         
    CLD               KIL($D2)             ORA #$77          RRA $0000         
    CLI               KIL($F2)             ORA $78           RRA $0000,X       
    CLV               NOP($7A)             ORA $79,X         RRA $0000,Y       
    CMP #$28          NOP($5A)             ORA $7A7B         RRA ($00,X)       
    CMP $29           NOP($1A)             ORA $7C7D,X       RRA ($00),Y       
    CMP $2A,X         NOP($3A)             ORA $7E7F,Y       SLO $00           
    CMP $2B2C         NOP($DA)             ORA ($80,X)       SLO $00,X         
    CMP $2D2E,X       NOP($FA)             ORA ($81),Y       SLO $0000         
    CMP $2F30,Y       NOP($80) #$00        PHA               SLO $0000,X       
    CMP ($31,X)       NOP($82) #$00        PHP               SLO $0000,Y       
    CMP ($32),Y       NOP($89) #$00        PLA               SLO ($00,X)       
    CPX #$33          NOP($C2) #$00        PLP               SLO ($00),Y       
    CPX $34           NOP($E2) #$00        ROL A             SRE $00           
    CPX $3536         NOP($04) $00         ROL $82           SRE $00,X         
    CPY #$37          NOP($64) $00         ROL $83,X         SRE $0000         
    CPY $38           NOP($44) $00         ROL $8485         SRE $0000,X       
    CPY $393A         NOP($0C) $000        ROL $8687,X       SRE $0000,Y       
    DEC $3B           NOP($14) $00,        ROR A             SRE ($00,X)       
    DEC $3C,X         NOP($34) $00,        ROR $88           SRE ($00),Y       
    DEC $3D3E         NOP($54) $00,        ROR $89,X         ANC($0B) #$00     
    DEC $3F40,X       NOP($74) $00,        ROR $8A8B         ANC($2B) #$00     
    DEX               NOP($D4) $00,        ROR $8C8D,X       ALR #$00          
    DEY               NOP($F4) $00,        RTI               ARR #$00          
    EOR #$41          NOP($1C) $000        RTS               XAA #$00          
    EOR $42           NOP($3C) $000        SBC #$8E          AHX ($00),Y       
    EOR $43,X         NOP($5C) $000        SBC $8F           AHX $0000,Y       
    EOR $4445         NOP($7C) $000        SBC $90,X         TAS $0000,Y       
    EOR $4647,X       NOP($DC) $000        SBC $9192         SHX $0000,Y       
    EOR $4849,Y       NOP($FC) $000        SBC $9394,X       SHY $0000,X       
    EOR ($4A,X)       LAX #$00             SBC $9596,Y       LAS $0000,Y       
    EOR ($4B),Y       LAX $00              SBC ($97,X)       AXS #$00          

# Architecture

### Code

The emulator itself runs inside the ST monad and provides a very small load/store interface for manipulation of CPU / RAM state from the emulation code. The design is strongly inspired by the [A bit of ST and a bit of IO: Designing a DCPU-16 emulator](http://jaspervdj.be/posts/2012-04-12-st-io-dcpu-16.html) blog post. The typeclass interface causes quite significant overhead, some of which can be mitigated through `{-# SPECIALIZE #-}` pragmas. This unfortunately is also not without its issues. See [Ticket #8331](http://ghc.haskell.org/trac/ghc/ticket/8331), for instance.

### API

The basic emulator interface looks as follows:

```haskell
runEmulator ::
    Processor                -> -- Model of the processor to be emulated
    [(B.ByteString, Word16)] -> -- List of program binaries and their offsets
    [(LoadStore, L8R16)]     -> -- Store operations to set up simulator state
    [Cond]                   -> -- The simulator will stop when any of these conditions are met
    [Cond]                   -> -- Success conditions to verify once stopped
    TraceMode                -> -- Level of tracing
    Int                      -> -- MB of trace log ring buffer space
    ( [Cond]                    -- Success conditions which were met
    , [Cond]                    -- ...not met
    , [Cond]                    -- Stopping conditions met
    , String                    -- Debug string of last CPU state
    , String                    -- Instruction the PC is pointing at
    , B.ByteString              -- Last traceMB MB of the execution trace
    )
```

And an example:

```haskell
runEmulator NMOS_6502
            [ (bin, 0x0600) ]
            [ (PC, Right 0xC000)
            , (SP, Left 0xFD)
            ]
            [ CondOpC BRK
            , CondLoopPC
            , CondCycleR 100000 (maxBound :: Word64)
            ]
            ( [ CondLS SP $ Left 0x81
              , CondLS SR $ Left $ srFromString "N-1--I--"
              , CondLS A  $ Left 0x55
              , CondLS X  $ Left 0x2A
              , CondLS Y  $ Left 0x73
              , CondLS (Addr 0x022A) (Left 0x55)
              , CondCycleR 25735 25735
              ]
              ++ ( makeStackCond 0xFF $
                       "      00 34 0F 4C B5 D5 4E 35 7D 7B B4 8D 04 35 " ++
                       "0D 50 B4 A0 76 B5 9B 00 B5 BF 32 B5 BB 3A 35 3B " ++
                       "EC B5 FE 22 B5 B3 42 B5 F2 BA B5 FF 80 B5 80 CC " ++
                       "75 66 EF 35 7B 78 35 6A 9D B4 D7 79 35 59 11 34 " ++
                       "35 01 34 01 33 35 11 3B 35 33 ED B5 E4 21 37 00 " ++
                       "42 35 40 43 34 01 00 B5 FE 9A B4 FE 35 B4 97 F1 " ++
                       "B4 FE 3B B4 FE F3 B4 FC 38 B4 FF FF 37 FF 98 35 " ++
                       "99 33 37 33 EF 35 F0 39 35 3A F1 B4 F0 36 35 37 "
                 )
            )
            trMode
            traceMB
```
The actual emulation code operates on the CPU / RAM state using a small typeclass interface defined in `MonadEmulator.hs`:

```haskell
data LoadStore = A | X | Y | SR | SP | PC | PCL | PCH | Addr Word16

class (Monad m, Applicative m) => MonadEmulator m where
    load8      :: LoadStore -> m Word8
    load16     :: LoadStore -> m Word16
    store8     :: LoadStore -> Word8  -> m ()
    store16    :: LoadStore -> Word16 -> m ()
    trace      :: String -> m ()
    traceM     :: m String -> m ()
    runNoTrace :: m a -> m a
    advCycles  :: Word64 -> m ()
    getCycles  :: m Word64
    getModel   :: m Processor
```

### Future Work

The final goal would be extending the CPU emulator to a full system, most likely the NES. Concerning the CPU emulator currently implemented, there is still plenty of room for improvement.

The `src/sandbox` folder contains various test cases and experiments. One of the biggest gripes with the current codebase is the redundancy and verbosity of the actual emulation code in the Emulator module. The idea is to first get a working emulator and test suite in place, and then find the optimal code structure to meet the requirements. The folder `src/sandbox/THTest` contains some early experiments to generate the instruction emulation code from small fragments using Template Haskell. This is heavily inspired by [Bisqwit's](http://bisqwit.iki.fi/) incredible [sub-1000 line NES emulator](http://bisqwit.iki.fi/jutut/kuvat/programming_examples/nesemu1/) and the clever encoding / CPP + template code for representing and generating the effects of each instruction.

One especially nasty bug still exists, and I suspect it to be an issue in GHC's garbage collector. See the comment from `Test.hs:252`:

    -- Workaround for a GC bug. Without this, the GC just isn't collecting. If we
    -- allocate some memory with an unboxed mutable vector inside the ST monad in
    -- the emulator (ring buffer trace log), the memory seems to be retained
    -- sometimes. There's no reference to the vector outside of ST. Even if we
    -- never do anything but create the vector and put it inside the Reader record,
    -- and then only return a single Int from ST, which is immediately evaluated,
    -- the memory is retained (sometimes...). All memory profiles always show we
    -- never allocate more than one vector at a time, yet multiple runs would cause
    -- OS memory for multiple vectors to be used and would eventually cause an
    -- out-of-memory error. Even then the RTS would not collect. Forcing collection
    -- after leaving ST and evaluating all return values seems to solve the problem
    -- entirely. This seems like a bug in the GC, running out of OS memory instead
    -- of garbage collecting allocations it (demonstrably) knows how to free, while
    -- all RTS memory profiles confirm it is indeed not referenced. In the parallel
    -- case this only alleviates the situation, not fixing it entirely like for the
    -- serial version before. Unfortunately, the bug is rather hard to reduce to a
    -- simple reproduction case

Any suggestions regarding this, workarounds, analysis, ideas on how to best report this bug etc., would be much appreciated.

Performance could be described as 'barely acceptable'. Running all 92608051 cycles of the 'Functional 6502 Test' takes ~33.5sec on an older 2.2Ghz Core 2 Duo processor, giving us ~2.75Mhz for just the CPU.

Here's a profile run generated while running the entire test suite:

    COST CENTRE                MODULE        %time %alloc
    
    decodeInstructionM         Instruction    15.3    1.6
    runEmulator.loop           Emulator       11.8   59.5
    execute                    Execution      11.8   13.7
    decodeOpCode               Instruction     5.0    0.0
    load8                      MonadEmulator   3.2    1.8
    detectLoopOnPC             Execution       2.8    1.0
    bne                        Execution       2.6    0.7
    setNZ.isN                  Execution       2.5    1.4
    checkCond                  Condition       2.1    2.5
    store16                    MonadEmulator   2.0    0.0
    update16                   Execution       1.8    0.8
    lda                        Execution       1.8    0.4
    load16                     MonadEmulator   1.7    0.3
    trace                      MonadEmulator   1.7    0.0
    store16.(...)              MonadEmulator   1.6    2.2
    advCycles                  MonadEmulator   1.6    0.5
    store8Trace                Execution       1.6    0.9
    loadOperand8.loadAndTrace  Execution       1.5    0.9
    store16Trace               Execution       1.4    0.1
    cmp                        Execution       1.2    0.3
    execute.ilen               Execution       1.2    0.3
    updateNZ                   Execution       1.1    0.4
    getOperandPageCrossPenalty Execution       1.1    0.9
    makeRingBuffer             RingBufferLog   0.0    2.3

The amount of inlining (into `runEmulator.loop`, for instance) used makes the results unfortunately somewhat harder to interpret.

One idea to speed up the test suite would be to not only run individual tests in parallel, but to use snapshots to further parallelize segments of long-running tests. Like the parallel testing this wouldn't help actual emulator speed, but being able to run more tests in shorter time is helpful during development.

Exploring type system features to represent more invariants of the 6502 directly on the type level would be useful. We could also split `MonadEmulator` into separate read / write interfaces for more access control.

It would be interesting to explore if Haskell's natural laziness could be used effectively to defer emulation work until it is actually needed. Often the result of computations like status register updates is never used.

The Makefile based build system should be replaced with Cabal.

There's very little error handling throughout the program, this should be improved as the emulator matures.

# Legal

This program is published under the [MIT License](http://en.wikipedia.org/wiki/MIT_License).

Some of the test suite entries have been developed by other authors and a different license may apply.

# Author

Developed by Tim C. Schroeder, visit my [website](http://www.blitzcode.net) to learn more.

