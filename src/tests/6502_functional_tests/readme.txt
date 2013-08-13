
6502 functional tests version 23-jul-2013
Taken from http://2m5.de/6502_Emu/6502_functional_tests.zip
-----------------------------------------------------------

6502_functional_test.bin is 6502_functional_test.a65 assembled with the
AS65 assembler:

as65 -l -m -w -h0 -z 6502_functional_test.a65

This gives a single binary image. I used the Linux version of AS65 from:

(http://www.kingswood-consulting.co.uk/assemblers/).

The source needs to have to have the following flag set to produce
tighter object code (<16K):

load_data_direct = 0

