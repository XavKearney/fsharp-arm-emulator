MOV R0, #0x80000000
ADDS R0, R0, R0
MOVS R0, #0
MOV R0, #0x45
ADD R0, R0, #0x2500
ADD R0, R0, #0xe90000
ADD R0, R0, #0x99000000
MOV R1, #0x7d
ADD R1, R1, #0x2900
ADD R1, R1, #0x750000
ADD R1, R1, #0x88000000
MOV R2, #0x32
ADD R2, R2, #0xaf00
ADD R2, R2, #0x5c0000
ADD R2, R2, #0x15000000
MOV R3, #0xc0
ADD R3, R3, #0x5d00
ADD R3, R3, #0x140000
ADD R3, R3, #0xf1000000
MOV R4, #0xad
ADD R4, R4, #0x5100
ADD R4, R4, #0xc60000
ADD R4, R4, #0xe8000000
MOV R5, #0x7c
ADD R5, R5, #0xa300
ADD R5, R5, #0x5e0000
ADD R5, R5, #0xb1000000
MOV R6, #0x94
ADD R6, R6, #0x8b00
ADD R6, R6, #0x100000
ADD R6, R6, #0x7b000000
MOV R7, #0xf4
ADD R7, R7, #0x8900
ADD R7, R7, #0x1d0000
ADD R7, R7, #0xf2000000
MOV R8, #0xc5
ADD R8, R8, #0x3c00
ADD R8, R8, #0x50000
ADD R8, R8, #0x8f000000
MOV R9, #0x7d
ADD R9, R9, #0x8500
ADD R9, R9, #0x330000
ADD R9, R9, #0x74000000
MOV R10, #0xa4
ADD R10, R10, #0x3600
ADD R10, R10, #0xeb0000
ADD R10, R10, #0xe000000
MOV R11, #0xee
ADD R11, R11, #0x5200
ADD R11, R11, #0x690000
ADD R11, R11, #0xda000000
MOV R12, #0x8f
ADD R12, R12, #0x1300
ADD R12, R12, #0x490000
ADD R12, R12, #0x4f000000
MOV R13, #0xdf
ADD R13, R13, #0x4900
ADD R13, R13, #0x1d0000
ADD R13, R13, #0xbc000000
MOV R14, #0x81
ADD R14, R14, #0xac00
ADD R14, R14, #0x770000
ADD R14, R14, #0x9a000000


RSB R6,R6,R14,ROR #0
MOV R13, #0x1000
LDMIA R13, {R1-R12}
MOV R0, #0
              ADDMI R0, R0, #8
              ADDEQ R0, R0, #4
              ADDCS R0, R0, #2
              ADDVS R0, R0, #1
