MOV R0, #0x80000000
ADDS R0, R0, R0
MOVS R0, #0
MOV R0, #0x9
ADD R0, R0, #0x7300
ADD R0, R0, #0x520000
ADD R0, R0, #0x99000000
MOV R1, #0xf7
ADD R1, R1, #0x1000
ADD R1, R1, #0x240000
ADD R1, R1, #0x16000000
MOV R2, #0xcc
ADD R2, R2, #0x8900
ADD R2, R2, #0x5d0000
ADD R2, R2, #0xd7000000
MOV R3, #0x4f
ADD R3, R3, #0x2400
ADD R3, R3, #0x2a0000
ADD R3, R3, #0x47000000
MOV R4, #0xb9
ADD R4, R4, #0x6700
ADD R4, R4, #0x3c0000
ADD R4, R4, #0x83000000
MOV R5, #0xec
ADD R5, R5, #0x2600
ADD R5, R5, #0xab0000
ADD R5, R5, #0x17000000
MOV R6, #0x68
ADD R6, R6, #0x9c00
ADD R6, R6, #0x4d0000
ADD R6, R6, #0xe9000000
MOV R7, #0x68
ADD R7, R7, #0xc400
ADD R7, R7, #0x3f0000
ADD R7, R7, #0xc9000000
MOV R8, #0xb3
ADD R8, R8, #0xf00
ADD R8, R8, #0xdf0000
ADD R8, R8, #0xb5000000
MOV R9, #0xed
ADD R9, R9, #0x8e00
ADD R9, R9, #0xe40000
ADD R9, R9, #0x6b000000
MOV R10, #0x2
ADD R10, R10, #0xb700
ADD R10, R10, #0x730000
ADD R10, R10, #0x7b000000
MOV R11, #0xe7
ADD R11, R11, #0x1700
ADD R11, R11, #0x430000
ADD R11, R11, #0xb8000000
MOV R12, #0x7
ADD R12, R12, #0xe800
ADD R12, R12, #0x100000
ADD R12, R12, #0x50000000
MOV R13, #0x7d
ADD R13, R13, #0x8b00
ADD R13, R13, #0xcf0000
ADD R13, R13, #0xe000000
MOV R14, #0xfc
ADD R14, R14, #0x1000
ADD R14, R14, #0x850000
ADD R14, R14, #0xd4000000


SUB R12,R8,R14
MOV R13, #0x1000
LDMIA R13, {R1-R12}
MOV R0, #0
              ADDMI R0, R0, #8
              ADDEQ R0, R0, #4
              ADDCS R0, R0, #2
              ADDVS R0, R0, #1
