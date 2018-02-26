MOV R0, #0
ADDS R0, R0, R0
MOVS R0, #-1
MOV R0, #0x1
MOV R1, #0x0
MOV R2, #0x0
MOV R3, #0x4
MOV R4, #0x2
MOV R5, #0x0
MOV R6, #0x2
MOV R7, #0x5
MOV R8, #0x3
MOV R9, #0x0
MOV R10, #0x9
MOV R11, #0x4
MOV R12, #0x0
MOV R13, #0x0
MOV R14, #0x0


RSB R0,R5,R5,RRX
MOV R13, #0x1000
LDMIA R13, {R1-R12}
MOV R0, #0
              ADDMI R0, R0, #8
              ADDEQ R0, R0, #4
              ADDCS R0, R0, #2
              ADDVS R0, R0, #1
