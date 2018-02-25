MOV R0, #0
ADDS R0, R0, R0
MOVS R0, #1
MOV R0, #0x1
MOV R1, #0x2
MOV R2, #0x7
MOV R3, #0x3
MOV R4, #0x3
MOV R5, #0x5
MOV R6, #0x1
MOV R7, #0x9
MOV R8, #0x0
MOV R9, #0x5
MOV R10, #0x6
MOV R11, #0x2
MOV R12, #0x0
MOV R13, #0x0
MOV R14, #0x0


ADD R9,R15,R7
MOV R13, #0x1000
LDMIA R13, {R1-R12}
MOV R0, #0
              ADDMI R0, R0, #8
              ADDEQ R0, R0, #4
              ADDCS R0, R0, #2
              ADDVS R0, R0, #1
