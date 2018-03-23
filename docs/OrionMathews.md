# Orion Mathews

This documentation covers how the BitArithmetic.fs module and the corresponding test module BitArithmeticTests.fs work.

## Contributions to Group
BitArithmetic.fs parses and executes the following 13 instructions:

* MOV
* MVN
* AND
* ORR
* EOR
* BIC 
* LSL
* LSR
* ASR
* ROR
* RRX 
* TST
* TEQ 

The parsing and execution of these instructions has been thoroughly tested in BitArithmeticTests.fs. 
To help the group (and others) understand how the instructions are implemented, significant time has been spent choosing the module types and refactoring the code. This has made the module more readable and reasonably concise (although there is still room for improvement). Result monads have been used with useful error messages to help with implementing and using the GUI.

My chosen module structure will integrate well other memebers modules as it abides by the group set standards. It is also clear how the data flows though the parse function and gets executed. To increase compatibility my code is structured around the interfaces defined in CommonData and CommonLex which all group members would use.

## Features
| Instruction     | Basic implementation | ARM Specification implementation | Flexible operator implementation |
|-----------------|----------------------|----------------------------------|----------------------------------|
| MOV/MVN         | ✔️                  | ✔️                               | ✔️                               |
| AND/ORR/EOR/BIC | ✔️                  | ✔️                               | ✔️                               |
| LSL/LSR/ASR/ROR | ✔️                  | ✔️                               | ✔️     (No shifts)               |
| RRX             | ✔️                  | ✔️                               | NA                               |
| TST/TEQ         | ✔️                  | ✔️                               | ✔️                               |


# Code specification

The specifications for my code are largely dictated by the ARM specifications (See http://infocenter.arm.com/help/topic/com.arm.doc.dui0552a/CIHDFHCC.html).

## Updating the condition flags

The negative (N) and zero (Z) flag are updated based on the evaluated result. If the result is negative the N flag is set, if it is zero then the Z flag is set. 

None of the instructions processed in this module manupulate the overflow (V) flag.

For instructions MOVS, MVNS, ANDS, ORRS, ORNS, EORS, BICS, TEQ or TST, the carry flag is updated to bit 31 of the evaluated flexible operand, if the evaluated flexible operand is greater than 255, otherwise these instructions do not affect the carry flag.

LSLS, LSRS, ASRS, RROS and RRXS update the carry flag depending on the last bit shifted out of the 32 bits. 

## Instruction Operation

### MOV/MVN

General syntax: 

INSTRUCTION{S}{cond} dest, op1 {, SHIFT_op #expression}

Examples
* `MOV R0, R2, LSL #4` : would shift the value in R2 left by 4 places and place the result in R0
* `MVN R0, R2, LSL #4` : would shift the value in R2 left by 4 places then invert all the bits and place the result in R0

### AND/ORR/EOR/BIC

General syntax: 

INSTRUCTION{S}{cond} dest, op1, op2 {, SHIFT_op #expression}

Example
* `AND R0, R2, R7, LSL #4` : would shift the value in R7 left by 4 places, then AND the result with the value contained in R2 and place the result in R0.
* `EOR R1, R3, R8, LSL #4` : would shift the value in R8 left by 4 places, then exclusive ORR (EOR) the result with the value contained in R3 and place the result in R1.

### LSL/LSR/ASR/ROR

General syntax: 

INSTRUCTION{S}{cond} dest, op1, op2


Examples
* `LSL R0, R2, #4` : would shift the value in R2 left by 4 places and place the result in R0
* `ROR R6, R5, R8` : would shift the value in R5 left by the value in R8 and place the result in R6

### RRX

General syntax: 

RRX{S}{cond} op1, op2


Examples
* `RRX R6, R2` : would shift the value in R2 right by 1 place, shifting the carry flag into the MSB and placing the result in R0

### TST/TEQ 

General syntax: 

INSTRUCTION{cond} op1, op2 {, SHIFT_op #expression}


Examples
* `TST R6, R2` : would AND the value in R6 and the value in R2 setting the status bits on the result
* `TEQ R6, R5` : would EOR the value in R6 and the value in R5 setting the status bits on the result


## Differences from visUAL

### Shifting

The value that is used to shift the other value is modulo 32. This is because that is how f# preforms the bit shift left opperation (<<<).
visUAL does not do this.

### Setting carry

Updating the condition flags is done as previously described. This conforms with the ARM specification. However in visUAL operations such as  "ANDS R0, R2, #256" set the carry flag in visUAL but would not set the carry flag for my code.



# Testing 

In order of tests in BitArithmeticTests.fs:

* testLit :

    Unit tests for the toLit function. Tests the basic allowed literals (i.e. #1, #0 and #-1) as well as basic expressions and some corner cases.

* testLitRan:

    Random tests for toLit function. Tests toLit function against a modified version of checkLiteral (Given in tick 3 feedback). Sucesfully completes 10,000 random tests of integers between -2147483648 and 2147483647.

* testParse: 

    21 unit tests of the parse function. Covers basic parsing of all instructions as well as testing return errors are correct.

* RORtest:

    Tests property that a ROR by integer multiple of 32 is equal to its selft. The integer multiple of 32 is selected randomly, it can be any integer multiple of 32 between 0 and 320. 

* testMOVsRandomised, testBitArithRandomised, testShiftsRandomised, testTSTandTEQRandomised: 

    Randomised testing of all instruction parsing except RRX. Random instruction lines are generated by converting randomly generated operands to strings and concatinating them together. This random instruction (string) is passed to the parser and the output is compared against the origional operands. Each test runs 10,000 times.

* testsExeMOV, testsExeMOVS, testsExeMVNS, testsExeANDS, testsExeEORS, testsExeORRS, testsExeBIC, testsExeLSLS, testsExeLSRS, testsExeASRS, testsExeRORS: 
    
    Tests execution of instructions against visUAL for all instructions except RRX, TST and TEQ. Ensures the flags and registers are updated just as visUAL would have updated them given the same instruction.

### Testing summary table:
| Instruction     | Unit parse testing | Random parse testing (property based) | Unit execution testing (visUAL Testing) |
|-----------------|--------------------|---------------------------------------|-----------------------------------------|
| MOV/MVN         | ✔️                 | ✔️                                   | ✔️                                     |
| AND/ORR/EOR/BIC | ✔️                 | ✔️                                   | ✔️                                     |
| LSL/LSR/ASR/ROR | ✔️                 | ✔️                                   | ✔️                                      |
| RRX             | ✔️                 | ✖️                                   | ✖️                                      |
| TST/TEQ         | ✔️                 | ✔️                                   | ✖️                                      |
