# Dirk Brink
This documentation concerns the `Arithmetic.fs` source file, along with its tests in `ArithmeticTests.fs`.

## Contributions to Group
`Arithmetic.fs` implements the following ARM instructions:
- `ADD`
- `ADC`
- `SUB`
- `SBC`
- `RSB`
- `RSC`
- `CMP`
- `CMN`

`CMP` and `CMN` have a slightly different structure to the other instructions and so I decided after a lot of debate and trial and error that it was easier to create different parsing functions for the 2 sets of instructions.  

I created 2 new `InstrClass` values - `ARITH` and `COMP` - to represent the 2 sets. Initially the parsing of both the instruction types was implemented completely with regular expression active pattern matching.  However the matching strings proved to be extemely long and then became very hard to read.  I therefore decided to refactor my code to combine `String.Split` with the active pattern matching. I believe the end result is cleaner and more readable than the former.  

My code should integrate well with the group as I tried to use the same general code structure as some of the other members of my group.  This will make it easier for the others to pick up my code and make edits to it if needed.  The return types should also integrate fairly easily with top level group code.

My parsing code assumes capital letters for all inputs (excluding labels).  The assumption is that each line will be capitalised in top level group code.

## Table of Features
| Feature | Tests |  
| :---:|:---:|
| Basic ADD/ADC/SUB/SBC | ✔️
| Basic CMP/CMN | ✔️
| Expressions in op2 | ✔️
| BIDMAS expression execution (improvement from visUAL) | ✔️
| Basic RSB/RSC | Partly tested

## Testing
#### Parsing
The basic parsing tests are unit tests that test the functionality of `parseArithLine` and `parseCompLine`. The unit tests consist of operand strings that are passed into the parser to return a Result type of registers and an `OpCode` DU.  The manual unit tests are used to test basic functionality, but mainly error handling.  Any operand strings that do not conform to the correct format should throw a parsing error.  

There are also automated parsing tests for the `parse` function.  `testArithParse` and `testCompParse` are the two test functions that perform this automated testing.  Valid data is automatically generated and made into a string and ultimately into `LineData`.  These test functions run 10,000 automated tests each.

#### Execution
I performed execution tests using the provided visualTest framework.  Testing using this framework proved fairly tricky.

During testing I encountered some issues with PC.  Since PC is different for both visUAL and my f# code, arithmetic gave different values and hence were uncomparable.  I found it easiest to disable PC for testing (however this may have unintended consequences of missing bugs).

Testing RSB and RSC also proved fairly difficult. This is due to the fact that there is a restriction on op2 that it has to be >= 0.  There is no way to check this before runtime sadly.  To deal with this, I decided to implement the same restriction in my code and throw a `failwithf` error.  The downside of this is that I could not fully test RSB and RSC with my current testing model.  I ran automated tests numerous times to see if my code failed.  I was unable to catch an error before a runtime error.  Therefore I'm fairly confident the RSB and RSC instructions work for most cases, but given more time I'd make a larger effort to test edge cases using another method.    


## Differences from visUAL
For the arithmetic instructions in visUAL there are numerous restrictions on registers and literal expressions.  These almost exclusively involve the use of SP and PC.  I encounterd most of these restrictions when performing automated testing with visUAL.  Conforming to all of these restrictions without severely limiting test scope proved quite tricky and it is quite possible that my automated tests are over-restrictive.  

After some debate, I decided that I would make my implementation less restrictive than visUAL.  For all the arithmetic instructions I removed all of the restrictions except for those concerning PC and SP as the target register.  These registers are important to program execution and so should not be freely editable.  I decided that their use in op1 and op2 should be unrestricted for a easier user experience.  

| visUAL Restriction | My implemetation |
| :---:|:---:|
| ADC, RSB and RSC second operand cannot be SP or PC | Unrestricted
| ADC, RSB and RSC first operand cannot be label, SP or PC | Unrestricted
| Op1 cannot be PC when op2 is register controlled shift | Unrestricted
| Target register can only be R13 is op2 is a literal | Unrestricted
| Op1 can only be PC if op2 is a Literal | Unrestricted
| Op1 cannot be PC when op2 is register controlled shift | Unrestricted
| SUB instruction first operand cannot be R15 | Unrestricted
| SUB instruction second operand cannot be R15 | Unrestricted
| ADD second operand cannot be R13 or R15 | Unrestricted
| SUB/ADD dest can only be R13 if op1 is R13 and op2 is constant or LSL #1/#2/#3 | Changed to any second operand but op1 must still be SP
| SUB second operand cannot be PC or SP | Unrestricted
| SBC first operand cannot be SP or PC | Unrestricted
| RegisterRegisterShift cannot be PC for any instructions | Unrestricted

Upon delving further into testing with a larger spread of numbers it became apparent that my `flexOp2` function had some differences to visUAL.

| Shift op | My implemetation |
| :---:|:---:|
| LSL | Same as visUAL 
| LSR | Same as visUAL
| ASR | Same as visUAL
| RRX | Same as visUAL
| ROR | No restriction (different from visUAL). Works with all literals

My final testing concerned the CMP and CMN instructions.  As with the other arithmetic instructions, there are lots of restrictions on PC and SP.  Since there is not target register I decided to remove all restrictions. 


### What am I testing?

The tests take around 30-40 mins to run to completion.  My tests consist of 57 parsing based tests and 2 fully randomised tests using the VisualTest framework.  The parsing based tests are made up of unit tests and randomised parsing tests.  The unit tests are focused on testing the parsing functions in ways that aim to break the parser and cause errors (which should of course be caught).  This includes cases where the input is completely invalid (i.e. does not make sense in the ARM spec).  I made this the goal of the unit tests because it is easier to test these error causing conditions with manual, carefully crafted input strings.  The parsing tests also include randomly generated data to verify the function of the parsing functions (`testCompParse` and `testArithParse`).  

The final part of my testing was fully randomised testing using the VisualTest framework to check against visUAL and ensure flags and registers were set correctly on all inputs.  This was the most difficult part of testing mainly due to the numerous restrictions that visUAL places on input values.  As I result I had to restrict the possible range of input data to ensure all tests were passed.

Randomised testing is conducted on all of the following opcodes:

| Restriction | Reason |
| :---:|:---:|
| ADC, RSB, RSC, SBC first operand cannot be SP or PC | visUAL has this restriction of PC and SP (removed in my implementation)
| No PC as target register | Although visUAL only places this restriction on certain opcodes, it proved to cause problems since the PC is integral to the execution of a program.  The easiest solution was to place this restriction on tests, but of course this could be improved to widen the coverage of tests.
| RSB and RSC removed from main test run | During testing I would perform automated randomised tests on these 2 opcodes, however I often ran into the issue of the second operand being negative (which is not allowed in visUAL).  There is no way to evaluate the value of a register before runtime and as such it was not possible for me to make this restriction in tests.  The easiest way was to disable testing for the main test run.  Not very thorough solution and would definitely have been improved given more time.
| RegisterShift and RegisterRegisterShift removed from testing | I made some changes in my implementation that differ from visUAL
| Flags N and Z cannot be set to 1 simultaneously | This case is impossible in ARM spec and visUAL.
| SUB and ADD dest can only be R13 if op1 is also R13 | This restriction is enforced by visUAL and is also enforced by my implementation.
| CMP and CMN second operand cannot be R15 and R13 | This is a restriction enforced by visUAL but the restriction is removed in my implementation.