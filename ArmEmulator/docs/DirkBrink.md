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

## Table of Features
| Instruction Class | Feature | Tests |  
| :---:|:---:|:---:|
| ARITH | Basic operations | :heavy_check_mark:
| ARITH | Operation with/without `S` suffix | :heavy_check_mark:
| ARITH | BIDMAS expression evaluation | :heavy_check_mark:
| ARITH | 32 bit number restriction of 8 bit even rotate | hold
| ARITH | Binary, Hex and Decimal numbers | :heavy_check_mark:

## Testing
#### Parsing
The basic parsing tests are unit tests that test the functionality of `parseArithLine` and `parseCompLine`. The unit tests consist of operand strings that are passed into the parser to return a Result type of registers and an `OpCode` DU.  The manual unit tests are used to test basic functionality, but mainly error handling.  Any operand strings that do not conform to the correct format should throw a parsing error.  

There are also automated parsing tests for the `parse` function.  `testArithParse` and `testCompParse` are the two test functions that perform this automated testing.  Valid data is automatically generated and made into a string and ultimately into `LineData`.  These test functions run 10,000 automated tests each.

#### Execution
The execution tests 
