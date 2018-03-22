# ARM Assembler Instructions - APSW

## How my code will contribute to the group deliverable
My code will contribute to the group deliverable in that it supplies six crucial instructions (EQU, DCD, Fill, LDR, STR and ADR). Without these instructions you can do nothing with memory other than using the stack.
To make sure my code is compatible I have used result monads everywhere to avoid using any exceptions whatsover and have tried to write useful error messages. For instance I start my error messages with the function where they are occuring and include useful vairable to show what is wrong.
I also have tried to use intuitve function names and give useful comments so that my code is easy to understand. <br />
If we were to take this project forward further I would like to change my error messages from being very useful for debugging to being more useful when displayed in the GUI. Some of the error messages are alreaady useful, however I suspect there are some that would want changing.

## What is the specification of my code
Details of the visual specification for my code and how it differs from what I have implemented can be found for each function in the "General Description of how my Code works" section

## A description of my testing
My testing is unfinished. I prioritised getting the execution for each instruction working and testing the parse tested more thoroughly. I did this because the main source of errors and supplying error messages is in the parsing as this is where all the unexpected inputs can be. I have unit tests and 1000 randomised testing working for each of my parse functions.<br />
I was intending to add testing against visual to test the execution functions but in the end I didn't have time. As far as I can tell all of my instructions work as they should, but there could be a corner case or two in which they fail due to the lack of visual testing.<br />
I have 161 unit tests and evidence of the passing can be found in a screenshot in the images folder for the READ_ME.
<br />
<br />
<br />
<br />
<br />
<br />
<br />
<br />

# General Description of how the Code works
This section covers six instructions seperated into three groups:

  - Memory Instructions - LDR and STR
  - Label Instructions - DCD, EQU, Fil
  - ADR - ADR

I seperated the instructions in this way because each group contained instrucitons which recieved their input in the same form and so could be easily parsed together.

<br />

# General Parsing
Although I made three seperate functions for parsing each group of instructions, I use a general parsing function, `parse`, to call them. Each individual parsing function returns a record containing all the information necessary to execute any of the functions in the group. 

For example, my `parseAdrIns` function parses ADR instuctions and returns a record of type `ADRInstr`
```fsharp
type ADRInstr =
        {
            InstructionType: Result<ADRInstrType,String>;
            DestReg: Result<RName,String>;
            SecondOp: LabelAdrExpr;
        }
```
This record encapsulates all the information needed to execute an ADR instruction. These three different types of record are then unified using a discriminated union (DU) as seen below.
```fsharp
    type LabelAndMemGeneralParse = 
                          | LabelO Result<labelInstrstring> 
                          | MemO of Result<MemInstr,string> 
                          | AdrO of Result<ADRInstr,string>
```
This is the type of one of the fields of the record returned by the (general) `parse` function.

<br />

# LDR & STR
### Documentation of visUAL implementation
There are six ways in which you can implement LDR and STR nicely summarised in this table taken from [Dr Clarke's Second Year Architecture Course](https://intranet.ee.ic.ac.uk/t.clarke/arch/html16/CT6.html "EE2 CompArch")
| Instruction | Meaning | Label |
| ----------- | ------- | ----- |
| LDR Ra, [Rb] | Ra := Mem~32~[Rb] | Base Case |
| LDR Ra, [Rb, #N] | Ra := Mem~32~[Rb+N] | Num Increment |
| LDR Ra, [Rb], #N | Ra := Mem~32~[Rb]; Rb := Rb + N | Post Indexing |
| LDR Ra, [Rb, #N]! | Ra := Mem~32~[Rb+N]; Rb := Rb + N | Pre Indexing |
| LDR Ra, [Rb,Rc] | Ra := Mem~32~[Rb+Rc] | Adding Registers |
| LDR Ra, [Rb,Rc,LSL #N] | Ra := Mem~32~[Rb+Rc*2^N^] | Shifted Register |

  - Note: You can add the `!` to the end of any line. Ie: if you add an `!` to the end a Shifted Register type line then you will increment `Rb` by `Rc*2^N^`.
  - Note: You can swap LDR for STR in any of these instructions

### visUAL Quirks 
1. In the shifted register case, if you give it a negative N, then instead of logically shifting Rc to the right by 1 (x -> x/2) it ignores the Rc and the shift and implements it the same as if it was the base case LDR Ra,[Rb].
2. In the ARM instruction guide which can be found [here](http://infocenter.arm.com/help/index.jsp?topic=/com.arm.doc.dui0552a/BABCAEDD.html "ARM instruction guide") it states that N in the shifted case is limited to 0-3. However in visUAL this number is only limited to  being positive.

### How I've Tested it 
So far I have tested my LDR and STR parsing using a series of 48 unit tests going through most of the combinations of the six ways in which LDR and STR can be implemented. <br />
They have been tested for valid inputs and error messages using unit tests and have also been tested for valid inputs using 10,000 randomised tests.<br />
I have also tested the execution for valid inputs using unit tests. It is only necessary to test the execution for valid inputs as the parser will filter out any non valid inputs and return an error, and if the parser returns an error the execution functions are not run.


<br />

# DCD, EQU & Fill

## *DCD*
### Documentation of visUAL implementation
*Eg*: `label DCD 1, 3, 8, 2`

The DCD instruction defines a block of consecutive memory. In visual you must define word aligned memory, however in the ARM specifications you can define unaligned memory. 
### visUAL Quirks 
As of yet no quirks found for DCD

### Parsing
The DCD instruction is parsed alongside EQU and Fill using the `parseLabelIns` function. This function returns a  record of type:
```fsharp
type labelInstr =
        {
            //The Instruction type: DCD, EQU, Fill or Error 
            InstructionType: Result<LabelInstrType,String>;
            
            //The label of the line
            Name: LabelL; 

            //The evaluated expression to assign to the symbol     
            EQUExpr: Result<uint32,String> option;

            //What to fill memory with
            DCDValueList: ValueList option;

            //How many empty words to fill (evaluated expression)    
            FillN: Result<uint32,String> option;
        }
```
The `InstructionType` and `Name` fields are not options as they will always occur, whereas only one of `EQUExpr`, `DCDValueList` and `FillN` can occur in any one record and so they are options. 

All five fields are monads to deal deal with any errors which could occur in the parsing and the whole record is also returned from the function in monad form. If any of the individual fields are `Error "message"` instead of `Ok` then the record is returned as `Error "Concatenation of all the seperate error messages"`.  

### Execution
The execution for DCD has not yet been implemented
### How I've tested  it  
The parsing and the execution have both been tested for DCD. The parsing has been tested using 13 unit tests which test both valid inputs and invalid inputs resulting in error messages using unit tests. Both valid and invalid inputs have also been tested with 10,000 randomised tests.<br/>
The execution has only been tested for valid inputs and using unit tests. This is because due to the way the topLevel code is structured the parse funciton will filter out any invalid inputs and then if the return from the parse function is an error the execution functions will not be ran.

<br />

## *Fill*
### Documentation of visUAL implementation
*Eg*: label Fill N

*Eg*: `ghi Fill 4`

The Fill instruction declares a series of empty consecutive words in memory. Because it declares words and not bytes N must be a positive multiple of 4.

### visUAL Quirks 
1. Interestingly if you input an N value of 0 then it still generates a label, although because this label corresponds to an initialised memory block of length zero, this gives a multiple labels pointing to the same address. As can be seen below:
![Symbol Tool on Visual](READ_ME_images/fill0.PNG?raw=true "Title")
2. In the 
### Execution
### How I've tested it  
The parsing and the execution have both been tested for FILL. The parsing has been tested using 8 unit tests which test both valid inputs and invalid inputs resulting in error messages using unit tests. Both valid and invalid inputs have also been tested with 10,000 randomised tests.<br/>
The execution has only been tested for valid inputs and using unit tests. This is because due to the way the topLevel code is structured the parse funciton will filter out any invalid inputs and then if the return from the parse function is an error the execution functions will not be ran.

<br />

## *EQU*
### Documentation of visUAL implementation

### visUAL Quirks 

1. It is interesting that you can use the EQU instruction to assign the same symbol multiple values.
2. Very interstingly you can evaluate the expression to form any number that can be represented by a uint32. For example -257 can be assigned to a label using EQU, however it cannot be loaded into a register using ADR.
    - This means that you will not be able access all addresses in memory as the expression in ADR cannot evaluate to all the possible uint32 addresses.

### Parsing

### Execution


### How I've Tested it 
The parsing and the execution have both been tested for EQU. The parsing has been tested using 7 unit tests which test both valid inputs and invalid inputs resulting in error messages using unit tests. These unit tests test a series of edge cases such as negative inputs and complicated expressions. This is also tested further in checkEvalExpression<br />
Both valid and invalid inputs have also been tested with 10,000 randomised tests.<br/>
The execution has only been tested for valid inputs and using unit tests. This is because due to the way the topLevel code is structured the parse funciton will filter out any invalid inputs and then if the return from the parse function is an error the execution functions will not be ran.



<br />

# ADR
### Documentation of visUAL implementation
#### Examples
*Note*: Let `testL` be a label for the address 256 (0x100) and `testL2` be the label for 260 (0x104)
| Instruction | Meaning | Label |
| ------ | ------ | ------ |
| ADR R0 label | R0 := 256 | Base Case |
| ADR R0 testL + 4 | R0 := (256 + 4) = 260 | Addition |
| ADR R0 4\*2 + testL | R0 := (4\*2 + 256) = 264 | Left Multiplication |
| ADR R0 testL + 4\*2 | R0 := (256 + 4)\*2 = 520 | Right Multiplication |
| ADR R0 4\*2 + testL + 2\*2 | R0 := (4\*2 + 256 + 4)\*2 = 528 | Left & Right Multiplication |
| ADR R0 testL + testL2 | R0 := (256 + 260) = 516 | Adding Labels |

### visUAL Quirks 
Note from the examples above that the visUAL does not parse the expression for the label using the normal BIDMAS rules, instead it seems to use a lect accumulate multiplication method.
1. If you have an expression instead of a label for the second operand of ADR then it does not add normally.
  - Eg: if testL = 256
  - testL + 4\*2 -> (256 + 4)\*2 -> 520
  - 4\*2 + testL + 2\*2 -> (8 + 256 + 2)\*2 -> 532
  - This seems to be a left accumulate addition method.

2. 
Instead of this I have decided to evalute the expressions using BIDMAS as it seems a more logical way to do it.

### Parsing
#### evalExpression Function explanation
This is the function that evaluates the expressions used in ADR and the EQU. 
The function calls a recursive sub function which step by step seperates the string input, first by brackets, then +, - and * operators respectively. It splits the string around these operators and then evaluates the expressions either side. 
At some point it will reach and end case which could be one of the following: 
  - A decimal number, Eg 5
  - A hex number in form 1, Eg 0x5
  - A hex number in form 2, Eg &5
  - A binary number, Eg 0b101
  - A label which will evaluate to either an address or a value depending on whether it was initialised using DCD or EQU. Eg testLabel or testLabel2 

CREDIT: I have used and adapted Tom Clarke's makeAndCheckLiteral function (https://intranet.ee.ic.ac.uk/t.clarke/hlp/T3fback53.html) in order to check that the expression can be made by rotating an 8-bit literal right by an even number of bits.

### Execution


### How I've Tested it 
The parsing and the execution have both been tested for ADR. The parsing has been tested using 11 unit tests which test both valid inputs and invalid inputs resulting in error messages using unit tests. These examine the main "Base Cases" of the expressions which can be evaluated such as 1+1, 2*3 and 4-5. There are also some more complicated tests in here. <br/>
Both valid and invalid inputs have also been tested with 10,000 randomised tests.<br/>
The execution has only been tested for valid inputs and using unit tests. This is because due to the way the topLevel code is structured the parse funciton will filter out any invalid inputs and then if the return from the parse function is an error the execution functions will not be ran.


