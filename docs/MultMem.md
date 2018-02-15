# MultMem
This documentation covers how the `MultMem.fs` and `MultMemTests.fs` files work, what instructions they cover, and how these are tested.
_Author: Xav Kearney_

## Instructions
The instructions covered are:
- `LDM`
- `STM`

(`B`, `BL` and `END` are to be added)
### LDM - Load Multiple Registers
`LDM` is ARM's _Load Multiple Registers_ instruction. It has the syntax:
```LDM[dir]{cond} source[!], {list of registers}```
An example command would be:
```LDM R0, {R3,R7,R9}```
The registers in the `{list of registers}` are loaded with the word values of memory addresses based on the value of the `source` register. The optional `!` indicates whether to write-back the value of the final memory location to the source  The `dir` (direction) optional suffix indicates how the source memory addresses are calculated based on this value, and takes one of following forms:

| Direction | Aliases       | Effect                             |
| --------- | :-----------: | :--------------------------------: |
| `""`      | `"FD"`, `IA"` | Full Descending (Increment After)  |
| `"EA"`    | `"DB"`        | Empty Ascending (Decrement Before) |
| `"FA"`    | None          | Full Ascending                     |
| `"ED"`    | None          | Empty Descending                   |

_Ascending_ instructions decrease the memory address with each load. _Descending_ does the opposite.
_Empty_ instructions increment the memory address in the `source` register before any data is loaded. The register list is always accessed in order of increasing register numbers (i.e. `R0` before `R1`).
The following restrictions apply:
- The `source` register must not be `PC`.
- The register list must not contain `SP`.
- The register list must not contain `PC` if it contains `LR`.
- If write-back is specified, the register list must not contain the `source` register.
### STM - Store Multiple Registers
`STM` is ARM's _Store Multiple Registers_ instruction, essentially the opposite to `LDM`. It has the syntax:
```STM[dir]{cond} dest[!], {list of registers}```
An example command would be:
```STM R0, {R3,R7,R9}```
The word values of the registers in the `{list of registers}` are stored in the memory addresses based on the value of the `dest` (destination) register. The optional `!` indicates whether to write-back the value of the final memory location to the destination register. The `dir` (direction) optional suffix indicates how the source memory addresses are calculated based on this value, and takes one of following forms (NB: different to `LDM`):

| Direction | Aliases       | Effect                             |
| --------- | :-----------: | :--------------------------------: | 
| `""`      | `"EA"`, `IA"` | Empty Ascending (Increment After)  |
| `"FD"`    | `"DB"`        | Full Descending (Decrement Before) |
| `"FA"`    | None          | Full Ascending                     |
| `"ED"`    | None          | Empty Descending                   |

 _Ascending_ instructions increase the memory address with each load. _Descending_ does the opposite.
_Empty_ instructions increment the memory address in the `dest` register after data is loaded. The register list is always accessed in order of increasing register numbers (i.e. `R0` before `R1`).

The following restrictions apply:
- The `dest` register must not be `PC`.
- The register list must not contain `SP` or `PC`.
- If write-back is specified, the register list must not contain the `dest` register.

## Implementation
The following details how the above instructions are implemented in the `MultMem` module.
### LDM & STM
#### Parsing
The `parse` function first determines whether some given `LineData` corresponds to an opcode that the `MultMem` module handles. These are determined from the `multMemSpec`, which defines the roots and possible suffixes of all the opcodes handled. If an instruction is handled by `MultMem` and is of instruction class `MEM` (in this context, either an `LDM` or `STM` instruction) then the root, suffix and operands of the instruction are processed by the `makeMultMemInstr` function.

`makeMultMemInstr` first calls a `parseOps` function to convert the operands string into a target register (either source or destination), a boolean for whether to write-back, and a list of `RName`s corresponding to the register list. This is achieved by first splitting the operands string by `,` to determine the target register, and then checking to see if the string ends with `!` (indicating a write-back suffix). The register list is then recombined and processed into a list of `RName`s.

The `MultMemInstr` type includes all parameters required to execute any valid `LDM/STM` instruction, given CPU state in the form of a `DataPath`.

An error at any stage has a corresponding error message which bubbles up to the original call. A function `checkValid` ensures that the resulting parsed `MultMemInstr` adheres to the restrictions detailed above. If valid, the `parse` function returns a `Result<MultMemInstr,string>`.

#### Execution
Execution is performed by the `execMultMem` function, which is passed a parsed `MultMemInstr` as a parameter, along with CPU state in the form of a `DataPath`. 

First, the relevant data from the `MultMemInstr` is extracted and the `checkValid` instruction is used again to ensure validity. The register list is sorted, the initial memory address determined and the direction embedded into a function `dirOp` which performs either addition or subtraction. The list of registers is then recursively traversed and adjusts the given `DataPath` accordingly. The result is either an Error (string), or the modified `cpuData` wrapped in a Result monad.

## Testing
### LDM & STM
#### Parsing
The `parseOps` function is tested with a series of unit tests (`parseOps Unit`) to determine correct operation both for simple, valid instructions and also for invalid instructions where the expected output is the correct error message.

The more thorough testing occurs in the completely randomised property-based test defined within `testParse`. This generates `LineData` based on randomly generated parameters, including randomly chosen aliases, and then the expected result after parsing is generated by matching the randomly generated parameters to determine if they are valid. Every possible expected result is covered by the match, and the property test is run 10,000 times.

#### Execution
Initially, manually tests were performed to confirm that the `execMultMem` function correctly modifies CPU state according to the ARM specification.

However, for maximum robustness, a property-based test is used to compare the effect of a random instruction on CPU state with the CPU state after the same instruction is processed by [VisUAL](https://salmanarif.bitbucket.io/visual/). This is achieved with the help of a `VisualTesting` framework, written by [Dr. Tom Clarke](https://intranet.ee.ic.ac.uk/electricalengineering/eepeople/person.asp?s=NIL&id=75) and adapted into a package, with some minor modifications to allow initialised memory, by Xav Kearney.

This property-based test randomly generates both initial CPU state (registers, memory and flags) and also the instruction parameters. The random parameters are first checked to see if they conform to the restrictions set both by ARM (as above) and VisUAL (see below). If not, the test is skipped. If the parameters are valid they are converted into the forms used by both `MultMem.fs` and VisUAL (which differ slightly due to types), and the results are compared. Some sorting is required to transform the resulting VisUAL register & memory state into the same form as the result of `execMultMem`.

If no errors are returned, the register contents, memory address data and flags are all checked for equality to consider a test passed. By default 10,000 tests are run (although the actual test number is much lower given the validity constraints). Increasing this to 100,000 takes roughly 15 minutes to run and results in a 100% pass rate.

#### Differences from VisUAL
In testing, a number of differences from VisUAL were discovered:
1. VisUAL does not allow instructions without a suffix, e.g. `LDM R0, {R1}`. 
   - These instructions are excluded from testing, but allowed by both the parsing and execution functions because it is a trivial difference, conforms to the ARM spec, and allows easier, more readable code to be written.
2. VisUAL does not allow `LDM/STM` instructions to access memory space below `0x1000`, which is reserved for code.
   - Tests are limited to a starting memory address above `0x1030` (to allow instructions which decrement by up to `0x30`)
   - This restriction does not apply to this F# implementation. Memory locations can be set a `Code` or a `DataLoc` types, and this distinction is handled within the execution; accessing a `Code` memory location throws an error.
3. VisUAL does not allow the target register to be within the register list, even when the writeback suffix is not used.
    - This restriction makes sense within the educational context of VisUAL. However, this module aims to adhere to the ARM specification as closely as possible, so this module allows the target register to be within the register list given that the writeback suffix is not used.
4. VisUAL requires memory addresses to be divisible by 4.
    - This is not a restriction used by the `MultMem` module, but the module does incremement addresses by 4 so that the addresses can be considered as referring to bytes, to maximise compatibility with VisUAL. This memory model may change during the group phase if it allows for better error checking.
4. It is not possible to test with register lists larger than 12 registers as a result of the `VisualTesting` framework. This is because the memory addresses are loaded into registers.
5. It is not possible to test when `R15` is within the register list against VisUAL because it causes the code to branch to an unwanted instruction location.


## TODO
- [ ] Add support for `{R0-R5}` in the register list
- [ ] Add support for `B/BL/END` instructions
