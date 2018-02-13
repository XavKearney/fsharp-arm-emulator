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
| Direction        | Aliases           | Effect  | List Access  |
| ------------- |:-------------:|:-----:|:-----:|
| `""`      | `"FD"`, `IA"` | Full Descending (Increment After) | Right-to-left |
| `"EA"`      | `"DB"` | Empty Ascending (Decrement Before)  | Right-to-left |
| `"FA"`      | None | Full Ascending  | Left-to-right |
| `"ED"`      | None | Empty Descending  | Left-to-right |
_Ascending_ instructions decrease the memory address with each load. _Descending_ does the opposite.
_Empty_ instructions increment the memory address in the `source` register before any data is loaded. The register list is accessed either right-to-left or left-to-right according to the combination of these two options (shown in the table above).
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
| Direction        | Aliases           | Effect  | List Access  |
| ------------- |:-------------:|:-----:|:-----:|
| `""`      | `"EA"`, `IA"` | Empty Ascending (Increment After) | Left-to-right |
| `"FD"`      | `"DB"` | Full Descending (Decrement Before)  | Right-to-left |
| `"FA"`      | None | Full Ascending  | Left-to-right |
| `"ED"`      | None | Empty Descending  | Right-to-left |
 _Ascending_ instructions increase the memory address with each load. _Descending_ does the opposite.
_Empty_ instructions increment the memory address in the `dest` register after data is loaded. The register list is accessed either right-to-left or left-to-right according to the combination of these two options (shown in the table above).

The following restrictions apply:
- The `dest` register must not be `PC`.
- The register list must not contain `SP` or `PC`.
- If write-back is specified, the register list must not contain the `dest` register.

## Implementation
The following details how the above instructions are implemented in the `MultMem` module.
### Parsing
The `parse` function first determines whether some given `LineData` corresponds to an opcode that the `MultMem` module handles. These are determined from the `multMemSpec`, which defines the roots and possible suffixes of all the opcodes handled. If an instruction is handled by `MultMem` and is of instruction class `MEM` (in this context, either an `LDM` or `STM` instruction) then the root, suffix and operands of the instruction are processed by the `makeMultMemInstr` function.
`makeMultMemInstr` first calls a `parseOps` function to convert the operands string into a target register (either source or destination), a boolean for whether to write-back, and a list of `RName`s corresponding to the register list. This is achieved by first splitting the operands string by `,` to determine the target register, and then checking to see if the string ends with `!`. The register list is then recombined and processed into a list of registers.
An error at any stage has a corresponding error message which bubbles up to the original call.
Before `makeMultMemInstr` returns a `MultMemInstr` with all the required data, the `checkValid` function ensures that the result adheres to the restrictions detailed above. If it does, the `parse` function returns a `Result<MultMemInstr,string>` which includes all of the necessary information to execute the instruction.

### Execution
Execution is performed by the `execMultMem` instruction. Based on the instruction type, it determines the memory address incrememnt order, the initial memory address and the order of the list. It then recursively traverses the register list and adjusts the given `cpuData` (of `DataPath` type) accordingly. The result is either an error (string), or the modified `cpuData` wrapped in a Result monad.

## Testing
### Parsing
The `parseOps` function is tested with a series of unit tests (`parseOps Unit`) to determine correct operation both for simple, valid instructions and also for invalid instructions where the expected output is the correct error message.
The more thorough testing occurs in the completely randomised property-based test defined within `testParse`. This generates `LineData` based on randomly generated parameters, including randomly chosen aliases, and then the expected result after parsing is generated by matching the randomly generated parameters to determine if they are valid. Every possible expected result is covered by the match, and the property test is run 10,000 times.