# fsharp-arm-emulator
The goal of this project is to create a fully functional ARM Assembly Language Emulator in F#.

**For individual project statements, see the `docs` directory.**

## Running the Project
If you don't have `paket` installed, run `.paket/paket.bootstrapper.exe` and then `.paket/paket.exe install`. `paket` should now be installed.

To run from a fresh clone, if you have `paket` installed then from the root directory run:
`paket update`

This will install the necessary packages.

Then you can run the project at any time using:

`dotnet run --project ArmEmulator/ArmEmulator.fsproj`

NB: To test using the `VisualTest` framework, you must first populate the `visualapp/visual/jre/` folder with the correct binaries which can be downloaded [here](http://i.xav.ai/visual.zip).


## Structure
This project is both a self-contained emulator for ARM code, and also used as the back-end emulation for an electron app, whose repository is [here](https://github.com/djb15/arm-emulator-gui). As such, this project contains two `.fsproj` files:
1. `ArmEmulator.fsproj` - this is the main project file to be used in this repository. It includes all modules and all tests.
2. `EmulatorInterface.fsproj` - this is a dummy project file used by the electron app, which excludes all tests and the `Main.fs` file.

The project is split into independent modules, each with their own file of tests. The modules, in the order that the project includes them, along with their respective purposes and level of testing are:

| Module | Purpose | Unit Tests | Property-based Tests | Tested vs. VisUAL |
|---|---|---|---|---|
| `VTest` | Framework for testing against [VisUAL](https://salmanarif.bitbucket.io/visual/). | n/a | n/a | n/a |
| `CommonData` | Defines generic, project-wide types for data processing. | n/a | n/a | n/a |
| `CommonLex` | Defines generic, project-wide types for parsing instructions.  | n/a | n/a | n/a |
| `ParseExpr` | Defines generic, project-wide types for parsing instructions.  | ✔️ | ✖️ | n/a |
| `Arithmetic` | Implements arithmetic instructions, e.g. `ADD`,`SUB`,`CMP`  | ✔️ | ✔️ | ✔️ |
| `BitArithmetic` | Implements bitwise arithmetic instructions, e.g. `MOV`,`ORR`,`LSL`  | ✔️ | ✔️ | ✔️ |
| `Mem` | Implements memory-based instructions, e.g. `LDR`,`ADR`,`FILL`  | ✔️ | ✔️ | ✖️ |
| `MultMem` | Implements multiple-location memory-based instructions, as well as misc. instructions, e.g. `LDM`,`STM`,`B`  | ✔️ | ✔️ | ✔️|
| `TopLevel` | Implements complete emulation & instruction execution | ✔️ | ✖️  | n/a |
| `Main` | CLI to run tests or execute instructions from file. | ✖️ | ✖️ | n/a |

Unit tests have been written for all modules with significant complexity. Property-based testing has been employed in all cases where randomised generation of parameters is feasible (e.g. in `TopLevel`, randomly generating a random source file is not feasible).

Proof of the tests passing:
![Proof of tests passing](http://i.xav.ai/eR8FdJ.png)

## Features
### Instructions
The complete set of ARM instructions supported by this project are:

| Module | Instruction | Comments |
|:---:|:--:|:---|
| `Arithmetic` | `ADD` |   |
| `Arithmetic` | `SUB` |   |
| `Arithmetic` | `ADC` |   |
| `Arithmetic` | `SBC`  |   |
| `Arithmetic` | `RSB`  |   |
| `Arithmetic` | `RSC`  |   |
| `Arithmetic` | `CMP`  |   |
| `Arithmetic` | `CMN`  |   |
| `BitArithmetic`  | `MOV`  |   |
| `BitArithmetic`  | `MVN`  |   |
| `BitArithmetic`  | `AND`  | Flag setting conforms to ARM spec, not VisUAL. |
| `BitArithmetic`  | `ORR`  |  |
| `BitArithmetic`  | `EOR`  | Flag setting conforms to ARM spec, not VisUAL. |
| `BitArithmetic`  | `BIC`  |   |
| `BitArithmetic`  | `LSL`  | Shift value is modulo 32. |
| `BitArithmetic`  | `LSR`  | Shift value is modulo 32. |
| `BitArithmetic`  | `ASR`  | Shift value is modulo 32. |
| `BitArithmetic`  | `ROR`  |   |
| `BitArithmetic`  | `RRX`  |   |
| `BitArithmetic`  | `TST`  |   |
| `BitArithmetic`  | `TEQ`  |   |
| `Mem`  | `LDR`  | Does not support the `LDR RX, =someLabel` syntax.  |
| `Mem`  | `STR`  |   |
| `Mem`  | `ADR`  |   |
| `Mem`  | `FILL`  |   |
| `Mem`  | `DCD`  |   |
| `Mem`  | `EQU`  | Does not support >1 forward reference. |
| `MultMem`  | `LDM`  |   |
| `MultMem`  | `STM`  |   |
| `MultMem`  | `B`  |   |
| `MultMem`  | `BL`  |   |
| `MultMem`  | `END`  |   |

Unless otherwise stated, the syntax for each instruction is the same as for [VisUAL](https://salmanarif.bitbucket.io/visual/supported_instructions.html). More information on each of the instructions can be found at the [Arm InfoCenter](http://infocenter.arm.com/help/index.jsp?topic=/com.arm.doc.dui0552a/CIHDFHCC.html).

All instructions can be paired with any of the ARM condition codes, which can also be found in the InfoCenter.

For more detailed information on individual modules and instructions, see the individual documentation in the `docs` folder.

### Top Level

The `TopLevel` module brings all of the instructions together and enables execution of a complete program in the form of a list of strings. It provides a simple interface for the GUI, which only has to provide the source file as a list of strings, as well as the current CPU & symbol state (which can easily be initialised using `initDataPath`).

The features of this module include:

- Forward & backward referencing of labels in instructions (2 pass parsing).
- Module-specific error handling. Each module can optionally return a custom error type in an `Error` monad (this is currently just a string).
- Errors returned with a corresponding line number, to enable syntax highlighting in the GUI.
- Branches using `B`, `BL`, `MOV R15` or `MVN R15` behave as expected, causing a program loop. _NB: in the GUI code, tail recursion is not optimised by FABLE and therefore > 500 branches results in a stack overflow.
- Infinite loop protection: an error is thrown if more than 100,000 branches are taken. (this could be changed to a setting)
- Program counter is always 8 greater than the current instruction value.
- Parsing lines is immune to arbitrary whitespace within the line.
- All condition codes are supported - an instruction's execution is dependent on current flag contents.
- Arbitrary initialisation of registers, flags and memory contents (allows GUI to maintain CPU state)
- Code is stored in memory.
- Code is protected from being overwritten by instructions (returns an error). _NB: if an instruction was somehow overwritten, execution would continue as if it was not_
- `EQU` instructions execute correctly; they are not stored in memory.

Please see the [app respository](https://github.com/djb15/arm-emulator-gui) for details of the GUI features.

The `Main` module also defines a command line interface to the `TopLevel` code, which enables loading and execution from an arbitrary file location. The resultant CPU state is stored in a JSON file in another arbitrary file location. Registers and flags can also be initialised via the CLI.

## Use of Github
Throughout the group stage of the project we adopted agile development practices. Weekly sprint goals were set in the first two weeks, with daily goals set in the last week. 

Each team member was allocated tasks. Individual tasks were completed on branches, and pull requests were submitted once a branch was deemed complete by the author. Branches were not allowed to be merged without an approved review from another team member. All tests were required to pass before merging a pull request into master.

This module was referenced as a submodule in the electron app (GUI) repository, meaning any changes here could automatically be included in the app with a simple `git pull --recurse-submodules`.

_NB: Originally, Travis CI was set up to automatically run tests on branches/commits but, as it runs on Linux, it wasn't compatible with the VisualTesting framework and so CI was disabled._