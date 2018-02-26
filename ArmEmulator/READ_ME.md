# ARM Assembler Instructions - APSW


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
So far I have tested my LDR and STR parser using a series of 36 unit tests going through most of the combinations of the six ways in which LDR and STR can be implemented, however so far they have only been tested for valid inputs.


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
Currently only the parsing of DCD has been tested, and it has only been tested using unit tests. 

<br />

## *Fill*
### Documentation of visUAL implementation
*Eg*: label Fill N

*Eg*: `ghi Fill 4`

The Fill instruction declares a series of empty consecutive words in memory. Because it declares words and not bytes N must be a positive multiple of 4.

### visUAL Quirks 

### Parsing

### Execution
### How I've tested it  

<br />

## *EQU*
### Documentation of visUAL implementation

### visUAL Quirks 
It is interesting that you can use the EQU instruction to assign the same symbol multiple values. 

### Parsing

### Execution


### How I've Tested it 
So fair I have tested these three instuctions using a series of 33 unit tests which test base case inputs and some edge cases such as negative inputs to the #N part of Fill N.

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
I have so far tested the parsing of this function with a series of unit tests. These examine the main "Base Cases" of the expressions which can be evaluated such as 1+1, 2*3 and 4-5. There are also some more complicated tests in here. However I intend to add random/systematic testing after I have all the functionality needed for the project.

<br />
<br />
<br />
<br />
<br />
<br />
<br />
<br />

Testing F# code blocks
```fsharp
        let test = 1
```
three or more ...
---
hyphens 
***
Asterisks

___

Underscores
<br />
<br />
<br />
<br />
<br />
<br />
<br />
<br />


# Demo Stuff
  - Import a HTML file and watch it magically convert to Markdown
  - Drag and drop images (requires your Dropbox account be linked)


You can also:
  - Import and save files from GitHub, Dropbox, Google Drive and One Drive
  - Drag and drop markdown and HTML files into Dillinger
  - Export documents as Markdown, HTML and PDF

Markdown is a lightweight markup language based on the formatting conventions that people naturally use in email.  As [John Gruber] writes on the [Markdown site][df1]

> The overriding design goal for Markdown's
> formatting syntax is to make it as readable
> as possible. The idea is that a
> Markdown-formatted document should be
> publishable as-is, as plain text, without
> looking like it's been marked up with tags
> or formatting instructions.

This text you see here is *actually* written in Markdown! To get a feel for Markdown's syntax, type some text into the left window and watch the results in the right.

### Tech

Dillinger uses a number of open source projects to work properly:

* [AngularJS] - HTML enhanced for web apps!
* [Ace Editor] - awesome web-based text editor
* [markdown-it] - Markdown parser done right. Fast and easy to extend.
* [Twitter Bootstrap] - great UI boilerplate for modern web apps
* [node.js] - evented I/O for the backend
* [Express] - fast node.js network app framework [@tjholowaychuk]
* [Gulp] - the streaming build system
* [Breakdance](http://breakdance.io) - HTML to Markdown converter
* [jQuery] - duh

And of course Dillinger itself is open source with a [public repository][dill]
 on GitHub.

### Installation

Dillinger requires [Node.js](https://nodejs.org/) v4+ to run.

Install the dependencies and devDependencies and start the server.

```sh
$ cd dillinger
$ npm install -d
$ node app
```

For production environments...

```sh
$ npm install --production
$ NODE_ENV=production node app
```

### Plugins

Dillinger is currently extended with the following plugins. Instructions on how to use them in your own application are linked below.

| Plugin | README |
| ------ | ------ |
| Dropbox | [plugins/dropbox/README.md][PlDb] |
| Github | [plugins/github/README.md][PlGh] |
| Google Drive | [plugins/googledrive/README.md][PlGd] |
| OneDrive | [plugins/onedrive/README.md][PlOd] |
| Medium | [plugins/medium/README.md][PlMe] |
| Google Analytics | [plugins/googleanalytics/README.md][PlGa] |


### Development

Want to contribute? Great!

Dillinger uses Gulp + Webpack for fast developing.
Make a change in your file and instantanously see your updates!

Open your favorite Terminal and run these commands.

First Tab:
```sh
$ node app
```

Second Tab:
```sh
$ gulp watch
```

(optional) Third:
```sh
$ karma test
```
#### Building for source
For production release:
```sh
$ gulp build --prod
```
Generating pre-built zip archives for distribution:
```sh
$ gulp build dist --prod
```
### Docker
Dillinger is very easy to install and deploy in a Docker container.

By default, the Docker will expose port 8080, so change this within the Dockerfile if necessary. When ready, simply use the Dockerfile to build the image.

```sh
cd dillinger
docker build -t joemccann/dillinger:${package.json.version}
```
This will create the dillinger image and pull in the necessary dependencies. Be sure to swap out `${package.json.version}` with the actual version of Dillinger.

Once done, run the Docker image and map the port to whatever you wish on your host. In this example, we simply map port 8000 of the host to port 8080 of the Docker (or whatever port was exposed in the Dockerfile):

```sh
docker run -d -p 8000:8080 --restart="always" <youruser>/dillinger:${package.json.version}
```

Verify the deployment by navigating to your server address in your preferred browser.

```sh
127.0.0.1:8000
```

#### Kubernetes + Google Cloud

See [KUBERNETES.md](https://github.com/joemccann/dillinger/blob/master/KUBERNETES.md)


### Todos

 - Write MORE Tests
 - Add Night Mode

License
----

MIT


**Free Software, Hell Yeah!**

[//]: # (These are reference links used in the body of this note and get stripped out when the markdown processor does its job. There is no need to format nicely because it shouldn't be seen. Thanks SO - http://stackoverflow.com/questions/4823468/store-comments-in-markdown-syntax)


   [dill]: <https://github.com/joemccann/dillinger>
   [git-repo-url]: <https://github.com/joemccann/dillinger.git>
   [john gruber]: <http://daringfireball.net>
   [df1]: <http://daringfireball.net/projects/markdown/>
   [markdown-it]: <https://github.com/markdown-it/markdown-it>
   [Ace Editor]: <http://ace.ajax.org>
   [node.js]: <http://nodejs.org>
   [Twitter Bootstrap]: <http://twitter.github.com/bootstrap/>
   [jQuery]: <http://jquery.com>
   [@tjholowaychuk]: <http://twitter.com/tjholowaychuk>
   [express]: <http://expressjs.com>
   [AngularJS]: <http://angularjs.org>
   [Gulp]: <http://gulpjs.com>

   [PlDb]: <https://github.com/joemccann/dillinger/tree/master/plugins/dropbox/README.md>
   [PlGh]: <https://github.com/joemccann/dillinger/tree/master/plugins/github/README.md>
   [PlGd]: <https://github.com/joemccann/dillinger/tree/master/plugins/googledrive/README.md>
   [PlOd]: <https://github.com/joemccann/dillinger/tree/master/plugins/onedrive/README.md>
   [PlMe]: <https://github.com/joemccann/dillinger/tree/master/plugins/medium/README.md>
   [PlGa]: <https://github.com/RahulHP/dillinger/blob/master/plugins/googleanalytics/README.md>
