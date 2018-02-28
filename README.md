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

[Insert some details on the structure of the project here.]

## Roadmap

- [x] Initial project setup
- [ ] Create the roadmap
- [ ] Task delegation