# Database-aware controls using Castle Game Engine UI

The main file in this repository is the Pascal unit `CastleDBControls` in `src/castledbcontrols.pas`.

It defines database-aware controls like `TCastleDBEdit`, very similar to standard Lazarus `TDBEdit`. Database-aware controls can easily be connected to display/edit a database record using Pascal `TDataSource` class (that may, in turn, be connected to an incredible number of database types). The controls in `CastleDBControls` unit use _Castle Game Engine_ user interface, so they descend from `TCastleUserInterface`, they are rendered using OpenGL(ES) (or any future CGE renderer), and they can be designed using [Castle Game Engine Editor](https://castle-engine.io/manual_editor.php).

## License

Copyright 2018 Michalis Kamburelis .

The `src/castledbcontrols.pas` is open-source and available on the same license as _Castle Game Engine_ core, which is [LGPL with static linking exception](https://github.com/castle-engine/castle-engine/blob/master/COPYING.md). Simplifying: you can use this unit in your closed-source applications, but you must publish your modifications to this unit openly.

The rest of the code in this repository `castle-db-aware-controls` is under a permissive _Apache 2.0 License_. Simplifying: you can use it however you like (including for closed-source projects), just keep a mention that the original code is copyright by Michalis Kamburelis.
