# Database-aware controls using Castle Game Engine UI

## What is this

The main file in this repository is the Pascal unit `CastleDBControls` in `src/castledbcontrols.pas`. It defines database-aware controls like `TCastleDBEdit`, very similar to standard Lazarus `TDBEdit`.

- _Database-aware controls_ can easily be connected to display/edit a database record using Pascal `TDataSource` class. `TDataSource` connects to a `TDataSet`, and FPC/Lazarus include a large number of `TDataSet` descendants for nearly every database technology (SQL or not) existing.

- The controls in `CastleDBControls` unit use [Castle Game Engine](https://castle-engine.io/) user interface, so they descend from [TCastleUserInterface](https://castle-engine.io/manual_2d_user_interface.php), they are rendered using OpenGL(ES) (or any future CGE renderer), and they can be designed using [Castle Game Engine Editor](https://castle-engine.io/manual_editor.php).

The example in `examples/test_cge_database_controls/` presents simple editing of a table using CGE user interface. Note that it places non-visual non-CGE components (`TDataSource` and `TDbf`) in a data module. You cannot place them in CGE UI (`.castle-user-interface` file) for now. Alternatively, you could of course just create and initialize `TDataSource` and `TDbf` completely from Pascal code.

## Database format (DBF) used in the examples

For the purpose of the examples (in `examples/` subdirectory), I use a simple "database", which is just a single table in the DBF file format. It is handled through `TDbf` class in Pascal, descendant of `TDataSet`. This allows to easily run examples on any system, without installing or configuring anything -- you don't need any server, or any dynamic client library, to read DBF files.

The sample DBF file (address book) is copied from Lazarus examples.

Initially I wanted to use something even simpler: [TSdfDataSet](http://wiki.freepascal.org/TSdfDataSet), which is [pretty much just a CSV file](http://wiki.freepascal.org/SDF). Unfortunately, it crashes with _Access Violation_ after adding a new record (tested with FPC 3.0.4 and latest 3.3.1 revision 39471). (TODO: I have not yet submitted it to FPC team. If you want to create a good bugreport about this, please do, and drop me a link.)

Of course, `TCastleDBEdit` will work with any database that can be expressed using Pascal `TDataSet` descendant (and thus also works with `TDataSource`), so you can as well use any full-featured SQL database there.

## License

Copyright 2018 Michalis Kamburelis .

Everything in this repository `castle-db-aware-controls` is under a permissive _Apache 2.0 License_. Simplifying: you can use it however you like (including for closed-source projects), just keep a mention that the original code is copyright by Michalis Kamburelis.

The unit `src/castledbcontrols.pas` is dual-licenced: you can alternatively use it on the same license as _Castle Game Engine_ core, which is [LGPL with static linking exception](https://github.com/castle-engine/castle-engine/blob/master/COPYING.md).
