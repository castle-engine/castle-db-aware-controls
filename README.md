# Database-aware controls using Castle Game Engine UI

The main file in this repository is the Pascal unit `CastleDBControls` in `src/castledbcontrols.pas`. It defines database-aware controls like `TCastleDBEdit`, very similar to standard Lazarus `TDBEdit`.

- Database-aware controls can easily be connected to display/edit a database record using Pascal `TDataSource` class. `TDataSource` connects to a `TDataSet`, and FPC/Lazarus include a large number of `TDataSet` for nearly every database technology (SQL or not) out there.

- The controls in `CastleDBControls` unit use _Castle Game Engine_ user interface, so they descend from `TCastleUserInterface`, they are rendered using OpenGL(ES) (or any future CGE renderer), and they can be designed using [Castle Game Engine Editor](https://castle-engine.io/manual_editor.php).

For the purpose of this demo, I chose a simple "database", which defines a single table using [TSdfDataSet](http://wiki.freepascal.org/TSdfDataSet), which is [pretty much just a CSV file](http://wiki.freepascal.org/SDF). This allows to easily run it on any system, without installing or configuring anything. Of course, `TCastleDBEdit` will work with any database that can be expressed using Pascal `TDataSet` descendant (and thus also works with `TDataSource`), so you can as well use any full-featured SQL database there.

Note that (right now) you cannot use non-visual non-CGE components, like `TDataSource` and `TSdfDataSet`, on the designed CGE UI (in .castle-user-interface file). So our example in `examples/cge_monster_database_example/` uses a data module to configure `TDataSource` and `TSdfDataSet` visually. Alternatively, you could of course just create and initialize `TDataSource` and `TSdfDataSet` completely from Pascal code.

TODO: Doing Insert on TSdfDataSet is broken, it later causes Access Violation at post.

## License

Copyright 2018 Michalis Kamburelis .

Everything in this repository `castle-db-aware-controls` is under a permissive _Apache 2.0 License_. Simplifying: you can use it however you like (including for closed-source projects), just keep a mention that the original code is copyright by Michalis Kamburelis.

The unit `src/castledbcontrols.pas` is dual-licenced: you can alternatively use it on the same license as _Castle Game Engine_ core, which is [LGPL with static linking exception](https://github.com/castle-engine/castle-engine/blob/master/COPYING.md). Simplifying: you can use this unit in your closed-source applications, but you must publish your modifications to this unit openly.
