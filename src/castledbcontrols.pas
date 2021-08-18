{
  Copyright 2018-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine DB Aware Controls".

  "Castle Game Engine DB Aware Controls" is free software; see the file LICENSE,
  included in this distribution, for details about the copyright.

  "Castle Game Engine DB Aware Controls" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Database-aware user interface controls, rendered using Castle Game Engine. }
unit CastleDBControls;

interface

uses SysUtils, Classes, DB,
  CastleControls, CastleClassUtils;

type
  {$define read_interface}
  {$I castledbcontrols_fielddatalink.inc}
  {$I castledbcontrols_edit.inc}
  {$undef read_interface}

implementation

uses CastleUIControls, CastleComponentSerialize;

{$define read_implementation}
{$I castledbcontrols_fielddatalink.inc}
{$I castledbcontrols_edit.inc}
{$undef read_implementation}

initialization
  RegisterSerializableComponent(TCastleDBEdit, 'Database-Aware Edit');
end.
