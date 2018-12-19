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

uses CastleControls;

type
  { Database-aware edit box, rendered using Castle Game Engine. }
  TCastleDBEdit = class(TCastleEdit)
  end;

implementation

uses CastleComponentSerialize;

initialization
  RegisterSerializableComponent(TCastleDBEdit, 'Database-Aware Edit');
end.
