{
  Copyright 2021-2021 Michalis Kamburelis.

  This file is part of "Castle Game Engine DB Aware Controls".

  "Castle Game Engine DB Aware Controls" is free software; see the file LICENSE,
  included in this distribution, for details about the copyright.

  "Castle Game Engine DB Aware Controls" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Register in CGE editor some standard FPC components,
  to be able to set them to as "non-visual components" in CGE editor. }
unit CastleRegisterStandardComponents;

interface

implementation

uses DB, Dbf,
  CastleComponentSerialize;

initialization
  RegisterSerializableComponent(TDataSource, 'Data Source');
  RegisterSerializableComponent(TDbf, 'DBF');
end.
