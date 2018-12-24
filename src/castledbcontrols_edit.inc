{%MainUnit castledbctrls.pp}

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

{$ifdef read_interface}

  { Database-aware edit box, rendered using Castle Game Engine.

    The following properties are controlled by the data source:
    Text, MaxLength.
  }
  TCastleDBEdit = class(TCastleEdit)
  strict private
    FDataLink: TFieldDataLink;
    procedure DataChange(Sender: TObject);
    procedure UpdateData(Sender: TObject);
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DoChange; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetFocused(const Value: boolean); override;
    property Field: TField read GetField;
  published
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ This is based on TDBEdit implementation in Lazarus,
  lazarus/lcl/include/dbedit.inc . }

constructor TCastleDBEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := @DataChange;
  FDataLink.OnUpdateData := @UpdateData;
end;

destructor TCastleDBEdit.Destroy;
begin
  FreeAndNil(FDataLink);
  inherited;
end;

procedure TCastleDBEdit.DataChange(Sender: TObject);
var
  DataLinkField: TField;
begin
  DataLinkField := FDataLink.Field;
  if DataLinkField <> nil then
  begin
    Text := DatalinkField.Text;
    if (DataLinkField.DataType in
         [ftString, ftFixedChar, ftWidestring, ftFixedWideChar]) and
       (MaxLength = 0) then
      MaxLength := DatalinkField.Size;
  end else
  begin
    Text := '';
    MaxLength := 0;
  end;
end;

procedure TCastleDBEdit.UpdateData(Sender: TObject);
begin
  FDataLink.Field.Text := Text;
end;

function TCastleDBEdit.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

function TCastleDBEdit.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

function TCastleDBEdit.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TCastleDBEdit.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

procedure TCastleDBEdit.SetDataSource(Value: TDataSource);
begin
  ChangeDataSource(Self, FDataLink, Value);
end;

procedure TCastleDBEdit.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and
     (FDataLink <> nil) and
     (AComponent = DataSource) then
    DataSource := nil;
end;

procedure TCastleDBEdit.DoChange;
begin
  FDataLink.Edit;
  FDataLink.Modified;
  inherited;
end;

procedure TCastleDBEdit.SetFocused(const Value: boolean);
begin
  inherited;
  if Value then
  begin
    if FDataLink.Edit then
      Text := FDataLink.Field.Text;
  end else
  begin
    { When focus lost, finalize editing that was started
      in TCastleDBEdit.DoChange. }
    if FDataLink.Editing then
    begin
      FDataLink.UpdateRecord;
      FDataLink.IsModified := False;
    end else
      FDataLink.Reset;
  end;
end;

{$endif read_implementation}
