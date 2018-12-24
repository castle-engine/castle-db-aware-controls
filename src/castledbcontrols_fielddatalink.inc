{%MainUnit castledbctrls.pp}

{
  Copyright 2003-2018 Lazarus developers.

  This is adjusted from DBCtrls unit in Lazarus, lazarus/lcl/dbctrls.pp .
  Lazarus license is the same as Castle Game Engine.
  We don't want to just use DBCtrls unit, as CGE user interface
  should not depend on LCL.

  This file is part of "Castle Game Engine DB Aware Controls".

  "Castle Game Engine DB Aware Controls" is free software; see the file LICENSE,
  included in this distribution, for details about the copyright.

  "Castle Game Engine DB Aware Controls" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}

  { Utility class to implement DB-aware controls. }
  TFieldDataLink = class(TDataLink)
  private
    FField: TField;
    FFieldName: string;
    FControl: TComponent;
    // Callbacks
    FOnDataChange: TNotifyEvent;
    FOnEditingChange: TNotifyEvent;
    FOnUpdateData: TNotifyEvent;
    FOnActiveChange: TNotifyEvent;
    // Curent State of Affairs
    FEditing: Boolean;
    IsModified: Boolean;
    function FieldCanModify: boolean;
    function IsKeyField(aField: TField): Boolean;
    function GetCanModify: Boolean;
    // set current field
    procedure SetFieldName(const Value: string);
    procedure UpdateField;
    // make sure the field/fieldname is valid before we do stuff with it
    procedure ValidateField;
  protected
    // Testing Events
    procedure ActiveChanged; override;
    procedure EditingChanged; override;
    procedure LayoutChanged; override;
    procedure RecordChanged(aField: TField); override;
    procedure UpdateData; override;

    procedure FocusControl(aField: TFieldRef); Override;
  public
    constructor Create;
    // for control intitiating db changes etc
    function Edit: Boolean;

    procedure Modified;
    procedure Reset;

    // Attached control
    property Control: TComponent read FControl write FControl;

    // Basic DB interfaces
    property Field: TField read FField;
    property FieldName: string read FFieldName write SetFieldName;

    // Current State of DB
    property CanModify: Boolean read GetCanModify;
    property Editing: Boolean read FEditing;

    // Our Callbacks
    property OnDataChange: TNotifyEvent read FOnDataChange write FOnDataChange;
    property OnEditingChange: TNotifyEvent read FOnEditingChange write FOnEditingChange;
    property OnUpdateData: TNotifyEvent read FOnUpdateData write FOnUpdateData;
    property OnActiveChange: TNotifyEvent read FOnActiveChange write FOnActiveChange;
  end;

{$endif read_interface}

{$ifdef read_implementation}

function TFieldDataLink.FieldCanModify: boolean;
var
  FieldList: TList;
  i: Integer;
begin
  result := Assigned(FField);
  if not result then
    exit;

  if FField.FieldKind=fkLookup then
  begin
    FieldList := TList.Create;
    try
      DataSet.GetFieldList(FieldList, FField.KeyFields);
      result := (FieldList.Count>0);
      i := 0;
      while result and (i<FieldList.Count) do
      begin
        result := TField(FieldList[i]).CanModify;
        inc(i);
      end;
    finally
      FieldList.Free;
    end;
  end else
    result := FField.CanModify;
end;

function TFieldDataLink.IsKeyField(aField: TField): Boolean;
var
  KeyFieldName, KeyFields: String;
  StrPos: Integer;
begin
  KeyFields := FField.KeyFields;
  StrPos := 1;
  while StrPos <= Length(KeyFields) do
  begin
    KeyFieldName := ExtractFieldName(KeyFields, StrPos);
    if SameText(aField.FieldName, KeyFieldName) then
    begin
      Result := True;
      Exit;
    end;
  end;
  Result := False;
end;

{TFieldDataLink  Private Methods}
{
  If the field exists and can be modified, then
  we CanModify as long as this hasn't been set
  ReadOnly somewhere else. Do we need any extra tests here?
}
function TFieldDataLink.GetCanModify: Boolean;
begin
  if FieldCanModify then
    Result := not ReadOnly
  else
    Result := False;
end;

{
  Set the FieldName and then notify the changes though EditingChanged and Reset
  Ensure FField is nil if something goes wrong or FieldName is empty
}
procedure TFieldDataLink.SetFieldName(const Value: string);
begin
  if FFieldName <> Value then
  begin
    FFieldName := Value;
    UpdateField;
    if Active then
    begin
      EditingChanged;
      Reset;
    end;
  end;
end;

procedure TFieldDataLink.UpdateField;
begin
  if Active and (FFieldName <> '') then
    FField := DataSet.FieldByName(FFieldName)
  else
    FField := nil;
end;

{
  This function checks if FField is still associated with the dataset
  If not update the field
}
procedure TFieldDataLink.ValidateField;
begin
  if not (DataSet.FindField(FFieldName) = FField) then
    UpdateField;
end;


{TFieldDataLink  Protected Methods}

{ Delphi Help ->
    Changes to the Active property trigger the ActiveChanged method.
    If an OnActiveChange event handler is assigned, ActiveChanged calls
    this event handler. If ActiveChanged is triggered by a transition into
    an active state, then before calling the event handler, ActiveChanged makes
    sure that the Field for this TFieldDataLink is still valid.
  <-- Delphi Help

   Update the field instance. When not Active field will be set to nil
   Call OnActiveChange
}
procedure TFieldDataLink.ActiveChanged;
begin
  if FFieldName <> '' then
  begin
    UpdateField;
    EditingChanged;
    Reset;
  end;
  if Assigned(FOnActiveChange) then
    FOnActiveChange(Self);
end;

{ Delphi Help ->
    Changing the field binding can change the validity of the CanModify
    property, since individual field components can disallow edits. If
    TFieldDataLink is in an editing state when the Field property is changed,
    EditingChanged checks the CanModify property. If CanModify is False, it
    changes back out of the editing state.

    Note: This differs significantly from the inherited EditingChanged method
    of TDataLink. The functionality of the inherited method is replaced in
    TFieldDataLink by the OnEditingChange event handler.
  <-- Delphi Help

  ok so another event... but this time we simply change modified state
  if Editing and not CanModify? or do we also change to match if
  if not Editing and CanModify? i.e If Editing <> CanModify??  Will assume
  the latter just in case. easy to change back if I am wrong.

  Also based on this we replace parent routine, so do we need to keep track
  of Editing state ourself? I hope this is right. Anyone know for sure?

  OK .. based on the Modified routine we need to turn off
  our IsModified routine when succesfull right? so for now just turn
  it off as per my example.
}
procedure TFieldDataLink.EditingChanged;
var
  RealEditState : Boolean;
begin
  RealEditState := (CanModify and Inherited Editing);

  if (FEditing <> RealEditState) then
  begin
    FEditing := RealEditState;
    if not FEditing then
      IsModified := False;
    if Assigned(FOnEditingChange) then
      FOnEditingChange(Self);
  end;
end;

{ Delphi Help ->
    LayoutChanged is called after changes in the layout of one of the
    containers of the Control for this TFieldDataLink that might change the
    validity of its field binding. For example, if the Control is embedded
    within a TCustomDBGrid, and one of the columns is deleted, the Field
    property for the Control might become invalid.
  <-- Delphi Help

  Ensure FField is valid and notify
}
procedure TFieldDataLink.LayoutChanged;
begin
  ValidateField;
  if FField <> nil then
  begin
    EditingChanged;
    RecordChanged(nil);
  end;
end;

{ Delphi Help ->
    Applications can not call this protected method. It is triggered
    automatically when the contents of the current record change.
    RecordChanged calls the OnDataChange event handler if there is one.

    TDataLink.RecordChanged:
    The Field parameter indicates which field of the current record has changed in value.
    If Field is nil (Delphi) or NULL (C++), any number of fields within the current record may have changed.
  <-- Delphi Help

  Call Reset if AField = FField or aField = nil
}
procedure TFieldDataLink.RecordChanged(aField: TField);
begin
  if (aField = nil) or (aField = FField) or
   ((FField <> nil) and (FField.FieldKind = fkLookup) and IsKeyField(aField)) then
    Reset;
end;

{ Delphi Help ->
    UpdateData overrides the default UpdateData method to call the
    OnUpdateData event handler where the data-aware control can write any
    pending edits to the record in the dataset.
  <-- Delphi Help

  where..can write pending events. So I guess when we have already
  called Modified? Aka if not IsModified exit otherwise call event?
  works for me.
}
procedure TFieldDataLink.UpdateData;
begin
  if not IsModified then
    exit;
  try
    if Assigned(FOnUpdateData) then
      FOnUpdateData(Self);
  finally
    IsModified := False;
  end;
end;

{ Delphi Help ->
    Call FocusControl to give the Control associated with this TFieldDataLink
    object the input focus. FocusControl checks whether the Control can receive
    input focus, and if so, calls its SetFocus method to move focus to the
    Control.
  <-- Delphi Help

  Check if the field matches and if Control is TWinControl than call SetFocus
  Set the FieldRef to nil so no other control get focus
}

procedure TFieldDataLink.FocusControl(aField: TFieldRef);
begin
  { TODO:
    This doesn't do anything for CGE now, until we implement something
    like TUIContrainer.PersistentFocus. }
end;

{TFieldDataLink  Public Methods}

constructor TFieldDataLink.Create;
begin
  inherited Create;
  VisualControl := True;
  //FField := nil;
  //FFieldname := '';
end;

{ Delphi Help ->
    Use Edit to try to ensure that the contents of the field can be modified.
    A return value of True indicates that the field was already in an editing
    state, or that the DataSource was successfully changed to allow editing.
    A return value of False indicates that the DataSource could not be changed
    to allow editing. For example, if the CanModify property is False, Edit
    fails, and returns False.
  <-- Delphi Help

  ok so the way I see it, since the inherited function calls EditingChanged,
  which we have already overriden to modify our own Editing state if its invalid,
  I should just be calling the inherited routine here, but only if CanModify,
  since there is no point otherwise. But since we _are_ keeping track of editing
  state ourselves we return our own state, not the inherited. If anyone know
  better please fix.
}
function TFieldDataLink.Edit: Boolean;
begin
  if (not FEditing) and CanModify then
    inherited Edit;

  Result := FEditing;
end;

{ Delphi Help ->
    Call Modified when the Control for this TFieldDataLink begins processing
    edits.
  <-- Delphi Help

  ok so. well _that's_ helpfull. for the moment going to keep track
  by adding an IsModified... based on the other functions thus far
  we need to know whether we are in state, so I am assuming it goes

  Call Modified ->
    IsModified:=True;//Waiting for modifications

  Call SomeFunction->
    If IsModified then begin
      (do something)
      IsModified := False;//All modifications complete
    end
    else
     (do something else? exit?);
}
procedure TFieldDataLink.Modified;
begin
  IsModified := True;
end;

{ Delphi Help ->
    The Control that owns a TFieldDataLink object calls its Reset method to
    process a UI action that cancels edits to the field. Reset calls the
    OnDataChange event handler without writing any pending changes to the
    record in the dataset.
  <-- Delphi Help

  Just call to the OnDataChange Event, and turn off IsModified
}
procedure TFieldDataLink.Reset;
begin
  if Assigned(FOnDataChange) then
    FOnDataChange(Self);

  IsModified := False;
end;

{ global utility ------------------------------------------------------------- }

procedure ChangeDataSource(AControl: TCastleUserInterface; Link: TDataLink;
  NewDataSource: TDataSource);
begin
  if Link.DataSource=NewDataSource then exit;
  if Link.DataSource<>nil then
    Link.DataSource.RemoveFreeNotification(AControl);
  Link.DataSource:=NewDataSource;
  if Link.DataSource<>nil then
    Link.DataSource.FreeNotification(AControl);
end;

{$endif read_implementation}
