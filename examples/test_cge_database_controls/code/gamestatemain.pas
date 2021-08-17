{
  Copyright 2018-2021 Michalis Kamburelis.

  This file is part of "Castle Game Engine DB Aware Controls".

  "Castle Game Engine DB Aware Controls" is free software; see the file LICENSE,
  included in this distribution, for details about the copyright.

  "Castle Game Engine DB Aware Controls" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Main state, where most of the application logic takes place. }
unit GameStateMain;

interface

uses Classes,
  CastleVectors, CastleUIState, CastleComponentSerialize,
  CastleUIControls, CastleControls, CastleKeysMouse, CastleDBControls;

type
  { Main state, where most of the application logic takes place. }
  TStateMain = class(TUIState)
  private
    { Components designed using CGE editor, loaded from gamestatemain.castle-user-interface. }
    LabelRecordInfo: TCastleLabel;
    EditFirstName, EditLastName, EditWww: TCastleDBEdit;
    ButtonNext, ButtonPrevious, ButtonNew, ButtonDelete,
      ButtonOpenInBrowser: TCastleButton;

    { Update LabelRecordInfo.Caption. }
    procedure UpdateRecordInfo;

    procedure ClickNext(Sender: TObject);
    procedure ClickPrevious(Sender: TObject);
    procedure ClickNew(Sender: TObject);
    procedure ClickDelete(Sender: TObject);
    procedure ClickOpenInBrowser(Sender: TObject);
    procedure SomeEditChanged(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Stop; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
  end;

var
  StateMain: TStateMain;

implementation

uses SysUtils, TypInfo, DB,
  CastleOpenDocument, CastleURIUtils,
  DataModuleSampleDatabase;

{ TStateMain ----------------------------------------------------------------- }

constructor TStateMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gamestatemain.castle-user-interface';
end;

procedure TStateMain.Start;
begin
  inherited;

  DbModule := TDbModule.Create(FreeAtStop);

  { Find components, by name, that we need to access from code }
  LabelRecordInfo := DesignedComponent('LabelRecordInfo') as TCastleLabel;
  EditFirstName := DesignedComponent('EditFirstName') as TCastleDBEdit;
  EditLastName := DesignedComponent('EditLastName') as TCastleDBEdit;
  EditWww := DesignedComponent('EditWww') as TCastleDBEdit;
  ButtonNext := DesignedComponent('ButtonNext') as TCastleButton;
  ButtonPrevious := DesignedComponent('ButtonPrevious') as TCastleButton;
  ButtonNew := DesignedComponent('ButtonNew') as TCastleButton;
  ButtonDelete := DesignedComponent('ButtonDelete') as TCastleButton;
  ButtonOpenInBrowser := DesignedComponent('ButtonOpenInBrowser') as TCastleButton;

  { Connect our TCastleDBEdit instances to TDataSource from data module.
    For now, we cannot do this visually when editing main.castle-user-interface
    in CGE editor. }
  EditFirstName.DataSource := DbModule.DataSourceSample;
  EditLastName.DataSource := DbModule.DataSourceSample;
  EditWww.DataSource := DbModule.DataSourceSample;

  { Connect OnXxx events to controls. }
  ButtonNext.OnClick := @ClickNext;
  ButtonPrevious.OnClick := @ClickPrevious;
  ButtonNew.OnClick := @ClickNew;
  ButtonDelete.OnClick := @ClickDelete;
  ButtonOpenInBrowser.OnClick := @ClickOpenInBrowser;
  EditFirstName.OnChange := @SomeEditChanged;
  EditLastName.OnChange := @SomeEditChanged;
  EditWww.OnChange := @SomeEditChanged;

  { Open dataset }
  DbModule.DataSetSample.FilePath := URIToFilenameSafe('castle-data:/');
  DbModule.DataSetSample.Open;
  UpdateRecordInfo;
end;

procedure TStateMain.Stop;
begin
  { It is not really necessary to call this explicitly,
    the DataSetSample will automatically close before it's destroyed in case
    of this simple application. }
  DbModule.DataSetSample.Close;

  inherited;
end;

procedure TStateMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  { This virtual method is executed every frame.}
end;

procedure TStateMain.ClickNext(Sender: TObject);
begin
  DbModule.DataSetSample.Next;
  UpdateRecordInfo;
end;

procedure TStateMain.ClickPrevious(Sender: TObject);
begin
  DbModule.DataSetSample.Prior;
  UpdateRecordInfo;
end;

procedure TStateMain.ClickNew(Sender: TObject);
begin
  DbModule.DataSetSample.Insert;
  DbModule.DataSetSample.ClearFields;
  DbModule.DataSetSampleFIRSTNAME.Value := 'New First Name';
  DbModule.DataSetSampleLASTNAME.Value := 'New Last Name';
  DbModule.DataSetSampleWWW.Value := 'http://example.com/';
  UpdateRecordInfo;
end;

procedure TStateMain.ClickDelete(Sender: TObject);
begin
  DbModule.DataSetSample.Delete;
  UpdateRecordInfo;
end;

procedure TStateMain.UpdateRecordInfo;
begin
  LabelRecordInfo.Caption := Format('Record %d / %d, DataSet Modified: %s, DataSet State: %s', [
    DbModule.DataSetSample.RecNo,
    DbModule.DataSetSample.RecordCount,
    BoolToStr(DbModule.DataSetSample.Modified, true),
    GetEnumName(TypeInfo(TDataSetState), Ord(DbModule.DataSetSample.State))
  ]);
end;

procedure TStateMain.ClickOpenInBrowser(Sender: TObject);
begin
  OpenURL(DbModule.DataSetSampleWWW.Value);
end;

procedure TStateMain.SomeEditChanged(Sender: TObject);
begin
  UpdateRecordInfo; // show that Modified changed to true
end;

end.
