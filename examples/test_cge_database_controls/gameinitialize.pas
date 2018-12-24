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

{ Application initialization and logic. }
unit GameInitialize;

interface

implementation

uses SysUtils, Classes, TypInfo, DB,
  CastleWindow, CastleScene, CastleControls, CastleLog, CastleOpenDocument,
  CastleFilesUtils, CastleSceneCore, CastleKeysMouse, CastleColors,
  CastleUIControls, CastleApplicationProperties, CastleComponentSerialize,
  CastleDBControls,
  DataModuleSampleDatabase;

var
  Window: TCastleWindowCustom;

type
  { Class to handle application events. }

  { TEventsHandler }

  TEventsHandler = class(TComponent)
  public
    LabelRecordInfo: TCastleLabel;

    { One-time initialization of resources. }
    procedure Initialize(Sender: TObject);

    { Update LabelRecordInfo.Caption. }
    procedure UpdateRecordInfo;

    procedure ClickNext(Sender: TObject);
    procedure ClickPrevious(Sender: TObject);
    procedure ClickNew(Sender: TObject);
    procedure ClickDelete(Sender: TObject);
    procedure ClickOpenInBrowser(Sender: TObject);
  end;

var
  EventsHandler: TEventsHandler;

{ TEventsHandler methods ---------------------------------------------------- }

procedure TEventsHandler.Initialize(Sender: TObject);
var
  Ui: TCastleUserInterface;
  EditFirstName, EditLastName, EditWww: TCastleDBEdit;
  ButtonNext, ButtonPrevious, ButtonNew, ButtonDelete,
    ButtonOpenInBrowser: TCastleButton;
begin
  DbModule := TDbModule.Create(Application);

  { Adjust container settings,
    e.g. for a scalable UI (adjusts to any window size in a smart way). }
  Window.Container.LoadSettings('castle-data:/CastleSettings.xml');

  { Load designed user interface }
  Ui := UserInterfaceLoad('castle-data:/main.castle-user-interface', Window);
  Window.Controls.InsertFront(Ui);

  { Find necessary controls from main.castle-user-interface }
  EditFirstName := Window.FindRequiredComponent('EditFirstName') as TCastleDBEdit;
  EditLastName := Window.FindRequiredComponent('EditLastName') as TCastleDBEdit;
  EditWww := Window.FindRequiredComponent('EditWww') as TCastleDBEdit;
  ButtonNext := Window.FindRequiredComponent('ButtonNext') as TCastleButton;
  ButtonPrevious := Window.FindRequiredComponent('ButtonPrevious') as TCastleButton;
  ButtonNew := Window.FindRequiredComponent('ButtonNew') as TCastleButton;
  ButtonDelete := Window.FindRequiredComponent('ButtonDelete') as TCastleButton;
  ButtonOpenInBrowser := Window.FindRequiredComponent('ButtonOpenInBrowser') as TCastleButton;
  LabelRecordInfo := Window.FindRequiredComponent('LabelRecordInfo') as TCastleLabel;

  { Connect our TCastleDBEdit instances to TDataSource from data module.
    For now, we cannot do this visually when editing main.castle-user-interface
    in CGE editor. }
  EditFirstName.DataSource := DbModule.DataSourceSample;
  EditLastName.DataSource := DbModule.DataSourceSample;
  EditWww.DataSource := DbModule.DataSourceSample;

  { Connect OnClick events to our buttons. }
  ButtonNext.OnClick := @ClickNext;
  ButtonPrevious.OnClick := @ClickPrevious;
  ButtonNew.OnClick := @ClickNew;
  ButtonDelete.OnClick := @ClickDelete;
  ButtonOpenInBrowser.OnClick := @ClickOpenInBrowser;

  { Open dataset }
  DbModule.DataSetSample.Open;
  UpdateRecordInfo;
end;

procedure TEventsHandler.ClickNext(Sender: TObject);
begin
  DbModule.DataSetSample.Next;
  UpdateRecordInfo;
end;

procedure TEventsHandler.ClickPrevious(Sender: TObject);
begin
  DbModule.DataSetSample.Prior;
  UpdateRecordInfo;
end;

procedure TEventsHandler.ClickNew(Sender: TObject);
begin
  DbModule.DataSetSample.Insert;
  DbModule.DataSetSample.ClearFields;
  DbModule.DataSetSampleFIRSTNAME.Value := 'New First Name';
  DbModule.DataSetSampleLASTNAME.Value := 'New Last Name';
  DbModule.DataSetSampleWWW.Value := 'http://example.com/';
  UpdateRecordInfo;
end;

procedure TEventsHandler.ClickDelete(Sender: TObject);
begin
  DbModule.DataSetSample.Delete;
  UpdateRecordInfo;
end;

procedure TEventsHandler.UpdateRecordInfo;
begin
  LabelRecordInfo.Caption := Format('Record %d / %d, DataSet Modified: %s, DataSet State: %s', [
    DbModule.DataSetSample.RecNo,
    DbModule.DataSetSample.RecordCount,
    BoolToStr(DbModule.DataSetSample.Modified, true),
    GetEnumName(TypeInfo(TDataSetState), Ord(DbModule.DataSetSample.State))
  ]);
end;

procedure TEventsHandler.ClickOpenInBrowser(Sender: TObject);
begin
  OpenURL(DbModule.DataSetSampleWWW.Value);
end;

initialization
  { Set ApplicationName early, as our log uses it.
    Optionally you could also set ApplicationProperties.Version here. }
  ApplicationProperties.ApplicationName := 'test_cge_database_controls';

  { Start logging. Do this as early as possible,
    to log information and eventual warnings during initialization. }
  InitializeLog;

  { Initialize Application.OnInitialize. }
  EventsHandler := TEventsHandler.Create(Application);
  Application.OnInitializeEvent := @EventsHandler.Initialize;

  { Create and assign Application.MainWindow. }
  Window := TCastleWindowCustom.Create(Application);
  Application.MainWindow := Window;
end.
