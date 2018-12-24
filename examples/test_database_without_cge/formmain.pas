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

{ Main form of the LCL example, showing off database-controls in LCL
  (without CGE). }
unit FormMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, dbf, FileUtil, Forms, Controls, Graphics,
  Dialogs, DbCtrls, StdCtrls;

type

  { TMainForm }

  TMainForm = class(TForm)
    ButtonOpenInBrowser: TButton;
    ButtonDelete: TButton;
    ButtonNext: TButton;
    ButtonPrevious: TButton;
    ButtonNew: TButton;
    DataSetSampleCOUNTRY: TStringField;
    DataSetSampleEMAIL: TStringField;
    DataSetSampleFAX: TStringField;
    DataSetSampleFIRSTNAME: TStringField;
    DataSetSampleLASTNAME: TStringField;
    DataSetSampleMOBILE: TStringField;
    DataSetSampleSTREET: TStringField;
    DataSetSampleTELEPHONE: TStringField;
    DataSetSampleTOWN: TStringField;
    DataSetSampleWWW: TStringField;
    DataSetSampleZIP: TStringField;
    DataSourceSample: TDataSource;
    EditFirstName: TDBEdit;
    EditLastName: TDBEdit;
    EditWww: TDBEdit;
    DataSetSample: TDbf;
    LabelTitle: TLabel;
    LabelFirstName: TLabel;
    LabelLastName: TLabel;
    LabelWww: TLabel;
    LabelRecordInfo: TLabel;
    procedure ButtonDeleteClick(Sender: TObject);
    procedure ButtonNewClick(Sender: TObject);
    procedure ButtonNextClick(Sender: TObject);
    procedure ButtonOpenInBrowserClick(Sender: TObject);
    procedure ButtonPreviousClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    procedure UpdateRecordInfo;
  public
  end;

var
  MainForm: TMainForm;

implementation

uses TypInfo, LCLIntf;

{$R *.lfm}

procedure TMainForm.ButtonNextClick(Sender: TObject);
begin
  DataSetSample.Next;
  UpdateRecordInfo;
end;

procedure TMainForm.ButtonOpenInBrowserClick(Sender: TObject);
begin
  OpenURL(DataSetSampleWWW.Value);
end;

procedure TMainForm.ButtonNewClick(Sender: TObject);
begin
  DataSetSample.Insert;
  DataSetSample.ClearFields;
  DataSetSampleFIRSTNAME.Value := 'New First Name';
  DataSetSampleLASTNAME.Value := 'New Last Name';
  DataSetSampleWWW.Value := 'http://example.com/';
  UpdateRecordInfo;
end;

procedure TMainForm.ButtonDeleteClick(Sender: TObject);
begin
  DataSetSample.Delete;
  UpdateRecordInfo;
end;

procedure TMainForm.ButtonPreviousClick(Sender: TObject);
begin
  DataSetSample.Prior;
  UpdateRecordInfo;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  DataSetSample.Open;
  UpdateRecordInfo;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  { It is not really necessary to call this explicitly,
    the DataSetSample will automatically close before it's destroyed in case
    of this simple application. }
  DataSetSample.Close;
end;

procedure TMainForm.UpdateRecordInfo;
begin
  LabelRecordInfo.Caption := Format('Record %d / %d, DataSet Modified: %s, DataSet State: %s', [
    DataSetSample.RecNo,
    DataSetSample.RecordCount,
    BoolToStr(DataSetSample.Modified, true),
    GetEnumName(TypeInfo(TDataSetState), Ord(DataSetSample.State))
  ]);
end;

end.
