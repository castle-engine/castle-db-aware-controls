unit FormMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SdfData, db, FileUtil, Forms, Controls, Graphics, Dialogs,
  DbCtrls, StdCtrls;

type
  TMainForm = class(TForm)
    ButtonDelete: TButton;
    ButtonNext: TButton;
    ButtonPrevious: TButton;
    ButtonNew: TButton;
    DataSetMonstersDescription: TStringField;
    DataSetMonstersHitPoints: TStringField;
    DataSetMonstersName: TStringField;
    DataSourceMonsters: TDataSource;
    DBEditName: TDBEdit;
    DBEditDescription: TDBEdit;
    DBEditHitPoints: TDBEdit;
    LabelTitle: TLabel;
    LabelName: TLabel;
    LabelDescription: TLabel;
    LabelHitPoints: TLabel;
    DataSetMonsters: TSdfDataSet;
    LabelRecordInfo: TLabel;
    procedure ButtonDeleteClick(Sender: TObject);
    procedure ButtonNewClick(Sender: TObject);
    procedure ButtonNextClick(Sender: TObject);
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

uses TypInfo;

{$R *.lfm}

procedure TMainForm.ButtonNextClick(Sender: TObject);
begin
  DataSetMonsters.Next;
  UpdateRecordInfo;
end;

procedure TMainForm.ButtonNewClick(Sender: TObject);
begin
  DataSetMonsters.Insert;
  DataSetMonstersName.Value := 'New Monster Name';
  DataSetMonstersDescription.Value := 'New Monster Description';
  DataSetMonstersHitPoints.Value := '100';
  UpdateRecordInfo;
end;

procedure TMainForm.ButtonDeleteClick(Sender: TObject);
begin
  DataSetMonsters.Delete;
  UpdateRecordInfo;
end;

procedure TMainForm.ButtonPreviousClick(Sender: TObject);
begin
  DataSetMonsters.Prior;
  UpdateRecordInfo;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  DataSetMonsters.Open;
  UpdateRecordInfo;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  { It is not really necessary to call this explicitly,
    the dataset will automatically close before it's destroyed in case
    of this simple application. }
  DataSetMonsters.Close;
end;

procedure TMainForm.UpdateRecordInfo;
begin
  LabelRecordInfo.Caption := Format('Record %d / %d, DataSet Modified: %s, DataSet State: %s', [
    DataSetMonsters.RecNo,
    DataSetMonsters.RecordCount,
    BoolToStr(DataSetMonsters.Modified, true),
    GetEnumName(TypeInfo(TDataSetState), Ord(DataSetMonsters.State))
  ]);
end;

end.

