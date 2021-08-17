unit DataModuleSampleDatabase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, Dbf;

type

  { TDbModule }

  TDbModule = class(TDataModule)
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
    DataSetSample: TDbf;
  private

  public

  end;

var
  DbModule: TDbModule;

implementation

initialization

{$R *.lfm}

end.

