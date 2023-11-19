{
Copyright (c) 2012-2013, Loginov Dmitry Sergeevich
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
}

{ *************************************************************************** }
{                                                                             }
{                                                                             }
{                                                                             }
{ Модуль fbUtilsDBStruct - модуль описания структуры БД                       }
{ (c) 2012 Логинов Дмитрий Сергеевич                                          }
{ Последнее обновление: 09.05.2012                                            }
{ Протестировано на D7, D2007, D2010, D-XE2                                   }
{ Адрес сайта: http://loginovprojects.ru/                                     }
{ e-mail: loginov_d@inbox.ru                                                  }
{                                                                             }
{ *************************************************************************** }

{
Модель позволяет описать лишь некоторые объекты базы данных. Его целесообразно
использовать при разработке коробочных приложений, где структура базы данных
зачастую относительно простая.
}

{$IFDEF FPC}
{$MODE DELPHI}{$H+}{$CODEPAGE UTF8}
{$ENDIF}

unit fbUtilsDBStruct;
    
interface

uses
  SysUtils, Classes, fbTypes, fbSomeFuncs;

type
  {Описание поля}
  TfbFieldDesc = class
    { Имя поля. Используется для поиска. Если при поиске имя поля в заданной
      таблице не найдено, то поле создается }
    FName: string;

    { Тип / домен поля }
    FType: string;

    { Значение по умолчанию. Если поле не должно иметь значение по
      умолчанию, указывайте пустую строку }
    FDefault: string;

    { Укажите True, если значение обязательно }
    FNotNull: TFBNotNull;
  end;

  {Информация о домене для поля в БД на данный момент}
  TfbFieldDomain = class
    DomainName: string; // Доменное имя поля, например "RDB$639"
    CurLength: Integer; // Текущая длина поля
  end;

  {Описание домена}
  TfbDomainDesc = class
    FName: string;         // Имя домена
    FType: string;         // Описание домена
    FDefault: string;      // Значение по умолчанию
    FNotNull: TFBNotNull;  // NOT NULL
    FCheck: string;        // Выражение проверки CHECK
  end;

  {Описание первичного ключа}
  TfbPrimaryKeyDesc = record
    FName: string;

    { Перечень полей, входящих в первичный ключ }
    FConstraintFields: string;
  end;

  {Описание внешнего ключа}
  TfbForeignKeyDesc = class
    { Наименование внешнего ключа }
    FName: string;

    { Имя таблицы, в которой данный ключ создается }
    FTableName: string;

    { Перечень полей, входящих во внешний ключ }
    FConstraintFields: string;

    { Имя таблицы, на которую ключ ссылается }
    FRefTableName: string;

    { Имена полей таблицы FRefTableName, на которые ссылаюттся поля FConstraintFields}
    FRefConstraintFields: string;
  end;

  {Описание индекса}
  TfbIndexDesc = class
    { Наименование индекса }
    FName: string;

    { Значения должны быть уникальными }
    FIsUnique: Boolean;

    { Порядок сортировки }
    FSorting: TFBSorting;

    { Перечень полей, входящих в индекс }
    FConstraintFields: string;
  end;

  {Описание триггера}
  TfbTriggerDesc = class
    { Наименование триггера }
    FName: string;

    { Имя таблицы }
    FTableName: string;

    { Момент срабатывания триггера }
    FEventTime: TFBTriggerEventTime;

    { События, на которые должен реагировать триггер }
    FEvents: TFBTriggerEvents;

    { Нужно ли делать триггер активным  }
    FState: TFBTriggerState;

    { Порядок срабатывания триггера }
    FPos: Integer;

    { Локальные переменные триггера }
    FVarDesc: string;

    { Исходный код (тело триггера) }
    FBody: string;

    { Хэш исходного текста триггера (из базы получить исходный текст - это проблема) }
    FHash: Cardinal;
  end;

  {Описание хранимой процедуры}
  TfbProcedureDesc = class
    { Наименование процедуры }
    FName: string;

    {Описание входных полей}
    FInFieldsDesc: string;

    {Описание выходных полей}
    FOutFieldsDesc: string;

    {Описание переменных}
    FVarDesc: string;

    {Тело процедуры}
    FBody: string;

    { Хэш исходного текста процедуры }
    FHash: Cardinal;
  end;

  {Описание таблицы}
  TfbTableDesc = class
  private
    FFieldList: TList;
    FPrimaryKey: TfbPrimaryKeyDesc; // Первичный ключ таблицы
    FName: string;
    FIndexList: TList;
    FTriggerList: TList;
    FChecksList: TStringList;
    FUseModifyDate: TFBTriggerState; // Использовать или нет поле MODIFYDATE
    FDBDesc: TObject; // Ссылка на объект TfbDataBaseDesc
    procedure Clear;
  public
    constructor Create;
    destructor Destroy; override;
    property TableName: string read FName;
    property FieldList: TList read FFieldList;
    property IndexList: TList read FIndexList;
    property ChecksList: TStringList read FChecksList;
    property PrimaryKey: TfbPrimaryKeyDesc read FPrimaryKey;
    property TriggerList: TList read FTriggerList;

  private {Виртуальные функции}

    procedure SetUseModifyDate(Value: TFBTriggerState); virtual;
    function GetUseModifyDate: TFBTriggerState; virtual;
  public {Виртуальные функции}

    { Добавляет описание поля базы данных }
    procedure AddField(AName, AType, ADefault: string; NotNull: TFBNotNull); virtual;

    { Устанавливает первичный ключ таблицы }
    procedure SetPrimaryKey(AName, ConstraintFields: string); virtual;

    { Добавляет индекс}
    procedure AddIndex(AName: string; IsUnique: Boolean;
      ASorting: TFBSorting; ConstraintFields: string); virtual;

    { Добавляет выражение проверки для таблицы }
    procedure AddCheck(AName: string; ACheck: string); virtual;

    { Добавляет триггер. В качестве TriggerName можно задавать пустую строку. В
      этом случае имя триггера будет сформировано автоматически. В качестве TriggerPos
      (порядок срабатывания триггера) рекомендуется значение > 0 }
    procedure AddTrigger(TriggerEventTime: TFBTriggerEventTime; TriggerEvents: TFBTriggerEvents;
      TriggerPos: Integer; TriggerState: TFBTriggerState; TriggerName: string;
      TriggerVarDesc, TriggerBody: string); virtual;

    { Добавляет триггер для поддержки автоинкремента. Если CreateGenerator=TRUE,
      то создает генератор с именем AGenName автоматически }
    procedure AddAutoIncTrigger(ATriggerName, AFieldName, AGenName: string; CreateGenerator: Boolean); virtual;

    { Определяет, следует ли использовать MODIFYDATE }
    property UseModifyDateTrigger: TFBTriggerState read GetUseModifyDate write SetUseModifyDate;
  end;

  {Описание базы данных}
  TfbDataBaseDesc = class
  private
    procedure Clear;
  public

    FTableList: TList;           // Список таблиц
    FDomainList: TList;          // Список доменов
    FForeignKeyList: TList;      // Список внешних ключей
    FGeneratorList: TStringList; // Список генераторов
    FProcedureList: TList;       // Список хранимых процедур
    FAddErrException: Boolean;   // Определяет, нужно ли добавлять объект исключения

    constructor Create;
    destructor Destroy; override;

  public {Виртуальные функции}

    { Номер версии }
    function GetVersion: Integer; virtual;

    { Добавляет описание домена }
    procedure AddDomain(AName, AType, ADefault: string; NotNull: TFBNotNull; ACheck: string); virtual;

    { Добавляет описание таблицы }
    function AddTable(TableName: string): TfbTableDesc; virtual;

    { Добавляет внешний ключ }
    procedure AddForeignKey(AName, TableName, ConstraintFields,
      RefTableName, RefConstraintFields: string); virtual;

    { Добавляет счетчик генератора }
    procedure AddGenerator(AName: string; StartValue: Int64); virtual;

    { Добавляет хранимую процедуру }
    procedure AddProcedure(AName, InFieldsDesc, OutFieldsDesc, VarDesc, Body: string); virtual;

    { Добавляет объект исключения "ERR" }
    procedure AddDefaultException; virtual;
  end;


function FBCreateDataBaseDesc: TfbDataBaseDesc;
procedure FBFreeDataBaseDesc(dbDesc: TfbDataBaseDesc);

{$IFDEF FBUTILSDLL} // Замечания по директиве смотрите в модуле fbUtilsBase.pas
exports
  FBCreateDataBaseDesc name 'ibxFBCreateDataBaseDesc',
  FBFreeDataBaseDesc name 'ibxFBFreeDataBaseDesc';
{$ENDIF}

implementation


function FBCreateDataBaseDesc: TfbDataBaseDesc;
begin
  Result := TfbDataBaseDesc.Create;
end;

procedure FBFreeDataBaseDesc(dbDesc: TfbDataBaseDesc);
begin
  dbDesc.Free;
end;

{ TfbTable }

procedure TfbTableDesc.AddField(AName, AType, ADefault: string; NotNull: TFBNotNull);
var
  AField: TfbFieldDesc;
begin
  AField := TfbFieldDesc.Create;
  AField.FName := AnsiUpperCase(AName);
  AField.FType := AType;
  AField.FDefault := ADefault;

  AField.FNotNull := NotNull;

  FFieldList.Add(AField);
end;

procedure TfbTableDesc.AddIndex(AName: string; IsUnique: Boolean;
  ASorting: TFBSorting; ConstraintFields: string);
var
  Ind: TfbIndexDesc;
begin
  Ind := TfbIndexDesc.Create;
  Ind.FName := AnsiUpperCase(AName);
  Ind.FIsUnique := IsUnique;
  Ind.FSorting := ASorting;
  Ind.FConstraintFields := AnsiUpperCase(ConstraintFields);

  FIndexList.Add(Ind);
end;

procedure TfbTableDesc.AddTrigger(TriggerEventTime: TFBTriggerEventTime;
  TriggerEvents: TFBTriggerEvents; TriggerPos: Integer;
  TriggerState: TFBTriggerState; TriggerName: string; TriggerVarDesc, TriggerBody: string);
var
  Trig: TfbTriggerDesc;
  ActStr, S: string;
begin
  if TriggerEventTime = trBefore then
    ActStr := 'B'
  else
    ActStr := 'A';

  if trInsert in TriggerEvents then
    ActStr := ActStr + 'I';
  if trUpdate in TriggerEvents then
    ActStr := ActStr + 'U';
  if trDelete in TriggerEvents then
    ActStr := ActStr + 'D';
  ActStr := ActStr + IntToStr(TriggerPos);

  // При необходимости формируем TriggerName
  if TriggerName = '' then
    TriggerName := FName + '_' + ActStr;

  if TriggerState = trsActive then // Требуется для вычисления хэша
    S := '_A'
  else
    S := '_I';

  Trig := TfbTriggerDesc.Create;
  try
    Trig.FName := AnsiUpperCase(TriggerName);
    Trig.FTableName := FName;
    Trig.FEventTime := TriggerEventTime;
    Trig.FEvents := TriggerEvents;
    Trig.FState := TriggerState;
    Trig.FPos := TriggerPos;
    Trig.FVarDesc := TriggerVarDesc;
    Trig.FBody := TriggerBody;
    Trig.FHash := GenerateStringHashLY(ActStr + S + '_' + TriggerVarDesc + '_' + TriggerBody);
  except
    Trig.Free;
    raise;
  end;

  FTriggerList.Add(Trig);

end;

procedure TfbTableDesc.AddAutoIncTrigger(ATriggerName, AFieldName, AGenName: string; CreateGenerator: Boolean);
begin
  AFieldName := AnsiUpperCase(AFieldName);
  AGenName := AnsiUpperCase(AGenName);

  if CreateGenerator then
    TfbDataBaseDesc(FDBDesc).AddGenerator(AGenName, 0);

  if ATriggerName = '' then
    ATriggerName := FName + '_AUTOINC';

  AddTrigger(trBefore, [trInsert], 0, trsActive,
    ATriggerName,
    '', // Локальные переменные - отсутствуют
    Format('IF (NEW."%0:s" IS NULL) THEN NEW."%0:s"=GEN_ID("%1:s", 1);',
      [AFieldName, AGenName]));
end;

procedure TfbTableDesc.AddCheck(AName: string; ACheck: string);
begin
  FChecksList.Values[AnsiUpperCase(AName)] := ACheck;
end;


procedure TfbTableDesc.Clear;
var
  I: Integer;
begin
  for I := 0 to FFieldList.Count - 1 do
    TfbFieldDesc(FFieldList[I]).Free;
  FFieldList.Clear;

  for I := 0 to FIndexList.Count - 1 do
    TfbIndexDesc(FIndexList[I]).Free;
  FIndexList.Clear;

  for I := 0 to FTriggerList.Count - 1 do
    TfbTriggerDesc(FTriggerList[I]).Free;
  FTriggerList.Clear;
end;

constructor TfbTableDesc.Create;
begin
  inherited;
  FFieldList := TList.Create;
  FIndexList := TList.Create;
  FTriggerList := TList.Create;
  FChecksList := TStringList.Create;
end;

destructor TfbTableDesc.Destroy;
begin
  Clear;
  FFieldList.Free;
  FIndexList.Free;
  FTriggerList.Free;
  FChecksList.Free;
  inherited;
end;

function TfbTableDesc.GetUseModifyDate: TFBTriggerState;
begin
  Result := FUseModifyDate;
end;

procedure TfbTableDesc.SetPrimaryKey(AName, ConstraintFields: string);
begin
  FPrimaryKey.FName := AnsiUpperCase(AName);
  FPrimaryKey.FConstraintFields := AnsiUpperCase(ConstraintFields);
end;

procedure TfbTableDesc.SetUseModifyDate(Value: TFBTriggerState);
const
  TrCode =
    '  IF ((NEW.MODIFYDATE IS NULL) AND (OLD.MODIFYDATE IS NOT NULL)) THEN EXIT;'#13#10+ // Позволяет возвращать значение в NULL
    '  IF ((OLD.MODIFYDATE IS NULL) AND (NEW.MODIFYDATE IS NOT NULL)) THEN EXIT;'#13#10+ // Позволяем устанавливать произвольное значение (только в первый раз)
    '  NEW.MODIFYDATE = CURRENT_TIMESTAMP;'; // Присваиваем текущий момент времени
begin
  FUseModifyDate := Value;

  if FUseModifyDate <> trsNone then
  begin
    // Добавляем поле MODIFYDATE
    AddField('MODIFYDATE', 'TIMESTAMP', '', CanNull);

    // Добавляем индекс
    AddIndex(FName + '_MDIDX', False, Ascending, '"MODIFYDATE"');

    // Добавляем триггер
    AddTrigger(trBefore, [trInsert, trUpdate], 10, Value, FName + '_MDBIU', '', TrCode);
  end;
end;

{ TfbDataBase }

procedure TfbDataBaseDesc.AddDomain(AName, AType, ADefault: string;
  NotNull: TFBNotNull; ACheck: string);
var
  ADomain: TfbDomainDesc;
begin
  ADomain := TfbDomainDesc.Create;

  ADomain.FName := AnsiUpperCase(AName);
  ADomain.FType := AType;
  ADomain.FDefault := ADefault;

  ADomain.FNotNull := NotNull;
  ADomain.FCheck := ACheck;

  FDomainList.Add(ADomain);
end;

procedure TfbDataBaseDesc.AddDefaultException;
begin
  FAddErrException := True;
end;

procedure TfbDataBaseDesc.AddForeignKey(AName, TableName, ConstraintFields,
  RefTableName, RefConstraintFields: string);
var
  AForeignKey: TfbForeignKeyDesc;
begin
  AForeignKey := TfbForeignKeyDesc.Create;
  AForeignKey.FName := AnsiUpperCase(AName);
  AForeignKey.FTableName := AnsiUpperCase(TableName);
  AForeignKey.FConstraintFields := AnsiUpperCase(ConstraintFields);
  AForeignKey.FRefTableName := AnsiUpperCase(RefTableName);
  AForeignKey.FRefConstraintFields := AnsiUpperCase(RefConstraintFields);

  FForeignKeyList.Add(AForeignKey);
end;

procedure TfbDataBaseDesc.AddGenerator(AName: string; StartValue: Int64);
begin
  FGeneratorList.Values[AnsiUpperCase(AName)] := IntToStr(StartValue);
end;

procedure TfbDataBaseDesc.AddProcedure(AName, InFieldsDesc, OutFieldsDesc, VarDesc, Body: string);
var
  AProc: TfbProcedureDesc;
begin
  AName := AnsiUpperCase(AName);

  AProc := TfbProcedureDesc.Create;
  AProc.FName := AName;
  AProc.FInFieldsDesc := InFieldsDesc;
  AProc.FOutFieldsDesc := OutFieldsDesc;
  AProc.FVarDesc := VarDesc;
  AProc.FBody := Body;
  AProc.FHash := GenerateStringHashLY(AName + '_' + InFieldsDesc + '_' +
    OutFieldsDesc + '_' + VarDesc + '_' + Body);

  FProcedureList.Add(AProc);
end;

function TfbDataBaseDesc.AddTable(TableName: string): TfbTableDesc;
begin
  Result := TfbTableDesc.Create;
  Result.FName := AnsiUpperCase(TableName);
  Result.FDBDesc := Self;

  FTableList.Add(Result);
end;

procedure TfbDataBaseDesc.Clear;
var
  I: Integer;
begin
  for I := 0 to FTableList.Count - 1 do
    TfbTableDesc(FTableList[I]).Free;
  FTableList.Clear;

  for I := 0 to FDomainList.Count - 1 do
    TObject(FDomainList[I]).Free;
  FDomainList.Clear;

  for I := 0 to FForeignKeyList.Count - 1 do
    TObject(FForeignKeyList[I]).Free;
  FForeignKeyList.Clear;

  for I := 0 to FProcedureList.Count - 1 do
    TObject(FProcedureList[I]).Free;
  FProcedureList.Clear;
end;

constructor TfbDataBaseDesc.Create;
begin
  inherited;
  FTableList := TList.Create;
  FDomainList := TList.Create;
  FForeignKeyList := TList.Create;
  FGeneratorList := TStringList.Create;
  FProcedureList := TList.Create;
end;

destructor TfbDataBaseDesc.Destroy;
begin
  Clear;
  FTableList.Free;
  FDomainList.Free;
  FForeignKeyList.Free;
  FGeneratorList.Free;
  FProcedureList.Free;
  inherited;
end;

function TfbDataBaseDesc.GetVersion: Integer;
begin
  Result := 1;
end;

end.
