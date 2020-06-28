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
{ Модуль fbUtilsIniFiles - содержит функции, позволяющие работать с базой     }
{ данных так же, как и с INI-файлом                                           }
{ (c) 2012 Логинов Дмитрий Сергеевич                                          }
{ Последнее обновление: 30.04.2012                                            }
{ Протестировано на D7, D2007, D2010, D-XE2                                   }
{ Адрес сайта: http://loginovprojects.ru/                                     }
{ e-mail: loginov_d@inbox.ru                                                  }
{                                                                             }
{ *************************************************************************** }

{
Данный модуль является достаточно автономным, однако рекомендуется с ним работать
через интерфейсы, реализованные в модуле fbUtils.pas

Перед использованием объекта TFBIniFile Вам необходимо создать в базе данных
следующую таблицу:

      CREATE TABLE CONFIGPARAMS(
        FILENAME     VARCHAR(100) NOT NULL, -- Наименование INI-файла (по умолчанию: CONF)
        COMPUTERNAME VARCHAR(100) NOT NULL, -- Наименование компьютера. * - любой компьютер
        USERNAME     VARCHAR(100) NOT NULL, -- Имя пользователя. * - ВСЕ пользователи.
        SECTIONNAME  VARCHAR(100) NOT NULL, -- Наименование секции
        PARAMNAME    VARCHAR(100) NOT NULL, -- Имя параметра
        PARAMVALUE   VARCHAR(10000),        -- Строковое значение параметра
        PARAMBLOB    BLOB,                  -- Для хранения больших двоичных объектов (в том числе текстовых документов)
        PARAMBLOBHASH VARCHAR(50),          -- Для хранения хэша двоичного объекта
        MODIFYDATE   TIMESTAMP,             -- Дата и время изменения записи
        MODIFYUSER   VARCHAR(100),          -- Имя пользователя, который внес изменения
        PRIMARY KEY (FILENAME, COMPUTERNAME, USERNAME, SECTIONNAME, PARAMNAME)) -- Первичный ключ (5 полей)

Имена полей могут быть другими, но при этом Вам придется исправлять модуль fbTypes.
Длину полей Вы можете взять на свое усмотрение. Здесь даны рекомендуемые значения.
Поле PARAMBLOB должно иметь  SUB_TYPE 0 (Binary) (иначе, если там Text, то функции
чтения работают неправильно, добавляют лишние байты).
При желании Вы можете сделать целочисленный автоинкрементный первичный ключ, а указанные
здесь (в PRIMARY KEY) поля оформить в виде уникального индекса
}

unit fbUtilsIniFiles;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, SysUtils, Classes, IBDatabase, IBCustomDataSet, SyncObjs, DB, Math,
  fbUtilsBase, fbUtilsPool, fbSomeFuncs, fbTypes;

type
  {ВНИМАНИЕ! Не меняйте виртуальные функции местами! Они должы располагаться в
   той же последовательности, как и в fbUtils.pas}
  TFBIniFile = class(TObject)
  private
    FAlwaysConnected: Boolean;// Определяет, должно ли подключение быть установленно на протяжении всей работы программы
    FPoolProfileName: string; // Имя профиля из пула подключений
    FConnParams: TFBConnectionParams; // Параметры подключения к Firebird
    FComputerName: string;    // Имя компьютера (по умолчанию - текущий)
    FUserName: string;        // Имя пользователя (по умолчанию - текущий пользователь Windows)
    FFileName: string;        // Имя файла (по аналогии с INI-файлами)

    FDB: TIBDatabase;         // Подключение к БД (из пула)
    FTranW: TIBTransaction;   // Транзакция на запись
    FTranR: TIBTransaction;   // Транзакция на чтение

    dsRead: TIBDataSet;       // Набор данных на чтение заданного параметна
    dsIns: TIBDataSet;        // Набор данных на вставку нового параметра
    dsUpd: TIBDataSet;        // Набор данных на изменение существующего параметра
    dsCustom: TIBDataSet;     // Набор данных для выполнения прочих операций
    //dsInsOrUpd: TIBDataSet; - не реализована, т.к. данной возможности не было в старых версиях Firebird

    FObjs: TObjHolder;

    FCritSect: TCriticalSection; // Критическая секция для предотвращения одновременного доступа к объекту из разных потоков

    FWasBeginWorkCall: Boolean; // TRUE, если была вызвана функция BeginWork

    procedure CheckParams(const Section, Key: string; CheckKey: Boolean = True);
    function InternalValueExists(AnyComp, AnyUser: Boolean; const Section, Key: string; var Value: string; ReadHash: Boolean): Boolean;
    procedure SetParams(ds: TIBDataSet; AnyComp, AnyUser: Boolean; const Section, Key: string);

    procedure ConnectDB(Internal: Boolean);
    procedure DisconnectDB(Internal: Boolean);

  protected
    function GetUserName: string; virtual;
    procedure SetUserName(const Value: string); virtual;
    function GetComputerName: string; virtual;
    procedure SetComputerName(const Value: string); virtual;
    function GetPoolProfileName: string; virtual;
    procedure SetPoolProfileName(const Value: string); virtual;

    {Основные функции ReadXXX и WriteXXX, работающие с текстовым полем в базе данных}
    procedure DoWriteString(AnyComp, AnyUser: Boolean; const Section, Key, Value: String); virtual;
    procedure DoWriteInteger(AnyComp, AnyUser: Boolean; const Section, Key: string; Value: Integer); virtual;
    procedure DoWriteBool(AnyComp, AnyUser: Boolean; const Section, Key: string; Value: Boolean); virtual;
    procedure DoWriteFloat(AnyComp, AnyUser: Boolean; const Section, Key: string; Value: Double); virtual;
    procedure DoWriteDateTime(AnyComp, AnyUser: Boolean; const Section, Key: string; Value: TDateTime); virtual;
    procedure DoWriteDate(AnyComp, AnyUser: Boolean; const Section, Key: string; Value: TDateTime); virtual;
    procedure DoWriteTime(AnyComp, AnyUser: Boolean; const Section, Key: string; Value: TDateTime); virtual;

    function DoReadString(AnyComp, AnyUser: Boolean; const Section, Key, Default: String): string; virtual;
    function DoReadInteger(AnyComp, AnyUser: Boolean; const Section, Key: string; Default: Integer): Integer; virtual;
    function DoReadBool(AnyComp, AnyUser: Boolean; const Section, Key: string; Default: Boolean): Boolean; virtual;
    function DoReadFloat(AnyComp, AnyUser: Boolean; const Section, Key: string; Default: Double): Double; virtual;
    function DoReadDateTime(AnyComp, AnyUser: Boolean; const Section, Key: string; Default: TDateTime): TDateTime; virtual;
    function DoReadDate(AnyComp, AnyUser: Boolean; const Section, Key: string; Default: TDateTime): TDateTime; virtual;
    function DoReadTime(AnyComp, AnyUser: Boolean; const Section, Key: string; Default: TDateTime): TDateTime; virtual;

    {Функции ReadXXX и WriteXXX, работающие с BLOB-полем в базе данных}
    procedure DoWriteStream(AnyComp, AnyUser: Boolean; const Section, Key: String; AStream: TStream); virtual;
    function DoReadStream(AnyComp, AnyUser: Boolean; const Section, Key: String; AStream: TStream): Boolean; virtual;

    {Записывает заданный текст в BLOB-поле. Текст в BLOB-поле хранится в кодировке UNICODE}
    procedure DoWriteText(AnyComp, AnyUser: Boolean; const Section, Key: String; Value: string); virtual;

    {Считывает текст из BLOB-поля}
    function DoReadText(AnyComp, AnyUser: Boolean; const Section, Key: String; Default: String): string; virtual;

    {Записывает произвольные двоичные данные в BLOB-поле}
    procedure DoWriteBinaryData(AnyComp, AnyUser: Boolean; const Section, Key: string; const Buffer; BufSize: Integer); virtual;

    {Считывает двоичные данные из BLOB-поля}
    function DoReadBinaryData(AnyComp, AnyUser: Boolean; const Section, Key: string; var Buffer; BufSize: Integer): Integer; virtual;

    {Основные функции для работы с секциями}

    {Считывает список наименований параметров}
    procedure DoReadSection(AnyComp, AnyUser: Boolean; const Section: string; Strings: TStrings); virtual;

    {Считывает имена секций}
    procedure DoReadSections(AnyComp, AnyUser: Boolean; Strings: TStrings); virtual;

    {Для указанной секции считывает наименования параметров и их значения}
    procedure DoReadSectionValues(AnyComp, AnyUser: Boolean; const Section: string; Strings: TStrings); virtual;

    {Проверяет, существует ли заданная секция}
    function DoSectionExists(AnyComp, AnyUser: Boolean; const Section: string): Boolean; virtual;

    {Проверяет, существует ли заданный параметр}
    function DoValueExists(AnyComp, AnyUser: Boolean; const Section, Key: string): Boolean; virtual;

    {Удаляет указанную секцию}
    procedure DoEraseSection(AnyComp, AnyUser: Boolean; const Section: string); virtual;

    {Удаляет заданный параметр и его значение}
    procedure DoDeleteKey(AnyComp, AnyUser: Boolean; const Section, Key: String); virtual;
  public

    {Внимание! Для каждой функции предлагается 3 варианта вызова:
     1 - "привычный": указывается имя секции и имя параметра, как и при работе с TIniFile.
         Параметр, записанный таким способом, будет доступен с любого компьютера и
         для любого пользователя. Предназначен для глобальных, общедоступных параметров
     2 - "подробный": позволяет задать дополнительно 2 параметра: AnyComp и AnyUser.
         Если AnyComp (пер. "любой компьютер") равен False, то параметр будет доступен
         ТОЛЬКО с этого же компьютера. Если AnyUser (пер. "любой пользователь") равен False,
         то параметр будет доступен ТОЛЬКО для данного пользователя. При необходимости
         параметры AnyComp и AnyUser можно кобминировать. Например, если AnyComp=True и AnyUser=False,
         то параметр будет доступен для данного пользователя, независимо от того,
         за каким компьютером он работает.
     3 - "упрощенный": аналогичный первому варианту, однако имя секции указывать не
         требуется (используется секция FBIniDefSection (PARAMS))}

    {== ФУНКЦИИ ДЛЯ ЗАПИСИ В INI ==}

    {Запись строки}
    procedure WriteString(const Section, Key, Value: String); overload;
    procedure WriteString(AnyComp, AnyUser: Boolean; const Section, Key, Value: String); overload;
    procedure WriteString(const Key, Value: String); overload;

    {Запись логического значения}
    procedure WriteBool(const Section, Key: string; Value: Boolean); overload;
    procedure WriteBool(AnyComp, AnyUser: Boolean; const Section, Key: string; Value: Boolean); overload;
    procedure WriteBool(const Key: string; Value: Boolean); overload;

    {Запись вещественного числа Double}
    procedure WriteFloat(const Section, Key: string; Value: Double); overload;
    procedure WriteFloat(AnyComp, AnyUser: Boolean; const Section, Key: string; Value: Double); overload;
    procedure WriteFloat(const Key: string; Value: Double); overload;

    {Запись даты и времени}
    procedure WriteDateTime(const Section, Key: string; Value: TDateTime); overload;
    procedure WriteDateTime(AnyComp, AnyUser: Boolean; const Section, Key: string; Value: TDateTime); overload;
    procedure WriteDateTime(const Key: string; Value: TDateTime); overload;

    {Запись целочисленного значения}
    procedure WriteInteger(const Section, Key: string; Value: Integer); overload;
    procedure WriteInteger(AnyComp, AnyUser: Boolean; const Section, Key: string; Value: Integer); overload;
    procedure WriteInteger(const Key: string; Value: Integer); overload;

    {Запись даты}
    procedure WriteDate(const Section, Key: string; Value: TDateTime); overload;
    procedure WriteDate(AnyComp, AnyUser: Boolean; const Section, Key: string; Value: TDateTime); overload;
    procedure WriteDate(Key: string; Value: TDateTime); overload;

    {Запись времени}
    procedure WriteTime(const Section, Key: string; Value: TDateTime); overload;
    procedure WriteTime(AnyComp, AnyUser: Boolean; const Section, Key: string; Value: TDateTime); overload;
    procedure WriteTime(Key: string; Value: TDateTime); overload;

    {== Функции для записи в INI двоичных объектов (в поле BLOB) ==}

    {Запись потока TStream}
    procedure WriteStream(const Section, Key: String; AStream: TStream); overload;
    procedure WriteStream(AnyComp, AnyUser: Boolean; const Section, Key: String; AStream: TStream); overload;
    procedure WriteStream(Key: String; AStream: TStream); overload;

    {Запись текста (любого объема; хранится в формате Unicode)}
    procedure WriteText(const Section, Key: String; Value: string); overload;
    procedure WriteText(AnyComp, AnyUser: Boolean; const Section, Key: String; Value: string); overload;
    procedure WriteText(Key: String; Value: string); overload;

    {Запись произвольного двоичного буфера (например, массива)}
    procedure WriteBinaryData(const Section, Key: string; const Buffer; BufSize: Integer); overload;
    procedure WriteBinaryData(AnyComp, AnyUser: Boolean; const Section, Key: string; const Buffer; BufSize: Integer); overload;
    procedure WriteBinaryData(Key: string; const Buffer; BufSize: Integer); overload;

    {== ФУНКЦИИ ДЛЯ ЗАПИСИ В INI ==}

    {Чтение строки}
    function ReadString(const Section, Key, Default: String): string; overload;
    function ReadString(AnyComp, AnyUser: Boolean; const Section, Key, Default: String): string; overload;
    function ReadString(const Key, Default: String): string; overload;

    {Чтение целочисленного значения}
    function ReadInteger(const Section, Key: string; Default: Integer): Integer; overload;
    function ReadInteger(AnyComp, AnyUser: Boolean; const Section, Key: string; Default: Integer): Integer; overload;
    function ReadInteger(const Key: string; Default: Integer): Integer; overload;

    {Чтение логического значения}
    function ReadBool(const Section, Key: string; Default: Boolean): Boolean; overload;
    function ReadBool(AnyComp, AnyUser: Boolean; const Section, Key: string; Default: Boolean): Boolean; overload;
    function ReadBool(const Key: string; Default: Boolean): Boolean; overload;

    {Чтение вещественного значения (Double)}
    function ReadFloat(const Section, Key: string; Default: Double): Double; overload;
    function ReadFloat(AnyComp, AnyUser: Boolean; const Section, Key: string; Default: Double): Double; overload;
    function ReadFloat(const Key: string; Default: Double): Double; overload;

    {Чтение даты и времени}
    function ReadDateTime(const Section, Key: string; Default: TDateTime): TDateTime; overload;
    function ReadDateTime(AnyComp, AnyUser: Boolean; const Section, Key: string; Default: TDateTime): TDateTime; overload;
    function ReadDateTime(const Key: string; Default: TDateTime): TDateTime; overload;

    {Чтение даты}
    function ReadDate(const Section, Key: string; Default: TDateTime): TDateTime; overload;
    function ReadDate(AnyComp, AnyUser: Boolean; const Section, Key: string; Default: TDateTime): TDateTime; overload;
    function ReadDate(Key: string; Default: TDateTime): TDateTime; overload;

    {Чтение времени}
    function ReadTime(const Section, Key: string; Default: TDateTime): TDateTime; overload;
    function ReadTime(AnyComp, AnyUser: Boolean; const Section, Key: string; Default: TDateTime): TDateTime; overload;
    function ReadTime(Key: string; Default: TDateTime): TDateTime; overload;

    {== Функции для чтения из INI двоичных объектов (из поля BLOB) ==}

    {Чтение в поток TStream}
    function ReadStream(AnyComp, AnyUser: Boolean; const Section, Key: String; AStream: TStream): Boolean; overload;
    function ReadStream(const Section, Key: String; AStream: TStream): Boolean; overload;
    function ReadStream(Key: String; AStream: TStream): Boolean; overload;

    {Чтение текста}
    function ReadText(AnyComp, AnyUser: Boolean; const Section, Key: String; Default: String): string; overload;
    function ReadText(const Section, Key: String; Default: String): string; overload;
    function ReadText(Key: String; Default: String): string; overload;

    {Чтение в двоичный буфер}
    function ReadBinaryData(AnyComp, AnyUser: Boolean; const Section, Key: string; var Buffer; BufSize: Integer): Integer; overload;
    function ReadBinaryData(const Section, Key: string; var Buffer; BufSize: Integer): Integer; overload;
    function ReadBinaryData(Key: string; var Buffer; BufSize: Integer): Integer; overload;


    {== ФУНКЦИИ ДЛЯ РАБОТЫ С СЕКЦИЯМИ ==}

    {==ReadSection - считывает список наименований параметров для заданной секции}
    procedure ReadSection(const Section: string; Strings: TStrings); overload;
    procedure ReadSection(AnyComp, AnyUser: Boolean; const Section: string; Strings: TStrings); overload;
    procedure ReadSection(Strings: TStrings); overload;

    {==ReadSections - считывает наименования секций}
    procedure ReadSections(Strings: TStrings); overload;
    procedure ReadSections(AnyComp, AnyUser: Boolean; Strings: TStrings); overload;

    {==ReadSectionValues - для указанной секции считывает наименования параметров и их значения}
    procedure ReadSectionValues(const Section: string; Strings: TStrings); overload;
    procedure ReadSectionValues(AnyComp, AnyUser: Boolean; const Section: string; Strings: TStrings); overload;
    procedure ReadSectionValues(Strings: TStrings); overload;

    {== SectionExists - Проверяет, существует ли заданная секция}
    function SectionExists(const Section: string): Boolean; overload;
    function SectionExists(AnyComp, AnyUser: Boolean; const Section: string): Boolean; overload;
    function SectionExists: Boolean; overload;

    {== ValueExists - Проверяет, существует ли заданный параметр}
    function ValueExists(const Section, Key: string): Boolean; overload;
    function ValueExists(AnyComp, AnyUser: Boolean; const Section, Key: string): Boolean; overload;
    function ValueExists(Key: string): Boolean; overload;

    {== EraseSection - Удаляет указанную секцию}
    procedure EraseSection(const Section: string); overload;
    procedure EraseSection(AnyComp, AnyUser: Boolean; const Section: string); overload;
    procedure EraseSection; overload;

    {== DeleteKey - Удаляет заданный параметр и его значение}
    procedure DeleteKey(const Section, Key: String); overload;
    procedure DeleteKey(AnyComp, AnyUser: Boolean; const Section, Key: String); overload;
    procedure DeleteKey(const Key: String); overload;
  public
    { Устанавливает параметры подключения к базе данных. Если указано имя профиля,
      то использовать параметры не обязательно! Параметры следует указывать до начала
      работы с функциями ReadXXX или WriteXXX }
    procedure SetConnectionParams(AServerName: string; APort: Integer; ADataBase: string;
      AUserName: string; APassword: string; ACharSet: string); virtual;

    { Начало работы с объектом. Рекомендуется вызывать при чтении/записи большого
      количества параметров. Это ускоряет работу, если используется активная
      работа с пулом подключений }
    procedure BeginWork; virtual;

    { Окончание работы с объектом. Предварительно должен быть вызван метод BeginWork }
    procedure EndWork; virtual;

    { Позволяет указать имя пользователя (по умолчанию берется имя текущего пользователя Windows) }
    property UserName: string read GetUserName write SetUserName;

    { Позволяет указать имя компьютера (по умолчанию берется имя текущего компьютера) }
    property ComputerName: string read GetComputerName write SetComputerName;

    { Позволяет указать имя профиля из пула подключений (по умолчанию берется FBDefDB) }
    property PoolProfileName: string read GetPoolProfileName write SetPoolProfileName;

  public
    {Конструктор. Параметр только один: AlwaysConnected. Если он = TRUE, то подключение
     к базе данных устанавливается один раз (берется из пула) и держится на всем
     протяжении работы программы. Если он = FALSE, то подключение берется из пула
     только на момент выполнения операций ReadXXX и WriteXXX)}
    { Позволяет указать имя файла (имитация нескольких INI-файлов). По умолчанию: CONF
      Если раньше Ваше приложение работало с множеством INI-файлов, то Вы можете
      при создании объекта указать имя INI-файла. Приложение претерпит минимум изменений. }
    constructor Create(AFileName: string; AlwaysConnected: Boolean);

    {Деструктор}
    destructor Destroy; override;
  end;

{Создает объект TFBIniFile}
function FBCreateIniFile(AFileName: string; AlwaysConnected: Boolean): TFBIniFile;

{Уничтожает заданный объект TFBIniFile}
procedure FBFreeIniFile(Ini: TFBIniFile);

{$IFDEF FBUTILSDLL} // Замечания по директиве смотрите в модуле fbUtilsBase.pas
exports
  FBCreateIniFile name 'ibxFBCreateIniFile',
  FBFreeIniFile name 'ibxFBFreeIniFile';
{$ENDIF}

implementation

const
  SAnyComp = '*'; // Любой компьютер
  SAnyUser = '*'; // ВСЕ пользователи

function FBCreateIniFile(AFileName: string; AlwaysConnected: Boolean): TFBIniFile;
begin
  Result := TFBIniFile.Create(AFileName, AlwaysConnected);
end;

procedure FBFreeIniFile(Ini: TFBIniFile);
begin
  Ini.Free;
end;

{ TFBIniFile }

procedure TFBIniFile.BeginWork;
begin
  FCritSect.Enter;
  try
    if FWasBeginWorkCall then
      raise Exception.Create('Call of TFBIniFile.EndWork was skipped!'); // Где-то не был вызван метод TFBIniFile.EndWork
    FWasBeginWorkCall := True;
    ConnectDB(False);
  except
    on E: Exception do
    begin
      FWasBeginWorkCall := False;
      FCritSect.Leave;
      raise ReCreateEObject(E, 'TFBIniFile.BeginWork');
    end;
  end;
end;

procedure TFBIniFile.CheckParams(const Section, Key: string; CheckKey: Boolean = True);
begin
  if Section = '' then
    raise Exception.Create('Section name is empty!'); //Имя секции не указано

  if CheckKey then
    if Key = '' then
      raise Exception.Create('Parameter name is empty!'); // Имя параметра не указано
end;

procedure TFBIniFile.ConnectDB(Internal: Boolean);
begin
  if FDB = nil then // Если объект подключения к БД еще не создан...
  begin
    if FConnParams.cpDataBase <> '' then
      FDB := FBPoolGetConnectionByParams(FConnParams.cpServerName, FConnParams.cpPort,
        FConnParams.cpDataBase, FConnParams.cpUserName, FConnParams.cpPassword, FConnParams.cpCharSet,
        @FTranR, @FTranW, trRCRO, trRCRWW, '')
    else
      FDB := FBPoolGetConnectionByProfile(FPoolProfileName, @FTranR, @FTranW, trRCRO, trRCRWW, '');

    dsRead.Database := FDB;
    dsRead.Transaction := FTranR;
    dsIns.Database := FDB;
    dsIns.Transaction := FTranW;
    dsUpd.Database := FDB;
    dsUpd.Transaction := FTranW;
    dsCustom.Database := FDB;
    dsCustom.Transaction := FTranW;
  end;

  if not FTranR.Active then
    FTranR.Active := True;
end;

constructor TFBIniFile.Create(AFileName: string; AlwaysConnected: Boolean);
begin
  inherited Create;
  FAlwaysConnected := AlwaysConnected;
  FComputerName := GetCurrentComputerName;
  FUserName := GetCurrentUserName;
  FFileName := AFileName;
  FPoolProfileName := FBDefDB; // Имя профиля параметров подключений в пуле по умолчанию
  FObjs := TObjHolder.Create;

  FObjs.RegObj(FCritSect, TCriticalSection.Create);

  FObjs.RegObj(dsRead, TIBDataSet.Create(nil));
  dsRead.SelectSQL.Text := Format(
    'SELECT %s, %s, %s FROM %s '+
    'WHERE %s=:FILENAME AND %s=:COMPUTERNAME AND %s=:USERNAME AND %s=:SECTIONNAME AND %s=:PARAMNAME',
    [FBIniFieldParamValue, FBIniFieldParamBlob, FBIniFieldParamBlobHash, FBIniTableConfigParams,
     FBIniFieldFileName, FBIniFieldComputerName, FBIniFieldUserName, FBIniFieldSectionName, FBIniFieldParamName]);

  FObjs.RegObj(dsIns, TIBDataSet.Create(nil));
  dsIns.SelectSQL.Text := Format(
    ' INSERT INTO %s '+
    ' (%s,%s,%s,%s,%s,%s,%s,%s,%s,%s) VALUES '+
    ' (:FILENAME, :COMPUTERNAME, :USERNAME, :SECTIONNAME, :PARAMNAME, :PARAMVALUE, :PARAMBLOB, :PARAMBLOBHASH, CURRENT_TIMESTAMP, :MODIFYUSER)',
    [FBIniTableConfigParams, FBIniFieldFileName, FBIniFieldComputerName, FBIniFieldUserName, FBIniFieldSectionName,
     FBIniFieldParamName, FBIniFieldParamValue, FBIniFieldParamBlob, FBIniFieldParamBlobHash,
     FBIniFieldModifyDate, FBIniFieldModifyUser]);

  FObjs.RegObj(dsUpd, TIBDataSet.Create(nil));
  dsUpd.SelectSQL.Text := Format(
    ' UPDATE %s SET %s = :PARAMVALUE, %s = :PARAMBLOB, %s=:PARAMBLOBHASH, %s=CURRENT_TIMESTAMP, %s=:MODIFYUSER '+
    ' WHERE (%s=:FILENAME) AND (%s=:COMPUTERNAME) AND (%s=:USERNAME) AND '+
    '       (%s=:SECTIONNAME) AND (%s=:PARAMNAME) ',
    [FBIniTableConfigParams, FBIniFieldParamValue, FBIniFieldParamBlob, FBIniFieldParamBlobHash,
     FBIniFieldModifyDate, FBIniFieldModifyUser,
     FBIniFieldFileName, FBIniFieldComputerName, FBIniFieldUserName, FBIniFieldSectionName, FBIniFieldParamName]);

  FObjs.RegObj(dsCustom, TIBDataSet.Create(nil));
end;

destructor TFBIniFile.Destroy;
begin
  FAlwaysConnected := False; // Отключаем режим "постоянное подключение"
  DisconnectDB(True);        // Отключаемся от базы данных
  FObjs.Free;
  inherited;
end;

procedure TFBIniFile.DisconnectDB(Internal: Boolean);
begin
  if not FAlwaysConnected then
  begin
    if FWasBeginWorkCall then // Если вызван метод BeginWork
      if Internal then        // и если это внутренний вызов
        Exit;                 // то подключение не прерываем

    if Assigned(FDB) then
    begin
      FBPoolReturnConnection(FDB, '');
      FDB := nil;
      FTranW := nil;
      FTranR := nil;
    end;
  end;
end;

procedure TFBIniFile.EndWork;
begin
  try
    if not FWasBeginWorkCall then
      raise Exception.Create('Call of TFBIniFile.BeginWork was skipped'); // Был пропущен вызов TFBIniFile.BeginWork

    try
      DisconnectDB(False);
    finally
      FWasBeginWorkCall := False;
      FCritSect.Leave;
    end;

  except
    on E: Exception do
      raise ReCreateEObject(E, 'TFBIniFile.EndWork');
  end;

end;

function TFBIniFile.GetComputerName: string;
begin
  Result := FComputerName;
end;

function TFBIniFile.GetPoolProfileName: string;
begin
  Result := FPoolProfileName;
end;

function TFBIniFile.GetUserName: string;
begin
  Result := FUserName;
end;

procedure TFBIniFile.DoDeleteKey(AnyComp, AnyUser: Boolean; const Section,
  Key: String);
begin
  try
    FCritSect.Enter;
    try
      ConnectDB(True);
      try
        CheckParams(Section, Key, False);
        FTranW.Active := True;
        try
          dsCustom.SelectSQL.Text := Format(
            'DELETE FROM %s '+
            'WHERE %s=:FILENAME AND %s=:COMPUTERNAME AND %s=:USERNAME AND %s=:SECTIONNAME AND %s=:PARAMNAME ',
            [FBIniTableConfigParams, FBIniFieldFileName, FBIniFieldComputerName, FBIniFieldUserName,
             FBIniFieldSectionName, FBIniFieldParamName]);
          SetParams(dsCustom, AnyComp, AnyUser, Section, Key);

          dsCustom.ExecSQL;
          FTranW.Commit;

        finally
          FTranW.Active := False;
        end;
      finally
        DisconnectDB(True);
      end;
    finally
      FCritSect.Leave;
    end;
  except
    on E: Exception do
      raise ReCreateEObject(E, 'EraseSection');
  end;
end;

procedure TFBIniFile.DoEraseSection(AnyComp, AnyUser: Boolean;
  const Section: string);
begin
  try
    FCritSect.Enter;
    try
      ConnectDB(True);
      try
        CheckParams(Section, '', False);
        FTranW.Active := True;
        try
          dsCustom.SelectSQL.Text := Format(
            'DELETE FROM %s '+
            'WHERE %s=:FILENAME AND %s=:COMPUTERNAME AND %s=:USERNAME AND %s=:SECTIONNAME ',
            [FBIniTableConfigParams, FBIniFieldFileName, FBIniFieldComputerName, FBIniFieldUserName, FBIniFieldSectionName]);
          SetParams(dsCustom, AnyComp, AnyUser, Section, '');

          dsCustom.ExecSQL;
          FTranW.Commit;

        finally
          FTranW.Active := False;
        end;
      finally
        DisconnectDB(True);
      end;
    finally
      FCritSect.Leave;
    end;
  except
    on E: Exception do
      raise ReCreateEObject(E, 'EraseSection');
  end;
end;

function TFBIniFile.DoReadBinaryData(AnyComp, AnyUser: Boolean; const Section, Key: string;
  var Buffer; BufSize: Integer): Integer;
var
  ms: TMemoryStream;
  Ar: PByteArray;
  I: Integer;
begin
  try
    ms := TMemoryStream.Create;
    try
      Result := 0;
      if DoReadStream(AnyComp, AnyUser, Section, Key, ms) then
      begin
        Result := Min(BufSize, ms.Size);
        if Result > 0 then
        begin
          ms.Position := 0;
          ms.Read(Buffer, Result);
          if Result < BufSize then
          begin // Обнуляем оставшуюся часть буфера
            Ar := @Buffer;
            for I := Result to BufSize - 1 do
              Ar[I] := 0;
          end;
        end;
      end;
    finally
      ms.Free;
    end;
  except
    on E: Exception do
      raise ReCreateEObject(E, 'ReadBinaryData');
  end;
end;

function TFBIniFile.DoReadBool(AnyComp, AnyUser: Boolean; const Section,
  Key: string; Default: Boolean): Boolean;
begin
  Result := ReadInteger(AnyComp, AnyUser, Section, Key, Byte(Default)) = 1;
end;

function TFBIniFile.ReadBool(const Key: string; Default: Boolean): Boolean;
begin
  Result := ReadBool(FBIniDefSection, Key, Default);
end;

function TFBIniFile.ReadDateTime(const Key: string;
  Default: TDateTime): TDateTime;
begin
  Result := ReadDateTime(FBIniDefSection, Key, Default);
end;

function TFBIniFile.DoReadDate(AnyComp, AnyUser: Boolean; const Section,
  Key: string; Default: TDateTime): TDateTime;
var
  S: string;
begin
  try
    S := ReadString(AnyComp, AnyUser, Section, Key, DateToStr(Default, FBFormatSettings));
    S := Copy(S, 1, 10) + ' 00:00:00.000';
  except
    on E: Exception do
      raise ReCreateEObject(E, 'ReadDate');
  end;

  try
    Result := StrToDateTime(S, FBFormatSettings);
  except
    Result := Default;
  end;
end;

function TFBIniFile.DoReadDateTime(AnyComp, AnyUser: Boolean; const Section,
  Key: string; Default: TDateTime): TDateTime;
var
  S: string;
begin
  try
    S := ReadString(AnyComp, AnyUser, Section, Key, DateTimeToStr(Default, FBFormatSettings));
  except
    on E: Exception do
      raise ReCreateEObject(E, 'ReadDateTime');
  end;

  try
    Result := StrToDateTime(S, FBFormatSettings);
  except
    Result := Default;
  end;
end;

function TFBIniFile.ReadFloat(const Key: string; Default: Double): Double;
begin
  Result := ReadFloat(FBIniDefSection, Key, Default);
end;

function TFBIniFile.DoReadFloat(AnyComp, AnyUser: Boolean; const Section,
  Key: string; Default: Double): Double;
var
  S: string;
begin
  try
    S := ReadString(AnyComp, AnyUser, Section, Key, FloatToStr(Default, FBFormatSettings));
  except
    on E: Exception do
      raise ReCreateEObject(E, 'ReadFloat');
  end;

  try
    Result := StrToFloat(S, FBFormatSettings);
  except
    Result := Default;
  end;
end;

function TFBIniFile.ReadInteger(const Key: string; Default: Integer): Integer;
begin
  Result := ReadInteger(FBIniDefSection, Key, Default);
end;

function TFBIniFile.DoReadInteger(AnyComp, AnyUser: Boolean; const Section,
  Key: string; Default: Integer): Integer;
var
  S: string;
begin
  try
    S := ReadString(AnyComp, AnyUser, Section, Key, IntToStr(Default));
  except
    on E: Exception do
      raise ReCreateEObject(E, 'ReadInteger');
  end;

  try
    Result := StrToInt(S);
  except
    Result := Default;
  end;
end;

function TFBIniFile.ReadString(const Key, Default: String): string;
begin
  Result := ReadString(FBIniDefSection, Key, Default);
end;

procedure TFBIniFile.DoReadSection(AnyComp, AnyUser: Boolean;
  const Section: string; Strings: TStrings);
begin
  try
    FCritSect.Enter;
    try
      ConnectDB(True);
      try
        CheckParams(Section, '', False);
        FTranW.Active := True;
        try
          dsCustom.SelectSQL.Text := Format(
            'SELECT %s FROM %s '+
            'WHERE %s=:FILENAME AND %s=:COMPUTERNAME AND %s=:USERNAME AND %s=:SECTIONNAME '+
            'ORDER BY 1',
            [FBIniFieldParamName, FBIniTableConfigParams, FBIniFieldFileName,
             FBIniFieldComputerName, FBIniFieldUserName, FBIniFieldSectionName]);
          SetParams(dsCustom, AnyComp, AnyUser, Section, '');

          dsCustom.Open;
          Strings.BeginUpdate;
          try
            Strings.Clear;
            while not dsCustom.Eof do
            begin
              Strings.Add(dsCustom.Fields[0].AsString);
              dsCustom.Next;
            end;
          finally
            Strings.EndUpdate;
          end;
          dsCustom.Close;

        finally
          FTranW.Active := False;
        end;
      finally
        DisconnectDB(True);
      end;
    finally
      FCritSect.Leave;
    end;
  except
    on E: Exception do
      raise ReCreateEObject(E, 'ReadSection');
  end;

end;

procedure TFBIniFile.DoReadSections(AnyComp, AnyUser: Boolean; Strings: TStrings);
begin
  try
    FCritSect.Enter;
    try
      ConnectDB(True);
      try
        FTranW.Active := True;
        try
          dsCustom.SelectSQL.Text := Format(
            'SELECT DISTINCT %s FROM %s '+
            'WHERE %s=:FILENAME AND %s=:COMPUTERNAME AND %s=:USERNAME '+
            'ORDER BY 1',
            [FBIniFieldSectionName, FBIniTableConfigParams, FBIniFieldFileName, FBIniFieldComputerName, FBIniFieldUserName]);
          SetParams(dsCustom, AnyComp, AnyUser, '', '');

          dsCustom.Open;
          Strings.BeginUpdate;
          try
            Strings.Clear;
            while not dsCustom.Eof do
            begin
              Strings.Add(dsCustom.Fields[0].AsString);
              dsCustom.Next;
            end;
          finally
            Strings.EndUpdate;
          end;
          dsCustom.Close;

        finally
          FTranW.Active := False;
        end;
      finally
        DisconnectDB(True);
      end;
    finally
      FCritSect.Leave;
    end;
  except
    on E: Exception do
      raise ReCreateEObject(E, 'ReadSections');
  end;

end;

procedure TFBIniFile.DoReadSectionValues(AnyComp, AnyUser: Boolean;
  const Section: string; Strings: TStrings);
begin
  try
    FCritSect.Enter;
    try
      ConnectDB(True);
      try
        CheckParams(Section, '', False);
        FTranW.Active := True;
        try
          dsCustom.SelectSQL.Text := Format(
            'SELECT %s, %s FROM %s '+
            'WHERE %s=:FILENAME AND %s=:COMPUTERNAME AND %s=:USERNAME AND %s=:SECTIONNAME '+
            'ORDER BY 1',
            [FBIniFieldParamName, FBIniFieldParamValue, FBIniTableConfigParams, FBIniFieldFileName,
             FBIniFieldComputerName, FBIniFieldUserName, FBIniFieldSectionName]);
          SetParams(dsCustom, AnyComp, AnyUser, Section, '');

          dsCustom.Open;
          Strings.BeginUpdate;
          try
            Strings.Clear;
            while not dsCustom.Eof do
            begin
              Strings.Add(dsCustom.Fields[0].AsString + '=' + dsCustom.Fields[1].AsString);
              dsCustom.Next;
            end;
          finally
            Strings.EndUpdate;
          end;
          dsCustom.Close;

        finally
          FTranW.Active := False;
        end;
      finally
        DisconnectDB(True);
      end;
    finally
      FCritSect.Leave;
    end;
  except
    on E: Exception do
      raise ReCreateEObject(E, 'ReadSectionValues');
  end;
end;

function TFBIniFile.DoReadStream(AnyComp, AnyUser: Boolean; const Section,
  Key: String; AStream: TStream): Boolean;
begin
  AStream.Size := 0;
  try
    FCritSect.Enter;
    try
      ConnectDB(True);
      try
        CheckParams(Section, Key);
        SetParams(dsRead, AnyComp, AnyUser, Section, Key);

        dsRead.Open;
        Result := not dsRead.Eof;
        if Result then
          TBlobField(dsRead.Fields[1]).SaveToStream(AStream);
        dsRead.Close;
      finally
        DisconnectDB(True);
      end;
    finally
      FCritSect.Leave;
    end;
  except
    on E: Exception do
      raise ReCreateEObject(E, 'ReadStream');
  end;
end;

function TFBIniFile.DoReadString(AnyComp, AnyUser: Boolean; const Section, Key,
  Default: String): string;
begin
  Result := Default;
  try
    FCritSect.Enter;
    try
      ConnectDB(True);
      try
        CheckParams(Section, Key);
        SetParams(dsRead, AnyComp, AnyUser, Section, Key);

        dsRead.Open;
        if not dsRead.Eof then
          Result := dsRead.Fields[0].AsString;
        dsRead.Close;
      finally
        DisconnectDB(True);
      end;
    finally
      FCritSect.Leave;
    end;
  except
    on E: Exception do
      raise ReCreateEObject(E, 'ReadString');
  end;
end;

function TFBIniFile.DoReadText(AnyComp, AnyUser: Boolean; const Section,
  Key: String; Default: String): string;
var
  ms: TMemoryStream;
  ws: WideString;
begin
  ms := TMemoryStream.Create;
  try
    if DoReadStream(AnyComp, AnyUser, Section, Key, ms) then
    begin
      ms.Position := 0;
      if ms.Size < 2 then
        ws := ''
      else
      begin
        SetLength(ws, ms.Size div SizeOf(WideChar));
        ms.Read(ws[1], Length(ws) * SizeOf(WideChar));
      end;
      Result := ws;
    end else
      Result := Default;
  finally
    ms.Free;
  end;
end;

function TFBIniFile.DoReadTime(AnyComp, AnyUser: Boolean; const Section,
  Key: string; Default: TDateTime): TDateTime;
var
  S: string;
begin
  try
    S := ReadString(AnyComp, AnyUser, Section, Key, TimeToStr(Default, FBFormatSettings));
  except
    on E: Exception do
      raise ReCreateEObject(E, 'ReadTime');
  end;

  try
    Result := StrToTime(S, FBFormatSettings);
  except
    Result := Default;
  end;
end;

function TFBIniFile.DoSectionExists(AnyComp, AnyUser: Boolean;
  const Section: string): Boolean;
var
  AList: TStringList;
begin
  AList := TStringList.Create;
  try
    DoReadSections(AnyComp, AnyUser, AList); // Считываем имена всех секций
    Result := AList.IndexOf(Section) >= 0;   // Проверяем, есть ли среди них искомая секция
  finally
    AList.Free;
  end;
end;

function TFBIniFile.DoValueExists(AnyComp, AnyUser: Boolean; const Section,
  Key: string): Boolean;
const
  SNotExists = '<#@_not_exists_@#>';
var
  S: string;
begin
  S := DoReadString(AnyComp, AnyUser, Section, Key, SNotExists);
  Result := S <> SNotExists;
end;

procedure TFBIniFile.SetComputerName(const Value: string);
begin
  if Trim(Value) = '' then
    raise Exception.Create('Computer name is empty');
  FCritSect.Enter; // На всякий случай защищаем переменную с помощью крит. секции
  FComputerName := Value;
  FCritSect.Leave;
end;

procedure TFBIniFile.SetConnectionParams(AServerName: string; APort: Integer;
  ADataBase, AUserName, APassword, ACharSet: string);
begin
  with FConnParams do
  begin
    cpServerName := AServerName;
    cpPort := APort;
    cpDataBase := ADataBase; // Имя базы данных как раз определяет, нужно ли использовать FConnParams
    cpUserName := AUserName;
    cpPassword := APassword;
    cpCharSet := ACharSet;
  end;
end;

procedure TFBIniFile.SetParams(ds: TIBDataSet; AnyComp, AnyUser: Boolean;
  const Section, Key: string);
var
  UserName, ComputerName: string;
begin
  if AnyUser then
    UserName := SAnyUser
  else
    UserName := FUserName;

  if AnyComp then
    ComputerName := SAnyComp
  else
    ComputerName := FComputerName;

  ds.ParamByName('FILENAME').AsString := AnsiUpperCase(FFileName);
  ds.ParamByName('COMPUTERNAME').AsString := AnsiUpperCase(ComputerName);
  ds.ParamByName('USERNAME').AsString     := AnsiUpperCase(UserName);


  if Section <> '' then
    ds.ParamByName('SECTIONNAME').AsString  := AnsiUpperCase(Section);
  if Key <> '' then
    ds.ParamByName('PARAMNAME').AsString    := AnsiUpperCase(Key);
end;

procedure TFBIniFile.SetPoolProfileName(const Value: string);
begin
  FCritSect.Enter; // На всякий случай защищаем переменную с помощью крит. секции
  FPoolProfileName := Value;
  FCritSect.Leave;
end;

procedure TFBIniFile.SetUserName(const Value: string);
begin
  if Trim(Value) = '' then
    raise Exception.Create('User name is empty');
  FCritSect.Enter;
  FUserName := Value;
  FCritSect.Leave;
end;

function TFBIniFile.InternalValueExists(AnyComp, AnyUser: Boolean; const Section, Key: string; var Value: string; ReadHash: Boolean): Boolean;
begin
  Value := '';
  CheckParams(Section, Key);
  SetParams(dsRead, AnyComp, AnyUser, Section, Key);
  dsRead.Open;
  Result := not dsRead.Eof;
  if Result then
  begin
    if ReadHash then
      Value := dsRead.Fields[2].AsString
    else
      Value := dsRead.Fields[0].AsString;
  end;
  dsRead.Close;
end;

procedure TFBIniFile.DoWriteBinaryData(AnyComp, AnyUser: Boolean; const Section, Key: string;
  const Buffer; BufSize: Integer);
var
  ms: TMemoryStream;
begin
  try
    ms := TMemoryStream.Create;
    try
      if BufSize > 0 then
        ms.WriteBuffer(Buffer, BufSize);
      DoWriteStream(AnyComp, AnyUser, Section, Key, ms);
    finally
      ms.Free;
    end;
  except
    on E: Exception do
      raise ReCreateEObject(E, 'WriteBinaryData');
  end;
end;

procedure TFBIniFile.DoWriteBool(AnyComp, AnyUser: Boolean; const Section, Key: string; Value: Boolean);
begin
  try
    WriteInteger(AnyComp, AnyUser, Section, Key, Byte(Value));
  except
    on E: Exception do
      raise ReCreateEObject(E, 'WriteBool');
  end;
end;

procedure TFBIniFile.WriteBool(const Key: string; Value: Boolean);
begin
  WriteBool(FBIniDefSection, Key, Value);
end;

procedure TFBIniFile.WriteDateTime(const Key: string; Value: TDateTime);
begin
  WriteDateTime(FBIniDefSection, Key, Value);
end;

procedure TFBIniFile.DoWriteDate(AnyComp, AnyUser: Boolean; const Section,
  Key: string; Value: TDateTime);
begin
  try
    WriteString(AnyComp, AnyUser, Section, Key, DateToStr(Value, FBFormatSettings));
  except
    on E: Exception do
      raise ReCreateEObject(E, 'WriteDate');
  end;
end;

procedure TFBIniFile.DoWriteDateTime(AnyComp, AnyUser: Boolean; const Section,
  Key: string; Value: TDateTime);
begin
  try
    WriteString(AnyComp, AnyUser, Section, Key, DateTimeToStr(Value, FBFormatSettings));
  except
    on E: Exception do
      raise ReCreateEObject(E, 'WriteDateTime');
  end;
end;

procedure TFBIniFile.WriteFloat(const Key: string; Value: Double);
begin
  WriteFloat(FBIniDefSection, Key, Value);
end;

procedure TFBIniFile.DoWriteFloat(AnyComp, AnyUser: Boolean; const Section,
  Key: string; Value: Double);
begin
  try
    WriteString(AnyComp, AnyUser, Section, Key, FloatToStr(Value, FBFormatSettings));
  except
    on E: Exception do
      raise ReCreateEObject(E, 'WriteFloat');
  end;
end;

procedure TFBIniFile.WriteInteger(const Key: string; Value: Integer);
begin
  WriteInteger(FBIniDefSection, Key, Value);
end;

procedure TFBIniFile.DoWriteInteger(AnyComp, AnyUser: Boolean; const Section,
  Key: string; Value: Integer);
begin
  try
    WriteString(AnyComp, AnyUser, Section, Key, IntToStr(Value));
  except
    on E: Exception do
      raise ReCreateEObject(E, 'WriteInteger');
  end;
end;

procedure TFBIniFile.WriteString(const Key, Value: String);
begin
  WriteString(FBIniDefSection, Key, Value);
end;

procedure TFBIniFile.DoWriteStream(AnyComp, AnyUser: Boolean; const Section,
  Key: String; AStream: TStream);
var
  ds: TIBDataSet;
  OldHash, NewHash: string;
begin
  try
    FCritSect.Enter;
    try
      ConnectDB(True);
      try
        AStream.Position := 0;
        // Вычисляем хэш
        NewHash := CalcStreamHash(AStream);

        if InternalValueExists(AnyComp, AnyUser, Section, Key, OldHash, True) then
        begin
          if FBIniCheckBlobHash then
            if NewHash = OldHash then Exit; // Если то же самое значение, то выходим
          ds := dsUpd;
        end
        else
          ds := dsIns;

        FTranW.Active := True;
        try
          SetParams(ds, AnyComp, AnyUser, Section, Key);
          ds.ParamByName('PARAMVALUE').Clear;
          ds.ParamByName('PARAMBLOB').LoadFromStream(AStream);
          ds.ParamByName('PARAMBLOBHASH').AsString := NewHash;
          ds.ParamByName('MODIFYUSER').AsString := FUserName; // Без AnsiUpperCase
          ds.ExecSQL;

          FTranW.Commit;
        except
          FTranW.Rollback;
          raise;
        end;
      finally
        DisconnectDB(True);
      end;
    finally
      FCritSect.Leave;
    end;
  except
    on E: Exception do
      raise ReCreateEObject(E, 'WriteStream');
  end;
end;

procedure TFBIniFile.DoWriteString(AnyComp, AnyUser: Boolean; const Section, Key, Value: String);
var
  ds: TIBDataSet;
  OldValue: string;
begin
  try
    FCritSect.Enter;
    try
      ConnectDB(True);
      try
        if InternalValueExists(AnyComp, AnyUser, Section, Key, OldValue, False) then
        begin
          if OldValue = Value then Exit; // Если то же самое значение, то выходим
          ds := dsUpd;
        end
        else
          ds := dsIns;

        FTranW.Active := True;
        try
          SetParams(ds, AnyComp, AnyUser, Section, Key);
          ds.ParamByName('PARAMVALUE').AsString := Value;
          ds.ParamByName('PARAMBLOB').Clear;
          ds.ParamByName('PARAMBLOBHASH').Clear;
          ds.ParamByName('MODIFYUSER').AsString := FUserName; // Без AnsiUpperCase
          ds.ExecSQL;

          FTranW.Commit;
        except
          FTranW.Rollback;
          raise;
        end;
      finally
        DisconnectDB(True);
      end;
    finally
      FCritSect.Leave;
    end;
  except
    on E: Exception do
      raise ReCreateEObject(E, 'WriteString');
  end;
end;

procedure TFBIniFile.DoWriteText(AnyComp, AnyUser: Boolean; const Section,
  Key: String; Value: string);
var
  ws: WideString;
  ms: TMemoryStream;
begin
  ws := Value; {для НЕ-юникодных Дельфи произойдет автоматическая перекодировка}
  ms := TMemoryStream.Create;
  try
    if ws <> '' then
      ms.Write(ws[1], Length(ws) * SizeOf(WideChar));
    DoWriteStream(AnyComp, AnyUser, Section, Key, ms);
  finally
    ms.Free;
  end;
end;

procedure TFBIniFile.DoWriteTime(AnyComp, AnyUser: Boolean; const Section,
  Key: string; Value: TDateTime);
begin
  try
    WriteString(AnyComp, AnyUser, Section, Key, TimeToStr(Value, FBFormatSettings));
  except
    on E: Exception do
      raise ReCreateEObject(E, 'WriteTime');
  end;
end;

procedure TFBIniFile.WriteBool(const Section, Key: string; Value: Boolean);
begin
  WriteBool(True, True, Section, Key, Value);
end;

procedure TFBIniFile.WriteDateTime(const Section, Key: string;
  Value: TDateTime);
begin
  WriteDateTime(True, True, Section, Key, Value);
end;

procedure TFBIniFile.WriteFloat(const Section, Key: string; Value: Double);
begin
  WriteFloat(True, True, Section, Key, Value);
end;

procedure TFBIniFile.WriteInteger(const Section, Key: string; Value: Integer);
begin
  WriteInteger(True, True, Section, Key, Value);
end;

procedure TFBIniFile.WriteString(const Section, Key, Value: String);
begin
  WriteString(True, True, Section, Key, Value);
end;

function TFBIniFile.ReadBool(const Section, Key: string;
  Default: Boolean): Boolean;
begin
  Result := ReadBool(True, True, Section, Key, Default);
end;

function TFBIniFile.ReadDateTime(const Section, Key: string;
  Default: TDateTime): TDateTime;
begin
  Result := ReadDateTime(True, True, Section, Key, Default);
end;

function TFBIniFile.ReadFloat(const Section, Key: string;
  Default: Double): Double;
begin
  Result := ReadFloat(True, True, Section, Key, Default);
end;

function TFBIniFile.ReadInteger(const Section, Key: string;
  Default: Integer): Integer;
begin
  Result := ReadInteger(True, True, Section, Key, Default);
end;

function TFBIniFile.ReadString(const Section, Key, Default: String): string;
begin
  Result := ReadString(True, True, Section, Key, Default);
end;

function TFBIniFile.ReadBool(AnyComp, AnyUser: Boolean; const Section,
  Key: string; Default: Boolean): Boolean;
begin
  Result := DoReadBool(AnyComp, AnyUser, Section, Key, Default);
end;

function TFBIniFile.ReadDateTime(AnyComp, AnyUser: Boolean; const Section,
  Key: string; Default: TDateTime): TDateTime;
begin
  Result := DoReadDateTime(AnyComp, AnyUser, Section, Key, Default);
end;

function TFBIniFile.ReadFloat(AnyComp, AnyUser: Boolean; const Section,
  Key: string; Default: Double): Double;
begin
  Result := DoReadFloat(AnyComp, AnyUser, Section, Key, Default);
end;

function TFBIniFile.ReadInteger(AnyComp, AnyUser: Boolean; const Section,
  Key: string; Default: Integer): Integer;
begin
  Result := DoReadInteger(AnyComp, AnyUser, Section, Key, Default);
end;

function TFBIniFile.ReadString(AnyComp, AnyUser: Boolean; const Section, Key,
  Default: String): string;
begin
  Result := DoReadString(AnyComp, AnyUser, Section, Key, Default);
end;

procedure TFBIniFile.WriteBool(AnyComp, AnyUser: Boolean; const Section,
  Key: string; Value: Boolean);
begin
  DoWriteBool(AnyComp, AnyUser, Section, Key, Value);
end;

procedure TFBIniFile.WriteDateTime(AnyComp, AnyUser: Boolean; const Section,
  Key: string; Value: TDateTime);
begin
  DoWriteDateTime(AnyComp, AnyUser, Section, Key, Value);
end;

procedure TFBIniFile.WriteFloat(AnyComp, AnyUser: Boolean; const Section,
  Key: string; Value: Double);
begin
  DoWriteFloat(AnyComp, AnyUser, Section, Key, Value);
end;

procedure TFBIniFile.WriteInteger(AnyComp, AnyUser: Boolean; const Section,
  Key: string; Value: Integer);
begin
  DoWriteInteger(AnyComp, AnyUser, Section, Key, Value);
end;

procedure TFBIniFile.WriteString(AnyComp, AnyUser: Boolean; const Section, Key,
  Value: String);
begin
  DoWriteString(AnyComp, AnyUser, Section, Key, Value);
end;


procedure TFBIniFile.ReadSection(AnyComp, AnyUser: Boolean;
  const Section: string; Strings: TStrings);
begin
  DoReadSection(AnyComp, AnyUser, Section, Strings);
end;

procedure TFBIniFile.ReadSection(const Section: string; Strings: TStrings);
begin
  DoReadSection(True, True, Section, Strings);
end;

procedure TFBIniFile.ReadSection(Strings: TStrings);
begin
  DoReadSection(True, True, FBIniDefSection, Strings);
end;

procedure TFBIniFile.ReadSections(Strings: TStrings);
begin
  DoReadSections(True, True, Strings);
end;

procedure TFBIniFile.ReadSections(AnyComp, AnyUser: Boolean; Strings: TStrings);
begin
  DoReadSections(AnyComp, AnyUser, Strings);
end;

procedure TFBIniFile.ReadSectionValues(Strings: TStrings);
begin
  DoReadSectionValues(True, True, FBIniDefSection, Strings);
end;

procedure TFBIniFile.ReadSectionValues(const Section: string;
  Strings: TStrings);
begin
  DoReadSectionValues(True, True, Section, Strings);
end;

procedure TFBIniFile.ReadSectionValues(AnyComp, AnyUser: Boolean;
  const Section: string; Strings: TStrings);
begin
  DoReadSectionValues(AnyComp, AnyUser, Section, Strings);
end;


procedure TFBIniFile.EraseSection;
begin
  DoEraseSection(True, True, FBIniDefSection);
end;

procedure TFBIniFile.EraseSection(const Section: string);
begin
  DoEraseSection(True, True, Section);
end;

procedure TFBIniFile.EraseSection(AnyComp, AnyUser: Boolean;
  const Section: string);
begin
  DoEraseSection(AnyComp, AnyUser, Section);
end;

procedure TFBIniFile.DeleteKey(const Key: String);
begin
  DoDeleteKey(True, True, FBIniDefSection, Key);
end;

procedure TFBIniFile.DeleteKey(const Section, Key: String);
begin
  DoDeleteKey(True, True, Section, Key);
end;

procedure TFBIniFile.DeleteKey(AnyComp, AnyUser: Boolean; const Section,
  Key: String);
begin
  DoDeleteKey(AnyComp, AnyUser, Section, Key);
end;


procedure TFBIniFile.WriteStream(AnyComp, AnyUser: Boolean; const Section,
  Key: String; AStream: TStream);
begin
  DoWriteStream(AnyComp, AnyUser, Section, Key, AStream);
end;

procedure TFBIniFile.WriteStream(const Section, Key: String; AStream: TStream);
begin
  DoWriteStream(True, True, Section, Key, AStream);
end;

procedure TFBIniFile.WriteStream(Key: String; AStream: TStream);
begin
  DoWriteStream(True, True, FBIniDefSection, Key, AStream);
end;

function TFBIniFile.ReadStream(AnyComp, AnyUser: Boolean; const Section,
  Key: String; AStream: TStream): Boolean;
begin
  Result := DoReadStream(AnyComp, AnyUser, Section, Key, AStream);
end;

function TFBIniFile.ReadStream(const Section, Key: String; AStream: TStream): Boolean;
begin
  Result := DoReadStream(True, True, Section, Key, AStream);
end;

function TFBIniFile.ReadStream(Key: String; AStream: TStream): Boolean;
begin
  Result := DoReadStream(True, True, FBIniDefSection, Key, AStream);
end;


procedure TFBIniFile.WriteText(AnyComp, AnyUser: Boolean; const Section,
  Key: String; Value: string);
begin
  DoWriteText(AnyComp, AnyUser, Section, Key, Value);
end;

procedure TFBIniFile.WriteText(const Section, Key: String; Value: string);
begin
  DoWriteText(True, True, Section, Key, Value);
end;

procedure TFBIniFile.WriteText(Key, Value: string);
begin
  DoWriteText(True, True, FBIniDefSection, Key, Value);
end;

function TFBIniFile.ReadText(AnyComp, AnyUser: Boolean; const Section,
  Key: String; Default: String): string;
begin
  Result := DoReadText(AnyComp, AnyUser, Section, Key, Default);
end;

function TFBIniFile.ReadText(const Section, Key: String;
  Default: String): string;
begin
  Result := DoReadText(True, True, Section, Key, Default);
end;

function TFBIniFile.ReadText(Key, Default: String): string;
begin
  Result := DoReadText(True, True, FBIniDefSection, Key, Default);
end;

procedure TFBIniFile.WriteBinaryData(AnyComp, AnyUser: Boolean; const Section,
  Key: string; const Buffer; BufSize: Integer);
begin
  DoWriteBinaryData(AnyComp, AnyUser, Section, Key, Buffer, BufSize);
end;

procedure TFBIniFile.WriteBinaryData(const Section, Key: string; const Buffer;
  BufSize: Integer);
begin
  DoWriteBinaryData(True, True, Section, Key, Buffer, BufSize);
end;

procedure TFBIniFile.WriteBinaryData(Key: string; const Buffer;
  BufSize: Integer);
begin
  DoWriteBinaryData(True, True, FBIniDefSection, Key, Buffer, BufSize);
end;

function TFBIniFile.ReadBinaryData(AnyComp, AnyUser: Boolean; const Section,
  Key: string; var Buffer; BufSize: Integer): Integer;
begin
  Result := DoReadBinaryData(AnyComp, AnyUser, Section, Key, Buffer, BufSize);
end;

function TFBIniFile.ReadBinaryData(const Section, Key: string; var Buffer;
  BufSize: Integer): Integer;
begin
  Result := DoReadBinaryData(True, True, Section, Key, Buffer, BufSize);
end;

function TFBIniFile.ReadBinaryData(Key: string; var Buffer;
  BufSize: Integer): Integer;
begin
  Result := DoReadBinaryData(True, True, FBIniDefSection, Key, Buffer, BufSize);
end;


function TFBIniFile.SectionExists(AnyComp, AnyUser: Boolean;
  const Section: string): Boolean;
begin
  Result := DoSectionExists(AnyComp, AnyUser, Section);
end;

function TFBIniFile.SectionExists(const Section: string): Boolean;
begin
  Result := DoSectionExists(True, True, Section);
end;

function TFBIniFile.SectionExists: Boolean;
begin
  Result := DoSectionExists(True, True, FBIniDefSection);
end;

function TFBIniFile.ValueExists(AnyComp, AnyUser: Boolean; const Section,
  Key: string): Boolean;
begin
  Result := DoValueExists(AnyComp, AnyUser, Section, Key);
end;

function TFBIniFile.ValueExists(const Section, Key: string): Boolean;
begin
  Result := DoValueExists(True, True, Section, Key);
end;

function TFBIniFile.ValueExists(Key: string): Boolean;
begin
  Result := DoValueExists(True, True, FBIniDefSection, Key);
end;

procedure TFBIniFile.WriteDate(AnyComp, AnyUser: Boolean; const Section,
  Key: string; Value: TDateTime);
begin
  DoWriteDate(AnyComp, AnyUser, Section, Key, Value);
end;

procedure TFBIniFile.WriteDate(const Section, Key: string; Value: TDateTime);
begin
  DoWriteDate(True, True, Section, Key, Value);
end;

procedure TFBIniFile.WriteDate(Key: string; Value: TDateTime);
begin
  DoWriteDate(True, True, FBIniDefSection, Key, Value);
end;

procedure TFBIniFile.WriteTime(AnyComp, AnyUser: Boolean; const Section,
  Key: string; Value: TDateTime);
begin
  DoWriteTime(AnyComp, AnyUser, Section, Key, Value);
end;

procedure TFBIniFile.WriteTime(const Section, Key: string; Value: TDateTime);
begin
  DoWriteTime(True, True, Section, Key, Value);
end;

procedure TFBIniFile.WriteTime(Key: string; Value: TDateTime);
begin
  DoWriteTime(True, True, FBIniDefSection, Key, Value);
end;


function TFBIniFile.ReadDate(Key: string; Default: TDateTime): TDateTime;
begin
  Result := DoReadDate(True, True, FBIniDefSection, Key, Default);
end;

function TFBIniFile.ReadDate(const Section, Key: string;
  Default: TDateTime): TDateTime;
begin
  Result := DoReadDate(True, True, Section, Key, Default);
end;

function TFBIniFile.ReadDate(AnyComp, AnyUser: Boolean; const Section,
  Key: string; Default: TDateTime): TDateTime;
begin
  Result := DoReadDate(AnyComp, AnyUser, Section, Key, Default);
end;

function TFBIniFile.ReadTime(Key: string; Default: TDateTime): TDateTime;
begin
  Result := DoReadTime(True, True, FBIniDefSection, Key, Default);
end;

function TFBIniFile.ReadTime(const Section, Key: string;
  Default: TDateTime): TDateTime;
begin
  Result := DoReadTime(True, True, Section, Key, Default);
end;

function TFBIniFile.ReadTime(AnyComp, AnyUser: Boolean; const Section,
  Key: string; Default: TDateTime): TDateTime;
begin
  Result := DoReadTime(AnyComp, AnyUser, Section, Key, Default);
end;

end.
