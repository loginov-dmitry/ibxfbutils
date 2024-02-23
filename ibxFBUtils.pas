{$IFDEF FPC}
{$MODE DELPHI}{$H+}{$CODEPAGE UTF8}
{$ENDIF}

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
{ Модуль ibxFBUtils - интерфейсный модуль для библиотеки функций IBXFBUtils   }
{ (c) 2012-2013 Логинов Дмитрий Сергеевич                                     }
{ Начало разработки проекта: 24.03.2012                                       }
{ Последнее обновление: 09.05.2012                                            }
{ Протестировано на D7, D2007, D2010, D-XE2 (32-разрядная)                    }
{ Адрес сайта: http://loginovprojects.ru/                                     }
{ e-mail: loginov_d@inbox.ru                                                  }
{                                                                             }
{ *************************************************************************** }

{
Библиотека IBXFBUtils содержит функции, существенным образом облегчающие
работу с базами данных Firebird.
Библиотека основана на компонентах Interbase Express (IBE).
Может компилироваться в EXE, либо в пакет BPL, либо в DLL.

Компиляция в DLL (fbUtils.dll) имеет смысл при разработке сложного многомодульного
приложения и позволяет сократить общий объем исполняемого кода и предоставить
доступ к глобальным объектам библиотеки IBXFBUtils из любого двоичного модуля.
При использовании fbUtils.dll:
- DLL должна быть скомпилирована в той же версии Delphi, что и EXE
- должна быть включена опция компиляции FBUTILSDLL (см. подробности в модуле fbUtilsBase)
- DLL и EXE должны быть скомпилированы с run-time пакетами BPL. ibxpress - обязательно!
- в EXE (в секции Uses) должны быть указаны модули fbUtils и fbTypes
- приложение не будет работать, если что-то сделано не правильно. Вы увидите причину ошибки.

ВНИМАНИЕ! ЕСЛИ ВЫ ИСПОЛЬЗУЕТЕ DELPHI 7, то ОБЯЗАТЕЛЬНО обновите библиотеку IBX,
иначе проект может даже не скомпилироваться.
Ссылка для скачивания: http://ibase.ru/ibx/ibxdp711.zip

ВНИМАНИЕ! ВЫ НЕ МОЖЕТЕ РАБОТАТЬ С БД FIREBIRD, ЕСЛИ КОМПИЛИРУЕТЕ 64-РАЗРЯДНОЕ
ПРИЛОЖЕНИЕ (НЕСОВМЕСТИМОСТЬ ИЗ-ЗА КОМПОНЕНТОВ IBX)!

В данном модуле - только интерсейсные функции, без реализации.

В данном модуле отсутствуют подробные комментарии для каждой функции. Смотрите
подробности в том модуле, в котором реализована соответствующая функция.

Всю работу Вам следует выполнять с объектом "fb" класса TfbUtils.

Все текстовые сообщения (для пользователя) объявлены в разделе resourcestring, поэтому
для локализации приложения используйте стандартный менеджер трансляции Delphi.

Имена объектов в БД Firebird должны быть написаны по-английски. Если имена даны
по русски, то библиотека fbUtils скорее всего откажется с ними работать.

Перед началом работы рекомендуется ознакомиться с файлом ibxfbutils.html
}

unit ibxFBUtils;

interface
uses
{$IFnDEF FPC}
  Windows,
{$ELSE}
  LCLIntf, LCLType, //LMessages, LazUTF8,
{$ENDIF}
  SysUtils, Classes, IBDatabase, IBCustomDataSet, fbTypes, ParamsUtils, Variants;

type
  TfbPool = class(TObject)
  public
    {Добавляет параметры подключения в профиль с указанным именем}
    procedure AddConnectionProfile(AProfileName: string; AServerName: string; APort: Integer;
      ADataBase: string; AUserName: string; APassword: string; ACharSet: string);

    {Добавляет параметры подключения в профиль "по умолчанию"}
    procedure AddDefaultConnectionProfile(AServerName: string; APort: Integer;
      ADataBase: string; AUserName: string; APassword: string; ACharSet: string);

    {Получить подключение из пула по имени профиля}
    function GetConnection(AProfileName: string; ReadTran: PIBTransaction = nil; WriteTran: PIBTransaction = nil;
      ReadTranType: TTransactionType = trRCRO; WriteTranType: TTransactionType = trRCRW): TIBDatabase; overload;

    {Получить подключение из пула по указанным параметрам}
    function GetConnection(AServerName: string; APort: Integer; ADataBase: string;
      AUserName: string; APassword: string; ACharSet: string; ReadTran: PIBTransaction = nil;
      WriteTran: PIBTransaction = nil; ReadTranType: TTransactionType = trRCRO;
      WriteTranType: TTransactionType = trRCRW): TIBDatabase; overload;

    {Получить подключение из пула для профиля "по умолчанию"}
    function GetDefaultConnection(ReadTran: PIBTransaction = nil; WriteTran: PIBTransaction = nil;
      ReadTranType: TTransactionType = trRCRO; WriteTranType: TTransactionType = trRCRW): TIBDatabase;

    {Возвращает ненужное подключение обратно в пул}
    procedure ReturnConnection(FDB: TIBDatabase);

    { Возвращает количество подключений в пуле }
    function GetPoolSize: Integer;
  end;

  TfbBackupRestore = class(TObject)
  public
    {Осуществляет резервирование базы данных средствами Firebird Service-API}
    procedure BackupDatabaseOnServer(AServerName: string; APort: Integer; ADBName, ABackupFile,
      AUser, APassw: string; ABackupOptions: TFBBackupOptions; AProgressProc: TBackupRestoreProgressProc);

    {Осуществляет резервирование базы данных и копирует файл резервной копии с сервера
     на компьютер клиента.}
    procedure BackupDatabaseAndCopyFromServer(AServerName: string; APort: Integer; ADBName, ABackupFile,
      AUser, APassw: string; ABackupOptions: TFBBackupOptions; AProgressProc: TBackupRestoreProgressProc;
      ASourBackupFileOnServer, ADestBackupFileOnClient: string; TryDeleteSourBackupFile: Boolean);

    {Осуществляет восстановление базы данных средствами Firebird Service-API}
    procedure RestoreDatabaseOnServer(AServerName: string; APort: Integer; ADBName, ABackupFile,
      AUser, APassw: string; ARestoreOptions: TFBRestoreOptions; AProgressProc: TBackupRestoreProgressProc);

    {Копирует байл резервной копии на сервер и производит восстановление базы данных}
    procedure CopyBackupToServerAndRestoreDatabase(AServerName: string; APort: Integer; ADBName, ABackupFile,
      AUser, APassw: string; ARestoreOptions: TFBRestoreOptions; AProgressProc: TBackupRestoreProgressProc;
      ABackupFileOnClient, ABackupFileOnServer: string; TryDeleteBackupFileOnClient,
      TryDeleteBackupFileOnServer: Boolean);
  end;

  {Внимание! TFBIniFile - абстрактный класс. Расположение функций относительно друг друга играет очень
   большое значение. Не изменяйте положение функций.
   НЕЛЬЗЯ создавать объект таким способом: ini := TFBIniFile.Create(). Вместо этого
   следует использовать способ: ini := fb.Ini.CreateIni(False)}
  TFBIniFile = class(TObject)
  protected
    { == ВИРТУАЛЬНЫЕ АБСТРАКТНЫЕ ФУНКЦИИ. Не меняйте их! == }
    function GetUserName: string; virtual; abstract;
    procedure SetUserName(const Value: string); virtual; abstract;
    function GetComputerName: string; virtual; abstract;
    procedure SetComputerName(const Value: string); virtual; abstract;
    function GetPoolProfileName: string; virtual; abstract;
    procedure SetPoolProfileName(const Value: string); virtual; abstract;

    procedure DoWriteString(AnyComp, AnyUser: Boolean; const Section, Key, Value: String); virtual; abstract;
    procedure DoWriteInteger(AnyComp, AnyUser: Boolean; const Section, Key: string; Value: Integer); virtual; abstract;
    procedure DoWriteBool(AnyComp, AnyUser: Boolean; const Section, Key: string; Value: Boolean); virtual; abstract;
    procedure DoWriteFloat(AnyComp, AnyUser: Boolean; const Section, Key: string; Value: Double); virtual; abstract;
    procedure DoWriteDateTime(AnyComp, AnyUser: Boolean; const Section, Key: string; Value: TDateTime); virtual; abstract;
    procedure DoWriteDate(AnyComp, AnyUser: Boolean; const Section, Key: string; Value: TDateTime); virtual; abstract;
    procedure DoWriteTime(AnyComp, AnyUser: Boolean; const Section, Key: string; Value: TDateTime); virtual; abstract;

    function DoReadString(AnyComp, AnyUser: Boolean; const Section, Key, Default: String): string; virtual; abstract;
    function DoReadInteger(AnyComp, AnyUser: Boolean; const Section, Key: string; Default: Integer): Integer; virtual; abstract;
    function DoReadBool(AnyComp, AnyUser: Boolean; const Section, Key: string; Default: Boolean): Boolean; virtual; abstract;
    function DoReadFloat(AnyComp, AnyUser: Boolean; const Section, Key: string; Default: Double): Double; virtual; abstract;
    function DoReadDateTime(AnyComp, AnyUser: Boolean; const Section, Key: string; Default: TDateTime): TDateTime; virtual; abstract;
    function DoReadDate(AnyComp, AnyUser: Boolean; const Section, Key: string; Default: TDateTime): TDateTime; virtual; abstract;
    function DoReadTime(AnyComp, AnyUser: Boolean; const Section, Key: string; Default: TDateTime): TDateTime; virtual; abstract;

    procedure DoWriteStream(AnyComp, AnyUser: Boolean; const Section, Key: String; AStream: TStream); virtual; abstract;
    function DoReadStream(AnyComp, AnyUser: Boolean; const Section, Key: String; AStream: TStream): Boolean; virtual; abstract;

    procedure DoWriteText(AnyComp, AnyUser: Boolean; const Section, Key: String; Value: string); virtual; abstract;
    function DoReadText(AnyComp, AnyUser: Boolean; const Section, Key: String; Default: String): string; virtual; abstract;

    procedure DoWriteBinaryData(AnyComp, AnyUser: Boolean; const Section, Key: string; const Buffer; BufSize: Integer); virtual; abstract;
    function DoReadBinaryData(AnyComp, AnyUser: Boolean; const Section, Key: string; var Buffer; BufSize: Integer): Integer; virtual; abstract;

    procedure DoReadSection(AnyComp, AnyUser: Boolean; const Section: string; Strings: TStrings); virtual; abstract;
    procedure DoReadSections(AnyComp, AnyUser: Boolean; Strings: TStrings); virtual; abstract;
    procedure DoReadSectionValues(AnyComp, AnyUser: Boolean; const Section: string; Strings: TStrings); virtual; abstract;
    function DoSectionExists(AnyComp, AnyUser: Boolean; const Section: string): Boolean; virtual; abstract;
    function DoValueExists(AnyComp, AnyUser: Boolean; const Section, Key: string): Boolean; virtual; abstract;
    procedure DoEraseSection(AnyComp, AnyUser: Boolean; const Section: string); virtual; abstract;
    procedure DoDeleteKey(AnyComp, AnyUser: Boolean; const Section, Key: String); virtual; abstract;
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
    { Устанавливает параметры подключения к базе данных. Если указано имя профиля
      (свойство PoolProfileName), то использовать параметры не обязательно }
    procedure SetConnectionParams(AServerName: string; APort: Integer; ADataBase: string;
      AUserName: string; APassword: string; ACharSet: string); virtual; abstract;

    { Начало работы с объектом (вызывать - при массовом чтении / записи) }
    procedure BeginWork; virtual; abstract;

    { Окончание работы с объектом }
    procedure EndWork; virtual; abstract;

    { Позволяет указать имя пользователя (по умолчанию берется имя текущего пользователя Windows) }
    property UserName: string read GetUserName write SetUserName;

    { Позволяет указать имя компьютера (по умолчанию берется имя текущего компьютера) }
    property ComputerName: string read GetComputerName write SetComputerName;

    { Позволяет указать имя профиля из пула подключений (по умолчанию берется FBDefDB) }
    property PoolProfileName: string read GetPoolProfileName write SetPoolProfileName;
  end;

  { Вспомогательный класс для создания объектов TFBIniFile. Допольнительно содержит
    глобальную переменную DefIni }
  TfbIniCreator = class(TObject)
  private
    FDefIni: TFBIniFile;
  public
    {Глобальная переменная. Перед ее использованием необходимо вызвать процедуру CreateDefIni}
    property DefIni: TFBIniFile read FDefIni;

    {Создает объект TFBIniFile и возвращает ссылку на него.
     Внимание! Если Вы создали объект с помощью CreateIni, то должны самостоятельно
     его уничтожить (с помощью FreeIni)}
    function CreateIni(AFileName: string = FBIniDefFileName; PoolProfileName: string = FBDefDB; AlwaysConnected: Boolean = False): TFBIniFile; overload;

    {Создает объект TFBIniFile. Следует указать параметры подключения к БД}
    function CreateIni(AServerName: string; APort: Integer; ADataBase: string; AUserName: string; APassword: string;
      ACharSet: string; AFileName: string = FBIniDefFileName; AlwaysConnected: Boolean = False): TFBIniFile; overload;

    {Уничтожает заданный объект TFBIniFile}
    procedure FreeIni(Ini: TFBIniFile);

    {Создает глобальный объект DefIni}
    procedure CreateDefIni(AFileName: string = FBIniDefFileName; PoolProfileName: string = FBDefDB; AlwaysConnected: Boolean = False); overload;

    {Создает глобальный объект DefIni. Следует указать параметры подключения к БД}
    procedure CreateDefIni(AServerName: string; APort: Integer; ADataBase: string; AUserName: string; APassword: string;
      ACharSet: string; AFileName: string = FBIniDefFileName; AlwaysConnected: Boolean = False); overload;

    {Уничтожает объект DefIni}
    procedure FreeDefIni;

    destructor Destroy; override;
  end;

  {Абстрактный класс описания структуры таблицы}
  TfbTableDesc = class
  private {Виртуальные функции}
    procedure SetUseModifyDate(Value: TFBTriggerState); virtual; abstract;
    function GetUseModifyDate: TFBTriggerState; virtual; abstract;
  public {Виртуальные функции}

    { Добавить описание поля базы данных }
    procedure AddField(AName, AType, ADefault: string; NotNull: TFBNotNull); virtual; abstract;

    { Устанавливает первичный ключ таблицы }
    procedure SetPrimaryKey(AName, ConstraintFields: string); virtual; abstract;

    { Добавить индекс}
    procedure AddIndex(AName: string; IsUnique: Boolean;
      ASorting: TFBSorting; ConstraintFields: string); virtual; abstract;

    { Добавить выражение проверки для таблицы }
    procedure AddCheck(AName: string; ACheck: string); virtual; abstract;

    { Добавить триггер. В качестве TriggerName можно задавать пустую строку. В
      этом случае имя триггера будет сформировано автоматически. В качестве TriggerPos
      (порядок срабатывания триггера) рекомендуется значение > 0 }
    procedure AddTrigger(TriggerEventTime: TFBTriggerEventTime; TriggerEvents: TFBTriggerEvents;
      TriggerPos: Integer; TriggerState: TFBTriggerState; TriggerName: string;
      TriggerVarDesc, TriggerBody: string); virtual; abstract;

    { Добавляет триггер для автоинкремента }
    procedure AddAutoIncTrigger(ATriggerName, AFieldName, AGenName: string; CreateGenerator: Boolean); virtual; abstract;

    { Определяет, следует ли использовать MODIFYDATE }
    property UseModifyDateTrigger: TFBTriggerState read GetUseModifyDate write SetUseModifyDate;
  end;

  {Абстрактный класс описания структуры базы данных}
  TfbDataBaseDesc = class
  public {Виртуальные функции}
    { Номер версии }
    function GetVersion: Integer; virtual; abstract;

    { Добавляет описание домена }
    procedure AddDomain(AName, AType, ADefault: string; NotNull: TFBNotNull; ACheck: string); virtual; abstract;

    { Добавляет описание таблицы }
    function AddTable(TableName: string): TfbTableDesc; virtual; abstract;

    { Добавляет внешний ключ }
    procedure AddForeignKey(AName, TableName, ConstraintFields,
      RefTableName, RefConstraintFields: string); virtual; abstract;

    { Добавляет счетчик генератора }
    procedure AddGenerator(AName: string; StartValue: Int64); virtual; abstract;

    { Добавляет хранимую процедуру }
    procedure AddProcedure(AName, InFieldsDesc, OutFieldsDesc, VarDesc, Body: string); virtual; abstract;

    { Добавляет объект исключения "ERR" }
    procedure AddDefaultException; virtual; abstract;
  end;

  TfbDBStructCreator = class
  private
    FDefDBDesc: TfbDataBaseDesc;
  public
    constructor Create;
    destructor Destroy; override;
  public
    {Описание структуры БД по умолчанию (данная переменная нужна лишь для удобства
     программиста: Вам не потребуется объявлять собственную переменную для тех же целей)}
    property DefDBDesc: TfbDataBaseDesc read FDefDBDesc;

    {Создает объект TfbDataBaseDesc (описание структуры базы данных)}
    function CreateDataBaseDesc: TfbDataBaseDesc;

    {Уничтожает объект TfbDataBaseDesc}
    procedure FreeDataBaseDesc(dbDesc: TfbDataBaseDesc);

    {Создает объект DefDBDesc}
    procedure ReCreateDefDataBaseDesc;

    {Уничтожает объект DefDBDesc}
    procedure FreeDefDataBaseDesc;

    {Осуществляет проверку и необходимую коррекцию структуры базы данных}
    procedure CheckDataBaseStruct(fbDataBaseDesc: TfbDataBaseDesc; AServerName: string;
      APort: Integer; ADataBase: string; AUserName: string; APassword: string;
      ACharSet: string; LogProc: TFBLogEventsProc);

    {Осуществляет проверку и необходимую коррекцию структуры базы данных для DefDBDesc}
    procedure CheckDefDataBaseStruct(AServerName: string;
      APort: Integer; ADataBase: string; AUserName: string; APassword: string;
      ACharSet: string; LogProc: TFBLogEventsProc);
  end;

  TfbFieldsNamesValuesRec = record
    Names: array of string;
    Values: array of Variant;
  end;


  TfbUtils = class(TObject)
  private
    FPool: TfbPool;
    FBackupRestore: TfbBackupRestore;
    FIni: TfbIniCreator;
    FDBStruct: TfbDBStructCreator;
    function GetUserName: string;
    procedure SetUserName(const Value: string);
    function GetPassword: string;
    procedure SetPassword(const Value: string);
    function GetCodePage: string;
    procedure SetCodePage(const CodePage: string);
    procedure SetPort(APort: Integer);
    function GetPort: Integer;
    procedure InitFBUtils;
    function SplitFieldNamesValuesArray(FieldNamesValues: array of Variant; const ErrPrefix: string): TfbFieldsNamesValuesRec;
  public
    constructor Create;
    destructor Destroy; override;
  public
    {Пул подключений}
    property Pool: TfbPool read FPool;

    {Резервирование / восстановление}
    property br: TfbBackupRestore read FBackupRestore;

    {Работа с Ini-файлом}
    property Ini: TfbIniCreator read FIni;

    {Коррекция структуры базы даных (изменение метаинформации)}
    property DBStruct: TfbDBStructCreator read FDBStruct;

    {Имя пользователя для подключения к базе данных}
    property UserName: string read GetUserName write SetUserName;

    {Пароль пользователя для подключения к базе данных}
    property Password: string read GetPassword write SetPassword;

    {Кодовая страница (обычно одна и та же для всех баз данных}
    property CodePage: string read GetCodePage write SetCodePage;

    {TCP-порт Firebird}
    property Port: Integer read GetPort write SetPort;

    {Создает объект подключения к базе данных.
     Внимание! Не уничтожайте объект методом TIBDataBase.Free. Используйте fb.FreeConnection()}
    function CreateConnection(AServerName: string; APort: Integer; ADataBase: string;
      AUserName, APassword, ACodePage: string; TranType: TTransactionType = trDef;
      DoOpen: Boolean = True; AOwner: TComponent = nil): TIBDataBase;

    {Подключение к базе данных}
    procedure ConnectDB(FDB: TIBDatabase);

    {Отключение от базы данных}
    procedure DisconnectDB(FDB: TIBDatabase);

    {Удаление объекта подключения}
    procedure FreeConnection(FDB: TIBDatabase);

    {Создает тразакцию в соответствии с заданным TranType }
    function CreateTransaction(FDB: TIBDataBase; TranType: TTransactionType = trDef;
      AutoStart: Boolean = True; AOwner: TComponent = nil): TIBTransaction;

    {Создает набор данных TIBDataSet}
    function CreateDataSet(FDB: TIBDatabase; FTran: TIBTransaction; TranAutoStart: Boolean = True;
      AOwner: TComponent = nil): TIBDataSet;

    {Создает набор данных TIBDataSet и автоматически выполняет SELECT-запрос}
    function CreateAndOpenDataSet(FDB: TIBDatabase; FTran: TIBTransaction; SQL: string;
      ParamNames: array of string; ParamValues: array of Variant;
      AOwner: TComponent = nil): TIBDataSet;

    {Возвращает набор данных TIBDataSet по SQL-запросу. Если объект TIBDataSet не был создан,
     то создаёт его. Объекты TIBDataSet будут уничтожены автоматически при удалении транзакции, т.е.
     удалять объект TIBDataSet не обязательно! }
    function GetDataSet(FDB: TIBDatabase; FTran: TIBTransaction; SQL: string): TIBDataSet;

    {Возвращает набор данных TIBDataSet по SQL-запросу и автоматически выполняет SELECT-запрос}
    function GetAndOpenDataSet(FDB: TIBDatabase; FTran: TIBTransaction; SQL: string;
      ParamNames: array of string; ParamValues: array of Variant): TIBDataSet;

    {Создает набор данных TIBDataSet и выполняет SELECT-запрос к одной таблице}
    function CreateAndOpenTable(FDB: TIBDatabase; FTran: TIBTransaction;
      ATable, AFilter, AOrder: string;
      ParamNames: array of string; ParamValues: array of Variant;
      AOwner: TComponent = nil): TIBDataSet;

    {Выполняет указанный SQL-запрос}
    procedure ExecQuery(FDB: TIBDatabase; FTran: TIBTransaction; SQL: string;
      ParamNames: array of string; ParamValues: array of Variant);

    { Возвращает массив значений заданных полей указанной таблицы для первой найденной записи }
    function GetTableFieldValues(FDB: TIBDatabase; FTran: TIBTransaction; TableName: string; FieldNames: array of string;
      KeyFields: array of string; KeyValues: array of Variant; DefValues: array of Variant): OleVariant;

    { Возвращает (в формате TParamsRec) массив значений заданных полей указанной таблицы для первой найденной записи.
      ВНИМАНИЕ! Не пытайтесь получить значение поля в одном вызове с GetTableFieldNamedValues! Необходимо завести
      переменную типа TParamsRec и в неё присвоить результат функции GetTableFieldNamedValues }
    function GetTableFieldNamedValues(FDB: TIBDatabase; FTran: TIBTransaction; TableName: string; FieldNames: array of string;
      KeyFields: array of string; KeyValues: array of Variant; DefValues: array of Variant): TParamsRec;

    { Возвращает значение заданного поля указанной таблицы для первой найденной записи}
    function GetTableFieldValue(FDB: TIBDatabase; FTran: TIBTransaction; TableName: string; FieldName: string;
      KeyFields: array of string; KeyValues: array of Variant; DefValue: Variant): OleVariant;

    { Возвращает текущие дату / время на сервере Firebird }
    function GetCurrentDateTime(FDB: TIBDataBase; FTran: TIBTransaction): TDateTime;

    {Изменение записи в указанной таблице (названия полей чередуются со значениями
     через запятую, как это сделано в библиотеке SuperObject). Сохранить название
     процедуры UpdateRecord не удалось, из-за непонятливости компилятора}
    procedure UpdateRecordDB(FDB: TIBDataBase; FTran: TIBTransaction;
      TableName: string; KeyNamesValues: array of Variant;
      FieldNamesValues: array of Variant);

    {Изменение записи в указанной таблице}
    procedure UpdateRecord(FDB: TIBDataBase; FTran: TIBTransaction;
      TableName: string; KeyFields: array of string; KeyValues: array of Variant;
      FieldNames: array of string; AFieldValues: array of Variant);

    {Добавление записи в указанную таблицу}
    procedure InsertRecord(FDB: TIBDataBase; FTran: TIBTransaction;
      TableName: string; FieldNames: array of string; AFieldValues: array of Variant);

    {Добавление записи в указанную таблицу (более читабельная версия)}
    procedure InsertRecordDB(FDB: TIBDataBase; FTran: TIBTransaction;
      TableName: string; FieldNamesValues: array of Variant);

    {Удаление записи из указанной таблицу}
    procedure DeleteRecord(FDB: TIBDataBase; FTran: TIBTransaction;
      TableName: string; KeyFields: array of string; KeyValues: array of Variant);

    {Удаление записи из указанной таблицу (более читабельная версия)}
    procedure DeleteRecordDB(FDB: TIBDataBase; FTran: TIBTransaction;
      TableName: string; KeyNamesValues: array of Variant);

    {Обновляет или добавляет запись в указанную таблицу}
    procedure UpdateOrInsertRecord(FDB: TIBDataBase; FTran: TIBTransaction;
      TableName: string; FieldNames: array of string; AFieldValues: array of Variant;
      KeyFields: array of string);

    {Пересчет индексной статистики (используется EXECUTE BLOCK)}
    procedure RecomputeIndexStatistics(FDB: TIBDatabase);

    {Увеличивает значение генератора GeneratorName на IncValue и возвращает полученное значение.
     Вариант без указания транзакции для записи}
    function GenID(FDB: TIBDatabase; GeneratorName: string; IncValue: Integer = 1): Int64; overload;

    {Вариант с указанием транзакции для записи}
    function GenID(FDB: TIBDatabase; TranW: TIBTransaction; GeneratorName: string; IncValue: Integer = 1): Int64; overload;

    {Выполняет команду EXECUTE BLOCK и возвращает набор данных с полями OutFieldsDesc}
    function ExecuteBlock(FDB: TIBDataBase; FTran: TIBTransaction; OutFieldsDesc,
      VarDesc, Body: string): TIBDataSet; overload;

    {Аналогично function ExecuteBlock, но не возвращает никакого результата}
    procedure ExecuteBlock(FDB: TIBDataBase; FTran: TIBTransaction;
      VarDesc, Body: string); overload;

    {Производит очистку таблицы ATableName (для сборки мусора следует указать FTran=NIL)}
    procedure ClearTable(FDB: TIBDataBase; FTran: TIBTransaction; ATableName: string; AWhere: string = '';
      GarbageCollection: Boolean = True);

    {Возвращает версию библиотеки. Эту информацию имеет смысл использовать только
     если загружена библиотека FBUTILS.DLL. Для целей совместимости}
    function GetUtilsVersion: Integer;

    {Возвращает дескриптор загруженной библиотеки FBUTILS.DLL. Если она не используется
     или не загружена, то возвращает 0 }
    function GetLibHandle: THandle;

    procedure WriteToLog(AMessage: string; MsgType: Integer = 0);

    function FieldExists(FDB: TIBDatabase; FTran: TIBTransaction; const TableName: string; const FieldName: string): Boolean;
  public {Некоторые дополнительные функции}

    {Преобразует дату/время в строку в формате "yyyy-mm-dd hh:nn:ss.zzz"}
    function DateTimeToString(Value: TDateTime; UseMilliseconds: Boolean = True): string;

    {Преобразует дату в строку в формате "yyyy-mm-dd"}
    function DateToString(Value: TDateTime): string;

    {Преобразует время в строку в формате "hh:nn:ss.zzz"}
    function TimeToString(Value: TDateTime): string;
  end;



var
  { Глобальная переменная для работы с БД Firebird. Вам не требуется объявлять
    свои переменные, имеющие тип  TfbUtils.}
  fb: TfbUtils;

implementation

{$IFNDEF FBUTILSDLL}
uses fbUtilsBase, fbUtilsPool, fbUtilsBackupRestore, fbUtilsIniFiles,
  fbUtilsDBStruct, fbUtilsCheckDBStruct;
{$ENDIF}

{$IFDEF FBUTILSDLL}
const
  fbUtilsDLL = 'fbUtils.dll'; // Укажите свое имя DLL (например IBXFBUtils.dll) при необходимости
{$ENDIF}

resourcestring
  FBStrFBUtilsDLLNotFound = 'Модуль "%s" не загружен. ' + sLineBreak + 'В запуске приложения отказано!';

  FBStrFBUTILSDLLOptionNotFoundInDLL = 'Модуль "%s" загружен, однако он скомпилирован без '+
    'директивы FBUTILSDLL. В запуске приложения отказано!';

  FBStrDifferentPackages = 'Модуль "%s", либо вызывающий модуль скомпилированы без пакета "ibxpress", '+
    'либо используются разные версии библиотеки IBX. В запуске приложения отказано!';
var
  {$IFDEF FBUTILSDLL}
  fbUtilsHlib: THandle;
  {$ENDIF}

  FModuleName: string;

  {Процедуры и функции, реализованные в модуле fbUtilsBase}

  FBCreateConnection: function(AServerName: string; APort: Integer; ADataBase: string;
      AUserName, APassword, ACodePage: string; TranType: TTransactionType;
      DoOpen: Boolean; AOwner: TComponent; AModuleName: string): TIBDataBase;

  FBConnectDB: procedure(FDB: TIBDatabase; AModuleName: string);

  FBDisconnectDB: procedure(FDB: TIBDatabase; AModuleName: string);

  FBFreeConnection: procedure(FDB: TIBDatabase; AModuleName: string);

  FBCreateTransaction: function(FDB: TIBDataBase; TranType: TTransactionType; AutoStart: Boolean;
    AOwner: TComponent; AModuleName: string): TIBTransaction;

  FBCreateDataSet: function(FDB: TIBDatabase; FTran: TIBTransaction; TranAutoStart: Boolean;
    AOwner: TComponent; AModuleName: string): TIBDataSet;

  FBCreateAndOpenDataSet: function(FDB: TIBDatabase; FTran: TIBTransaction; SQL: string;
    ParamNames: array of string; ParamValues: array of Variant;
    AOwner: TComponent; AModuleName: string): TIBDataSet;

  FBGetDataSet: function(FDB: TIBDatabase; FTran: TIBTransaction; SQL: string; AModuleName: string): TIBDataSet;

  FBGetAndOpenDataSet: function(FDB: TIBDatabase; FTran: TIBTransaction; SQL: string;
    ParamNames: array of string; ParamValues: array of Variant; AModuleName: string): TIBDataSet;

  FBCreateAndOpenTable: function(FDB: TIBDatabase; FTran: TIBTransaction;
    ATable, AFilter, AOrder: string;
    ParamNames: array of string; ParamValues: array of Variant;
    AOwner: TComponent; AModuleName: string): TIBDataSet;

  FBExecQuery: procedure(FDB: TIBDatabase; FTran: TIBTransaction; SQL: string;
    ParamNames: array of string; ParamValues: array of Variant; AModuleName: string);

  FBGetTableFieldValues: function(fdb: TIBDatabase; tran: TIBTransaction; TableName: string; FieldNames: array of string;
    KeyFields: array of string; KeyValues: array of Variant; DefValues: array of Variant; AModuleName: string): OleVariant;

  FBUpdateRecordBase: procedure(FDB: TIBDataBase; FTran: TIBTransaction;
    TableName: string; KeyFields: array of string; KeyValues: array of Variant;
    FieldNames: array of string; AFieldValues: array of Variant; AModuleName: string);

  FBInsertRecordBase: procedure(FDB: TIBDataBase; FTran: TIBTransaction;
    TableName: string; FieldNames: array of string; AFieldValues: array of Variant;
    AModuleName: string);

  FBDeleteRecordBase: procedure(FDB: TIBDataBase; FTran: TIBTransaction;
    TableName: string; KeyFields: array of string; KeyValues: array of Variant;
    AModuleName: string);

  FBUpdateOrInsertRecordBase: procedure (FDB: TIBDataBase; FTran: TIBTransaction;
    TableName: string; FieldNames: array of string; AFieldValues: array of Variant;
    KeyFields: array of string; AModuleName: string);

  FBUtilsVersion: function : Integer;

  FBSetUserName: procedure (AUserName: string);
  FBGetUserName: function: string;
  FBSetPassword: procedure(APassword: string);
  FBGetPassword: function: string;
  FBSetCodePage: procedure(CodePage: string);
  FBGetCodePage: function: string;

  FBSetPort: procedure(APort: Integer);
  FBGetPort: function: Integer;

  FBRecomputeIndexStatistics: procedure(FDB: TIBDatabase; AModuleName: string);

  FBGenID: function(FDB: TIBDatabase; GeneratorName: string; IncValue: Integer; AModuleName: string): Int64;
  FBGenIDEx: function(FDB: TIBDatabase; FTran: TIBTransaction; GeneratorName: string; IncValue: Integer; AModuleName: string): Int64;

  FBExecuteBlockFunc: function(FDB: TIBDataBase; FTran: TIBTransaction; OutFieldsDesc,
    VarDesc, Body: string; AModuleName: string): TIBDataSet;

  FBExecuteBlockProc: procedure(FDB: TIBDataBase; FTran: TIBTransaction;
    VarDesc, Body: string; AModuleName: string);

  FBWriteToLog: procedure(AMessage: string; MsgType: Integer{TLDSLogType}; AModuleName: string);

  FBClearTable: procedure(FDB: TIBDataBase; FTran: TIBTransaction; ATableName, AWhere: string;
    GarbageCollection: Boolean; AModuleName: string);

  {$IFDEF FBUTILSDLL}
  FBPackagesIsCorrect: function(IBDBClass: TClass): Boolean;
  {$ENDIF}

  {Процедуры и функции, реализованные в модуле fbUtilsPool}

  FBPoolAddConnectionProfile: procedure(AProfileName: string; AServerName: string; APort: Integer;
   ADataBase: string; AUserName: string; APassword: string; ACharSet: string; AModuleName: string);

  FBPoolAddDefaultConnectionProfile: procedure(AServerName: string; APort: Integer;
   ADataBase: string; AUserName: string; APassword: string; ACharSet: string; AModuleName: string);

  FBPoolGetConnectionByProfile: function(AProfileName: string; ReadTran, WriteTran: PIBTransaction;
   ReadTranType, WriteTranType: TTransactionType; AModuleName: string): TIBDatabase;

  FBPoolGetConnectionByParams: function(AServerName: string; APort: Integer; ADataBase: string;
   AUserName: string; APassword: string; ACharSet: string; ReadTran, WriteTran: PIBTransaction;
   ReadTranType, WriteTranType: TTransactionType; AModuleName: string): TIBDatabase;

  FBPoolGetDefaultConnection: function(ReadTran, WriteTran: PIBTransaction;
   ReadTranType, WriteTranType: TTransactionType; AModuleName: string): TIBDatabase;

  FBPoolReturnConnection: procedure(FDB: TIBDatabase; AModuleName: string);

  FBPoolGetSize: function: Integer;

  {Процедуры и функции, реализованные в модуле fbUtilsBackupRestore}

  FBBackupDatabaseOnServer: procedure(AServerName: string; APort: Integer; ADBName, ABackupFile,
    AUser, APassw: string; ABackupOptions: TFBBackupOptions; AProgressProc: TBackupRestoreProgressProc; AModuleName: string);

  FBBackupDatabaseAndCopyFromServer: procedure(AServerName: string; APort: Integer; ADBName, ABackupFile,
    AUser, APassw: string; ABackupOptions: TFBBackupOptions; AProgressProc: TBackupRestoreProgressProc; ASourBackupFileOnServer,
    ADestBackupFileOnClient: string; TryDeleteSourBackupFile: Boolean; AModuleName: string);

  FBRestoreDatabaseOnServer: procedure(AServerName: string; APort: Integer; ADBName, ABackupFile,
    AUser, APassw: string; ARestoreOptions: TFBRestoreOptions; AProgressProc: TBackupRestoreProgressProc; AModuleName: string);

  FBCopyBackupToServerAndRestoreDatabase: procedure(AServerName: string; APort: Integer; ADBName, ABackupFile,
    AUser, APassw: string; ARestoreOptions: TFBRestoreOptions; AProgressProc: TBackupRestoreProgressProc;
    ABackupFileOnClient, ABackupFileOnServer: string; TryDeleteBackupFileOnClient, TryDeleteBackupFileOnServer: Boolean; AModuleName: string);


  {Процедуры и функции, реализованные в модуле fbUtilsIniFiles}

  FBCreateIniFile: function(AFileName: string; AlwaysConnected: Boolean): TFBIniFile;

  FBFreeIniFile: procedure(Ini: TFBIniFile);

  {Процедуры и функции, реализованные в модуле fbUtilsDBStruct}

  FBCreateDataBaseDesc: function: TfbDataBaseDesc;
  FBFreeDataBaseDesc: procedure(dbDesc: TfbDataBaseDesc);

  {Процедуры и функции, реализованные в модуле fbUtilsCheckDBStruct}
  FBCheckDBStruct: procedure(fbDataBaseDesc: TfbDataBaseDesc; AServerName: string;
    APort: Integer; ADataBase: string; AUserName: string; APassword: string;
    ACharSet: string; LogProc: TFBLogEventsProc; AModuleName: string);

{ TfbUtils }

procedure TfbUtils.ClearTable(FDB: TIBDataBase; FTran: TIBTransaction;
  ATableName, AWhere: string; GarbageCollection: Boolean);
begin
  FBClearTable(FDB, FTran, ATableName, AWhere, GarbageCollection, FModuleName);
end;

procedure TfbUtils.ConnectDB(FDB: TIBDatabase);
begin
  FBConnectDB(FDB, FModuleName);
end;

procedure TfbUtils.InitFBUtils;
begin
  { Определяем имя файла двоичного модуля }
  FModuleName := ExtractFileName(GetModuleName(HInstance));

  {$IFDEF FBUTILSDLL}
  fbUtilsHlib := LoadLibrary(PChar(fbUtilsDLL));

  if fbUtilsHlib = 0 then
  begin
    // Здесь можно добавить сообщение в какой-нибудь лог
    MessageBox(GetActiveWindow, PChar(Format(FBStrFBUtilsDLLNotFound, [fbUtilsDLL])), 'Error!', MB_ICONERROR);
    ExitProcess(0); // Выходим из приложения
  end else
  begin
    {Загружаем адреса функций, экспортируемых в fbUtilsBase.pas}
    @FBPackagesIsCorrect    := GetProcAddress(fbUtilsHlib, 'ibxFBPackagesIsCorrect');

    if not Assigned(FBPackagesIsCorrect) then
    begin
      MessageBox(GetActiveWindow, PChar(Format(FBStrFBUTILSDLLOptionNotFoundInDLL, [fbUtilsDLL])), 'Error!', MB_ICONERROR);
      ExitProcess(0); // Выходим из приложения
    end;

    if not FBPackagesIsCorrect(TIBDatabase) then
    begin
      MessageBox(GetActiveWindow, PChar(Format(FBStrDifferentPackages, [fbUtilsDLL])), 'Error!', MB_ICONERROR);
      ExitProcess(0); // Выходим из приложения
    end;

    @FBCreateConnection     := GetProcAddress(fbUtilsHlib, 'ibxFBCreateConnection');
    @FBConnectDB            := GetProcAddress(fbUtilsHlib, 'ibxFBConnectDB');
    @FBDisconnectDB         := GetProcAddress(fbUtilsHlib, 'ibxFBDisconnectDB');
    @FBFreeConnection       := GetProcAddress(fbUtilsHlib, 'ibxFBFreeConnection');
    @FBCreateTransaction    := GetProcAddress(fbUtilsHlib, 'ibxFBCreateTransaction');
    @FBCreateDataSet        := GetProcAddress(fbUtilsHlib, 'ibxFBCreateDataSet');
    @FBCreateAndOpenDataSet := GetProcAddress(fbUtilsHlib, 'ibxFBCreateAndOpenDataSet');
    @FBGetDataSet           := GetProcAddress(fbUtilsHlib, 'ibxFBGetDataSet');
    @FBGetAndOpenDataSet    := GetProcAddress(fbUtilsHlib, 'ibxFBGetAndOpenDataSet');
    @FBCreateAndOpenTable   := GetProcAddress(fbUtilsHlib, 'ibxFBCreateAndOpenTable');
    @FBExecQuery            := GetProcAddress(fbUtilsHlib, 'ibxFBExecQuery');
    @FBGetTableFieldValues  := GetProcAddress(fbUtilsHlib, 'ibxFBGetTableFieldValues');
    @FBUpdateRecordBase     := GetProcAddress(fbUtilsHlib, 'ibxFBUpdateRecordBase');
    @FBInsertRecordBase     := GetProcAddress(fbUtilsHlib, 'ibxFBInsertRecordBase');
    @FBDeleteRecordBase     := GetProcAddress(fbUtilsHlib, 'ibxFBDeleteRecordBase');
    @FBUpdateOrInsertRecordBase  := GetProcAddress(fbUtilsHlib, 'ibxFBUpdateOrInsertRecordBase');
    @FBUtilsVersion         := GetProcAddress(fbUtilsHlib, 'ibxFBUtilsVersion');
    @FBSetUserName          := GetProcAddress(fbUtilsHlib, 'ibxFBSetUserName');
    @FBGetUserName          := GetProcAddress(fbUtilsHlib, 'ibxFBGetUserName');
    @FBSetPassword          := GetProcAddress(fbUtilsHlib, 'ibxFBSetPassword');
    @FBGetPassword          := GetProcAddress(fbUtilsHlib, 'ibxFBGetPassword');
    @FBSetCodePage          := GetProcAddress(fbUtilsHlib, 'ibxFBCodePage');
    @FBGetCodePage          := GetProcAddress(fbUtilsHlib, 'ibxFBCodePage');
    @FBSetPort              := GetProcAddress(fbUtilsHlib, 'ibxFBSetPort');
    @FBGetPort              := GetProcAddress(fbUtilsHlib, 'ibxFBGetPort');
    @FBRecomputeIndexStatistics := GetProcAddress(fbUtilsHlib, 'ibxFBRecomputeIndexStatistics');
    @FBGenID                := GetProcAddress(fbUtilsHlib, 'ibxFBGenID');
    @FBGenIDEx              := GetProcAddress(fbUtilsHlib, 'ibxFBGenIDEx');
    @FBExecuteBlockFunc     := GetProcAddress(fbUtilsHlib, 'ibxFBExecuteBlockFunc');
    @FBExecuteBlockProc     := GetProcAddress(fbUtilsHlib, 'ibxFBExecuteBlockProc');
    @FBWriteToLog           := GetProcAddress(fbUtilsHlib, 'ibxFBWriteToLog');
    @FBClearTable           := GetProcAddress(fbUtilsHlib, 'ibxFBClearTable');

    {Загружаем адреса функций, экспортируемых в fbUtilsPool.pas}
    @FBPoolAddConnectionProfile         := GetProcAddress(fbUtilsHlib, 'ibxFBPoolAddConnectionProfile');
    @FBPoolAddDefaultConnectionProfile  := GetProcAddress(fbUtilsHlib, 'ibxFBPoolAddDefaultConnectionProfile');
    @FBPoolGetConnectionByProfile       := GetProcAddress(fbUtilsHlib, 'ibxFBPoolGetConnectionByProfile');
    @FBPoolGetConnectionByParams        := GetProcAddress(fbUtilsHlib, 'ibxFBPoolGetConnectionByParams');
    @FBPoolGetDefaultConnection         := GetProcAddress(fbUtilsHlib, 'ibxFBPoolGetDefaultConnection');
    @FBPoolReturnConnection             := GetProcAddress(fbUtilsHlib, 'ibxFBPoolReturnConnection');
    @FBPoolGetSize                      := GetProcAddress(fbUtilsHlib, 'ibxFBPoolGetSize');

    {Загружаем адреса функций, экспортируемых в fbUtilsBackupRestore.pas}
    @FBBackupDatabaseOnServer                := GetProcAddress(fbUtilsHlib, 'ibxFBBackupDatabaseOnServer');
    @FBBackupDatabaseAndCopyFromServer       := GetProcAddress(fbUtilsHlib, 'ibxFBBackupDatabaseAndCopyFromServer');
    @FBRestoreDatabaseOnServer               := GetProcAddress(fbUtilsHlib, 'ibxFBRestoreDatabaseOnServer');
    @FBCopyBackupToServerAndRestoreDatabase  := GetProcAddress(fbUtilsHlib, 'ibxFBCopyBackupToServerAndRestoreDatabase');

    {Загружаем адреса функций, экспортируемых в fbUtilsIniFiles.pas}
    @FBCreateIniFile  := GetProcAddress(fbUtilsHlib, 'ibxFBCreateIniFile');
    @FBFreeIniFile    := GetProcAddress(fbUtilsHlib, 'ibxFBFreeIniFile');

    {Загружаем адреса функций, экспортируемых в fbUtilsDBStruct.pas}
    @FBCreateDataBaseDesc := GetProcAddress(fbUtilsHlib, 'ibxFBCreateDataBaseDesc');
    @FBFreeDataBaseDesc   := GetProcAddress(fbUtilsHlib, 'ibxFBFreeDataBaseDesc');

    {Загружаем адреса функций, экспортируемых в fbUtilsCheckDBStruct.pas}
    @FBCheckDBStruct := GetProcAddress(fbUtilsHlib, 'ibxFBCheckDBStruct');
  end;

  {$ELSE}
    {Загружаем адреса функций, реализованных в fbUtilsBase.pas}
    @FBCreateConnection     := @fbUtilsBase.FBCreateConnection;
    @FBConnectDB            := @fbUtilsBase.FBConnectDB;
    @FBDisconnectDB         := @fbUtilsBase.FBDisconnectDB;
    @FBFreeConnection       := @fbUtilsBase.FBFreeConnection;
    @FBCreateTransaction    := @fbUtilsBase.FBCreateTransaction;
    @FBCreateDataSet        := @fbUtilsBase.FBCreateDataSet;
    @FBCreateAndOpenDataSet := @fbUtilsBase.FBCreateAndOpenDataSet;
    @FBGetDataSet           := @fbUtilsBase.FBGetDataSet;
    @FBGetAndOpenDataSet    := @fbUtilsBase.FBGetAndOpenDataSet;
    @FBCreateAndOpenTable   := @fbUtilsBase.FBCreateAndOpenTable;
    @FBExecQuery            := @fbUtilsBase.FBExecQuery;
    @FBGetTableFieldValues  := @fbUtilsBase.FBGetTableFieldValues;
    @FBUpdateRecordBase     := @fbUtilsBase.FBUpdateRecordBase;
    @FBInsertRecordBase     := @fbUtilsBase.FBInsertRecordBase;
    @FBDeleteRecordBase     := @fbUtilsBase.FBDeleteRecordBase;
    @FBUpdateOrInsertRecordBase := @fbUtilsBase.FBUpdateOrInsertRecordBase;
    @FBUtilsVersion         := @fbUtilsBase.FBUtilsVersion;
    @FBSetUserName          := @fbUtilsBase.FBSetUserName;
    @FBGetUserName          := @fbUtilsBase.FBGetUserName;
    @FBSetPassword          := @fbUtilsBase.FBSetPassword;
    @FBGetPassword          := @fbUtilsBase.FBGetPassword;
    @FBSetCodePage          := @fbUtilsBase.FBSetCodePage;
    @FBGetCodePage          := @fbUtilsBase.FBGetCodePage;
    @FBSetPort              := @fbUtilsBase.FBSetPort;
    @FBGetPort              := @fbUtilsBase.FBGetPort;
    @FBRecomputeIndexStatistics := @fbUtilsBase.FBRecomputeIndexStatistics;
    @FBGenID                := @fbUtilsBase.FBGenID;
    @FBGenIDEx              := @fbUtilsBase.FBGenIDEx;
    @FBExecuteBlockFunc     := @fbUtilsBase.FBExecuteBlockFunc;
    @FBExecuteBlockProc     := @fbUtilsBase.FBExecuteBlockProc;
    @FBWriteToLog           := @fbUtilsBase.FBWriteToLog;
    @FBClearTable           := @fbUtilsBase.FBClearTable;

    {Загружаем адреса функций, реализованных в fbUtilsPool.pas}
    @FBPoolAddConnectionProfile         := @fbUtilsPool.FBPoolAddConnectionProfile;
    @FBPoolAddDefaultConnectionProfile  := @fbUtilsPool.FBPoolAddDefaultConnectionProfile;
    @FBPoolGetConnectionByProfile       := @fbUtilsPool.FBPoolGetConnectionByProfile;
    @FBPoolGetConnectionByParams        := @fbUtilsPool.FBPoolGetConnectionByParams;
    @FBPoolGetDefaultConnection         := @fbUtilsPool.FBPoolGetDefaultConnection;
    @FBPoolReturnConnection             := @fbUtilsPool.FBPoolReturnConnection;
    @FBPoolGetSize                      := @fbUtilsPool.FBPoolGetSize;

    {Загружаем адреса функций, реализованных в fbUtilsBackupRestore.pas}
    @FBBackupDatabaseOnServer                := @fbUtilsBackupRestore.FBBackupDatabaseOnServer;
    @FBBackupDatabaseAndCopyFromServer       := @fbUtilsBackupRestore.FBBackupDatabaseAndCopyFromServer;
    @FBRestoreDatabaseOnServer               := @fbUtilsBackupRestore.FBRestoreDatabaseOnServer;
    @FBCopyBackupToServerAndRestoreDatabase  := @fbUtilsBackupRestore.FBCopyBackupToServerAndRestoreDatabase;

    {Загружаем адреса функций, реализованных в fbUtilsIniFiles.pas}
    @FBCreateIniFile  := @fbUtilsIniFiles.FBCreateIniFile;
    @FBFreeIniFile    := @fbUtilsIniFiles.FBFreeIniFile;

    {Загружаем адреса функций, реализованных в fbUtilsDBStruct.pas}
    @FBCreateDataBaseDesc := @fbUtilsDBStruct.FBCreateDataBaseDesc;
    @FBFreeDataBaseDesc   := @fbUtilsDBStruct.FBFreeDataBaseDesc;

    {Загружаем адреса функций, реализованных в fbUtilsCheckDBStruct.pas}
    @FBCheckDBStruct := @fbUtilsCheckDBStruct.FBCheckDBStruct;
  {$ENDIF}
end;


constructor TfbUtils.Create;
begin
  inherited; {на всякий случай}
  InitFBUtils;
  FPool := TfbPool.Create;
  FIni := TfbIniCreator.Create;
  FBackupRestore := TfbBackupRestore.Create;
  FDBStruct := TfbDBStructCreator.Create;
end;

function TfbUtils.CreateAndOpenDataSet(FDB: TIBDatabase; FTran: TIBTransaction;
  SQL: string; ParamNames: array of string; ParamValues: array of Variant;
  AOwner: TComponent): TIBDataSet;
begin
  Result := FBCreateAndOpenDataSet(FDB, FTran, SQL, ParamNames, ParamValues, AOwner, FModuleName);
end;

function TfbUtils.CreateAndOpenTable(FDB: TIBDatabase; FTran: TIBTransaction;
  ATable, AFilter, AOrder: string; ParamNames: array of string;
  ParamValues: array of Variant; AOwner: TComponent): TIBDataSet;
begin
  Result := FBCreateAndOpenTable(FDB, FTran, ATable, AFilter, AOrder,
    ParamNames, ParamValues, AOwner, FModuleName);
end;

function TfbUtils.CreateConnection(AServerName: string; APort: Integer; ADataBase: string;
      AUserName, APassword, ACodePage: string; TranType: TTransactionType;
      DoOpen: Boolean; AOwner: TComponent): TIBDataBase;
begin
  Result := FBCreateConnection(AServerName, APort, ADataBase, AUserName, APassword,
    ACodePage, TranType, DoOpen, AOwner, FModuleName);
end;

function TfbUtils.CreateDataSet(FDB: TIBDatabase; FTran: TIBTransaction;
  TranAutoStart: Boolean; AOwner: TComponent): TIBDataSet;
begin
  Result := FBCreateDataSet(FDB, FTran, TranAutoStart, AOwner, FModuleName);
end;

function TfbUtils.CreateTransaction(FDB: TIBDataBase; TranType: TTransactionType;
  AutoStart: Boolean; AOwner: TComponent): TIBTransaction;
begin
  Result := FBCreateTransaction(FDB, TranType, AutoStart, AOwner, FModuleName);
end;

function TfbUtils.DateTimeToString(Value: TDateTime;
  UseMilliseconds: Boolean): string;
begin
  if UseMilliseconds then
    Result := FormatDateTime('yyyy/mm/dd hh:nn:ss.zzz', Value, FBFormatSettings)
  else
    Result := FormatDateTime('yyyy/mm/dd hh:nn:ss', Value, FBFormatSettings)
end;

function TfbUtils.DateToString(Value: TDateTime): string;
begin
  Result := FormatDateTime('yyyy/mm/dd', Value, FBFormatSettings);
end;

procedure TfbUtils.DeleteRecord(FDB: TIBDataBase; FTran: TIBTransaction;
  TableName: string; KeyFields: array of string; KeyValues: array of Variant);
begin
  FBDeleteRecordBase(FDB, FTran, TableName, KeyFields, KeyValues, FModuleName);
end;

procedure TfbUtils.DeleteRecordDB(FDB: TIBDataBase; FTran: TIBTransaction;
  TableName: string; KeyNamesValues: array of Variant);
const
  ErrPrefix = 'TfbUtils.DeleteRecordDB error: ';
var
  FieldsRec: TfbFieldsNamesValuesRec;
begin
  FieldsRec := SplitFieldNamesValuesArray(KeyNamesValues, ErrPrefix);
  DeleteRecord(FDB, FTran, TableName, FieldsRec.Names, FieldsRec.Values);
end;

destructor TfbUtils.Destroy;
begin
  Pool.Free;
  FBackupRestore.Free;
  FIni.Free;
  FDBStruct.Free;

  {$IFDEF FBUTILSDLL}
  if fbUtilsHlib <> 0 then
    FreeLibrary(fbUtilsHlib);
  {$ENDIF}

  inherited;
end;

procedure TfbUtils.DisconnectDB(FDB: TIBDatabase);
begin
  FBDisconnectDB(FDB, FModuleName);
end;

procedure TfbUtils.ExecQuery(FDB: TIBDatabase; FTran: TIBTransaction;
  SQL: string; ParamNames: array of string; ParamValues: array of Variant);
begin
  FBExecQuery(FDB, FTran, SQL, ParamNames, ParamValues, FModuleName);
end;

function TfbUtils.ExecuteBlock(FDB: TIBDataBase; FTran: TIBTransaction;
  OutFieldsDesc, VarDesc, Body: string): TIBDataSet;
begin
  Result := FBExecuteBlockFunc(FDB, FTran, OutFieldsDesc, VarDesc, Body, FModuleName);
end;

procedure TfbUtils.ExecuteBlock(FDB: TIBDataBase; FTran: TIBTransaction;
  VarDesc, Body: string);
begin
  FBExecuteBlockProc(FDB, FTran, VarDesc, Body, FModuleName);
end;

function TfbUtils.FieldExists(FDB: TIBDatabase; FTran: TIBTransaction;
  const TableName, FieldName: string): Boolean;
var
  ds: TIBDataSet;
begin
  ds := CreateAndOpenDataSet(FDB, FTran, Format('SELECT FIRST 0 * FROM "%s"', [TableName]), [], []);
  try
    Result := Assigned(ds.FindField(FieldName));
  finally
    ds.Free;
  end;
end;

procedure TfbUtils.FreeConnection(FDB: TIBDatabase);
begin
  FBFreeConnection(FDB, FModuleName);
end;

function TfbUtils.GenID(FDB: TIBDatabase; GeneratorName: string;
  IncValue: Integer): Int64;
begin
  Result := FBGenID(FDB, GeneratorName, IncValue, FModuleName);
end;

function TfbUtils.GenID(FDB: TIBDatabase; TranW: TIBTransaction; GeneratorName: string;
  IncValue: Integer): Int64;
begin
  if Assigned(FBGenIDEx) then
    Result := FBGenIDEx(FDB, TranW, GeneratorName, IncValue, FModuleName)
  else
    raise Exception.Create('TfbUtils: FBGenIDEx = nil');
end;

function TfbUtils.GetAndOpenDataSet(FDB: TIBDatabase; FTran: TIBTransaction; SQL: string;
  ParamNames: array of string; ParamValues: array of Variant): TIBDataSet;
begin
  if Assigned(FBGetAndOpenDataSet) then
    Result := FBGetAndOpenDataSet(FDB, FTran, SQL, ParamNames, ParamValues, FModuleName)
  else
    raise Exception.Create('TfbUtils: FBGetAndOpenDataSet = nil');
end;

function TfbUtils.GetCodePage: string;
begin
  if Assigned(FBGetCodePage) then
    Result := FBGetCodePage
  else
    Result := FBRusCharSet;
end;

function TfbUtils.GetCurrentDateTime(FDB: TIBDataBase; FTran: TIBTransaction): TDateTime;
var
  ds: TIBDataSet;
begin
  ds := fb.GetAndOpenDataSet(FDB, FTran, 'SELECT CURRENT_TIMESTAMP FROM RDB$DATABASE', [], []);
  Result := ds.Fields[0].AsDateTime;
  ds.Close;
end;

function TfbUtils.GetDataSet(FDB: TIBDatabase; FTran: TIBTransaction; SQL: string): TIBDataSet;
begin
  if Assigned(FBGetDataSet) then
    Result := FBGetDataSet(FDB, FTran, SQL, FModuleName)
  else
    raise Exception.Create('TfbUtils: FBGetDataSet = nil');
end;

function TfbUtils.GetLibHandle: THandle;
begin
{$IFDEF FBUTILSDLL}
  Result := fbUtilsHlib;
{$ELSE}
  Result := 0;
{$ENDIF}
end;

function TfbUtils.GetPassword: string;
begin
  Result := FBGetPassword;
end;

function TfbUtils.GetPort: Integer;
begin
  if Assigned(FBGetPort) then
    Result := FBGetPort()
  else
    Result := 30050; // Для совместимости с ПТК АЗС 12
end;

function TfbUtils.GetTableFieldNamedValues(FDB: TIBDatabase;
  FTran: TIBTransaction; TableName: string; FieldNames,
  KeyFields: array of string; KeyValues,
  DefValues: array of Variant): TParamsRec;
var
  ResValues: Variant;
  I: Integer;
begin
  ResValues := GetTableFieldValues(FDB, FTran, TableName, FieldNames, KeyFields, KeyValues, DefValues);
  Result.Clear;
  if High(FieldNames) <> VarArrayHighBound(ResValues, 1) then
    raise Exception.Create('TfbUtils: GetTableFieldNamedValues: High(FieldNames) <> VarArrayHighBound(ResValues)');
  for I := 0 to High(FieldNames) do
    Result.SetParam(FieldNames[I], ResValues[I]);
end;

function TfbUtils.GetTableFieldValue(FDB: TIBDatabase; FTran: TIBTransaction; TableName,
  FieldName: string; KeyFields: array of string; KeyValues: array of Variant;
  DefValue: Variant): OleVariant;
var
  v: OleVariant;
begin
  if Assigned(FBGetTableFieldValues) then
  begin
    v := FBGetTableFieldValues(FDB, FTran, TableName, [FieldName], KeyFields, KeyValues, [DefValue], FModuleName);
    Result := v[0];
  end
  else
    raise Exception.Create('TfbUtils: FBGetTableFieldValues = nil');
end;

function TfbUtils.GetTableFieldValues(FDB: TIBDatabase; FTran: TIBTransaction; TableName: string;
  FieldNames, KeyFields: array of string; KeyValues, DefValues: array of Variant): OleVariant;
begin
  if Assigned(FBGetTableFieldValues) then
    Result := FBGetTableFieldValues(FDB, FTran, TableName, FieldNames, KeyFields, KeyValues, DefValues, FModuleName)
  else
    raise Exception.Create('TfbUtils: FBGetTableFieldValues = nil');
end;

function TfbUtils.GetUserName: string;
begin
  Result := FBGetUserName;
end;

function TfbUtils.GetUtilsVersion: Integer;
begin
  Result := FBUtilsVersion;
end;

procedure TfbUtils.InsertRecord(FDB: TIBDataBase; FTran: TIBTransaction;
  TableName: string; FieldNames: array of string;
  AFieldValues: array of Variant);
begin
  FBInsertRecordBase(FDB, FTran, TableName, FieldNames, AFieldValues, FModuleName)
end;

procedure TfbUtils.InsertRecordDB(FDB: TIBDataBase; FTran: TIBTransaction;
  TableName: string; FieldNamesValues: array of Variant);
const
  ErrPrefix = 'TfbUtils.InsertRecordDB error: ';
var
  FieldsRec: TfbFieldsNamesValuesRec;
begin
  FieldsRec := SplitFieldNamesValuesArray(FieldNamesValues, ErrPrefix);
  InsertRecord(FDB, FTran, TableName, FieldsRec.Names, FieldsRec.Values);
end;

procedure TfbUtils.RecomputeIndexStatistics(FDB: TIBDatabase);
begin
  FBRecomputeIndexStatistics(FDB, FModuleName);
end;

procedure TfbUtils.SetCodePage(const CodePage: string);
begin
  if Assigned(FBSetCodePage) then
    FBSetCodePage(CodePage);
end;

procedure TfbUtils.SetPassword(const Value: string);
begin
  FBSetPassword(Value);
end;

procedure TfbUtils.SetPort(APort: Integer);
begin
  if Assigned(FBSetPort) then
    FBSetPort(APort);
end;

procedure TfbUtils.SetUserName(const Value: string);
begin
  FBSetUserName(Value);
end;

function TfbUtils.SplitFieldNamesValuesArray(
  FieldNamesValues: array of Variant; const ErrPrefix: string): TfbFieldsNamesValuesRec;
var
  I, Idx: Integer;
begin
  if Odd(Length(FieldNamesValues)) then
    raise Exception.Create(ErrPrefix + 'wrong params count in NamesValues array');

  SetLength(Result.Names,  Length(FieldNamesValues) div 2);
  SetLength(Result.Values, Length(FieldNamesValues) div 2);

  Idx := 0;
  for I := 0 to High(Result.Names) do
  begin
    Result.Names[I]  := FieldNamesValues[Idx]; Inc(Idx);
    Result.Values[I] := FieldNamesValues[Idx]; Inc(Idx);
  end;
end;

function TfbUtils.TimeToString(Value: TDateTime): string;
begin
  Result := FormatDateTime('hh:nn:ss.zzz', Value, FBFormatSettings);
end;

procedure TfbUtils.UpdateOrInsertRecord(FDB: TIBDataBase; FTran: TIBTransaction;
  TableName: string; FieldNames: array of string;
  AFieldValues: array of Variant; KeyFields: array of string);
begin
  FBUpdateOrInsertRecordBase(FDB, FTran, TableName,
    FieldNames, AFieldValues, KeyFields, FModuleName);
end;

procedure TfbUtils.UpdateRecordDB(FDB: TIBDataBase; FTran: TIBTransaction;
  TableName: string; KeyNamesValues, FieldNamesValues: array of Variant);
const
  ErrPrefix = 'TfbUtils.UpdateRecordDB error: ';
var
  KeysRec: TfbFieldsNamesValuesRec;
  FieldsRec: TfbFieldsNamesValuesRec;
begin
  KeysRec   := SplitFieldNamesValuesArray(KeyNamesValues,   ErrPrefix);
  FieldsRec := SplitFieldNamesValuesArray(FieldNamesValues, ErrPrefix);

  UpdateRecord(FDB, FTran, TableName, KeysRec.Names, KeysRec.Values, FieldsRec.Names, FieldsRec.Values);
end;

procedure TfbUtils.WriteToLog(AMessage: string; MsgType: Integer);
begin
  if Assigned(FBWriteToLog) then
    FBWriteToLog(AMessage, MsgType, FModuleName);
end;

procedure TfbUtils.UpdateRecord(FDB: TIBDataBase; FTran: TIBTransaction;
  TableName: string; KeyFields: array of string;
  KeyValues: array of Variant; FieldNames: array of string;
  AFieldValues: array of Variant);
begin
  FBUpdateRecordBase(FDB, FTran, TableName, KeyFields,
    KeyValues, FieldNames, AFieldValues, FModuleName);
end;

{ TfbPool }

procedure TfbPool.AddConnectionProfile(AProfileName, AServerName: string;
  APort: Integer; ADataBase, AUserName, APassword, ACharSet: string);
begin
  FBPoolAddConnectionProfile(AProfileName, AServerName,
    APort, ADataBase, AUserName, APassword, ACharSet, FModuleName);
end;

procedure TfbPool.AddDefaultConnectionProfile(AServerName: string;
  APort: Integer; ADataBase, AUserName, APassword, ACharSet: string);
begin
  FBPoolAddDefaultConnectionProfile(AServerName,
    APort, ADataBase, AUserName, APassword, ACharSet, FModuleName);
end;

function TfbPool.GetConnection(AServerName: string; APort: Integer; ADataBase,
  AUserName, APassword, ACharSet: string; ReadTran, WriteTran: PIBTransaction;
  ReadTranType, WriteTranType: TTransactionType): TIBDatabase;
begin
  Result := FBPoolGetConnectionByParams(AServerName, APort, ADataBase,
    AUserName, APassword, ACharSet, ReadTran, WriteTran,
    ReadTranType, WriteTranType, FModuleName);
end;

function TfbPool.GetConnection(AProfileName: string; ReadTran,
  WriteTran: PIBTransaction; ReadTranType,
  WriteTranType: TTransactionType): TIBDatabase;
begin
  Result := FBPoolGetConnectionByProfile(AProfileName, ReadTran,
    WriteTran, ReadTranType, WriteTranType, FModuleName);
end;

function TfbPool.GetDefaultConnection(ReadTran, WriteTran: PIBTransaction;
  ReadTranType, WriteTranType: TTransactionType): TIBDatabase;
begin
  Result := FBPoolGetDefaultConnection(ReadTran, WriteTran,
    ReadTranType, WriteTranType, FModuleName);
end;

function TfbPool.GetPoolSize: Integer;
begin
  Result := FBPoolGetSize;
end;

procedure TfbPool.ReturnConnection(FDB: TIBDatabase);
begin
  FBPoolReturnConnection(FDB, FModuleName);
end;


{ TfbBackupRestore }

procedure TfbBackupRestore.BackupDatabaseAndCopyFromServer(AServerName: string;
  APort: Integer; ADBName, ABackupFile, AUser, APassw: string;
  ABackupOptions: TFBBackupOptions; AProgressProc: TBackupRestoreProgressProc;
  ASourBackupFileOnServer, ADestBackupFileOnClient: string;
  TryDeleteSourBackupFile: Boolean);
begin
  FBBackupDatabaseAndCopyFromServer(AServerName, APort, ADBName, ABackupFile,
    AUser, APassw, ABackupOptions, AProgressProc, ASourBackupFileOnServer,
    ADestBackupFileOnClient, TryDeleteSourBackupFile, FModuleName);
end;

procedure TfbBackupRestore.BackupDatabaseOnServer(AServerName: string;
  APort: Integer; ADBName, ABackupFile, AUser, APassw: string;
  ABackupOptions: TFBBackupOptions; AProgressProc: TBackupRestoreProgressProc);
begin
  FBBackupDatabaseOnServer(AServerName, APort, ADBName, ABackupFile,
    AUser, APassw, ABackupOptions, AProgressProc, FModuleName);
end;

procedure TfbBackupRestore.CopyBackupToServerAndRestoreDatabase(
  AServerName: string; APort: Integer; ADBName, ABackupFile, AUser,
  APassw: string; ARestoreOptions: TFBRestoreOptions;
  AProgressProc: TBackupRestoreProgressProc; ABackupFileOnClient,
  ABackupFileOnServer: string; TryDeleteBackupFileOnClient,
  TryDeleteBackupFileOnServer: Boolean);
begin
  FBCopyBackupToServerAndRestoreDatabase(AServerName, APort, ADBName, ABackupFile,
    AUser, APassw, ARestoreOptions, AProgressProc, ABackupFileOnClient,
    ABackupFileOnServer, TryDeleteBackupFileOnClient, TryDeleteBackupFileOnServer, FModuleName);
end;

procedure TfbBackupRestore.RestoreDatabaseOnServer(AServerName: string;
  APort: Integer; ADBName, ABackupFile, AUser, APassw: string;
  ARestoreOptions: TFBRestoreOptions;
  AProgressProc: TBackupRestoreProgressProc);
begin
  FBRestoreDatabaseOnServer(AServerName, APort, ADBName, ABackupFile,
    AUser, APassw, ARestoreOptions, AProgressProc, FModuleName);
end;

{ TFBIniFile }

function TFBIniFile.ReadBool(const Key: string; Default: Boolean): Boolean;
begin
  Result := ReadBool(FBIniDefSection, Key, Default);
end;

function TFBIniFile.ReadDateTime(const Key: string;
  Default: TDateTime): TDateTime;
begin
  Result := ReadDateTime(FBIniDefSection, Key, Default);
end;

function TFBIniFile.ReadFloat(const Key: string; Default: Double): Double;
begin
  Result := ReadFloat(FBIniDefSection, Key, Default);
end;

function TFBIniFile.ReadInteger(const Key: string; Default: Integer): Integer;
begin
  Result := ReadInteger(FBIniDefSection, Key, Default);
end;

function TFBIniFile.ReadString(const Key, Default: String): string;
begin
  Result := ReadString(FBIniDefSection, Key, Default);
end;

procedure TFBIniFile.WriteBool(const Key: string; Value: Boolean);
begin
  WriteBool(FBIniDefSection, Key, Value);
end;

procedure TFBIniFile.WriteDateTime(const Key: string; Value: TDateTime);
begin
  WriteDateTime(FBIniDefSection, Key, Value);
end;

procedure TFBIniFile.WriteFloat(const Key: string; Value: Double);
begin
  WriteFloat(FBIniDefSection, Key, Value);
end;

procedure TFBIniFile.WriteInteger(const Key: string; Value: Integer);
begin
  WriteInteger(FBIniDefSection, Key, Value);
end;

procedure TFBIniFile.WriteString(const Key, Value: String);
begin
  WriteString(FBIniDefSection, Key, Value);
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

{ TfbIniCreator }

function TfbIniCreator.CreateIni(AFileName: string = FBIniDefFileName; PoolProfileName: string = FBDefDB; AlwaysConnected: Boolean = False): TFBIniFile;
begin
  Result := FBCreateIniFile(AFileName, AlwaysConnected);
  Result.PoolProfileName := PoolProfileName;
end;

procedure TfbIniCreator.CreateDefIni(AServerName: string; APort: Integer; ADataBase,
  AUserName, APassword, ACharSet: string; AFileName: string; AlwaysConnected: Boolean);
begin
  FreeDefIni;
  FDefIni := CreateIni(AServerName, APort, ADataBase, AUserName, APassword, ACharSet, AFileName, AlwaysConnected);
end;

function TfbIniCreator.CreateIni(AServerName: string; APort: Integer; ADataBase,
  AUserName, APassword, ACharSet: string; AFileName: string; AlwaysConnected: Boolean): TFBIniFile;
begin
  Result := FBCreateIniFile(AFileName, AlwaysConnected);
  Result.SetConnectionParams(AServerName, APort, ADataBase, AUserName, APassword, ACharSet);
end;

destructor TfbIniCreator.Destroy;
begin
  FreeDefIni;
  inherited;
end;

procedure TfbIniCreator.FreeDefIni;
begin
  if Assigned(DefIni) then
  begin
    FreeIni(DefIni);
    FDefIni := nil;
  end;
end;

procedure TfbIniCreator.FreeIni(Ini: TFBIniFile);
begin
  FBFreeIniFile(Ini);
end;

procedure TfbIniCreator.CreateDefIni(AFileName: string = FBIniDefFileName; PoolProfileName: string = FBDefDB; AlwaysConnected: Boolean = False);
begin
  FreeDefIni;
  FDefIni := CreateIni(AFileName, PoolProfileName, AlwaysConnected);
end;

{ TfbDBStructCreator }

procedure TfbDBStructCreator.CheckDataBaseStruct(fbDataBaseDesc: TfbDataBaseDesc;
  AServerName: string; APort: Integer; ADataBase, AUserName, APassword,
  ACharSet: string; LogProc: TFBLogEventsProc);
begin
  FBCheckDBStruct(fbDataBaseDesc, AServerName, APort, ADataBase, AUserName, APassword, ACharSet, LogProc, FModuleName);
end;

procedure TfbDBStructCreator.CheckDefDataBaseStruct(AServerName: string;
  APort: Integer; ADataBase, AUserName, APassword, ACharSet: string; LogProc: TFBLogEventsProc);
begin
  CheckDataBaseStruct(FDefDBDesc, AServerName, APort, ADataBase, AUserName, APassword, ACharSet, LogProc);
end;

constructor TfbDBStructCreator.Create;
begin
  ReCreateDefDataBaseDesc;
end;

function TfbDBStructCreator.CreateDataBaseDesc: TfbDataBaseDesc;
begin
  Result := FBCreateDataBaseDesc;
end;

destructor TfbDBStructCreator.Destroy;
begin
  FreeDefDataBaseDesc;
  inherited;
end;

procedure TfbDBStructCreator.ReCreateDefDataBaseDesc;
begin
  FreeDefDataBaseDesc; // На всякий случай
  FDefDBDesc := CreateDataBaseDesc();
end;

procedure TfbDBStructCreator.FreeDataBaseDesc(dbDesc: TfbDataBaseDesc);
begin
  FBFreeDataBaseDesc(dbDesc);
end;

procedure TfbDBStructCreator.FreeDefDataBaseDesc;
begin
  if Assigned(FDefDBDesc) then
  begin
    FreeDataBaseDesc(FDefDBDesc);
    FDefDBDesc := nil;
  end;
end;

initialization
  fb := TfbUtils.Create;
finalization
  fb.Free;
end.
