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
{ Модуль fbTypes - содержит объявление типов и констант для библиотеки fbUtils}
{ (c) 2012 Логинов Дмитрий Сергеевич                                          }
{ Последнее обновление: 30.04.2012                                            }
{ Протестировано на D7, D2007, D2010, D-XE2                                   }
{ Адрес сайта: http://loginovprojects.ru/                                     }
{ e-mail: loginov_d@inbox.ru                                                  }
{                                                                             }
{ *************************************************************************** }

{$IFDEF FPC}
{$MODE DELPHI}{$H+}{$CODEPAGE UTF8}
{$ENDIF}

unit fbTypes;

interface

uses
  SysUtils, Classes, IBDatabase;

type
  PIBTransaction = ^TIBTransaction;

  { Типы транзакции. Перечислены только наиболее часто используемые типы
    транзакций. Чаще всего следует использовать trDef (соответствует trRCRW).
    Для построения отчетов рекомендуется использовать trSSRW, поскольку гарантируется,
    что в отчет не попадую никакие новые данные, которые могли появиться в БД
    после старта транзакции. При необходимости Вы можете добавить дополнительные
    типы транзакции. }
  TTransactionType = (
    trNone,  // Транзакция отсутствует
    trDef,   // "Обычная" транзакция, то же, что и trRCRW
    trRCRO,  // READ COMMITED READ ONLY
    trRCRW,  // READ COMMITED READ WRITE (NO WAIT)
    trRCRWW, // READ COMMITED READ WRITE, WAIT
    trSSRW); // SNAP SHOT READ WRITE

  {callback-процедура для выполнения индикации резервирования/восстановления БД}
  TBackupRestoreProgressProc = procedure(ALastMsg: string; var Stop: Boolean) of object;

  {Опции резервирования БД Firebird}
  TFBBackupOption = (IgnoreChecksums, IgnoreLimbo, MetadataOnly, NoGarbageCollection,
    OldMetadataDesc, NonTransportable, ConvertExtTables);
  TFBBackupOptions = set of TFBBackupOption;

  {Опции восстановления БД Firebird}
  TFBRestoreOption = (DeactivateIndexes, NoShadow, NoValidityCheck, OneRelationAtATime,
    Replace, CreateNewDB, UseAllSpace, ValidationCheck);
  TFBRestoreOptions = set of TFBRestoreOption;

  TFBConnectionParams = record
    cpServerName: string; // Имя сервера, на котором запущена СУБД Firebird (или его IP-адрес)
    cpPort: Integer;      // Порт, на который настроена СУБД Firebird. По умолчанию = 3050
    cpDataBase: string;   // Полное имя файла БД (относительно серверного компьютера) или его алиас
    cpUserName: string;   // Имя пользователя для подключения к БД. По умолчанию =  SYSDBA
    cpPassword: string;   // Пароль для подключения к БД. По умолчанию =  masterkey
    cpCharSet: string;    // Кодовая страница, используемая для подключения к БД.
                          // Рекомендуется указывать ту же самую страницу, с помощью
                          // которой была создана база данных. Пример: WIN1251
  end;

  {Константы для модуля fbUtilsDBStruct.pas}
  TFBNotNull = (CanNull, NotNull);                     // Константы, определяющие, разрешен ли NOT NULL или нет
  TFBSorting = (Ascending, Descending);                // Сортировка для индексов: по возрастанию, по убыванию
  TFBTriggerEventTime = (trBefore, trAfter);           // Момент срабатывания триггера: до модификации / после модификации
  TFBTriggerEvent = (trInsert, trUpdate, trDelete);    // События, на которые срабатывает триггер
  TFBTriggerEvents = set of TFBTriggerEvent;           // Множество события, на которые должен срабатывать триггер
  TFBTriggerState = (trsNone, trsActive, trsInactive); // Состояние триггера: отсутствует / активное / неактивное

  { События для записи в лог. Совместимы с TLDSLogger }
  TFBLogEvent = (tlpNone, tlpInformation, tlpError, tlpCriticalError,
                 tlpWarning, tlpEvent, tlpDebug, tlpOther);

  { callback-процедура для логгирования событий }
  TFBLogEventsProc = procedure(Msg: string; LogEvent: TFBLogEvent);

const
  FBDefDB  = 'Default Database Profile'; // Имя профиля для базы данных по умолчанию
  FBDefServer   = '127.0.0.1'; // Сервер по умолчанию
  FBLocalhostIP = '127.0.0.1';  
  FBLocalServer = '';          // Для LOCAL-подключения имя сервера не указывается
  FBDefPort     =  3050;       // Порт по умолчанию
  FBLocalPort   = 0;           // Для LOCAL-подключения порт не указывается
  FBDefUser     = 'SYSDBA';    // Имя пользователя Firebird по умолчанию
  FBDefPassword = 'masterkey'; // Пароль пользователя Firebird по умолчанию

  FBTrustUser   = '';          // Имя пользователя для идентификации средствами Windows
  FBTrustPassword = '';

  FBDefCharSet  = '';          // Кодировка по умолчанию (по умолчанию - не задана)
  FBRusCharSet  = 'WIN1251';   // Константа для русской кодовой страницы
  FBUTF8CharSet  = 'UTF8';
  FBDefPageSize = 8192;        // Размер страницы БД по умолчанию

  FBCreateDBCharset =  {$IFDEF FB_DEF_WIN1251}FBRusCharSet{$ELSE}FBUTF8CharSet{$ENDIF};

  FBDefParamCheck = True;      // По умолчанию проверка параметров - ВКЛЮЧЕНА (при отключении тесты не проходят)

  FBPoolConnectionMaxTime = 60 * 5; // Максимальное время жизни неиспользуемых подключений в пуле, сек

  FBDefBackupOptions: TFBBackupOptions = [];              // Опции резервирования БД по умолчанию
  FBDefRestoreOptions: TFBRestoreOptions = [CreateNewDB]; // Опции восстановления БД по умолчанию

  {Константы для модуля fbUtilsIniFiles}
  FBIniTableConfigParams = 'CONFIGPARAMS'; // Таблица с конфигурационной информацией
  FBIniFieldFileName     = 'FILENAME';     // Имя INI-файла файла (по умолчанию - CONF)
  FBIniDefFileName       = 'CONF';         // Имя файла по умолчанию
  FBIniFieldComputerName = 'COMPUTERNAME'; // Имя компьютера, на котором должен дейстовать параметр
  FBIniFieldUserName     = 'USERNAME';     // Имя пользователя, для которого предназначен данный параметр
  FBIniFieldSectionName  = 'SECTIONNAME';  // Наименование секции (аналогично ини-файлам)
  FBIniDefSection        = 'PARAMS';       // Значение для поля "Наименование секции" по умолчанию

  FBIniFieldParamName    = 'PARAMNAME';    // Наименование параметра
  FBIniFieldParamValue   = 'PARAMVALUE';   // Значение параметра
  FBIniFieldParamBlob    = 'PARAMBLOB';    // BLOB-значение параметра (для больших двоичных объектов)
  FBIniFieldParamBlobHash = 'PARAMBLOBHASH'; // Строковое поле для хранения хэша двоичного объекта (hash1$hash2)
  FBIniFieldModifyDate   = 'MODIFYDATE';   // Дата и время изменения записи
  FBIniFieldModifyUser   = 'MODIFYUSER';   // Имя пользователя, который внес изменения
  FBIniCheckBlobHash     = True;           // Определяет, следует ли проверять хэш для BLOB-полей



var
  { Настройки форматирования даты/времени и вещественных чисел }
  FBFormatSettings: TFormatSettings;
  TransactionParams: array[TTransactionType] of string =
    ('',
     'read_committed'#13#10'rec_version'#13#10'nowait',  // READ COMMITED READ WRITE (NO WAIT)
     'read_committed'#13#10'rec_version'#13#10'nowait'#13#10'read', // READ COMMITED READ ONLY
     'read_committed'#13#10'rec_version'#13#10'nowait',  // READ COMMITED READ WRITE (NO WAIT)
     'read_committed'#13#10'rec_version'#13#10'wait',    // READ COMMITED READ WRITE, WAIT
     'concurrency'#13#10'nowait');                       // SNAP SHOT READ WRITE

implementation

procedure FBUpdateFormatSettings;
begin
  GetLocaleFormatSettings(0, FBFormatSettings);
  FBFormatSettings.LongDateFormat := 'yyyy/mm/dd';
  FBFormatSettings.ShortDateFormat := 'yyyy/mm/dd';
  FBFormatSettings.LongTimeFormat := 'hh:nn:ss.zzz';
  FBFormatSettings.ShortTimeFormat := 'hh:nn:ss.zzz';
  FBFormatSettings.DateSeparator := '-';
  FBFormatSettings.TimeSeparator := ':';
  FBFormatSettings.DecimalSeparator := '.';
end;

initialization
  FBUpdateFormatSettings;
finalization

end.
