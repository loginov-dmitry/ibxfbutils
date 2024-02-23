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
{ Модуль fbUtilsPool - содержит пул подключений для библиотеки fbUtils        }
{ (c) 2012 Логинов Дмитрий Сергеевич                                          }
{ Последнее обновление: 30.04.2012                                            }
{ Протестировано на D7, D2007, D2010, D-XE2                                   }
{ Адрес сайта: http://loginovprojects.ru/                                     }
{ e-mail: loginov_d@inbox.ru                                                  }
{                                                                             }
{ *************************************************************************** }

unit fbUtilsPool;

interface

uses
{$IFnDEF FPC}
  Windows,
{$ELSE}
  {$IFDEF MSWINDOWS}Windows, {$ENDIF}LCLIntf, LCLType, LMessages,
{$ENDIF}
  SysUtils, Classes, SyncObjs, DateUtils, fbUtilsBase, fbSomeFuncs, IBDatabase, IBCustomDataSet,
  fbTypes, IniFiles, LDSLogger, TimeIntervals;

type

  TFBConnectionPool = class(TBaseObject)
  private
    DBPoolList: TList; // Пул подключений к базе данных
    DBPoolCS: TCriticalSection;
    FProfileList: THashedStringList; // Список зарегистрированных профилей
    FLastLogTestConnectTime: TDateTime; // Время последнего логирования скорости операции TestConnected

    procedure ClearPool;

    {Очищает список профилей}
    procedure ClearConnectionProfiles;

    {Удаляет из пула старые подключения}
    procedure DeleteOldConnections;

  public { Конструктор и деструктор }

    constructor Create;
    destructor Destroy; override;

  public { Настройка профилей с параметрами подключения к БД }

    {Добавляет новый профиль подключения, т.е. набор параметров которому поставлено
     в соответствие некоторое имя. Пример вызова:
     AddConnectionProfile(FBDefDB, FBDefServer, FBDefPort, 'C:\DB\MyDB.FDB', FBDefUser, FBDefPassword, FBRusCharSet);
     Вместо FBDefDB можно указывать любое другое имя профиля. }
    procedure AddConnectionProfile(AProfileName: string; AServerName: string; APort: Integer;
      ADataBase: string; AUserName: string; APassword: string; ACharSet: string);

    {Вызывает метод AddConnectionProfile с профилем FBDefDB}
    procedure AddDefaultConnectionProfile(AServerName: string; APort: Integer;
      ADataBase: string; AUserName: string; APassword: string; ACharSet: string);

  public { Подключение к БД и отключение. Работа с пулом. }

    {Возвращает подключение, параметры которого ранее были сохранены в профиле AProfileName. Пример:
     FDB := GetConnection('MyDB', ReadTran, WriteTran, trRCRO, trRCRW)
     2018-08-30 - транзакция для чтения создаётся в любом случае - trRCRO }
    function GetConnection(AProfileName: string; ReadTran, WriteTran: PIBTransaction;
      ReadTranType, WriteTranType: TTransactionType; AModuleName: string): TIBDatabase; overload;

    {Возвращает подключение в соответствии с заданными параметрами. Пример:
     FDB := GetConnection(FBDefServer, FBDefPort, 'C:\DB\MyDB.FDB', FBDefUser, FBDefPassword,
       FBRusCharSet, ReadTran, WriteTran, trRCRO, trRCRW)}
    function GetConnection(AServerName: string; APort: Integer; ADataBase: string;
      AUserName: string; APassword: string; ACharSet: string; ReadTran, WriteTran: PIBTransaction;
      ReadTranType, WriteTranType: TTransactionType; AModuleName: string): TIBDatabase; overload;

    { Возвращает подключение для базы данных, для которой был настроен профиль FBDefDB
      Используются типы транзакций: trRCRO и trRCRW }
    function GetDefaultConnection(ReadTran, WriteTran: PIBTransaction; ReadTranType,
    WriteTranType: TTransactionType; AModuleName: string): TIBDatabase;

    { Кладет подключение обратно в пул }
    procedure ReturnConnection(FDB: TIBDatabase);

    { Возвращает количество подключений в пуле }
    function GetPoolSize: Integer;

    class procedure FreePool;
  end;

procedure FBPoolAddConnectionProfile(AProfileName: string; AServerName: string; APort: Integer;
  ADataBase: string; AUserName: string; APassword: string; ACharSet: string; AModuleName: string);

procedure FBPoolAddDefaultConnectionProfile(AServerName: string; APort: Integer;
  ADataBase: string; AUserName: string; APassword: string; ACharSet: string; AModuleName: string);

function FBPoolGetConnectionByProfile(AProfileName: string; ReadTran, WriteTran: PIBTransaction;
  ReadTranType, WriteTranType: TTransactionType; AModuleName: string): TIBDatabase;

function FBPoolGetConnectionByParams(AServerName: string; APort: Integer; ADataBase: string;
  AUserName: string; APassword: string; ACharSet: string; ReadTran, WriteTran: PIBTransaction;
  ReadTranType, WriteTranType: TTransactionType; AModuleName: string): TIBDatabase;

function FBPoolGetDefaultConnection(ReadTran, WriteTran: PIBTransaction;
  ReadTranType, WriteTranType: TTransactionType; AModuleName: string): TIBDatabase;

procedure FBPoolReturnConnection(FDB: TIBDatabase; AModuleName: string);

function FBPoolGetSize: Integer;

{$IFDEF FBUTILSDLL} // Замечания по директиве смотрите в модуле fbUtilsBase.pas
exports
  FBPoolAddConnectionProfile name 'ibxFBPoolAddConnectionProfile',
  FBPoolAddDefaultConnectionProfile name 'ibxFBPoolAddDefaultConnectionProfile',
  FBPoolGetConnectionByProfile name 'ibxFBPoolGetConnectionByProfile',
  FBPoolGetConnectionByParams name 'ibxFBPoolGetConnectionByParams',
  FBPoolGetDefaultConnection name 'ibxFBPoolGetDefaultConnection',
  FBPoolReturnConnection name 'ibxFBPoolReturnConnection',
  FBPoolGetSize name 'ibxFBPoolGetSize';
{$ENDIF}

resourcestring
  FBStrProfileNotFound = 'Профиль "%s" с параметрами подключения к БД не найден';
  FBStrPoolGetConnection = 'Получить подключение из пула';
  FBStrPoolReturnConnection = 'Вернуть подключение обратно в пул';

var
  DebugPool: Boolean;  

implementation

var
  {Пул подключений. Создается в секции initialization}
  FBPool: TFBConnectionPool;

  ConnectionId: Integer;

procedure FBPoolAddConnectionProfile(AProfileName: string; AServerName: string; APort: Integer;
      ADataBase: string; AUserName: string; APassword: string; ACharSet: string; AModuleName: string);
begin
  FBPool.AddConnectionProfile(AProfileName, AServerName, APort, ADataBase, AUserName, APassword, ACharSet);
end;

procedure FBPoolAddDefaultConnectionProfile(AServerName: string; APort: Integer;
  ADataBase: string; AUserName: string; APassword: string; ACharSet: string; AModuleName: string);
begin
  FBPool.AddDefaultConnectionProfile(AServerName, APort, ADataBase, AUserName, APassword, ACharSet);
end;

function FBPoolGetConnectionByProfile(AProfileName: string; ReadTran, WriteTran: PIBTransaction;
  ReadTranType, WriteTranType: TTransactionType; AModuleName: string): TIBDatabase;
begin
  Result := FBPool.GetConnection(AProfileName, ReadTran, WriteTran, ReadTranType, WriteTranType, AModuleName);
end;

function FBPoolGetConnectionByParams(AServerName: string; APort: Integer; ADataBase: string;
  AUserName: string; APassword: string; ACharSet: string; ReadTran, WriteTran: PIBTransaction;
  ReadTranType, WriteTranType: TTransactionType; AModuleName: string): TIBDatabase;
begin
  Result := FBPool.GetConnection(AServerName, APort, ADataBase, AUserName, APassword,
    ACharSet, ReadTran, WriteTran, ReadTranType, WriteTranType, AModuleName);
end;

function FBPoolGetDefaultConnection(ReadTran, WriteTran: PIBTransaction;
  ReadTranType, WriteTranType: TTransactionType; AModuleName: string): TIBDatabase;
begin
  Result := FBPool.GetDefaultConnection(ReadTran, WriteTran, ReadTranType, WriteTranType, AModuleName);
end;

procedure FBPoolReturnConnection(FDB: TIBDatabase; AModuleName: string);
begin
  FBPool.ReturnConnection(FDB);
end;

function FBPoolGetSize: Integer;
begin
  Result := FBPool.GetPoolSize;
end;

type

{ TFBConnectionPool }

  {Профиль, хранящий настройки подключения к базе данных}
  TConnectionProfile = class
    cpServerName: string; // Имя сервера, на котором запущена СУБД Firebird (или его IP-адрес)
    cpPort: Integer;      // Порт, на который настроена СУБД Firebird. По умолчанию = 3050
    cpDataBase: string;   // Полное имя файла БД (относительно серверного компьютера) или его алиас
    cpUserName: string;   // Имя пользователя для подключения к БД. По умолчанию =  SYSDBA
    cpPassword: string;   // Пароль для подключения к БД. По умолчанию =  masterkey
    cpCharSet: string;    // Кодовая страница, используемая для подключения к БД.
                          // Рекомендуется указывать ту же самую страницу, с помощью
                          // которой была создана база данных. Пример: WIN1251
  end;

  TDBPoolConnection = class
    dpConnProfile: TConnectionProfile; // Используемый профиль
    dpUsed: Boolean;                   // Подключение используется
    dpCloseTime: TDateTime;            // Момент времени, с которого никто не использует данное подключение
    dpLastTestTime: TDateTime;         // Время последнего тестирования подключения
    dpConnect: TIBDatabase;            // Само подключение к базе данных
    dpReadTran: TIBTransaction;        // Транзакция для чтения
    dpWriteTran: TIBTransaction;       // Транзакция для записи (и чтения)
  end;

function TFBConnectionPool.GetConnection(AProfileName: string; ReadTran, WriteTran: PIBTransaction;
  ReadTranType, WriteTranType: TTransactionType; AModuleName: string): TIBDatabase;
var
  pPool, pPoolTmp: TDBPoolConnection;
  cp: TConnectionProfile;
  I, Idx: Integer;
  IsConnected: Boolean;
  ti: TTimeInterval;
  sOldRefs: string;
  NeedTest: Boolean;
begin
  Result := nil;
  pPool := nil;
  NeedTest := False;
  if WriteTran = nil then
    WriteTranType := trNone;

  ReadTranType := trRCRO; // Транзакцию для чтения создаём в любом случае, она - долгоживущая

  if DebugPool then
    FBLogMsg(Format('Pool GetConnection: Enter (ProfileName=%s)', [AProfileName]), tlpInformation, AModuleName);

  try
    DeleteOldConnections; // Удаляем старые подключения

    DBPoolCS.Enter; // В рамках крит. секции должны выполняться только самые быстрые операции
    try
      // Отыскиваем параметры подключения для заданного профиля
      Idx := FProfileList.IndexOf(AProfileName);
      if Idx < 0 then
        raise Exception.CreateFmt(FBStrProfileNotFound, [AProfileName]);
      cp := TConnectionProfile(FProfileList.Objects[Idx]);

      // Пробуем найди готовое подключение в пуле
      for I := 0 to DBPoolList.Count - 1 do
      begin
        pPool := TDBPoolConnection(DBPoolList[I]);

        // Если в пуле найден неиспользуемый коннект...
        if (pPool.dpConnProfile = cp) and (not pPool.dpUsed) then
        begin
          Result := pPool.dpConnect;
          pPool.dpUsed := True;    // Ставим отметку, что объект используется

          NeedTest := SecondsBetween(Now, pPool.dpLastTestTime) > 10;
          pPool.dpCloseTime := 0;
          Break;
        end;
      end; // for I

      if Result = nil then // Подключение в пуле не было найдено.
      begin
        // Создаем новое подключение
        Result := FBCreateConnection(cp.cpServerName, cp.cpPort, cp.cpDataBase,
          cp.cpUserName, cp.cpPassword, cp.cpCharSet, trNone, False, nil, '');
        if ConnectionId = MaxInt then
          ConnectionId := 0;
        Inc(ConnectionId);
        Result.Name := Format('PoolDBConn%d', [ConnectionId]);
        // Добавляем объект в пул подключений
        pPool := TDBPoolConnection.Create;
        DBPoolList.Add(pPool);
        pPool.dpConnProfile := cp;
        pPool.dpUsed := True;      // Ставим отметку, что объект используется
        pPool.dpCloseTime := 0;
        pPool.dpConnect := Result;
        if Assigned(FBLogMsg) then
          FBLogMsg(Format('Pool: Создан новый объект "%s". Элементов: %d', [Result.Name, DBPoolList.Count]), tlpInformation, AModuleName);
        if DebugPool then
        begin
          sOldRefs := '';
          for I := 0 to DBPoolList.Count - 1 do
          begin
            pPoolTmp := TDBPoolConnection(DBPoolList[I]);
            if sOldRefs <> '' then sOldRefs := sOldRefs + ', ';
            sOldRefs := sOldRefs + '0x' + IntToHex(LPARAM(pPoolTmp.dpConnect), 8);
          end;
          FBLogMsg(Format('Pool GetConnection: FDBRefList=%s', [sOldRefs]), tlpInformation, AModuleName);
        end;
      end;
    finally
      DBPoolCS.Leave;
    end;

    // Выполняем подключение к базе данных
    if Assigned(Result) then
    begin
      IsConnected := Result.Connected;
      if IsConnected and NeedTest then
      begin
        ti.Start;

        if DebugPool then
          FBLogMsg('Pool GetConnection: TestConnected BEGIN...', tlpInformation, AModuleName);

        IsConnected := Result.TestConnected;
        if IsConnected then
          pPool.dpLastTestTime := Now;

        if DebugPool then
          FBLogMsg('Pool GetConnection: TestConnected END...', tlpInformation, AModuleName);

        if Assigned(FBLogMsg) and ((SecondsBetween(Now, FLastLogTestConnectTime) > 10 * 60) or (ti.ElapsedMilliseconds > 1000)) then
        begin
          FLastLogTestConnectTime := Now;
          FBLogMsg(Format('Pool: Время TestConnected для объекта %s составило %d мс', [Result.Name, ti.ElapsedMilliseconds]), tlpInformation, AModuleName);
        end;
      end;

      if not IsConnected then // Если подключение к БД не установлено
      begin
        Result.Connected := False; // На всякий случай
        try
          FBConnectDB(Result, AModuleName); // Выполняем подключение к БД
          pPool.dpLastTestTime := Now;
        except
          pPool.dpCloseTime := Now - OneHour;
          pPool.dpUsed := False;    // Сбрасываем отметку, что объект используется
          raise;
        end;
      end;

     if DebugPool then
       FBLogMsg(Format('Pool GetConnection: FDBRef=0x%s (ProfileName=%s)', [IntToHex(LPARAM(Result), 8), AProfileName]), tlpInformation, AModuleName);

      Result.DefaultTransaction := nil;

      // В любом случае создаём транзакцию для чтения
      if pPool.dpReadTran = nil then
        pPool.dpReadTran := FBCreateTransaction(Result, ReadTranType, False, Result, '');

      if not pPool.dpReadTran.Active then
      begin
        pPool.dpReadTran.Active := True;
        if DebugPool then
          FBLogMsg('Pool GetConnection: dpReadTran.Active := True', tlpInformation, AModuleName);
      end;

      // Если передана ссылка, то делаем читающую транзакцию - по умолчанию
      if Assigned(ReadTran) then
        Result.DefaultTransaction := pPool.dpReadTran;

      if WriteTranType > trNone then // Создаем транзакцию для записи
      begin
        pPool.dpWriteTran := FBCreateTransaction(Result, WriteTranType, False, Result, '');
        if Result.DefaultTransaction = nil then
          Result.DefaultTransaction := pPool.dpWriteTran;
      end;

      // Если не переданы ссылки ни на читающую ни на пишущую транзакцию, то делаем читающую транзакцию - по умолчанию
      if Result.DefaultTransaction = nil then
        Result.DefaultTransaction := pPool.dpReadTran;

      if not Result.DefaultTransaction.Active then
      begin
        Result.DefaultTransaction.Active := True;
        if DebugPool then
          FBLogMsg('Pool GetConnection: DefaultTransaction.Active := True', tlpInformation, AModuleName);
      end;

      if Assigned(ReadTran) then
        ReadTran^ := pPool.dpReadTran;
      if Assigned(WriteTran) then
        WriteTran^ := pPool.dpWriteTran;
    end else
      raise Exception.Create('Result=nil');

    if DebugPool then
      FBLogMsg(Format('Pool GetConnection: Exit (ProfileName=%s)', [AProfileName]), tlpInformation, AModuleName);

  except
    on E: Exception do
      raise ReCreateEObject(E, FBStrPoolGetConnection);
  end;
end;

function TFBConnectionPool.GetConnection(AServerName: string; APort: Integer;
  ADataBase, AUserName, APassword, ACharSet: string; ReadTran,
  WriteTran: PIBTransaction; ReadTranType,
  WriteTranType: TTransactionType; AModuleName: string): TIBDatabase;
var
  AProfileName: string; // Имя профиля
begin
  if LowerCase(AServerName) = 'localhost' then
    AServerName := FBLocalhostIP;
  // Генерируем имя профиля
  AProfileName := Format('%s_%d_%s_%s_%s', [AServerName, APort, ADataBase, AUserName, ACharSet]);
  // Регистрируем новый профиль
  AddConnectionProfile(AProfileName, AServerName, APort, ADataBase, AUserName, APassword, ACharSet);
  // Подключаемся к БД
  Result := GetConnection(AProfileName, ReadTran, WriteTran, ReadTranType, WriteTranType, AModuleName);
end;

function TFBConnectionPool.GetDefaultConnection(ReadTran,
  WriteTran: PIBTransaction; ReadTranType, WriteTranType: TTransactionType; AModuleName: string): TIBDatabase;
begin
  try
    Result := GetConnection(FBDefDB, ReadTran, WriteTran, ReadTranType, WriteTranType, AModuleName);
  except
    on E: Exception do
      raise ReCreateEObject(E, 'TFBConnectionPool.GetDefaultConnection');
  end;
end;

function TFBConnectionPool.GetPoolSize: Integer;
begin
  DBPoolCS.Enter;
  try
    Result := DBPoolList.Count;
  finally
    DBPoolCS.Leave;
  end;
end;

function PoolFreeTransactions(pPool: TDBPoolConnection; MustFree: Boolean): Boolean;
var
  DefTran: TIBTransaction;
begin
  Result := True;
  try
    DefTran := pPool.dpConnect.DefaultTransaction;
    if Assigned(DefTran) then
    begin
      if (DefTran <> pPool.dpReadTran) then
      begin
        if DefTran.InTransaction then
        try
          DefTran.Rollback;
        except
          Result := False;
          // По хорошему нужно вывести в лог
        end;
      end;
      pPool.dpConnect.DefaultTransaction := nil;
    end;

    if MustFree and Assigned(pPool.dpReadTran) then
    begin
      try
        pPool.dpReadTran.Free;
      except
        Result := False;
        // По хорошему нужно вывести в лог
      end;

      pPool.dpReadTran := nil;
    end;

    if Assigned(pPool.dpWriteTran) then
    begin
      try
        pPool.dpWriteTran.Free;
      except
        Result := False;
        // По хорошему нужно вывести в лог
      end;

      pPool.dpWriteTran := nil;
    end;

  except
    on E: Exception do
      raise ReCreateEObject(E, 'PoolFreeTransactions');
  end;
end;

procedure TFBConnectionPool.ReturnConnection(FDB: TIBDatabase);
var
  pPool: TDBPoolConnection;
  I: Integer;
  TranOk, DSOk, WasFind: Boolean;
begin
  if DebugPool then
    FBLogMsg(Format('Pool ReturnConnection: Enter (FDBRef=0x%s)', [IntToHex(LPARAM(FDB), 8)]), tlpInformation, '');

  try
    WasFind := False;
    DBPoolCS.Enter;
    try
      for I := 0 to DBPoolList.Count - 1 do
      begin
        pPool := TDBPoolConnection(DBPoolList[I]);
        if pPool.dpConnect = FDB then
        begin
          WasFind := True;
          pPool.dpUsed := False;

          try
            pPool.dpConnect.CloseDataSets; // Закрываем все открытые наборы данных
            DSOk := True;
          except
            DSOk := False;
            // По хорошему нужно вывести ошибку в лог
          end;

          TranOk := PoolFreeTransactions(pPool, False); // Закрываем транзакции (кроме транзакции для чтения)

          if TranOk and DSOk then
            pPool.dpCloseTime := Now; // Устанавливаем время закрытия только после успешного выполнения операций

          Break;
        end;
      end;
    finally
      DBPoolCS.Leave;
    end;

    if DebugPool then
      FBLogMsg(Format('Pool ReturnConnection: Exit (FDBRef=0x%s). WasFind=%d', [IntToHex(LPARAM(FDB), 8), Integer(WasFind)]), tlpInformation, '');
  except
    on E: Exception do
      raise ReCreateEObject(E, FBStrPoolReturnConnection);
  end;
end;

// Осуществляет очистку пула соединений
procedure TFBConnectionPool.AddConnectionProfile(AProfileName,
  AServerName: string; APort: Integer; ADataBase, AUserName, APassword,
  ACharSet: string);
var
  cp: TConnectionProfile;
  Idx: Integer;
begin
  if AProfileName = '' then
    raise Exception.Create('AddConnectionProfile -> Profile name not found');
  if ADataBase = '' then
    raise Exception.Create('AddConnectionProfile -> Database name not found');

  DBPoolCS.Enter;
  try
    UniqueString(AProfileName);
    Idx := FProfileList.IndexOf(AProfileName);
    if Idx >= 0 then
      cp := TConnectionProfile(FProfileList.Objects[Idx])
    else
    begin
      cp := TConnectionProfile.Create;
      FProfileList.AddObject(AProfileName, cp);
    end;
    UniqueString(AServerName);
    if LowerCase(AServerName) = 'localhost' then
      AServerName := FBLocalhostIP;
    cp.cpServerName := AServerName;

    cp.cpPort := APort;

    UniqueString(ADataBase);
    cp.cpDataBase := ADataBase;

    UniqueString(AUserName);
    cp.cpUserName := AUserName;

    UniqueString(APassword);
    cp.cpPassword := APassword;

    UniqueString(ACharSet);
    cp.cpCharSet := ACharSet;
  finally
    DBPoolCS.Leave;
  end;
end;

procedure TFBConnectionPool.AddDefaultConnectionProfile(AServerName: string;
  APort: Integer; ADataBase, AUserName, APassword, ACharSet: string);
begin
  AddConnectionProfile(FBDefDB, AServerName, APort, ADataBase, AUserName, APassword, ACharSet);
end;

procedure TFBConnectionPool.ClearConnectionProfiles;
var
  I: Integer;
begin
  try
    DBPoolCS.Enter;
    try
      for I := 0 to FProfileList.Count - 1 do
        FProfileList.Objects[I].Free;

      FProfileList.Clear;
    finally
      DBPoolCS.Leave;
    end;
  except
    on E: Exception do
      raise ReCreateEObject(E, 'ClearConnectionProfiles');
  end;
end;

procedure TFBConnectionPool.ClearPool();
var
  pPool: TDBPoolConnection;
  I: Integer;
  FDB: TIBDatabase;
begin
  try
    {$IfDef MSWINDOWS}
    if GetModuleHandle('GDS32.dll') = 0 then
    begin
      //raise Exception.Create('GetModuleHandle(GDS32.dll)=NULL');
      // Лучше не выдавать сообщений об ошибках при выходе из программы
    end else
    {$EndIf}
    begin
      DBPoolCS.Enter;
      try
        for I := 0 to DBPoolList.Count - 1 do
        begin
          pPool := TDBPoolConnection(DBPoolList[I]);
          PoolFreeTransactions(pPool, True);
          FDB := pPool.dpConnect;
          pPool.Free;

          try
            FBFreeConnection(FDB, '');
          except
            // По хорошему нужно вывести в лог
          end;
        end;
        DBPoolList.Clear;
      finally
        DBPoolCS.Leave;
      end;
    end;
  except
    on E: Exception do
      raise ReCreateEObject(E, 'ClearPool');
  end;
end;

constructor TFBConnectionPool.Create;
begin
  inherited Create;

  RegObj(DBPoolList, TList.Create);
  RegObj(DBPoolCS, TCriticalSection.Create);
  FProfileList := CreateHashedStringList;
end;

procedure TFBConnectionPool.DeleteOldConnections;
var
  pPool: TDBPoolConnection;
  I: Integer;
  AList: TList;
  sConnName: string;
  ConnCount: Integer;
begin
  try
    AList := TList.Create;
    try
      DBPoolCS.Enter;
      try
        // Закрываем все неиспользуемые подключения, живущие более 5 минут
        for I := DBPoolList.Count - 1 downto 0 do
        begin
          pPool := TDBPoolConnection(DBPoolList[I]);
          if not pPool.dpUsed then
            if SecondsBetween(Now, pPool.dpCloseTime) > FBPoolConnectionMaxTime then
            begin
              DBPoolList.Delete(I);
              AList.Add(pPool.dpConnect);

              // Закрываем транзакции. От БД здесь нельзя отключаться, чтобы
              // ничего не тормозило
              PoolFreeTransactions(pPool, True);
              pPool.Free;
            end;
        end;
        ConnCount := DBPoolList.Count;
      finally
        DBPoolCS.Leave;
      end;

      // Выносим удаление ненужных подключений за пределы критической секции
      for I := 0 to AList.Count - 1 do
      try
        sConnName := TIBDatabase(AList[I]).Name;
        FBFreeConnection(TIBDatabase(AList[I]), '');
        if Assigned(FBLogMsg) then
          FBLogMsg(Format('Pool: Удалён объект "%s". Элементов: %d', [sConnName, ConnCount]), tlpInformation, '');
      except
        // По хорошему нужно вывести в лог
      end;
    finally
      AList.Free;
    end;

  except
    on E: Exception do
      raise ReCreateEObject(E, 'TFBConnectionPool.DeleteOldConnections');
  end;
end;

destructor TFBConnectionPool.Destroy;
begin
  ClearPool(); // Очищаем пул соединений
  ClearConnectionProfiles(); // Очищаем список профилей
  // Объекты, созданные в конструкторе и зарегистрированные с помощью RegObj,
  // будут удалены автоматически
  inherited;
end;

class procedure TFBConnectionPool.FreePool;
begin
  FreeAndNil(FBPool);
end;

initialization
  DebugPool := DirectoryExists('C:\DEXE\FBUtilsDebugPool');
  FBPool := TFBConnectionPool.Create;
finalization
  TFBConnectionPool.FreePool;
end.
