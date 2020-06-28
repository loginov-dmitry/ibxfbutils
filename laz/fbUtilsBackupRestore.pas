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
{ Модуль fbUtilsBackupRestore - содержит функции для резервирования и         }
{ восстановления баз данных Firebird                                          }
{ (c) 2012 Логинов Дмитрий Сергеевич                                          }
{ Последнее обновление: 30.04.2012                                            }
{ Протестировано на D7, D2007, D2010, D-XE2                                   }
{ Адрес сайта: http://loginovprojects.ru/                                     }
{ e-mail: loginov_d@inbox.ru                                                  }
{                                                                             }
{ *************************************************************************** }
unit fbUtilsBackupRestore;

{$MODE Delphi}

interface
uses
  LCLIntf, LCLType, LMessages, SysUtils, Classes, IBDatabase, IBServices, fbSomeFuncs, fbUtilsBase,
  fbTypes, FileUtil;

{Осуществляет резервирование базы данных средствами Firebird Service-API.
 Весь процесс резервирования осуществляется на сервере (AServerName). Файл
 бэкапа ABackupFile следует задавать относительно сервера, на котором должно
 выполняться резервирование.}
procedure FBBackupDatabaseOnServer(AServerName: string; APort: Integer; ADBName, ABackupFile,
  AUser, APassw: string; ABackupOptions: TFBBackupOptions; AProgressProc: TBackupRestoreProgressProc; AModuleName: string);

{Осуществляет резервирование базы данных и копирует файл резервной копии с сервера
 на компьютер клиента.
 ASourBackupFileOnServer: имя файла резервной копии, доступное для клиентской программы
 ADestBackupFileOnClient: имя файла, под которым рез. копия будет сохранена на клиентском компьютере}
procedure FBBackupDatabaseAndCopyFromServer(AServerName: string; APort: Integer; ADBName, ABackupFile,
  AUser, APassw: string; ABackupOptions: TFBBackupOptions; AProgressProc: TBackupRestoreProgressProc;
  ASourBackupFileOnServer, ADestBackupFileOnClient: string; TryDeleteSourBackupFile: Boolean; AModuleName: string);

{Осуществляет восстановление базы данных средствами Firebird Service-API.
 Весь процесс восстановления осуществляется на сервере (AServerName). Файл
 бэкапа ABackupFile следует задавать относительно сервера, на котором должно
 выполняться рвосстановление.}
procedure FBRestoreDatabaseOnServer(AServerName: string; APort: Integer; ADBName, ABackupFile,
  AUser, APassw: string; ARestoreOptions: TFBRestoreOptions; AProgressProc: TBackupRestoreProgressProc; AModuleName: string);

{Копирует байл резервной копии на сервер и производит восстановление базы данных}
procedure FBCopyBackupToServerAndRestoreDatabase(AServerName: string; APort: Integer; ADBName, ABackupFile,
  AUser, APassw: string; ARestoreOptions: TFBRestoreOptions; AProgressProc: TBackupRestoreProgressProc;
  ABackupFileOnClient, ABackupFileOnServer: string; TryDeleteBackupFileOnClient,
  TryDeleteBackupFileOnServer: Boolean; AModuleName: string);

{$IFDEF FBUTILSDLL} // Замечания по директиве смотрите в модуле fbUtilsBase.pas
exports
  FBBackupDatabaseOnServer name 'ibxFBBackupDatabaseOnServer',      
  FBBackupDatabaseAndCopyFromServer name 'ibxFBBackupDatabaseAndCopyFromServer',
  FBRestoreDatabaseOnServer name 'ibxFBRestoreDatabaseOnServer',
  FBCopyBackupToServerAndRestoreDatabase name 'ibxFBCopyBackupToServerAndRestoreDatabase';
{$ENDIF}

resourcestring
  FBStrOperationAborted       = 'Операция прервана клиентом'; // Operation was aborted by client
  FBStrBackupOnServerNotFound = 'Файл бэкапа "%s" на сервере "%s" не найден или нет доступа';
  FBStrFileNotFound           = 'Файл "%s" не найден';
  FBStrCanNotDeleteFile       = 'Невозможно удалить файл "%s". Причина: %s';
  FBStrCanNotCopyFile         = 'Невозможно скопировать файл "%s" в "%s". Причина: %s';

implementation

procedure FBBackupDatabaseOnServer(AServerName: string; APort: Integer; ADBName, ABackupFile,
  AUser, APassw: string; ABackupOptions: TFBBackupOptions;
  AProgressProc: TBackupRestoreProgressProc; AModuleName: string);
var
  bs: TIBBackupService;
  LastMsg: string;
  Stop: Boolean;
begin
  try
    bs := TIBBackupService.Create(nil);
    try
      bs.Params.Text :=
        Format('user_name=%s%spassword=%s',
          [AUser, sLineBreak, APassw]);

      bs.DatabaseName := UTF8ToSys(ADBName);

      bs.BackupFile.Text := UTF8ToSys(ABackupFile);

      if AServerName = '' then
      begin
        bs.Protocol := Local;
      end else
      begin
        bs.ServerName := AServerName + '/' + IntToStr(APort);
        bs.Protocol := TCP;
      end;

      bs.LoginPrompt := False;
      bs.Verbose := True; // Включаем генерацию сообщений о ходе резервирования

      bs.Options := TBackupOptions(ABackupOptions); // Опции резервирования

      try
        bs.Active := True; // Подключаемся к Firebird
      except
        on E: Exception do
          if E is EAccessViolation then
            raise ReCreateEObject(E, 'Attach Error. IBX Bug.')
          else
            raise;
      end;
      try
        bs.ServiceStart; // Запускаем резервирование
        while not bs.Eof do
        begin
          LastMsg := SysToUTF8(bs.GetNextLine);
          if Assigned(AProgressProc) then
          begin
            Stop := False;
            AProgressProc(LastMsg, Stop);
            if Stop then
              raise Exception.Create(FBStrOperationAborted);
          end;
        end;
      finally
        bs.Active := False; // Отключаемся от Firebird
      end;
    finally
      bs.Free;
    end;
  except
    on E: Exception do
      raise ReCreateEObject(E, 'FBBackupDatabaseOnServer');
  end;
end;

procedure FBBackupDatabaseAndCopyFromServer(AServerName: string; APort: Integer; ADBName, ABackupFile,
  AUser, APassw: string; ABackupOptions: TFBBackupOptions; AProgressProc: TBackupRestoreProgressProc; ASourBackupFileOnServer,
  ADestBackupFileOnClient: string; TryDeleteSourBackupFile: Boolean; AModuleName: string);
begin
  try
    FBBackupDatabaseOnServer(AServerName, APort, ADBName, ABackupFile, AUser, APassw, ABackupOptions, AProgressProc, AModuleName);

    if not FileExistsUTF8(ASourBackupFileOnServer) { *Converted from FileExists*  } then
      raise Exception.CreateFmt(FBStrBackupOnServerNotFound, [ASourBackupFileOnServer, AServerName]);

    if FileExistsUTF8(ADestBackupFileOnClient) { *Converted from FileExists*  } then
      if not DeleteFileUTF8(ADestBackupFileOnClient) { *Converted from DeleteFile*  } then
        raise Exception.CreateFmt(FBStrCanNotDeleteFile, [ADestBackupFileOnClient, SysErrorMessageUTF8(GetLastOSError)]);

    // TODO: Требуется ли преобразование UTF8?
    if not CopyFile(PChar(ASourBackupFileOnServer), PChar(ADestBackupFileOnClient), True) then
      raise Exception.CreateFmt(FBStrCanNotCopyFile,
        [ASourBackupFileOnServer, ADestBackupFileOnClient, SysErrorMessageUTF8(GetLastOSError)]);

    if TryDeleteSourBackupFile then
      DeleteFileUTF8(ASourBackupFileOnServer); { *Converted from DeleteFile*  } // В случае неудачи ошибка не выдается!
  except
    on E: Exception do
      raise ReCreateEObject(E, 'FBBackupDatabaseAndCopyFromServer');
  end;
end;

procedure FBRestoreDatabaseOnServer(AServerName: string; APort: Integer; ADBName, ABackupFile,
  AUser, APassw: string; ARestoreOptions: TFBRestoreOptions;
  AProgressProc: TBackupRestoreProgressProc; AModuleName: string);
var
  rs: TIBRestoreService;
  LastMsg: string;
  Stop: Boolean;
begin
  try
    rs := TIBRestoreService.Create(nil);
    try
      rs.Params.Text :=
        Format('user_name=%s%spassword=%s',
          [AUser, sLineBreak, APassw]);

      rs.DatabaseName.Text := UTF8ToSys(ADBName);

      rs.BackupFile.Text := UTF8ToSys(ABackupFile);

      if AServerName = '' then
      begin
        rs.Protocol := Local;
      end else
      begin
        rs.ServerName := AServerName + '/' + IntToStr(APort);
        rs.Protocol := TCP;
      end;

      rs.LoginPrompt := False;
      rs.Verbose := True; // Включаем генерацию сообщений о ходе восстановления

      rs.Options := TRestoreOptions(ARestoreOptions); // Опции восстановления

      rs.Active := True; // Подключаемся к Firebird
      try
        rs.ServiceStart; // Запускаем восстановление
        while not rs.Eof do
        begin
          LastMsg := SysToUTF8(rs.GetNextLine);
          if Assigned(AProgressProc) then
          begin
            Stop := False;
            AProgressProc(LastMsg, Stop);
            if Stop then
              raise Exception.Create(FBStrOperationAborted);
          end;
        end;
      finally
        rs.Active := False; // Отключаемся от Firebird
      end;
    finally
      rs.Free;
    end;
  except
    on E: Exception do
      raise ReCreateEObject(E, 'FBRestoreDatabaseOnServer');
  end;
end;

procedure FBCopyBackupToServerAndRestoreDatabase(AServerName: string; APort: Integer; ADBName, ABackupFile,
  AUser, APassw: string; ARestoreOptions: TFBRestoreOptions; AProgressProc: TBackupRestoreProgressProc;
  ABackupFileOnClient, ABackupFileOnServer: string; TryDeleteBackupFileOnClient,
  TryDeleteBackupFileOnServer: Boolean; AModuleName: string);
begin
  try
    if not FileExistsUTF8(ABackupFileOnClient) { *Converted from FileExists*  } then
      raise Exception.CreateFmt(FBStrFileNotFound, [ABackupFileOnClient]);

    if FileExistsUTF8(ABackupFileOnServer) { *Converted from FileExists*  } then
      if not DeleteFileUTF8(ABackupFileOnServer) { *Converted from DeleteFile*  } then
        raise Exception.CreateFmt(FBStrCanNotDeleteFile, [ABackupFileOnServer, SysErrorMessageUTF8(GetLastOSError)]);

    // Копируем файл бэкапа на сервер
    // TODO: UTF8??
    if not CopyFile(PChar(ABackupFileOnClient), PChar(ABackupFileOnServer), True) then
      raise Exception.CreateFmt(FBStrCanNotCopyFile,
        [ABackupFileOnClient, ABackupFileOnServer, SysErrorMessageUTF8(GetLastOSError)]);

    // Осуществляет восстановление БД из резервной копии
    FBRestoreDatabaseOnServer(AServerName, APort, ADBName, ABackupFile,
      AUser, APassw, ARestoreOptions, AProgressProc, AModuleName);

    // Удаляем файлы резервной копии при необходимости
    if TryDeleteBackupFileOnServer then
      DeleteFileUTF8(ABackupFileOnServer); { *Converted from DeleteFile*  }
    if TryDeleteBackupFileOnClient then
      DeleteFileUTF8(ABackupFileOnClient); { *Converted from DeleteFile*  }

  except
    on E: Exception do
      raise ReCreateEObject(E, 'FBCopyBackupToServerAndRestoreDatabase');
  end;
end;


end.
