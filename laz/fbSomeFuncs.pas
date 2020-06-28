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
{ Модуль fbSomeFuncs - содержит некоторые вспомогательные функции для fbUtils }
{ (c) 2012 Логинов Дмитрий Сергеевич                                          }
{ Последнее обновление: 09.05.2012                                            }
{ Протестировано на D7, D2007, D2010, D-XE2                                   }
{ Адрес сайта: http://loginovprojects.ru/                                     }
{ e-mail: loginov_d@inbox.ru                                                  }
{                                                                             }
{ *************************************************************************** }

{
Функции, представленные в данном модуле, не имеют никакого отношения к базам
данных Firebird, автор их очень часто использует в различных своих проектах.
Все они протестированы годами.
Здесь находятся только те функции, которые используются в fbUtils
}



unit fbSomeFuncs;

{$MODE Delphi}

interface

uses
  {$IFDEF WINDOWS}Windows,{$ELSE}LCLIntf, LCLType, LCLProc, LMessages, Process,{$ENDIF WINDOWS}
  SysUtils, Classes, IniFiles, IB, FileUtil;

{$OVERFLOWCHECKS OFF}

type
  { Базовый класс, упрощающий управление временем жизни объектов. Благодаря
    данному классу можно минимизировать количество операторов TRY..FINALLY
    и тем самым улучшить читабельность кода.
    ОН НЕ ДОЛЖЕН СОДЕРЖАТЬ НИЧЕГО ЛИШНЕГО! }
  TBaseObject = class(TObject)
  private
    FRefList: TList;

    { Очищает список ссылок FRefList }
    procedure ClearRefList;

    { Регистрирует объект в списоке для автоматического удаления.
      Внимание!!! Если объект зарегистрирован в списке, то его удалять в
      других местах нельзя.
      Пример кода создания объекта:
      AList := RegObj(TStringList.Create) as TStringList; }
    function DoRegObj(Obj: TObject): TObject;

  protected
    { Выполняет уничтожение объекта. Можно перекрыть. }
    procedure ClearObjectRef(ARef: TObject); virtual;
  public
    constructor Create;
    destructor Destroy; override;

    { Создает объкт, регистрирует его в списке FRefList и записывает в параметр Ref }
    procedure RegObj(var Ref; Obj: TObject); overload;

    { Удаляет объект Obj из списка и уничтожает его }
    procedure FreeObj(Obj: TObject);

    { Создает список строк, удаляемый автоматически }
    function CreateStringList: TStringList;

    function CreateMemoryStream: TMemoryStream;

    function CreateHashedStringList: THashedStringList;

  end;

  { Тот же TBaseObject, но с другим названием. Для того, чтобы логически выделить
    его назначение: простой список ссылок на объекты. Тот же TObjectList, но
    гораздо удобнее }
  TObjHolder = class(TBaseObject);

{Пересоздает объект исключения, дописывая в его текст имя функции, в
 которой произошло данное исключение}
function ReCreateEObject(E: Exception; FuncName: string; WriteExceptClass: Boolean = True): Exception;

{Делает то же самое, что и стандартная функция StringReplace, только в сотни раз быстрее}
function FastStringReplace(const S: string; OldPattern: string; const NewPattern: string;
  Flags: TReplaceFlags = [rfReplaceAll]): string;

{Генерирует имя мьютекса для заданного файла для целей обеспечения его блокировки}
//function GenerateFileMutexName(const MutexPrefix, AFileName: string): string;

{ Создает общедоступный мьютекс с именем  AName. Мюьтекс можно использовать
  одновременно под разными пользователями. Мьютекс создается "незанятым".
  Для того, чтобы занять мьютекс, используйте WaitForSingleObject().
  Для освобождения мьютекса используйте ReleaseMutex().
  Если по каким-то причинам не удается создать мьютекс, то генерируется Exception}
//function CreateMutexShared(AName: string; LastError: PCardinal = nil): THandle;

{ Создает общедоступный объект FileMapping}
//function CreateFileMappingShared(hFile: THandle; flProtect, dwMaximumSizeHigh,
//  dwMaximumSizeLow: DWORD; lpName: string; LastError: PCardinal = nil): THandle;

{ Возвращает имя данного компьютера }
function GetCurrentComputerName: string;

{ Возвращает имя пользователя }
function GetCurrentUserName: string;

{ Возвращает временный каталог }
function GetTempPath: string;

{ Вычисляет хэш для TStream (используется несколько алгоритмов вычисления хэша) }
function CalcStreamHash(S: TStream): string;

{ Вычисляет хэш строки HashLY }
function GenerateStringHashLY(S: string): Cardinal;

{$IFNDEF D2009PLUS}
function CharInSet(C: Char; const CharSet: TSysCharSet): Boolean;
{$ENDIF}

implementation

type
  IBExceptClass = class of EIBError;

function ReCreateEObject(E: Exception; FuncName: string;
  WriteExceptClass: Boolean = True): Exception;
var
  S: string;
  //Sutf: string;
begin
  S := E.Message;

  // Если ошибка из IBX, и "<#" отсутствует, то преобразуем текст в UTF8
  if (E is EIBError) and (Pos(' <# ', S) = 0) then
    S := SysToUTF8(S);

  S := Format('%s -> %s', [FuncName, S]);

  //Sutf := SysToUTF8(S);

  // Гарантируем наличие <# ... #> в случае ошибки из IBX
  if WriteExceptClass or (E is EIBError) then
  begin
    if Pos(' <# ', S) = 0 then
      S := S + ' <# ' + E.ClassName + ' #>';
  end;

  if E is EIBError then
    Result := IBExceptClass(E.ClassType).Create(EIBError(E).SQLCode, EIBError(E).IBErrorCode, S)
  else
    Result := ExceptClass(E.ClassType).Create(S);
end;

{$IFNDEF D2009PLUS}
function CharInSet(C: Char; const CharSet: TSysCharSet): Boolean;
begin
  Result := C in CharSet;
end;
{$ENDIF}

function FastStringReplace(const S: string; OldPattern: string;
  const NewPattern: string;
  Flags: TReplaceFlags = [rfReplaceAll]): string;
var
  I, J, Idx: Integer;
  IsEqual: Boolean;
  UpperFindStr: string;
  pS: PChar; // Указатель на массив для сравнения символов
  CanReplace: Boolean;
begin
  if OldPattern = '' then
  begin
    Result := S;
    Exit;
  end;

  Result := '';
  if S = '' then Exit;

  if rfIgnoreCase in Flags then
  begin
    OldPattern := AnsiUpperCase(OldPattern);

    // Для режима "не учитывать регистр"
    // потребуется дополнительная строка
    UpperFindStr := AnsiUpperCase(S);

    pS := PChar(UpperFindStr);
  end else
    pS := PChar(S);

  // Если новая подстрока не превышает старой, то...
  if Length(OldPattern) >= Length(NewPattern) then
  begin
    SetLength(Result, Length(S));
  end else // Точный размер буфера не известен...
    SetLength(Result, (Length(S) + Length(OldPattern) +
      Length(NewPattern)) * 2);

  I := 1;
  Idx := 0;
  CanReplace := True;
  while I <= Length(S) do
  begin
    IsEqual := False;

    if CanReplace then // Если замена разрешена
    begin
      // Если I-й символ совпадает с OldPattern[1]
      if pS[I - 1] = OldPattern[1] then // Запускаем цикл поиска
      begin
        IsEqual := True;
        for J := 2 to Length(OldPattern) do
        begin
          if pS[I + J - 2] <> OldPattern[J] then
          begin
            IsEqual := False;
            Break; // Прерываем внутренний цикл
          end;
        end;

        // Совпадение найдено! Выполняем замену
        if IsEqual then
        begin
          for J := 1 to Length(NewPattern) do
          begin
            Inc(Idx);

            // Расширяем строку Result при необходимости
            if Idx > Length(Result) then
              SetLength(Result, Length(Result) * 2);

            Result[Idx] := NewPattern[J];
          end;

          // Пропускаем байты в исходной строке
          Inc(I, Length(OldPattern));

          if not (rfReplaceAll in Flags) then
            CanReplace := False; // Запрещаем дальнейшую замену
        end;
      end;
    end;

    // Если подстрока не найдена, то просто копируем символ
    if not IsEqual then
    begin
      Inc(Idx);

      // Расширяем строку Result при необходимости
      if Idx > Length(Result) then
        SetLength(Result, Length(Result) * 2);

      Result[Idx] := S[I];
      Inc(I);
    end;
  end; // while I <= Length(S) do

  // Ограничиваем длину строки-результата
  SetLength(Result, Idx);
end;

{ТОЛЬКО ДЛЯ WINDOWS
function GenerateFileMutexName(const MutexPrefix, AFileName: string): string;
var
  I: Integer;
begin
  Result := MutexPrefix + AnsiLowerCase(AFileName);

  // Удаляем все двойные символы backslash
  while Pos('\\', Result) > 0 do
    Result := StringReplace(Result, '\\', '\', [rfReplaceAll]);

  for I := 1 to Length(Result) do
    if CharInSet(Result[I], ['\', '/', ':', '*', '"', '?', '|', '<', '>']) then
      Result[I] := '_';
end;

function CreateMutexShared(AName: string; LastError: PCardinal = nil): THandle;
var
  SD:TSecurityDescriptor;
  SA:TSecurityAttributes;
  pSA: PSecurityAttributes;
begin
  try
    if not InitializeSecurityDescriptor(@SD, SECURITY_DESCRIPTOR_REVISION) then
      raise Exception.CreateFmt('Error InitializeSecurityDescriptor: %s', [SysErrorMessage(GetLastError)]);

    SA.nLength:=SizeOf(TSecurityAttributes);
    SA.lpSecurityDescriptor:=@SD;
    SA.bInheritHandle:=False;

    if not SetSecurityDescriptorDacl(SA.lpSecurityDescriptor, True, nil, False) then
      raise Exception.CreateFmt('Error SetSecurityDescriptorDacl: %s', [SysErrorMessage(GetLastError)]);

    pSA := @SA;

    Result := CreateMutex(pSA, False, PChar('Global\' + AName)); // Пытаемся создать с директивой Global
    if Result = 0 then
      Result := CreateMutex(pSA, False, PChar(AName)); // Пытаемся создать без директивы Global

    if Assigned(LastError) then
      LastError^ := GetLastError;

    if Result = 0 then
      raise Exception.CreateFmt('Error creating object "Mutex": %s', [SysErrorMessage(GetLastError)]);
  except
    on E: Exception do
      raise ReCreateEObject(E, 'CreateMutexShared');
  end;
end;

function CreateFileMappingShared(hFile: THandle; flProtect, dwMaximumSizeHigh, dwMaximumSizeLow: DWORD; lpName: string; LastError: PCardinal = nil): THandle;
var
  SD:TSecurityDescriptor;
  SA:TSecurityAttributes;
  pSA: PSecurityAttributes;
begin
  try
    if not InitializeSecurityDescriptor(@SD, SECURITY_DESCRIPTOR_REVISION) then
      raise Exception.CreateFmt('Error InitializeSecurityDescriptor: %s', [SysErrorMessage(GetLastError)]);

    SA.nLength:=SizeOf(TSecurityAttributes);
    SA.lpSecurityDescriptor:=@SD;
    SA.bInheritHandle:=False;

    if not SetSecurityDescriptorDacl(SA.lpSecurityDescriptor, True, nil, False) then
      raise Exception.CreateFmt('Error SetSecurityDescriptorDacl: %s', [SysErrorMessage(GetLastError)]);

    pSA := @SA;

    Result := CreateFileMapping(hFile, pSA, flProtect, dwMaximumSizeHigh, dwMaximumSizeLow, PChar('Global\' + lpName)); // Пытаемся создать с директивой Global
    if Result = 0 then
      Result := CreateFileMapping(hFile, pSA, flProtect, dwMaximumSizeHigh, dwMaximumSizeLow, PChar(lpName)); // Пытаемся создать без директивы Global

    if Assigned(LastError) then
      LastError^ := GetLastError;

    if Result = 0 then
      raise Exception.CreateFmt('Error creating object "FileMapping": %s', [SysErrorMessage(GetLastError)]);
  except
    on E: Exception do
      raise ReCreateEObject(E, 'CreateFileMappingShared');
  end;
end;
}

var
  sSavedComputerName: string;

function GetCurrentComputerName: string;
var
  {$IFDEF WINDOWS}
  AComputerName: string;
  ASize: Cardinal;
  {$ELSE}
  p: TProcess;
  sl: TStringList;
  {$ENDIF WINDOWS}
begin
  if sSavedComputerName <> '' then
  begin
    Result := sSavedComputerName;
  end else
  begin
    {$IFDEF WINDOWS}
    SetLength(AComputerName, MAX_COMPUTERNAME_LENGTH + 1);
    ASize := MAX_COMPUTERNAME_LENGTH + 1;
    GetComputerName(PChar(AComputerName), ASize);
    Result := PChar(AComputerName);
    Result := SysToUTF8(Result);
    {$ELSE}
    Result := GetEnvironmentVariableUTF8('HOSTNAME');
    if Result = '' then
      Result := GetEnvironmentVariableUTF8('HOST');
    if Result = '' then
    begin
      p := TProcess.Create(nil);
      sl := TStringList.Create;
      try
        p.CommandLine := 'hostname';
        p.Options := p.Options + [poWaitOnExit, poUsePipes];
        p.ShowWindow := swoHIDE;
        p.Execute;
        sl.LoadFromStream(p.Output);
        Result := Trim(sl.Text);
        Result := SysToUTF8(Result);
      finally
        p.Free;
        sl.Free;
      end;
    end;
    {$ENDIF WINDOWS}
    if Result = '' then
      Result := 'unknown_comp';
    sSavedComputerName := Result;
  end;
end;

var
  sSavedUserName: string;

function GetCurrentUserName: string;
{$IFDEF WINDOWS}
var
  AUserName: string;
  ASize: Cardinal;
{$ENDIF WINDOWS}
begin
  if sSavedUserName <> '' then
    Result := sSavedUserName
  else
  begin
    {$IFDEF WINDOWS}
    SetLength(AUserName, 100);
    ASize := 100;
    GetUserName(PChar(AUserName), ASize);
    Result := PChar(AUserName);
    Result := SysToUTF8(Result);
    {$ELSE}
    Result := GetEnvironmentVariableUTF8('USERNAME');
    if Result = '' then
      Result := GetEnvironmentVariableUTF8('USER');
    {$ENDIF WINDOWS}
    if Result = '' then
      Result := 'unknown_user';
    sSavedUserName := Result;
  end;
end;


function CalcStreamHash(S: TStream): string;
var
  HashLY, HashRot13: Cardinal;
  I: Integer;
  Ar: PByteArray;
  WasGetMem: Boolean;
begin
  HashLY := 0; // Данные отсутствуют
  HashRot13 := 0;
  if S.Size > 0 then
  begin
    WasGetMem := False;
    Ar := nil;
    try
      if S is TMemoryStream then
        Ar := TMemoryStream(S).Memory
      else
      begin
        GetMem(Ar, S.Size);
        WasGetMem := True;
        S.Position := 0;
        S.Read(Ar[0], S.Size);
      end;

      // Вычисляем HashLY
      for I := 0 to S.Size - 1 do
      begin
        HashLY := HashLY * 1664525 + Ar[I] + 1013904223;
      end;

      if HashLY = 0 then
        HashRot13 := $ABCDABCD
      else
        HashRot13 := HashLY; // Берем за основу ранее вычисленный хэш HashLY

      // Вычисляем ROT13
      for I := 0 to S.Size - 1 do
      begin
        HashRot13 := HashRot13 + Ar[I];
        HashRot13 := HashRot13 - ((HashRot13 shl 13) or (HashRot13 shr 19));
      end;

      if (HashLY = 0) and (HashRot13 = 0) then
      begin
        // Ситуация маловероятная.
        HashRot13 := $ABCDEFAB;
      end;

    finally
      if WasGetMem then
        FreeMem(Ar);
    end;
  end;

  Result := IntToStr(HashLY) + '$' + IntToStr(HashRot13);
end;

function GenerateStringHashLY(S: string): Cardinal;
var
  Hash: Cardinal;
  I: Integer;
  aStr: AnsiString;
begin
  Hash := 0;
  aStr := AnsiString(S); // Для юникодных Дельфи: преобразуем в Ansi-строку

  for I := 1 to Length(aStr) do
  begin
    Hash := Hash * 1664525 + Ord(aStr[I]) + 1013904223
  end;

  Result := Cardinal(Hash);
end;

function GetTempPath: string;
begin
  Result := IncludeTrailingPathDelimiter(GetTempDir);
end;

{ TBaseObject }

procedure TBaseObject.ClearObjectRef(ARef: TObject);
begin
  ARef.Free;
end;

procedure TBaseObject.ClearRefList;
var
  I: Integer;
  Obj: TObject;
begin
  if Assigned(FRefList) then
  begin
    for I := 0 to FRefList.Count - 1 do
    begin
      Obj := FRefList[I];
      ClearObjectRef(Obj);
    end;
    FRefList.Clear;
  end;
end;

constructor TBaseObject.Create;
begin
  FRefList := TList.Create;
end;

function TBaseObject.CreateHashedStringList: THashedStringList;
begin
  RegObj(Result, THashedStringList.Create);
end;

function TBaseObject.CreateStringList: TStringList;
begin
  RegObj(Result, TStringList.Create);
end;

function TBaseObject.CreateMemoryStream: TMemoryStream;
begin
  RegObj(Result, TMemoryStream.Create);
end;

destructor TBaseObject.Destroy;
begin
  if Assigned(FRefList) then
  begin
    ClearRefList; // Будут уничтожены все объекты, находящиеся в списке
    FRefList.Free;
  end
  else
    raise Exception.Create('TBaseObject.Destroy -> Call of constructor for TBaseObject was skipped!');
  inherited;
end;


procedure TBaseObject.RegObj(var Ref; Obj: TObject);
begin
  Pointer(Ref) := DoRegObj(Obj);
end;

function TBaseObject.DoRegObj(Obj: TObject): TObject;
begin
  if Assigned(FRefList) then
  begin
    FRefList.Add(Obj);
    Result := Obj;
  end
  else
    raise Exception.Create('TBaseObject.RegObj -> Call of constructor for TBaseObject was skipped!');

end;

procedure TBaseObject.FreeObj(Obj: TObject);
var
  Idx: Integer;
begin
  Idx := FRefList.IndexOf(Obj);
  if Idx >= 0 then
    FRefList.Delete(Idx);
  ClearObjectRef(Obj);
end;

end.