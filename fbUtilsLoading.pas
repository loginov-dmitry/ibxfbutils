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
{ Модуль fbUtilsLoading - модуль, решающий некоторые вопросы загрузки IBX     }
{ (c) 2012 Логинов Дмитрий Сергеевич                                          }
{ Последнее обновление: 09.05.2012                                            }
{ Протестировано на D7, D2007, D2010, D-XE2                                   }
{ Адрес сайта: http://loginovprojects.ru/                                     }
{ e-mail: loginov_d@inbox.ru                                                  }
{                                                                             }
{ *************************************************************************** }

{
Этот модуль Вы можете свободно изменять.
Его следует подключать к своему проекту только если Firebird установлен ручным
способом. В этом случае требуется явно загружать клиентскую библиотеку GDS32.dll
по указанному пути. Также следует выставить переменную окружения FIREBIRD.
В принципе, данный модуль не является строго обязательным, но автор его рекомендует
для избежания некоторых проблем.
Самое главное, чтобы Ваше приложение не пыталось загружать GDS32.dll из папки
WINDOWS\System32\, поскольку этот файл может перезаписываться разными версиями
Firebird. А если ранее на компьютере был установлен Interbase, то с очень большой
долей вероятности файл WINDOWS\System32\GDS32.dll относится именно к Interbase, а
не к Firebird.

Ручной способ весьма хорош при распространении "коробочных" приложений. Firebird
обычно ставится в ту же папку, что и сама программа и привязывается к TCP-порту,
отличающемуся от стандартного. В этом случае коробочное приложение не зависит
от того, есть ли на компьютере другие версии Firebird.
Также очень часто приложения распространяются с "встроенной" версией Firebird (embedded).
В этом случае файл fbembed.dll переименовывается в GDS32.dll, а компоненты IBX
продолжают работать с Firebird как ни в чем не бывало.
}

unit fbUtilsLoading;

interface

uses
{$IFnDEF FPC}
  Windows, IBIntf,
{$ELSE}
  {$IFDEF MSWINDOWS}Windows, {$ENDIF}LCLIntf, LCLType, LMessages,
{$ENDIF}
  SysUtils;

{$IFnDEF FPC}
{$IF RTLVersion >= 23.00}
   {$DEFINE DXE2PLUS}
{$IFEND}
{$ENDIF}

var
  HGDS32: THandle;

const
  {Укажите путь, по которому установлена СУБД Firebird}
  FirebirdPath = 'C:\ProgramsFolder\FIREBIRD\';

  {Укажите путь к библиотеке GDS32.dll}
  GDS32FileName = FirebirdPath + 'bin\GDS32.dll';

implementation

procedure InitFBClient;
var
  Err: string;

  function TryLoadGDS32: Boolean;
  {$IFDEF DXE2PLUS}
  var
    ServerType: string;
  {$ENDIF}
  begin
  {$IFDEF DXE2PLUS}
    { В новых версиях IBX чего-то намутили... }
    ServerType := 'IBServer';
    Result := IBIntf.GetGDSLibrary(ServerType).TryIBLoad;
  {$ELSE}
    Result := IBIntf.GetGDSLibrary.TryIBLoad;
    // Здесь будет возникать ошибка, если Вы используете Delphi 7 и забыли обновить
    // библиотеку IBX (ссылка на обновление: http://ibase.ru/ibx/ibxdp711.zip)
  {$ENDIF}
  end;

begin
  {При необходимости Вы можете добавить реакцию приложения на возможные проблемы,
   связанные с загрузкой GDS32.dll. По умолчанию никаких ошибок не выводится, автор
   предлагает только заготовки}

  {Устанавливаем переменную окружения FIREBIRD. Это путь, по которому библиотека
   GDS32.dll будет искать файл с сообщениями "firebird.msg" и log-файл "firebird.log"}
  if not SetEnvironmentVariable(PChar('FIREBIRD'), PChar(FirebirdPath)) then
  begin
    // Здесь можно разместить обработку ошибки
  end;

  if GetModuleHandle('GDS32.dll') = 0 then // Если библиотека GDS32.dll еще не загружена
  begin
    if FileExists(GDS32FileName) then
    begin
      HGDS32 := LoadLibrary(GDS32FileName); // Загружаем библиотеку GDS32.dll
      if HGDS32 = 0 then
      begin
        Err := 'Указанный файл GDS32.dll присутствует, однако загрузить его не удалось';
      end else
      begin
        if not TryLoadGDS32 then
        begin
          Err := 'Указанный файл GDS32.dll загрузить удалось, однако библиотеке IBX он чем-то не понравился';
        end;
      end;
    end else // Если указанный файл не найден
    begin
      if TryLoadGDS32 then
      begin
        Err := 'Указанный файл GDS32.dll не найден, однако на компьютере GDS32.dll есть. Приложение может работать некорректно!';
      end
      else
      begin
        Err := 'Указанный файл GDS32.dll не найден. Библиотека GDS32.dll не установлена на данном компьютере!';
      end;
    end;
  end else
  begin
    Err := 'Библиотека GDS32.dll загружена ПРЕЖДЕВРЕМЕННО из другого места';
  end;

  Trim(Err); // До этой строки можно увидеть сообщение в отладчике
end;

initialization
  InitFBClient;
finalization
  if HGDS32 <> 0 then // Если библиотека GDS32.dll была загружена в ЭТОМ модуле, то выгружаем ее
  try
    FreeLibrary(HGDS32);
  except
    // Ошибка при выгрузке GDS32.dll
  end;
end.
