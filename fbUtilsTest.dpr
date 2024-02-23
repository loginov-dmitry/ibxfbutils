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

// JCL_DEBUG_EXPERT_GENERATEJDBG OFF
// JCL_DEBUG_EXPERT_INSERTJDBG OFF
program fbUtilsTest;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

uses
{$IFDEF FPC}
  Interfaces,
{$ENDIF}
  Forms,
  TestForm in 'TestForm.pas' {Form1},
  fbUtilsBase in 'fbUtilsBase.pas',
  fbTypes in 'fbTypes.pas',
  fbSomeFuncs in 'fbSomeFuncs.pas',
  fbUtilsPool in 'fbUtilsPool.pas',
  {$IFnDEF FPC}fbUtilsLoading in 'fbUtilsLoading.pas',{$ENDIF}
  fbUtilsBackupRestore in 'fbUtilsBackupRestore.pas',
  fbUtilsIniFiles in 'fbUtilsIniFiles.pas',
  fbUtilsDBStruct in 'fbUtilsDBStruct.pas',
  fbUtilsCheckDBStruct in 'fbUtilsCheckDBStruct.pas',
  ibxFBUtils in 'ibxFBUtils.pas';

{$R *.res}

{$IFnDEF FPC}
{$IF RTLVersion >= 18.00}
   {$DEFINE D2007PLUS}
{$IFEND}
{$ENDIF}

begin
  Application.Initialize;
  {$IFDEF D2007PLUS}
  Application.MainFormOnTaskbar := True;
  ReportMemoryLeaksOnShutdown := True;
  {$ENDIF}
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
