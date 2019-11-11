unit util;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Dialogs, Process, StrUtils;

type

TUTil = class
  public
    class function CurrentExeDir() : String; static;
    class function StringToHex(S: string): string;
    class function HexToString(S: string): string;
    class function GetCurrentUserName: string;
    class function GetMacAddress : string;
end;
{$IFDEF WINDOWS}
function GetIfTable( pIfTable : Pointer;
                 VAR pdwSize  : LongInt;
                     bOrder   : LongInt ): LongInt; stdcall;
{$ENDIF}

implementation
{$IFDEF WINDOWS}
function GetIfTable( pIfTable : Pointer;
                 VAR pdwSize  : LongInt;
                     bOrder   : LongInt ): LongInt; stdcall; external 'IPHLPAPI.DLL';
{$ENDIF}

class function TUtil.CurrentExeDir() : String;
begin
  {$IFDEF LINUX}
  Result := ExtractFilePath(ParamStr(0));
  {$ENDIF}
  {$IFDEF DARWIN}
  Result := ExtractFilePath(ParamStr(0)) + '../../../';
  {$ENDIF}
  {$IFDEF WINDOWS}
  Result := ExtractFilePath(ParamStr(0));
  {$ENDIF}
end;

class function TUtil.StringToHex(S: string): string;
var
  i: integer;

begin
  Result := '';

  for i := 1 to Length( S ) do
    Result := Result + IntToHex( Ord( S[i] ), 2 );
end;

class function TUtil.HexToString(S: string): string;
var
  i: integer;

begin
  Result := '';

  for i := 1 to Length( S ) do
  begin
    if ((i mod 2) = 1) then
      Result := Result + Chr( StrToInt( '0x' + Copy( S, i, 2 )));
  end;
end;

class function TUtil.GetCurrentUserName: string;
begin
  Result := GetEnvironmentVariable('USERNAME');
  if Result = '' then
    Result := GetEnvironmentVariable('USER');
end;

class function TUtil.GetMacAddress : string;

{$IFDEF LINUX}
function GetLinuxMacAddress : string;
const
  linux_path     = '/sys/class/net/%s/address';
  default_device = 'eth0';

var
  f       : textfile;
  device,
  path,
  addr    : string;

begin
  Result := '';
  device := default_device;

  path := Format(linux_path,[device]);
  if (not FileExists(path)) then
  begin
     Result := '';
  end
  else
  begin
    AssignFile(f, path);
    reset(f);
    readln(f, addr);
    closefile(f);
    Result := addr;
  end;
end;
{$ENDIF}

{$IFDEF DARWIN}
function GetMacOSXMacAddress : string;
var
  theProcess: TProcess;
  theOutput: TStringList;
  i, j, theLine, thePos: integer;

begin
  theProcess := TProcess.Create(nil);
  theProcess.Executable := 'ifconfig';
  theProcess.Parameters.Add('en0');
  theProcess.Options := theProcess.Options + [poWaitOnExit, poUsePipes];
  theProcess.Execute;
  theOutput := TStringList.Create;
  theOutput.LoadFromStream(theProcess.Output);

  theLine := -1;
  for i := 0 to theOutput.Count - 1 do
  begin
    j := pos('ether', theOutput.Strings[i]);
    if (j > 0) then
    begin
      theLine := i;
      thePos := j + length('ether') + 1;
    end;
  end;
  if (theLine > -1) then
    Result := UpperCase(ExtractSubStr(theOutput.Strings[theLine], thePos, [' ']));

  theOutput.Free;
  theProcess.Free;
end;
{$ENDIF}

{$IFDEF WINDOWS}
function GetWinMacAddress: String;
const
  MAX_INTERFACE_NAME_LEN             = $100;
  ERROR_SUCCESS                      = 0;
  MAXLEN_IFDESCR                     = $100;
  MAXLEN_PHYSADDR                    = 8;

  MIB_IF_TYPE_ETHERNET               = 6;

  _MAX_ROWS_ = 20;

type

   MIB_IFROW            = Record
     wszName : Array[0 .. (MAX_INTERFACE_NAME_LEN * 2 - 1)] of char;
     dwIndex              : LongInt;
     dwType               : LongInt;
     dwMtu                : LongInt;
     dwSpeed              : LongInt;
     dwPhysAddrLen        : LongInt;
     bPhysAddr : Array[0 .. (MAXLEN_PHYSADDR-1)] of Byte;
     dwAdminStatus        : LongInt;
     dwOperStatus         : LongInt;
     dwLastChange         : LongInt;
     dwInOctets           : LongInt;
     dwInUcastPkts        : LongInt;
     dwInNUcastPkts       : LongInt;
     dwInDiscards         : LongInt;
     dwInErrors           : LongInt;
     dwInUnknownProtos    : LongInt;
     dwOutOctets          : LongInt;
     dwOutUcastPkts       : LongInt;
     dwOutNUcastPkts      : LongInt;
     dwOutDiscards        : LongInt;
     dwOutErrors          : LongInt;
     dwOutQLen            : LongInt;
     dwDescrLen           : LongInt;
     bDescr     : Array[0 .. (MAXLEN_IFDESCR - 1)] of Char;
     end;

   _IfTable = Record
                 nRows : LongInt;
                 ifRow : Array[1.._MAX_ROWS_] of MIB_IFROW;
              end;

var
   pIfTable  : ^_IfTable;
   TableSize : LongInt;
   tmp       : String;
   i,j       : Integer;
   ErrCode   : LongInt;
begin
   pIfTable := nil;
   //------------------------------------------------------------
   Result := '';
   try
      //-------------------------------------------------------
      // First: just get the buffer size.
      // TableSize returns the size needed.
      TableSize := 0; // Set to zero so the GetIfTabel function
                    // won't try to fill the buffer yet,
                    // but only return the actual size it needs.
      GetIfTable(pIfTable, TableSize, 1);
      if (TableSize < SizeOf(MIB_IFROW) + Sizeof(LongInt)) then
      begin
         Exit; // less than 1 table entry?!
      end; // if-end.

      // Second:
      // allocate memory for the buffer and retrieve the
      // entire table.
      GetMem(pIfTable, TableSize);
      ErrCode := GetIfTable(pIfTable, TableSize, 1);
      if (ErrCode <> ERROR_SUCCESS) then
      begin
         Exit; // OK, that did not work.
               // Not enough memory i guess.
      end; // if-end.

      // Read the ETHERNET addresses.
      for i := 1 to pIfTable^.nRows do
      try
         if (pIfTable^.ifRow[i].dwType=MIB_IF_TYPE_ETHERNET) and
             (pIfTable^.ifRow[i].dwOutOctets <> 0) then
         begin
            tmp := '';
            for j:=0 to pIfTable^.ifRow[i].dwPhysAddrLen-1 do
            begin
               tmp := tmp + format('%.2x:',
                      [ pIfTable^.ifRow[i].bPhysAddr[j] ] );
            end; // for-end.
            //-------------------------------------
            if Length(tmp)>0 then
            begin
              Result := Copy(tmp, 1, Length(tmp) - 1);
              Exit;
            end;
         end; // if-end.
      except
         Exit;
      end; // if-try-except-end.
   finally
      if Assigned(pIfTable) then FreeMem(pIfTable, TableSize);
   end; // if-try-finally-end.
end;
{$ENDIF}


begin
  Result := '';
  {$IFDEF LINUX}
  Result := GetLinuxMacAddress;
  {$ENDIF}
  {$IFDEF DARWIN}
  Result := GetMacOSXMacAddress;
  {$ENDIF}
  {$IFDEF WINDOWS}
  Result := GetWinMacAddress;
  {$ENDIF}
end;


end.


