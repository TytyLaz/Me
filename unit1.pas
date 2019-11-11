unit Unit1;

{$mode objfpc}{$H+}

interface

uses
//  Classes, SysUtils, sqlite3conn, sqldb, db, Forms, Controls, Graphics, Dialogs,
//  StdCtrls;

   Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,blcksock;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Edit5: TEdit;
    Edit6: TEdit;
    Edit7: TEdit;
    Edit8: TEdit;
    Edit9: TEdit;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label9: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation
uses util;

{$R *.lfm}

{ TForm1 }
function  MyGetLocalIPs: string;
var
  TcpSock: TTCPBlockSocket;
  ipList: TStringList;
begin
  Result := '';
  ipList := TStringList.Create;
  try
    TcpSock := TTCPBlockSocket.create;
    try
      TcpSock.ResolveNameToIP(TcpSock.LocalName, ipList);
      Result := ipList.CommaText;
    finally
      TcpSock.Free;
    end;
  finally
    ipList.Free;
  end;
end;

procedure extraireMots(s : string; into : TStrings; viderListe : boolean = false; sep : TSysCharSet = [' ', ',']);
var
  i, n : integer;
  currentWord : string;
begin
  if viderListe then into.Clear;
  n := length(s);
  i := 0;
  while (i <= n) do
  begin
    currentWord := '';
    { on saute les séparateurs  }
 // while (i <= n) and (s[i] in sep) do
      inc(i);
    { récupération du mot courant  }
    while (i <= n) and not (s[i] in sep) do
    begin
      currentWord := currentWord + s[i];
      inc(i);
    end;
//   if (currentWord <> '') then
      into.Add(currentWord);
  end;
 end;

procedure TForm1.FormCreate(Sender: TObject);
Var

  ip1,ip2,username,computername :string;
  ligne : TStringList;
  j:integer;
begin
ligne := TStringList.Create;
username:=GetEnvironmentVariable('USERNAME');
computername:= GetEnvironmentVariable('COMPUTERNAME');
ExtraireMots(MyGetLocalIPs,ligne,true,[',']);
j:=ligne.Count;
ip1:=Ligne[j-2];
ip2:=Ligne[J-1];
Edit5.Text:=username;
Edit6.Text:=computername;
Edit7.Text:=ip1;
Edit8.Text:=ip2;
Edit9.Text:=TUtil.GetMacAddress();
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Form1.close;
end;



end.

