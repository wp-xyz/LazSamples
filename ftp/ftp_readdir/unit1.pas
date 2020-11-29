{ Demonstrates how a directory can be read from an ftp server using synapse. }

unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnLogin: TButton;
    btnRead: TButton;
    edDirectory: TEdit;
    edFileMask: TEdit;
    edPort: TEdit;
    edPassword: TEdit;
    edUser: TEdit;
    edServer: TEdit;
    Label1: TLabel;
    lblDirectory: TLabel;
    lblServer: TLabel;
    lblPort: TLabel;
    lblPassword: TLabel;
    lblUserName: TLabel;
    ListBox: TListBox;
    Memo: TMemo;
    Splitter1: TSplitter;
    procedure btnLoginClick(Sender: TObject);
    procedure btnReadClick(Sender: TObject);
  private
    procedure ExecFTP(const ADir: String);
    procedure ExtractFiles;

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  ftpSend;

{ TForm1 }

procedure TForm1.btnLoginClick(Sender: TObject);
begin
  ExecFTP('');
end;

procedure TForm1.btnReadClick(Sender: TObject);
begin
  ExecFTP(edDirectory.Text);
end;

procedure TForm1.ExecFTP(const ADir: String);
var
  ftp: TFTPSend;
begin
  Memo.Lines.Clear;
  Listbox.Items.Clear;

  ftp := TFTPSend.Create;
  try
    ftp.Username := edUser.text;
    ftp.Password := edPassword.text;
    ftp.TargetHost := edServer.text;
    ftp.TargetPort := edPort.text;
    if not ftp.Login then
    begin
      ShowMessage('Cannot login');
      exit;
    end;

    if ADir <> '' then
      ftp.ChangeWorkingDir(ADir);

    ftp.List('', false);
    Memo.Lines.AddStrings(ftp.FtpList.Lines, false);

    if ADir <> '' then
      ExtractFiles;

    ftp.Logout;
  finally
    ftp.Free;
  end;
end;

procedure TForm1.ExtractFiles;
var
  s: String;
  ext: String;
begin
  if edFileMask.Text = '' then
    ext := '*'
  else if edFileMask.Text[1] <> '.' then
    ext := '.' + edFileMask.Text
  else
    ext := edFileMask.Text;

  for s in Memo.Lines do begin
    if s = '' then Continue;
    if s[1] = '-' then
      if (edFileMask.Text = '*') or (ExtractFileExt(s) = ext) then
        ListBox.Items.Add(Copy(s, 57, MaxInt));
  end;
end;

end.

