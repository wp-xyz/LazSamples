unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Menus, LCLType;

type

  { TMainForm }

  TMainForm = class(TForm)
    btnMinimizeToTray: TButton;
    miSeparator: TMenuItem;
    miAbout: TMenuItem;
    miShow: TMenuItem;
    miClose: TMenuItem;
    TrayPopupMenu: TPopupMenu;
    TrayIcon: TTrayIcon;
    procedure btnMinimizeToTrayClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure miAboutClick(Sender: TObject);
    procedure miShowClick(Sender: TObject);
    procedure miCloseClick(Sender: TObject);
    procedure TrayIconClick(Sender: TObject);
    procedure TrayIconDblClick(Sender: TObject);
  private

  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.btnMinimizeToTrayClick(Sender: TObject);
begin
  Hide;
  TrayIcon.Show;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  TrayIcon.Icon.Assign(Application.Icon);
  TrayIcon.BalloonTitle := 'TrayIcon Demo';
  TrayIcon.BalloonHint := 'This is the balloon hint of the tray icon.';
  TrayIcon.BalloonFlags := bfInfo;
  TrayIcon.Hint := 'Demo for using Application.ShowMainForm and a TrayIcon';
  TrayIcon.PopupMenu := TrayPopupMenu;
end;

procedure TMainForm.miAboutClick(Sender: TObject);
begin
  ShowMessage('This is a demo for using Application.ShowMainForm and a TrayIcon.');
end;

procedure TMainForm.miShowClick(Sender: TObject);
begin
  Show;
  TrayIcon.Hide;
end;

procedure TMainForm.TrayIconClick(Sender: TObject);
begin
  TrayIcon.ShowBalloonHint
end;

procedure TMainForm.miCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.TrayIconDblClick(Sender: TObject);
begin
  Show;
  TrayIcon.Hide;
end;

end.

