unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls;

const
  ballsColor: array[0..7] of TColor = (clBlack, clGreen,clRed, clTeal, clYellow, clBlue, clFuchsia, clWhite);

type

  TGameBoard = class
    private
    { private declarations }
    myCanvas : TCanvas;
    x_size,y_size:integer;
    cell_size : integer;
    left_margin,top_margin : integer;
    board : array[0..10,0..10] of integer;
    startSquare : TPoint;
    endSquare : TPoint;
    procedure InitBoard();
    procedure Clear();
    procedure DrawBoard();
    procedure DrawBall(x,y:integer; color: integer);
    procedure DrawSquare(x,y:integer; selected: boolean);
  public
    { public declarations }

    constructor Create(canvas : TCanvas);
    procedure Draw();
    procedure OnClick(x,y:integer);
  end;


  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Image1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { private declarations }
  public
    { public declarations }
  end;



  type ball = record
       x,y    : integer;
       color  : TColor;
       radius : integer;
       end;
var
  Form1: TForm1;
  gameboard:TGameBoard;
implementation

{$R *.lfm}

constructor TGameBoard.Create(canvas : TCanvas);

begin
  myCanvas := canvas;
  x_size := 7;
  y_size := 7;
end;

procedure TGameBoard.InitBoard();
var
  i,j:integer;
  rnd: integer;
begin
  startSquare := Point(0,0);
  endSquare := Point(0,0);
  for i:=0 to x_size do
   for j:=0 to y_size do
    begin
      board[i,j] := 0;
      rnd :=   + Random(30);
      if (rnd<8) then
           board[i,j] :=  1 + rnd;
    end;
end;

procedure TGameBoard.Clear();
begin
  myCanvas.Brush.Color:=clwhite;
  myCanvas.Brush.Style:=bssolid;
  myCanvas.Rectangle(0,0,myCanvas.Width, myCanvas.Height);
end;

procedure TGameBoard.DrawBoard();
var
  BoardWidth, BoardHeight : integer;
  dx : integer;
  i :integer;
begin
  BoardWidth := myCanvas.Height - 40;
  BoardHeight := BoardWidth;
  left_margin := ( myCanvas.Width - BoardWidth) div 2 ;
  top_margin := ( myCanvas.Height - BoardHeight) div 2 ;
  myCanvas.Brush.Color:=clLtGray;
  myCanvas.Brush.Style:=bssolid;
  myCanvas.Rectangle(left_margin,top_margin,BoardWidth+left_margin,BoardHeight+top_margin);

  dx := BoardWidth div x_size;
  cell_size := dx;
  myCanvas.Pen.Color:= clGreen;
  for i := 1 to x_size do
      begin
        myCanvas.MoveTo(left_margin + dx*i,top_margin);
        myCanvas.LineTo(left_margin + dx*i,BoardHeight+top_margin);
      end;
  for i := 1 to y_size do
      begin
        myCanvas.MoveTo(left_margin ,top_margin + dx*i);
        myCanvas.LineTo(left_margin + BoardWidth,top_margin + dx*i);
      end;
end;

procedure TGameBoard.DrawBall(x,y:integer; color: integer);
var
  xx,yy:integer;
  r:integer;
begin
  if (color = 0) then exit;

  xx := x*cell_size - cell_size div 2 + left_margin;
  yy := y*cell_size - cell_size div 2 + top_margin;
  r:=15;
  myCanvas.Pen.Color:= ballsColor[color];
  myCanvas.Brush.Color:=ballsColor[color];
  myCanvas.Ellipse(xx-r,yy-r,xx+r,yy+r);
end;

procedure TGameBoard.DrawSquare(x,y:integer; selected: boolean);
var
  xx,yy:integer;
  r:integer;
  begin
    if (x<1) then exit;
    xx := (x-1)*cell_size + left_margin;
    yy := (y-1)*cell_size + top_margin;
    myCanvas.Pen.Color:= clGreen;
    if selected then  myCanvas.Brush.Color:=clRed
           else myCanvas.Brush.Color:=clLtGray;

    myCanvas.Rectangle(xx,yy,xx+cell_size,yy+cell_size);

    DrawBall(x,y,board[x,y]);
  end;

procedure TGameBoard.Draw();
var
  i,j : integer;
begin
  Clear();
  DrawBoard();
  for i:=1 to x_size do
     for j:=1 to y_size do
      begin
        DrawBall(i,j, board[i,j]);
      end;
end;

procedure TGameBoard.OnClick(x,y:integer);
var
  xx,yy :integer;
begin
 xx :=(x-left_margin) div cell_size+1;
 yy :=(y-top_margin) div cell_size+1;
 if (startSquare.x = 0) then
      begin
        startSquare := Point(xx,yy);
        DrawSquare(xx,yy, true);
      end
 else
      begin
        DrawSquare(startSquare.x,startSquare.y, false);
        endSquare := Point(xx,yy);
        DrawSquare(xx,yy, true);
      end;




 //Draw();
end;


{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  gameboard := TGameBoard.Create(image1.Canvas);
  gameboard.InitBoard();
  gameboard.Draw();

end;


procedure TForm1.Image1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  label1.Caption := IntToStr(X);
  label2.Caption := IntToStr(Y);
  gameboard.OnClick(X,Y);
end;

end.

