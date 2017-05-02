unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  fgl,
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, LinesPlay;

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
    sf : array[0..10,0..10] of integer;
    board : array[0..10,0..10] of integer;
    startSquare : TPoint;
    endSquare : TPoint;
    counter: integer;
    path : array of TPoint;
    clearBallsCount:integer;
    clearBalls : array[0..100] of TPoint;
    procedure InitBoard();
    procedure Clear();
    procedure DrawBoard();
    procedure DrawTest();
    procedure ClearPath();
    procedure DrawPath();
    procedure CheckLines();
    procedure checkVertLine(x,y:integer);
    procedure checkHorzLine(x,y:integer);
    procedure checkDiag1Line(x,y:integer);
    procedure checkDiag2Line(x,y:integer);
    function addNewBalls():boolean;
    procedure InitSearch(s,e:TPoint);
    procedure DrawBall(x,y:integer; color: integer);
    procedure DrawSmallBall(x,y:integer; color: integer);
    procedure DrawSquare(x,y:integer; selected: boolean);
    function SearchPath(s,e:TPoint): integer;
    procedure FillNeighbors(list : array of TPoint);
  public
    { public declarations }

    constructor Create(canvas : TCanvas);
    procedure Draw();
    procedure OnClick(x,y:integer);
  end;


  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
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
  x_size := 9;
  y_size := 9;
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

procedure TGameBoard.DrawSmallBall(x,y:integer; color: integer);
var
  xx,yy:integer;
  r:integer;
begin
  xx := x*cell_size - cell_size div 2 + left_margin;
  yy := y*cell_size - cell_size div 2 + top_margin;
  r:=5;
  myCanvas.Pen.Color:= ballsColor[color];
  myCanvas.Brush.Color:=ballsColor[color];
  myCanvas.Ellipse(xx-r,yy-r,xx+r,yy+r);
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

    if selected then  myCanvas.Brush.Color:=clGray
           else myCanvas.Brush.Color:=clLtGray;

    myCanvas.Rectangle(xx,yy,xx+cell_size,yy+cell_size);

    myCanvas.Font.Color := clGreen;
    myCanvas.Font.Size := 10;
    myCanvas.TextOut(xx,yy, IntToStr(sf[x,y]));
    DrawBall(x,y,board[x,y]);
  end;

procedure TGameBoard.DrawTest();
var
  xx,yy:integer;
  i,j:integer;
  r:integer;
begin
  for i:=0 to 10 do
  for j:=0 to 10 do
  begin
    xx := (i-1)*cell_size + left_margin;
    yy := (j-1)*cell_size + top_margin;
    myCanvas.Font.Color := clGreen;
    myCanvas.Font.Size := 10;
//    myCanvas.Pen.Color:= ballsColor[color];
    myCanvas.Brush.Color:=clWhite;
    myCanvas.TextOut(xx,yy, IntToStr(sf[i,j]));
  end;
end;

procedure TGameBoard.ClearPath();
var
  i,n:integer;
begin
  n:=Length(path);
  for i:=0 to n-1 do
      DrawSquare(path[i].x,path[i].y,false);
   Form1.Refresh;
end;

procedure TGameBoard.DrawPath();
var
  i,n:integer;
begin
  n:=Length(path);
  for i:=n-1 downto 0 do
    begin
     DrawSmallBall(path[i].x,path[i].y,0);
     Sleep(20);
     Form1.Refresh;
    end;
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
//  DrawTest();
end;

procedure TGameBoard.InitSearch(s,e:TPoint);
var
  i,j:integer;
  list: array of TPoint;
begin
 for i:=0 to 10 do
 for j:=0 to 10 do
   if board[i,j] >0 then sf[i,j] := 100
      else sf[i,j] := 0;

 for i:=0 to 10 do
     begin
      sf[x_size+1,i] := 100;
      sf[0,i] := 100;
      sf[i,y_size+1] := 100;
      sf[i,0] := 100;
     end;

   counter := 1;
   SetLength(list, 1);
   list[0] := s;
   sf[s.x,s.y] := counter;
   FillNeighbors(list);
end;

procedure TGameBoard.FillNeighbors(list : array of TPoint);
var
  newlist : array of TPoint;
  i,n,c,l: integer;
  x,y:integer;
begin
// DrawTest();
 l:=Length(list);
  SetLength(newlist, 4*l);
  n:=0;
  x := list[0].x;
  y := list[0].y;
  c := sf[x,y] + 1;
  for i:=0 to l-1 do
    begin
     x := list[i].x;
     y := list[i].y;
     if sf[x-1,y] = 0 then
          begin
           sf[x-1,y] := c;
           newlist[n]:=Point(x-1,y);
           n:=n+1;
          end;
     if sf[x+1,y] = 0 then
          begin
           sf[x+1,y] := c;
           newlist[n]:=Point(x+1,y);
           n:=n+1;
          end;
     if sf[x,y-1] = 0 then
          begin
           sf[x,y-1] := c;
           newlist[n]:=Point(x,y-1);
           n:=n+1;
          end;
     if sf[x,y+1] = 0 then
          begin
           sf[x,y+1] := c;
           newlist[n]:=Point(x,y+1);
           n:=n+1;
          end;
    end;
  if n>0 then FillNeighbors(Slice(newlist, n));
end;

function TGameBoard.SearchPath(s,e:TPoint): integer;
var
  i,n,nn:integer;
  x,y:integer;
begin
  x:=e.x;
  y:=e.y;
  nn := sf[x,y];
  if nn > 0 then
       Begin
         SetLength(path, nn);
         path[0]:=Point(x,y);
         n:=nn;
         for i:=1 to nn-1 do
             begin
              if sf[x-1,y] = n-1 then
                   begin
                    path[i]:=Point(x-1,y);
                    x := x-1;
                   end
              else if sf[x+1,y] = n-1 then
                      begin
                        path[i]:=Point(x+1,y);
                        x := x+1;
                      end
              else if sf[x,y-1] = n-1 then
                      begin
                        path[i]:=Point(x,y-1);
                        y:=y-1;
                      end
              else if sf[x,y+1] = n-1 then
                   begin
                     path[i]:=Point(x,y+1);
                     y:=y+1;
                   end;
              n:=sf[x,y];
             end;
       end;
 result := nn;
end;

procedure  TGameBoard.CheckLines();
var
  x,y: integer;
  i:integer;
begin
 clearBallsCount:=0;
 for x:=1 to x_size do
 for y:=1 to y_size do
 begin
  if board[x,y] >0 then
       begin
         checkHorzLine(x,y);
         checkVertLine(x,y);
         checkDiag1Line(x,y);
         checkDiag2Line(x,y);
       end;
 end;
 for i:=0 to clearBallsCount do
  board[clearBalls[i].x,clearBalls[i].y]:=0;
end;

procedure  TGameBoard.checkVertLine(x,y:integer);
var
  color:integer;
  yy,i,len:integer;
begin
  color:=board[x,y];
  yy:=y+1;
  while board[x,yy] = color do
        yy:=yy+1;
  len := yy-y;
  if len >4 then
       begin
         for i:=0 to len-1 do
           clearBalls[ClearBallsCount+i]:=Point(x,y+i);
          ClearBallsCount:=ClearBallsCount+len;
       end
end;


procedure  TGameBoard.checkHorzLine(x,y:integer);
var
  color:integer;
  xx,i,len:integer;
begin
  color:=board[x,y];
  xx:=x+1;
  while board[xx,y] = color do
        xx:=xx+1;
  len := xx-x;
  if len >4 then
       begin
         for i:=0 to len-1 do
           clearBalls[ClearBallsCount+i]:=Point(x+i,y);
          ClearBallsCount:=ClearBallsCount+len;
       end
end;

procedure TGameBoard.checkDiag1Line(x,y:integer);
var
  color:integer;
  xx,yy,i,len:integer;
begin
  color:=board[x,y];
  xx:=x+1;
  yy:=y+1;
  while board[xx,yy] = color do
        begin
         xx:=xx+1;
         yy:=yy+1;
        end;
  len := xx-x;
  if len >4 then
       begin
         for i:=0 to len-1 do
           clearBalls[ClearBallsCount+i]:=Point(x+i,y+i);
          ClearBallsCount:=ClearBallsCount+len;
       end
end;

procedure TGameBoard.checkDiag2Line(x,y:integer);
var
  color:integer;
  xx,yy,i,len:integer;
begin
  color:=board[x,y];
  xx:=x+1;
  yy:=y-1;
  while board[xx,yy] = color do
        begin
         xx:=xx+1;
         yy:=yy-1;
        end;
  len := xx-x;
  if len >4 then
       begin
         for i:=0 to len-1 do
           clearBalls[ClearBallsCount+i]:=Point(x+i,y-i);
          ClearBallsCount:=ClearBallsCount+len;
       end
end;

function TGameBoard.addNewBalls():boolean;
var
   emptySquare : array[0..100] of TPoint;
   emptySquareCount:integer;
   x,y:integer;
   new1, new2, new3 :integer;
   c1,c2,c3:integer;
  begin
   emptySquareCount:=0;
   for x:=1 to x_size do
   for y:=1 to y_size do
    if board[x,y] = 0 then
         begin
          emptySquare[emptySquareCount]:=Point(x,y);
          emptySquareCount:=emptySquareCount+1;
         end;
   if emptySquareCount>2 then
        begin
           new1 := Random(emptySquareCount);
           new2 := Random(emptySquareCount-1);
           new3 := Random(emptySquareCount-2);
           if new1<=new2 then new2:= new2 +1;
           if new1<=new3 then new3:= new3 +1;
           if new2<=new3 then new3:= new3 +1;
           c1 := Random(7)+1;
           c2 := Random(7)+1;
           c3 := Random(7)+1;
           Board[emptySquare[new1].x,emptySquare[new1].y] := c1;
           Board[emptySquare[new2].x,emptySquare[new2].y] := c2;
           Board[emptySquare[new3].x,emptySquare[new3].y] := c3;
         result := true;
        end
   else
         result := false;
  end;

procedure TGameBoard.OnClick(x,y:integer);
var
  xx,yy :integer;
begin
 xx :=(x-left_margin) div cell_size+1;
 yy :=(y-top_margin) div cell_size+1;
 if (startSquare.x = 0) then
      begin
        if board[xx,yy] >0 then
             begin
              startSquare := Point(xx,yy);
              DrawSquare(xx,yy, true);
             end;
      end
 else
      begin
       if board[xx,yy] >0 then
             begin
               DrawSquare(startSquare.x,startSquare.y, false);
              startSquare := Point(xx,yy);
              DrawSquare(xx,yy, true);
             end
       else
             begin

              endSquare := Point(xx,yy);
              InitSearch(startSquare,endSquare);
              if SearchPath(startSquare,endSquare) >0  then
                   begin
                    DrawSquare(startSquare.x,startSquare.y, false);
                    DrawPath();
                    board[xx,yy] := board[startSquare.x,startSquare.y];
                    board[startSquare.x,startSquare.y] := 0;
                   // DrawSquare(startSquare.x,startSquare.y, false);
                    CheckLines();
                    addNewBalls();
                    Draw();
                   // DrawSquare(xx,yy, false);
                    //ClearPath();
                    startSquare.x :=0;
                   end;
             end;

      end;
// Draw();
end;


{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  gameboard := TGameBoard.Create(image1.Canvas);
  gameboard.InitBoard();
  gameboard.Draw();

end;

procedure TForm1.Button2Click(Sender: TObject);
begin
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

