unit LinesGame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

TLinesGame = class
public
  { public declarations }
  x_size,y_size:integer;
  sf : array[0..10,0..10] of integer;
  board : array[0..10,0..10] of integer;
  startSquare : TPoint;
  endSquare : TPoint;
  counter: integer;
  path : array of TPoint;
  clearBallsCount:integer;
  clearBalls : array[0..100] of TPoint;

  constructor Create();
  procedure InitBoard();
  procedure CheckLines();
  procedure checkVertLine(x,y:integer);
  procedure checkHorzLine(x,y:integer);
  procedure checkDiag1Line(x,y:integer);
  procedure checkDiag2Line(x,y:integer);
  function addNewBalls():boolean;
  procedure InitSearch(s,e:TPoint);
  function SearchPath(s,e:TPoint): integer;
  procedure FillNeighbors(list : array of TPoint);

end;

implementation

constructor TLinesGame.Create();

begin

  x_size := 9;
  y_size := 9;
end;

procedure TLinesGame.InitBoard();
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


procedure TLinesGame.InitSearch(s,e:TPoint);
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

procedure TLinesGame.FillNeighbors(list : array of TPoint);
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

function TLinesGame.SearchPath(s,e:TPoint): integer;
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

procedure  TLinesGame.CheckLines();
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

procedure  TLinesGame.checkVertLine(x,y:integer);
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


procedure  TLinesGame.checkHorzLine(x,y:integer);
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

procedure TLinesGame.checkDiag1Line(x,y:integer);
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

procedure TLinesGame.checkDiag2Line(x,y:integer);
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

function TLinesGame.addNewBalls():boolean;
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
end.

