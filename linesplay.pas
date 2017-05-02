unit LinesPlay;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

TLinesPlay = class
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
end;

implementation

constructor TLinesPlay.Create();

begin

  x_size := 9;
  y_size := 9;
end;
end.

