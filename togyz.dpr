program togyz;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  System.Classes,
  System.Math,
  tog in 'tog.pas';

var
  s: string;

procedure humanPlay;
var
  tBoard: TTogyzBoard;
  inp: String;
  inpInt: Integer;
begin
  tBoard := TTogyzBoard.Create;
  try
    while true do
    begin
      WriteLn(CRLF + 'Enter your move (1-9, 0 - exit): ');
      ReadLn(inp);

      inpInt := StrToIntDef(inp, 0);
      if inpInt = 0 then
        break;

      if ((inpInt >= 1) and (inpInt <= 9)) then
      begin
        tBoard.makeMove(inpInt);
        tBoard.printNotation;
        tBoard.printPosition;
        if tBoard.isGameFinished then
          break;
      end;
    end;

    tBoard.printNotation;
    tBoard.printPosition;
    WriteLn(CRLF + 'Game over: ' + tBoard.getScore + '. Result: ' +
      IntToStr(tBoard.getResult) + CRLF);
  finally
    tBoard.Free;
  end;
end;

procedure machinePlay;
var
  tBoard: TTogyzBoard;
  inp: String;
  i, num, win, draw, loss: Integer;
  t1: Cardinal;
begin
  while true do
  begin
    WriteLn(CRLF + 'Enter number of iterations (1-100000): ');
    ReadLn(inp);
    num := StrToIntDef(inp, 1);
    if ((num >= 1) and (num <= 100000)) then
      break;
  end;

  win := 0;
  draw := 0;
  loss := 0;
  t1 := TThread.GetTickCount;

  for i := 1 to num do
  begin
    tBoard := TTogyzBoard.Create;
    try
      while not tBoard.isGameFinished do
        tBoard.makeRandomMove;

      if (num <= 5) then
      begin
        tBoard.printPosition;
        WriteLn(CRLF + 'Game over: ' + tBoard.getScore + '. Result: ' +
          IntToStr(tBoard.getResult) + CRLF);
      end;

      if tBoard.getResult = 1 then
        Inc(win)
      else if tBoard.getResult = -1 then
        Inc(loss)
      else if tBoard.getResult = 0 then
        Inc(draw)
      else
        WriteLn('What??');
    finally
      tBoard.Free;
    end;
  end;

  WriteLn('Elapsed: ' + FloatToStr((TThread.GetTickCount - t1) / 1000) + ' sec');
  WriteLn('W: ' + IntToStr(win) + ', D: ' + IntToStr(draw) + ', L: ' +
    IntToStr(loss));
end;

procedure randomPlay;
var
  tBoard: TTogyzBoard;
  inp, ai: String;
  currentColor, color, move: Integer;
begin
  Randomize;
  currentColor := 0;

  tBoard := TTogyzBoard.Create;
  try
    while true do
    begin
      WriteLn(CRLF + 'Enter your color (0 - white, 1 - black): ');
      ReadLn(inp);
      color := StrToIntDef(inp, -1);
      if ((color = 0) or (color = 1)) then
        break;
    end;

    while not tBoard.isGameFinished do
    begin
      if currentColor = color then
      begin
        while true do
        begin
          WriteLn(CRLF + 'Enter your move (1-9, 0 - exit): ');
          ReadLn(inp);
          move := StrToIntDef(inp, -1);
          if ((move >= 0) and (move <= 9)) then
            break;
        end;

        if move = 0 then
          break;

        tBoard.makeMove(move);
        tBoard.printNotation;
        tBoard.printPosition;
        currentColor := System.Math.IfThen(currentColor = 0, 1, 0);
      end

      else
      begin
        ai := tBoard.makeRandomMove;
        WriteLn('AI move: ' + ai);
        tBoard.printPosition;
        currentColor := System.Math.IfThen(currentColor = 0, 1, 0);
      end;
    end;

    tBoard.printNotation;
    tBoard.printPosition;
    WriteLn(CRLF + 'Game over: ' + tBoard.getScore + '. Result: ' +
      IntToStr(tBoard.getResult) + CRLF);
  finally
    tBoard.Free;
  end;
end;

begin
  try
    WriteLn('Welcome to the TogyzKumalak world!' + CRLF);
    WriteLn('Enter the mode (h - human play, m - random machine, r - against random AI): ');

    ReadLn(s);
    if s = 'h' then
      humanPlay
    else if s = 'm' then
      machinePlay
    else if s = 'r' then
      randomPlay;
  except
    on E: Exception do
      WriteLn(E.ClassName, ': ', E.Message);
  end;

end.
