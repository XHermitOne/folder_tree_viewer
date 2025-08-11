{
Расширенные типы для использования в программе

Версия: 0.0.3.1
}
unit exttypes;


{$mode objfpc}{$H+}

interface

uses
  Classes, Contnrs;


type
  {Указатели на основные типы}
  // Целые
  PByte = ^Byte;		      // 0..255			                1 байт
  PShortInt = ^ShortInt;	  // -128..+127                     1 байт
  PWord = ^Word;		      // 0..65535                       2 байта
  PSmallInt = ^SmallInt;	  // -32768..+32767                 2 байта
  //PInteger = ^Integer;		  // -2147483648..+2147483647       4 байта
  PLongInt = ^LongInt;		  // -2147483648..+2147483647       4 байта
  PLongWord = ^LongWord;	  // 0..4294967295		            4 байта
  PCardinal = ^Cardinal;	  // 0..4294967295		            4 байта
  PInt64 = ^Int64;		      // -2^63..+2^63-1		            8 байт

  // Логические
  PBoolean = ^Boolean;		// 1 байт
  PByteBool = ^ByteBool;	// 1 байт
  PWordBool = ^WordBool;	// 2 байта
  PLongBool = ^LongBool;	// 4 байта

  // Вещественные
  PReal48 = ^Real48;		// 6 байт
  PReal = ^Real;                // 8 байт
  PSingle = ^Single;            // 4 байта
  PDouble = ^Double;            // 8 байт
  PExtended = ^Extended;        // 10 байт
  PComp = ^Comp;                // 8 байт
  PCurrency = ^Currency;        // 8 байт

  { Массив строк }
  TArrayOfString = Array Of String;

  STRING_ARRAY = Array[0..65535] of String;
  PSTRING_ARRAY = ^STRING_ARRAY;

  { Массив вариантов }
  VARIANT_ARRAY = Array[0..65535] of Variant;
  PVARIANT_ARRAY = ^VARIANT_ARRAY;

  { Запись строковых значений }
  TMemRecord = class(TStringList)
    public
      {
      Принудительно установить длину записи
      @param iCount Количество полей записи
      }
      procedure SetLength(iCount: Integer);
  end;

  { Набор значений строковых значений }
  TMemRecordSet = class(TObjectList)
    public
      function GetRecord(Index: Integer): TMemRecord;

      property Records[Index: Integer]: TMemRecord read GetRecord;
  end;

  { Таблица строковых значений }
  TMemTableOfString = TMemRecordSet;

  { Вектор строковых значений }
  TMemVectorItem = class(TObject)
    public
      datetime: AnsiString;
      value: AnsiString;
  end;


  TMemVectorOfString = class(TObjectList)
    public
      function GetPoint(Index: Integer): TMemVectorItem;

      function AddNewPoint(sDateTime: AnsiString; sValue: AnsiString): Integer;
      { Вывести на экран все точки. Для отладки }
      procedure PrintPoints();

      property Points[Index: Integer]: TMemVectorItem read GetPoint;
  end;

implementation

uses
  logfunc;

{
Принудительно установить длину записи
@param iCount Количество полей записи
}
procedure TMemRecord.SetLength(iCount: Integer);
var
  i: Integer;
begin
  if iCount > Count then
    for  i := Count to iCount - 1 do
      Add('')
  else
    for  i := Count - 1 downto iCount  do
      Delete(i);
end;

function TMemRecordSet.GetRecord(Index: Integer): TMemRecord;
begin
  Result := TMemRecord(Items[Index]);
end;

function TMemVectorOfString.GetPoint(Index: Integer): TMemVectorItem;
begin
  Result := TMemVectorItem(Items[Index]);
end;

{ Вывести на экран все точки. Для отладки }
procedure TMemVectorOfString.PrintPoints();
var
  i: Integer;
  point: TMemVectorItem;
begin
  for i := 0 to Count - 1 do
  begin
    point := GetPoint(i);
    logfunc.ServiceMsgFmt('Точка вектора <%s : %s>', [point.datetime, point.value]);
  end;
end;

function TMemVectorOfString.AddNewPoint(sDateTime: AnsiString; sValue: AnsiString): Integer;
var
  new_point: TMemVectorItem;
begin
  new_point := TMemVectorItem.Create;
  new_point.datetime := sDateTime;
  new_point.value := sValue;
  Result := Add(new_point);
end;

end.

