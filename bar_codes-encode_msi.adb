separate (Bar_Codes)

package body Encode_MSI is

  --  Adapted from Bar_Code_Drawing:
  --
  --    Drawing MSI bar codes (also called MSI Plessey and Modified Plessey)
  --
  --    Copyright (C) by PragmAda Software Engineering
  --
  --    Released under the terms of the 3-Clause BSD License.
  --    See https://opensource.org/licenses/BSD-3-Clause

  subtype Digit_Value is Integer range 0 .. 9;
  subtype Digit is Character range '0' .. '9';
  function D2N (D : Digit) return Natural is (Character'Pos (D) - Character'Pos ('0'));

  function Luhn_Checksum (Input : String) return Digit_Value is
    Sum           : Natural := 0;
    D             : Natural;
    reverse_index : Integer := Input'Last;
  begin
    for I in Input'Range loop
      D := D2N (Input (reverse_index));
      reverse_index := reverse_index - 1;
      if I rem 2 = 1 then
        D := 2 * D;
        if D > 9 then
          D := D - 9;
        end if;
      end if;
      Sum := Sum + D;
    end loop;
    return (9 * Sum) rem 10;
  end Luhn_Checksum;

  Symbol_Width : constant := 12;  --  Each digit has 4 bits of 3 bars
  Start_Width  : constant :=  3;  --  Start symbol is a 1 bit
  Stop_Width   : constant :=  4;  --  Stop symbol is 00; trailing white bars ignored

  function Valid (text : String) return Boolean is
    (for all C of text => C in Digit);

  function Code_Modules_Width (text : String) return Positive is
    (Symbol_Width * (text'Length + 1) + Start_Width + Stop_Width);  --  +1 for Luhn checksum digit

  procedure Draw (bc : in out Bar_Code; text : String) is

    procedure Bar (offset, width : Natural) is
    begin
      Filled_Rectangle
        (Bar_Code'Class (bc),  --  Will use the concrete child method for displaying a rectangle
           (left   => offset,
            bottom => 0,
            width  => width,
            height => 1));
    end Bar;

    X : Natural := 0;

    procedure Draw_Bit (Bit : in Natural) is
    begin
      Bar (X, (if Bit = 0 then 1 else 2));
      X := X + 3;
    end Draw_Bit;

    subtype Nibble is String (1 .. 4);

    type Nibble_Map is array (Digit_Value) of Nibble;

    Map : constant Nibble_Map :=
      (0 => "0000", 1 => "0001", 2 => "0010", 3 => "0011", 4 => "0100",
       5 => "0101", 6 => "0110", 7 => "0111", 8 => "1000", 9 => "1001");

    procedure Draw_Nibble (n : Nibble) is
    begin
      for bit of n loop
        Draw_Bit (D2N (bit));
      end loop;
    end Draw_Nibble;

    digit_bits : Nibble;

  begin
    if not Valid (text) then
      raise Cannot_Encode with "Message must be all in decimal digits";
    end if;

    Draw_Bit (1);  --  Start code
    for I in text'Range loop
      digit_bits := Map (D2N (text (I)));
      Draw_Nibble (digit_bits);
    end loop;
    Draw_Nibble (Map (Luhn_Checksum (text)));
    Draw_Bit (0);  --  Stop code
    Draw_Bit (0);
  end Draw;

  function Fitting (text : String) return Module_Box is
  (0, 0, Code_Modules_Width (text), 1);

end Encode_MSI;
