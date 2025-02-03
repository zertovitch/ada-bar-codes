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
  function D2N (D : Digit) return Digit_Value is (Character'Pos (D) - Character'Pos ('0'));

  function Luhn_Check_Digit (input : String) return Digit_Value is
    --  Compute the extra digit x such that the Luhn checksum is 0.
    sum           : Natural := 0;
    d, x          : Natural;
    odd_10_power  : Boolean := True;
  begin
    for char of reverse input loop
      d := D2N (char);
      if odd_10_power then
        d := 2 * d;
        if d > 9 then
          d := d - 9;
        end if;
      end if;
      sum := sum + d;
      odd_10_power := not odd_10_power;
    end loop;
    x := 9 * sum;  --  We want (x + sum) to be congruent to 0, modulo 10.
    return x rem 10;
  end Luhn_Check_Digit;

  Symbol_Width : constant := 12;  --  Each digit has 4 bits of 3 bars
  Start_Width  : constant :=  3;  --  Start symbol is a 1 bit
  Stop_Width   : constant :=  4;  --  Stop symbol is 00; trailing white bars ignored

  function Valid (text : String) return Boolean is
    (for all C of text => C in Digit);

  function Code_Modules_Width (text : String) return Positive is
    (Symbol_Width * (text'Length + 1) + Start_Width + Stop_Width);  --  +1 for Luhn check digit

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

    procedure Draw_Nibble (n : Digit_Value) is
    begin
      Draw_Bit (n / 8);
      Draw_Bit (n / 4 mod 2);
      Draw_Bit (n / 2 mod 2);
      Draw_Bit (n mod 2);
    end Draw_Nibble;

  begin
    if not Valid (text) then
      raise Cannot_Encode with "Message must be all in decimal digits";
    end if;

    Draw_Bit (1);  --  Start code
    for C of text loop
      Draw_Nibble (D2N (C));
    end loop;
    Draw_Nibble (Luhn_Check_Digit (text));
    Draw_Bit (0);  --  Stop code
    Draw_Bit (0);
  end Draw;

  function Fitting (text : String) return Module_Box is
  (0, 0, Code_Modules_Width (text), 1);

end Encode_MSI;
