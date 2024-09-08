separate (Bar_Codes)

package body Encode_UPCA_EAN13 is

  --  Adapted from Bar_Code_Drawing:
  --
  --    Drawing UPC-A/EAN-13 bar codes
  --
  --    Copyright (C) by PragmAda Software Engineering
  --
  --    Released under the terms of the 3-Clause BSD License.
  --    See https://opensource.org/licenses/BSD-3-Clause

  subtype Digit_Value is Integer range 0 .. 9;
  subtype Digit is Character range '0' .. '9';

  function Valid (text : in String; kind : Code_UPCA_EAN13) return Boolean is
    ((for all C of text => C in Digit)
     and
     ((text'Length = 11 and kind = Code_UPCA)
      or
      (text'Length = 12 and kind = Code_EAN13)));

  function Checksum (Text : in String) return Digit_Value is
    subtype S11 is String (1 .. 11);
    S : constant S11 := Text (Text'First + (if Text'Length = 11 then 0 else 1) .. Text'Last);
    Sum : Natural := 0;
  begin
    for I in S'Range loop
      Sum := Sum + (if I rem 2 = 0 then 1 else 3) * (Character'Pos (S (I)) - Character'Pos ('0'));
    end loop;

    if Text'Length = 12 then
      Sum := Sum + Character'Pos (Text (Text'First)) - Character'Pos ('0');
    end if;

    Sum := Sum rem 10;

    return (if Sum > 0 then 10 - Sum else Sum);
  end Checksum;

  function Checksum (Text : in String) return Digit is
    (Character'Val (Checksum (Text) + Character'Pos ('0')));

  --  The extra digit of EAN-13 is encoded through the usage
  --  of two bar code sets for other digits.
  Code_Modules_Width : constant := 95;

  procedure Draw (bc : in out Bar_Code; text : String; kind : Code_UPCA_EAN13) is
    S : constant String (1 .. text'Length) := text;

    subtype Digit_Pattern is String (1 .. 7);  --  Each digit takes 7 modules
    type Pattern_Map is array (Digit) of Digit_Pattern;

    Set_A_Map : constant Pattern_Map := ('0' => "0001101",  --  Bar patterns for alphabet A (left half)
                                         '1' => "0011001",
                                         '2' => "0010011",
                                         '3' => "0111101",
                                         '4' => "0100011",
                                         '5' => "0110001",
                                         '6' => "0101111",
                                         '7' => "0111011",
                                         '8' => "0110111",
                                         '9' => "0001011");
    Set_B_Map : constant Pattern_Map := ('0' => "0100111",  --  Bar patterns for alphabet B (left half)
                                         '1' => "0110011",
                                         '2' => "0011011",
                                         '3' => "0100001",
                                         '4' => "0011101",
                                         '5' => "0111001",
                                         '6' => "0000101",
                                         '7' => "0010001",
                                         '8' => "0001001",
                                         '9' => "0010111");
    Set_C_Map : constant Pattern_Map := ('0' => "1110010",  --  Bar patterns for alphabet B (right half)
                                         '1' => "1100110",
                                         '2' => "1101100",
                                         '3' => "1000010",
                                         '4' => "1011100",
                                         '5' => "1001110",
                                         '6' => "1010000",
                                         '7' => "1000100",
                                         '8' => "1001000",
                                         '9' => "1110100");
    End_Guard    : constant String := "101";
    Middle_Guard : constant String := "01010";

    type A_or_B is (A, B);
    type EAN_A_Pattern is array (1 .. 6) of A_or_B;
    type EAN_Pattern_Map is array (Digit) of EAN_A_Pattern;

    EAN_A : constant EAN_Pattern_Map := ('0' => (A, A, A, A, A, A),
                                         '1' => (A, A, B, A, B, B),
                                         '2' => (A, A, B, B, A, B),
                                         '3' => (A, A, B, B, B, A),
                                         '4' => (A, B, A, A, B, B),
                                         '5' => (A, B, B, A, A, B),
                                         '6' => (A, B, B, B, A, A),
                                         '7' => (A, B, A, B, A, B),
                                         '8' => (A, B, A, B, B, A),
                                         '9' => (A, B, B, A, B, A));

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

    procedure Draw (Pattern : in String) is
      done : array (Pattern'Range) of Boolean := (others => False);
      j, width : Integer;
    begin
      for i in Pattern'Range loop
        if Pattern (i) = '1' and then not done (i) then
          j := i;
          for k in i + 1 .. Pattern'Last loop
            exit when Pattern (k) /= '1';
            j := k;
            done (j) := True;
          end loop;
          width := j - i + 1;
          Bar (X, width);
        end if;
        X := X + 1;
      end loop;
    end Draw;

    Offset : constant Natural := text'Length - 11;
    UPC    : constant Boolean := text'Length = 11;

  begin
    if not Valid (text, kind) then
      raise Cannot_Encode
        with
          (case kind is
             when Code_UPCA  => "Message must be 11 decimal digits for UPC-A",
             when Code_EAN13 => "Message must be 12 decimal digits for EAN-13");
    end if;

    --  For vector graphics only: we need to squeeze the full displayed code
    --  into the bounding box. A "module" is the thinnest bar.
    bc.module_width  := bc.bounding.width / Real (Code_Modules_Width);
    bc.module_height := bc.bounding.height;  --  This is a 1D code: any bar takes the full height

    Draw (End_Guard);

    Draw_Left : for I in 1 + Offset .. 6 + Offset loop
      Draw
        ((if UPC then
            Set_A_Map (S (I))
          else
            (case EAN_A (S (1)) (I - Offset) is
               when A => Set_A_Map (S (I)),
               when B => Set_B_Map (S (I)))));
    end loop Draw_Left;

    Draw (Middle_Guard);

    Draw_Right : for I in 7 + Offset .. 11 + Offset loop
      Draw (Set_C_Map (S (I)));
    end loop Draw_Right;

    Draw (Set_C_Map (Checksum (text)));
    Draw (End_Guard);

  end Draw;

  function Fitting return Module_Box is
  (0, 0, Code_Modules_Width, 1);

end Encode_UPCA_EAN13;
