with Ada.Text_IO;

--  Nice to have: find optimal code.

package body Bar_Codes.Encode_Code_128 is

  --  See bc_work.xls, sheet: Code_128

  subtype Code_Range is Integer range 0 .. 106;

  type Sequence is array (Positive range <>) of Code_Range;

  function Compose_code (text : String) return Sequence is
    --  Worst case: we switch subcode for each symbol!
    max_length  : constant Integer := text'Length * 2 + 2;
    code        : Sequence (1 .. max_length);
    code_length : Natural := 0;
    --
    type Code_128_subcode is (undefined, A, B, C);
    subcode  : Code_128_subcode := undefined;
    checksum : Natural := 0;
    --
    procedure Add_symbol (symbol : Code_Range) is
    begin
      checksum := checksum + symbol * Integer'Max (1, code_length);
      code_length := code_length + 1;
      code (code_length) := symbol;
    end Add_symbol;
    --
    subtype Defined_subcode is Code_128_subcode range A .. C;
    --
    first_digit : Boolean;  --  First digit in a pair of digits for subcode C
    --
    procedure Switch_to (new_subcode : Defined_subcode) is
    begin
      if subcode = undefined then
        --  Start code A/B/C:
        case new_subcode is
          when A => Add_symbol (103);
          when B => Add_symbol (104);
          when C => Add_symbol (105);
        end case;
      else
        case new_subcode is
          when A => Add_symbol (101);
          when B => Add_symbol (100);
          when C => Add_symbol (099);
        end case;
      end if;
      if new_subcode = C then
        first_digit := True;
      end if;
      subcode := new_subcode;
      if verbosity > 0 then
        Ada.Text_IO.Put_Line ("[Code 128] switched to subcode " & Code_128_subcode'Image (subcode));
      end if;
    end Switch_to;
    --
    four_digits : Boolean;
    digit_buffer, digit : Natural;
  begin
    for i in text'Range loop
      if text (i) > ASCII.DEL then
        raise Cannot_Encode;
      end if;
    end loop;
    for i in text'Range loop
      --  Choice of a subcode
      case text (i) is
        when ASCII.NUL .. ASCII.US =>
          if subcode /= A then
            Switch_to (A);
          end if;
        when Character'Val (96) .. ASCII.DEL =>
          if subcode /= B then
            Switch_to (B);
          end if;
        when '0' .. '9' =>
          if subcode = C then
            --  If text (i) is meant to be the first digit of a pair,
            --  ensure there is a second digit after.
            if first_digit then
              if i = text'Last or else text (i + 1) not in '0' .. '9' then
                Switch_to (B);  --  We need to encode this digit with subcode A or B
              end if;
            end if;
          else
            if i + 3 <= text'Last then
              four_digits := True;
              for j in i + 1 .. i + 3 loop
                four_digits := four_digits and text (j) in '0' .. '9';
              end loop;
              if four_digits then
                Switch_to (C);
              end if;
            end if;
            if subcode = undefined then
              Switch_to (B);
            end if;
          end if;
        when others =>
          --  A or B is good.
          if subcode not in A .. B then
            Switch_to (B);  --  Just an assumption: characters like 'a' .. 'z' more likely.
          end if;
      end case;
      --  Encode text (i)
      case subcode is
        when undefined => null;
        when A =>
          if text (i) <= ASCII.US then
            Add_symbol (Character'Pos (text (i)) + 64);
          else
            Add_symbol (Character'Pos (text (i)) - 32);
          end if;
        when B =>
          Add_symbol (Character'Pos (text (i)) - 32);
        when C =>
          digit := Character'Pos (text (i)) - Character'Pos ('0');
          if first_digit then
            digit_buffer := digit;
          else
            Add_symbol (10 * digit_buffer + digit);
          end if;
          first_digit := not first_digit;
      end case;
    end loop;
    --  Checksum symbol
    Add_symbol (checksum mod 103);
    --  Stop symbol
    Add_symbol (106);
    --
    return code (1 .. code_length);
  end Compose_code;

  --  Here begins the graphics part.
  --  Each symbol drawn as a succession of bar, space, bar, space, bar, space.

  symbol_width     : constant := 11;  --  Each symbol has 3 bars and takes 11 "modules" in total.
  stop_extra_width : constant :=  2;  --  Supplemental bar after stop symbol.

  procedure Draw (bc : in out Bar_Code; text : String) is
    code : constant Sequence := Compose_code (text);
    --
    type Width_sequence is array (1 .. 5) of Positive;
    widths : constant array (Code_Range) of Width_sequence :=
      --  These are the widths for:  bar, space, bar, space, bar (last space width is implicit).
      (
          0 => (2, 1, 2, 2, 2),
          1 => (2, 2, 2, 1, 2),
          2 => (2, 2, 2, 2, 2),
          3 => (1, 2, 1, 2, 2),
          4 => (1, 2, 1, 3, 2),
          5 => (1, 3, 1, 2, 2),
          6 => (1, 2, 2, 2, 1),
          7 => (1, 2, 2, 3, 1),
          8 => (1, 3, 2, 2, 1),
          9 => (2, 2, 1, 2, 1),
         10 => (2, 2, 1, 3, 1),
         11 => (2, 3, 1, 2, 1),
         12 => (1, 1, 2, 2, 3),
         13 => (1, 2, 2, 1, 3),
         14 => (1, 2, 2, 2, 3),
         15 => (1, 1, 3, 2, 2),
         16 => (1, 2, 3, 1, 2),
         17 => (1, 2, 3, 2, 2),
         18 => (2, 2, 3, 2, 1),
         19 => (2, 2, 1, 1, 3),
         20 => (2, 2, 1, 2, 3),
         21 => (2, 1, 3, 2, 1),
         22 => (2, 2, 3, 1, 1),
         23 => (3, 1, 2, 1, 3),
         24 => (3, 1, 1, 2, 2),
         25 => (3, 2, 1, 1, 2),
         26 => (3, 2, 1, 2, 2),
         27 => (3, 1, 2, 2, 1),
         28 => (3, 2, 2, 1, 1),
         29 => (3, 2, 2, 2, 1),
         30 => (2, 1, 2, 1, 2),
         31 => (2, 1, 2, 3, 2),
         32 => (2, 3, 2, 1, 2),
         33 => (1, 1, 1, 3, 2),
         34 => (1, 3, 1, 1, 2),
         35 => (1, 3, 1, 3, 2),
         36 => (1, 1, 2, 3, 1),
         37 => (1, 3, 2, 1, 1),
         38 => (1, 3, 2, 3, 1),
         39 => (2, 1, 1, 3, 1),
         40 => (2, 3, 1, 1, 1),
         41 => (2, 3, 1, 3, 1),
         42 => (1, 1, 2, 1, 3),
         43 => (1, 1, 2, 3, 3),
         44 => (1, 3, 2, 1, 3),
         45 => (1, 1, 3, 1, 2),
         46 => (1, 1, 3, 3, 2),
         47 => (1, 3, 3, 1, 2),
         48 => (3, 1, 3, 1, 2),
         49 => (2, 1, 1, 3, 3),
         50 => (2, 3, 1, 1, 3),
         51 => (2, 1, 3, 1, 1),
         52 => (2, 1, 3, 3, 1),
         53 => (2, 1, 3, 1, 3),
         54 => (3, 1, 1, 1, 2),
         55 => (3, 1, 1, 3, 2),
         56 => (3, 3, 1, 1, 2),
         57 => (3, 1, 2, 1, 1),
         58 => (3, 1, 2, 3, 1),
         59 => (3, 3, 2, 1, 1),
         60 => (3, 1, 4, 1, 1),
         61 => (2, 2, 1, 4, 1),
         62 => (4, 3, 1, 1, 1),
         63 => (1, 1, 1, 2, 2),
         64 => (1, 1, 1, 4, 2),
         65 => (1, 2, 1, 1, 2),
         66 => (1, 2, 1, 4, 2),
         67 => (1, 4, 1, 1, 2),
         68 => (1, 4, 1, 2, 2),
         69 => (1, 1, 2, 2, 1),
         70 => (1, 1, 2, 4, 1),
         71 => (1, 2, 2, 1, 1),
         72 => (1, 2, 2, 4, 1),
         73 => (1, 4, 2, 1, 1),
         74 => (1, 4, 2, 2, 1),
         75 => (2, 4, 1, 2, 1),
         76 => (2, 2, 1, 1, 1),
         77 => (4, 1, 3, 1, 1),
         78 => (2, 4, 1, 1, 1),
         79 => (1, 3, 4, 1, 1),
         80 => (1, 1, 1, 2, 4),
         81 => (1, 2, 1, 1, 4),
         82 => (1, 2, 1, 2, 4),
         83 => (1, 1, 4, 2, 1),
         84 => (1, 2, 4, 1, 1),
         85 => (1, 2, 4, 2, 1),
         86 => (4, 1, 1, 2, 1),
         87 => (4, 2, 1, 1, 1),
         88 => (4, 2, 1, 2, 1),
         89 => (2, 1, 2, 1, 4),
         90 => (2, 1, 4, 1, 2),
         91 => (4, 1, 2, 1, 2),
         92 => (1, 1, 1, 1, 4),
         93 => (1, 1, 1, 3, 4),
         94 => (1, 3, 1, 1, 4),
         95 => (1, 1, 4, 1, 1),
         96 => (1, 1, 4, 3, 1),
         97 => (4, 1, 1, 1, 1),
         98 => (4, 1, 1, 3, 1),
         99 => (1, 1, 3, 1, 4),
        100 => (1, 1, 4, 1, 3),
        101 => (3, 1, 1, 1, 4),
        102 => (4, 1, 1, 1, 3),
        103 => (2, 1, 1, 4, 1),
        104 => (2, 1, 1, 2, 1),
        105 => (2, 1, 1, 2, 3),
        106 => (2, 3, 3, 1, 1)
      );
    x : Natural;
    --
    procedure Bar (offset, width : Natural) is
    begin
      Filled_Rectangle (
        Bar_Code'Class (bc),  --  Will use the concrete child method for displaying a rectangle
          (left   => x + offset,
           bottom => 0,
           width  => width,
           height => 1)
      );
    end Bar;
  begin
    --  For vector graphics only: we need to squeeze the full 2D code
    --  into the bounding box. A "module" is the thinnest bar.
    bc.module_width  := bc.bounding.width / Real (code'Length * symbol_width + stop_extra_width);
    bc.module_height := bc.bounding.height;  --  This is an 1D code, any bar takes the full height
    --
    for i in code'Range loop
      x := (i - 1) * symbol_width;
      declare
        ws : constant Width_sequence := widths (code (i));
      begin
        Bar (0,                                 ws (1));
        Bar (ws (1) + ws (2),                   ws (3));
        Bar (ws (1) + ws (2) + ws (3) + ws (4), ws (5));
      end;
    end loop;
    --  Extra bar after the Stop symbol; this gives the Reverse Stop symbol
    --  when the bar code is scanned turned 180°.
    x := code'Length * symbol_width;
    Bar (0, 2);
  end Draw;

  function Fitting (text : String) return Module_Box is
    code : constant Sequence := Compose_code (text);
  begin
    return (0, 0, code'Length * symbol_width + stop_extra_width, 1);
  end Fitting;

end Bar_Codes.Encode_Code_128;
