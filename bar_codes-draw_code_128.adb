--  !! TO DO: Code 128-C (double digits);
--  !! TO DO: Find optimal code

procedure Bar_Codes.Draw_Code_128 (bc : Bar_Code; text : String) is
  subtype Code_Range is Integer range 0 .. 106;
  --  Worst case, we switch subcode for each symbol!
  max_length       : constant Integer := text'Length * 2 + 2;
  code             : array (1 .. max_length) of Code_Range;
  code_length      : Natural := 0;
  --
  procedure Compose_code is
    type Code_128_subcode is (undefined, A, B, C);
    subcode          : Code_128_subcode := undefined;
    checksum         : Natural := 0;
    procedure Add_symbol (symbol : Code_Range) is
    begin
      checksum := checksum + symbol * Integer'Max (1, code_length);
      code_length := code_length + 1;
      code (code_length) := symbol;
    end;
    --
    procedure Switch_to (new_subcode : Code_128_subcode) is
    begin
      subcode := new_subcode;
      case new_subcode is
        when undefined => null;
        when A => Add_symbol (103);
        when B => Add_symbol (104);
        when C => Add_symbol (105);
      end case;
    end;
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
          raise Program_Error;  --  Not yet implemented.
      end case;
    end loop;
    --  Check symbol
    Add_symbol (checksum mod 103);
    --  Stop symbol
    Add_symbol (106);
  end Compose_code;
  --
  procedure Emit_code is
    type Width_sequence is array (1 .. 5) of Positive;
    width : constant array (Code_Range) of Width_sequence :=
      --  See bc_work.xls, sheet Code_128
      --  These are the widths for:  bar, space, bar, space, bar
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
    symbol_width     : constant := 11.0;
    stop_extra_width : constant :=  2.0;  --  Supplemental bar
    --  We need to squeeze the full bar code into the bounding box.
    --  A "module" is the width of the thinnest bar.
    module_width : constant Real :=
      bc.bounding.width / (Real (code_length) * symbol_width + stop_extra_width);
    x : Real;
    procedure Bar (offset, width : Natural) is
    begin
      Filled_Rectangle (
        Bar_Code'Class (bc),  --  Will use the concrete child method for displaying a rectangle
          (left   => x + Real (offset) * module_width,
           bottom => bc.bounding.bottom,
           width  => Real (width) * module_width,
           height => bc.bounding.height)
      );
    end Bar;
  begin
    for i in 1 .. code_length loop
      x := Real (i - 1) * symbol_width * module_width;
      declare
        ws : constant Width_sequence := width (code (i));
      begin
        Bar (0,                                 ws (1));
        Bar (ws (1) + ws (2),                   ws (3));
        Bar (ws (1) + ws (2) + ws (3) + ws (4), ws (5));
      end;
    end loop;
    --  Extra bar after the Stop symbol; this gives the Reverse Stop symbol
    --  when the bar code is scanned turned 180°.
    x := Real (code_length) * symbol_width * module_width;
    Bar (0, 2);
  end Emit_code;
  --
begin
  Compose_code;
  Emit_code;
end Bar_Codes.Draw_Code_128;
