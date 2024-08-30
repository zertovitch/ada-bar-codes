with Ada.Containers.Vectors;
with Ada.Text_IO;

package body Bar_Codes.Encode_DM is

  type Byte is mod 256;

  package Byte_Vectors is new Ada.Containers.Vectors (Natural, Byte);
  subtype Byte_Vector is Byte_Vectors.Vector;

  --  Translated (and later improved) from datamatrix.js:
  --
  --  https://github.com/datalog/datamatrix-svg
  --  under MIT license
  --  datamatrix.js has no dependencies
  --  Copyright (c) 2020 Constantine

  escape_switch_base_256 : constant := 231;
  escape_upper_shift     : constant := 235;

  function To_ASCII (text : String) return Byte_Vector is
  --  The "ASCII" encoding is more efficient for 7-bit
  --  values and pairs of digits.
    r : Byte_Vector;
    i : Positive := text'First;
    c, c1 : Byte;
  begin

    while i <= text'Last loop
      c := Character'Pos (text (i));
      c1 := (if i + 1 <= text'Last then Character'Pos (text (i + 1)) else 0);
      if c in 48 .. 57 and then c1 in 48 .. 57 then
        --  Two consecutive digits.
        r.Append ((c - 48) * 10 + c1 - 48 + 130);  --  Codes 130 .. 229 are used for "00" .. "99".
        i := i + 1;
      elsif c > 127 then
        r.Append (escape_upper_shift);
        r.Append ((c - 127) and 255);
      else
        r.Append (c + 1);
      end if;
      i := i + 1;
    end loop;

    return r;
  end To_ASCII;

  base_256_limit : constant := (255 - 37) * 250 + 249;

  function To_Base_256 (text : String) return Byte_Vector is
  --  The "BASE-256" encoding is aimed at binary data.
    r : Byte_Vector;
    use type Ada.Containers.Count_Type;
  begin
    if text'Length > base_256_limit then
      raise Cannot_Encode with "Message to be encoded is too long for the Base-256 method";
    end if;

    r.Append (escape_switch_base_256);

    if text'Length > 250 then
      --  Length high byte (in 255-state algo):
      r.Append (Byte (Integer'(37) + (text'Length / 250) rem 256));
    end if;

    --  Length low byte (in 255-state algo):
    r.Append (Byte (Integer'(text'Length rem 250 + 149 * Integer (r.Length + 1) rem 255 + 1) rem 256));

    for ch of text loop
      r.Append (Byte (Integer'(Character'Pos (ch) + 149 * Integer (r.Length + 1) rem 255 + 1) rem 256));
    end loop;

    return r;
  end To_Base_256;

  function To_DM_Bytes (t : String) return Byte_Vector is
    candidate, challenger : Byte_Vector;
    use Ada.Text_IO;
    use type Ada.Containers.Count_Type;
  begin
    candidate := To_ASCII (t);
    if verbosity_level > 0 then
      Put ("Data Matrix: text length:" & t'Length'Image);
      if verbosity_level > 1 then
        Put (", [" & t & ']');
      end if;
      New_Line;
      Put_Line ("Data Matrix: chosen encoding so far: ""ASCII"", length:" & candidate.Length'Image);
    end if;

    if t'Length <= base_256_limit then
      challenger := To_Base_256 (t);
      if challenger.Length < candidate.Length then
        candidate := challenger;
        if verbosity_level > 0 then
          Put_Line ("Data Matrix: chosen encoding so far: ""Base_256"", length:" & candidate.Length'Image);
        end if;
      end if;
    end if;

    return candidate;
    --  !! TBD: try other encodings: Edifact, ... (done in datamatrix.js).
  end To_DM_Bytes;

  type U16 is mod 2**16;

  procedure Calibrate_Rectangular
    (byte_count_message                                     : in     Natural;
     nr, nc, blocks, width, height, byte_count_symbol, rscw :    out Natural)
  is
    type Size_Range is range 0 .. 5;
    symbol_width  : constant array (Size_Range) of Integer := (16, 28, 24, 32, 32, 44);
    symbol_height : constant array (Size_Range) of Integer := (6, 6, 10, 10, 14, 14);
    rs_checkwords : constant array (Size_Range) of Integer := (7, 11, 14, 18, 24, 28);
  begin
    for j in Size_Range loop
      width  := symbol_width (j);
      height := symbol_height (j);
      byte_count_symbol := width * height / 8;
      rscw := rs_checkwords (j);
      exit when byte_count_symbol >= byte_count_message + rscw;
    end loop;
    --  Regions:
    nr := 1;
    nc := (if width > 25 then 2 else 1);
    blocks := 1;
  end Calibrate_Rectangular;

  procedure Calibrate_Square
    (byte_count_message                                     : in     Natural;
     nr, nc, blocks, width, height, byte_count_symbol, rscw :    out Natural)
  is
    rs_checkwords : constant array (0 .. 23) of Integer :=
      (5, 7, 10, 12, 14, 18, 20, 24, 28, 36, 42, 48, 56, 68, 84, 112, 144, 192, 224, 272, 336, 408, 496, 620);
    size_increment : Integer := 2;
  begin
    width  := 6;
    height := 6;
    for j in rs_checkwords'Range loop
      if width > 11 * size_increment then
        size_increment := 4 + Integer (U16 (size_increment) and 12);  --  Advance increment
      end if;
      width  := width  + size_increment;
      height := height + size_increment;
      byte_count_symbol := width * height / 8;
      rscw := rs_checkwords (j);
      exit when byte_count_symbol >= byte_count_message + rscw;
      if j = rs_checkwords'Last then
        raise Cannot_Encode with "Message to be encoded doesn't fit in any Data Matrix size";
      end if;
    end loop;
    --  Regions:
    nr := (if width > 27 then 2 * (width / 54) + 2 else 1);
    nc := nr;
    blocks := (if byte_count_symbol > 255 then 2 * (byte_count_symbol / 512) + 2 else 1);
  end Calibrate_Square;

  procedure Calibrate
    (byte_count_message                                     : in     Natural;
     want_rectangular                                       : in     Boolean;
     nr, nc, blocks, width, height, byte_count_symbol, rscw :    out Natural)
  is
  begin
    if want_rectangular and then byte_count_message < 50 then
      Calibrate_Rectangular
        (byte_count_message, nr, nc, blocks, width, height, byte_count_symbol, rscw);
    else
      Calibrate_Square
        (byte_count_message, nr, nc, blocks, width, height, byte_count_symbol, rscw);
    end if;
  end Calibrate;

  ----------
  -- Draw --
  ----------

  procedure Draw (bc : in out Bar_Code; text : String; dm_kind : Code_DM) is
    want_rectangular : constant Boolean := dm_kind = Code_DM_Rectangular;
    enc : Byte_Vector := To_DM_Bytes (text);
    height, width : Integer;
    nc, nr : Integer;
    fw, fh : Integer;
    border_size_x, border_size_y : Natural := 0;

    max_size : constant := 144;

    module : Grid (0 .. max_size - 1, 0 .. max_size - 1) := (others => (others => False));

    procedure bit (x, y : Integer) is
    begin
      module (y, x) := True;
      border_size_x := Integer'Max (border_size_x, x + 1);
      border_size_y := Integer'Max (border_size_y, y + 1);
    end bit;

    procedure Preparation is
      el : Natural := Natural (enc.Length);
      rs : array (0 .. 69) of Byte := (others => 0);   --  Reed / Solomon code
      rc : array (0 .. 69) of Byte := (others => 0);
      log : array (0 .. 255) of Integer := (others => 0);  --  log / exp table for multiplication
      exp : array (0 .. 254) of Integer := (others => 0);
      i, exp_i, l : Integer;
      s : Integer;
      blocks : Integer;
      x : Byte;
      rc_index : Natural;
      use Ada.Text_IO;
    begin
      Calibrate (el, want_rectangular, nr, nc, blocks, width, height, l, s);

      --  Region size
      fw := width / nc;
      fh := height / nr;

      --  First padding
      if el < l - s then
        el := el + 1;
        enc.Append (129);
      end if;

      --  More padding
      while el < l - s loop
        el := el + 1;
        enc.Append (Byte ((((149 * el) rem 253) + 130) rem 254));
        --  Put_Line ("DM: more padding");
      end loop;

      --  Reed Solomon error detection and correction
      s := s / blocks;

      exp_i := 1;
      --  log / exp table of Galois field
      for i in exp'Range loop
        exp (i) := exp_i;
        log (exp_i) := i;
        exp_i := exp_i + exp_i;
        if exp_i > 255 then
          exp_i := Integer ((U16 (exp_i) xor 301));
          --  "301 = a^8 + a^5 + a^3 + a^2 + 1"
        end if;
      end loop;

      --  RS generator polynomial
      rs (s) := 0;
      for i in 1 .. s loop
        rs (s - i) := 1;
        for j in s - i .. s - 1 loop
          rs (j) := rs (j + 1) xor Byte (exp ((log (Integer (rs (j))) + i) rem 255));
        end loop;
      end loop;

      --  RS correction data for each block
      for c in 0 .. blocks - 1 loop
        rc (0 .. s) := (others => 0);
        i := c;
        while i < el loop
          x := rc (0) xor enc (i);
          for j in 0 .. s - 1 loop
            rc (j) := rc (j + 1) xor Byte (if x /= 0 then exp ((log (Integer (rs (j))) + log (Integer (x))) rem 255) else 0);
          end loop;
          i := i + blocks;
        end loop;
        --  Interleaved correction data
        for i in 0 .. s - 1 loop
          rc_index := el + c + i * blocks;
          while enc.Last_Index < rc_index loop
            enc.Append (0);
          end loop;
          enc (rc_index) := rc (i);
        end loop;
      end loop;
      if verbosity_level > 1 then
        Put_Line ("DM: byte sequence including ECC:");
        for elem of enc loop
          Put_Line (elem'Image);
        end loop;
      end if;
    end Preparation;

    procedure Horizontal_Layout_Perimeter_Finder_Pattern is
      i : Integer := 0;
    begin
      while i < height + 2 * nr loop
        for j in 0 .. width + 2 * nc - 1 loop
          bit (j, i + fh + 1);
          if j rem 2 = 0 then
            bit (j, i);
          end if;
        end loop;
        i := i + fh + 2;
      end loop;
    end Horizontal_Layout_Perimeter_Finder_Pattern;

    procedure Vertical_Layout_Perimeter_Finder_Pattern is
      i : Integer := 0;
    begin
      while i < width + 2 * nc loop
        for j in 0 .. height - 1 loop
          bit (i, j + (j / fh) * 2 + 1);
          if j rem 2 = 1 then
            bit (i + fw + 1, j + (j / fh) * 2);
          end if;
        end loop;
        i := i + fw + 2;
      end loop;
    end Vertical_Layout_Perimeter_Finder_Pattern;

    procedure Draw_Data is
      step : Integer := 2;
      col  : Integer := 0;
      row  : Integer := 4;

      type Offset is record
        x, y : Integer;
      end record;

      type Layout_Type is array (0 .. 7) of Offset;

      --  Nominal layout (L-shaped) for displaying a byte:
      normal : constant Layout_Type :=
        ((0,  0),
        (-1,  0),
        (-2,  0),
         (0, -1),
        (-1, -1),
        (-2, -1),
        (-1, -2),
        (-2, -2));

      layout : Layout_Type;
      draw_it : Boolean;
      el : Byte;
      x, y : Integer;

      procedure Check_Corners is
      begin
        if row = height - 3 and then col = -1 then
          --  Corner A layout
          layout :=
               ((width, 6 - height),
                (width, 5 - height),
                (width, 4 - height),
                (width, 3 - height),
            (width - 1, 3 - height),
                    (3,          2),
                    (2,          2),
                    (1,          2));
        elsif row = height + 1 and then col = 1 and then width rem 8 = 0 and then height rem 8 = 6 then
          --  Corner D layout
          layout :=
            ((width - 2,     -height),
             (width - 3,     -height),
             (width - 4,     -height),
             (width - 2, -1 - height),
             (width - 3, -1 - height),
             (width - 4, -1 - height),
             (width - 2,          -2),
                    (-1,          -2));
        else
          if row = 0 and then col = width - 2 and then width rem 4 /= 0 then
            --  Corner B: omit upper left.
            draw_it := False;
          else
            if row not in 0 .. height - 1 or else col not in 0 .. width - 1 then
              --  We are outside.
              step := -step;  --  Turn around
              row := row + 2 + step / 2;
              col := col + 2 - step / 2;

              while row not in 0 .. height - 1 or else col not in 0 .. width - 1 loop
                row := row - step;
                col := col + step;
              end loop;
            end if;
            if row = height - 2 and then col = 0 and then width rem 4 /= 0 then
              layout :=  --  Corner B layout
                ((width - 1, 3 - height),
                 (width - 1, 2 - height),
                 (width - 2, 2 - height),
                 (width - 3, 2 - height),
                 (width - 4, 2 - height),
                         (0,          1),
                         (0,          0),
                         (0,         -1));

            elsif row = height - 2 and then col = 0 and then width rem 8 = 4 then
              layout :=  --  Corner C layout
                ((width - 1, 5 - height),
                 (width - 1, 4 - height),
                 (width - 1, 3 - height),
                 (width - 1, 2 - height),
                 (width - 2, 2 - height),
                         (0,          1),
                         (0,          0),
                         (0,         -1));
            elsif row = 1 and then col = width - 1 and then (width rem 8) = 0 and then (height rem 8) = 6 then
              --  Omit corner D
              draw_it := False;
            else
              layout := normal;
            end if;
          end if;
        end if;
      end Check_Corners;

    begin
      for elem of enc loop
        loop
          draw_it := True;
          Check_Corners;
          exit when draw_it;
          --  Diagonal steps (nothing drawn):
          row := row - step;
          col := col + step;
        end loop;

        el := elem;
        for j in Layout_Type'Range loop
          if (el and 1) /= 0 then
            x := col + layout (j).x;
            y := row + layout (j).y;

            --  Wrap around:
            if x < 0 then
              x := x + width;
              y := y + 4 - ((width + 4) rem 8);
            end if;
            if y < 0 then
              x := x + 4 - ((height + 4) rem 8);
              y := y + height;
            end if;

            --  Plot at (x, y), plus region gap
            bit (x + 2 * (x / fw) + 1,
                 y + 2 * (y / fh) + 1);
          end if;

          el := el / 2;
        end loop;

        --  Diagonal steps (byte `elem` was drawn):
        row := row - step;
        col := col + step;
      end loop;

      --  Unfilled corner:
      for i in reverse 0 .. width loop
        exit when i rem 4 = 0;
        bit (i, i);
      end loop;

    end Draw_Data;

  begin
    Preparation;
    Horizontal_Layout_Perimeter_Finder_Pattern;
    Vertical_Layout_Perimeter_Finder_Pattern;
    Draw_Data;
    Output_to_Media (bc, border_size_x, border_size_y, module);
  end Draw;

  -------------
  -- Fitting --
  -------------

  function Fitting (text : String; dm_kind : Code_DM) return Module_Box is
    want_rectangular : constant Boolean := dm_kind = Code_DM_Rectangular;
    enc : constant Byte_Vector := To_DM_Bytes (text);
    el : constant Natural := Natural (enc.Length);
    h, w : Integer;
    nc, nr : Integer;
    l : Integer;
    s : Integer;
    b : Integer;
    xx, yy : Integer;
  begin
    Calibrate (el, want_rectangular, nr, nc, b, w, h, l, s);
    xx := w + 2 * nc;
    yy := h + 2 * nr;
    return (0, 0, xx, yy);
  end Fitting;

end Bar_Codes.Encode_DM;
