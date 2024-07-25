with Ada.Containers.Vectors;
with Ada.Text_IO;

package body Bar_Codes.Encode_DM is

  type Byte is mod 256;

  package Byte_Vectors is new Ada.Containers.Vectors (Natural, Byte);
  subtype Byte_Vector is Byte_Vectors.Vector;

  --  Translated from datamatrix.js:
  --
  --  https://github.com/datalog/datamatrix-svg
  --  under MIT license
  --  datamatrix.js has no dependencies
  --  Copyright (c) 2020 Constantine

  function To_ASCII (text : String) return Byte_Vector is
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
        r.Append (235);
        r.Append ((c - 127) and 255);
      else
        r.Append (c + 1);
      end if;
      i := i + 1;
    end loop;

    return r;
  end To_ASCII;

  function To_Base_256 (text : String) return Byte_Vector is
    r : Byte_Vector;
    use type Ada.Containers.Count_Type;
  begin
    r.Append (231);  --  Switch to Base 256

    if text'Length > 250 then
      --  Length high byte (in 255 state algo):
      r.Append (Byte (Integer'(37) + (text'Length / 250) rem 256));
      --  !! Bork if "overflow" happens.
    end if;

    --  Length low byte (in 255 state algo):
    r.Append (Byte (Integer'(text'Length rem 250 + 149 * Integer (r.Length + 1) rem 255 + 1) rem 256));

    for ch of text loop
      r.Append (Byte (Integer'(Character'Pos (ch) + 149 * Integer (r.Length + 1) rem 255 + 1) rem 256));
    end loop;

    return r;
  end To_Base_256;

  function To_DM_Bytes (t : String) return Byte_Vector is
    candidate : Byte_Vector := To_ASCII (t);
    challenger : Byte_Vector;
    use Ada.Text_IO;
    use type Ada.Containers.Count_Type;
  begin
    if verbosity > 0 then
      Put ("Data Matrix: text length:" & t'Length'Image);
      if verbosity > 1 then
        Put (", [" & t & ']');
      end if;
      New_Line;
      Put_Line ("Data Matrix: chosen encoding so far: ""ASCII"", length:" & candidate.Length'Image);
    end if;
    challenger := To_Base_256 (t);
    if challenger.Length < candidate.Length then
      candidate := challenger;
      if verbosity > 0 then
        Put_Line ("Data Matrix: chosen encoding so far: ""Base_256"", length:" & candidate.Length'Image);
      end if;
    end if;

    return candidate;
    --  !! TBD: try other encodings: Edifact, ... (done in datamatrix.js).
  end To_DM_Bytes;

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
    i : Integer := 2;  --  size increment
    type Some_Unsigned is mod 2**32;
  begin
    width := 6;
    height := 6;
    for j in rs_checkwords'Range loop
      if width > 11 * i then
        i := 4 + Integer (Some_Unsigned (i) and 12);  --  Advance increment
      end if;
      width := width + i;
      height := height + i;
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
    h, w : Integer;
    nc, nr : Integer;
    fw, fh : Integer;
    border_size_x, border_size_y : Natural := 0;

    max_size : constant := 144;

    type U16 is mod 2**16;

    M : array (0 .. max_size - 1, 0 .. max_size - 1) of Boolean := (others => (others => False));

    procedure bit (x, y : Integer) is
    begin
      M (y, x) := True;
      border_size_x := Integer'Max (border_size_x, x + 1);
      border_size_y := Integer'Max (border_size_y, y + 1);
    end bit;

    procedure Preparation is
      el : Natural := Natural (enc.Length);
      --  !!  Possibly don't need to zero the arrays.
      rs : array (0 .. 69) of Byte := (others => 0);   --  Reed / Solomon code
      rc : array (0 .. 69) of Byte := (others => 0);
      lg : array (0 .. 255) of Integer := (others => 0);  --  log / exp table for multiplication
      ex : array (0 .. 254) of Integer := (others => 0);
      i, k, l : Integer;
      s : Integer;
      b : Integer;
      x : Byte;
      rc_index : Natural;
      use Ada.Text_IO;
    begin
      Calibrate (el, want_rectangular, nr, nc, b, w, h, l, s);

      --  Region size
      fw := w / nc;
      fh := h / nr;

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
      s := s / b;

      k := 1;
      --  log / exp table of Galois field
      for i in ex'Range loop
        ex (i) := k;
        lg (k) := i;
        k := k + k;
        if k > 255 then
          k := Integer ((U16 (k) xor 301));  --  "301 = a^8 + a^5 + a^3 + a^2 + 1"
        end if;
      end loop;

      --  RS generator polynomial
      rs (s) := 0;
      for i in 1 .. s loop
        rs (s - i) := 1;
        for j in s - i .. s - 1 loop
          rs (j) := rs (j + 1) xor Byte (ex ((lg (Integer (rs (j))) + i) rem 255));
        end loop;
      end loop;

      --  RS correction data for each block
      for c in 0 .. b - 1 loop
        rc (0 .. s) := (others => 0);
        i := c;
        while i < el loop
          x := rc (0) xor enc (i);
          for j in 0 .. s - 1 loop
            rc (j) := rc (j + 1) xor Byte (if x /= 0 then ex ((lg (Integer (rs (j))) + lg (Integer (x))) rem 255) else 0);
          end loop;
          i := i + b;
        end loop;
        --  Interleaved correction data
        for i in 0 .. s - 1 loop
          rc_index := el + c + i * b;
          while enc.Last_Index < rc_index loop
            enc.Append (0);
          end loop;
          enc (rc_index) := rc (i);
        end loop;
      end loop;
      if verbosity > 1 then
        Put_Line ("DM: byte sequence with ECC:");
        for elem of enc loop
          Put_Line (elem'Image);
        end loop;
      end if;
    end Preparation;

    procedure Layout_Perimeter_Finder_Pattern is
      i : Integer;
    begin
      --  Horizontal
      i := 0;
      while i < h + 2 * nr loop
        for j in 0 .. w + 2 * nc - 1 loop
          bit (j, i + fh + 1);
          if j rem 2 = 0 then
            bit (j, i);
          end if;
        end loop;
        i := i + fh + 2;
      end loop;

      --  Vertical
      i := 0;
      while i < w + 2 * nc loop
        for j in 0 .. h - 1 loop
          bit (i, j + (j / fh) * 2 + 1);
          if j rem 2 = 1 then
            bit (i + fw + 1, j + (j / fh) * 2);
          end if;
        end loop;
        i := i + fw + 2;
      end loop;
    end Layout_Perimeter_Finder_Pattern;

    procedure Draw_Data is
      s : Integer := 2;  --  step
      c : Integer := 0;  --  column
      r : Integer := 4;  --  row
      type Offset is record
        x, y : Integer;
      end record;
      type Layout is array (0 .. 7) of Offset;
      b : constant Layout :=  --  Nominal layout (L-shaped) for displaying a byte:
        ((0,  0),
        (-1,  0),
        (-2,  0),
         (0, -1),
        (-1, -1),
        (-2, -1),
        (-1, -2),
        (-2, -2));
      k : Layout;
      continue : Boolean;
      el : Byte;
      x, y : Integer;
    begin
      for elem of enc loop
        continue := False;
        if r = h - 3 and then c = -1 then
          --  Corner A layout
          k :=
               ((w, 6 - h),
                (w, 5 - h),
                (w, 4 - h),
                (w, 3 - h),
            (w - 1, 3 - h),
                (3,     2),
                (2,     2),
                (1,     2));
        elsif r = h + 1 and then c = 1 and then w rem 8 = 0 and then h rem 7 = 6 then
          --  Corner D layout
          k :=
           ((w - 2,     -h),
            (w - 3,     -h),
            (w - 4,     -h),
            (w - 2, -1 - h),
            (w - 3, -1 - h),
            (w - 4, -1 - h),
            (w - 2, -2),
               (-1,     -2));
        else
          if r = 0 and then c = w - 2 and then w rem 4 /= 0 then
            --  Corner B: omit upper left.
            continue := True;
          else
            if r < 0 or else c >= w or else r >= h or else c < 0 then
              --  Outside
              s := -s; --  Turn around
              r := r + 2 + s / 2;
              c := c + 2 - s / 2;

              while r < 0 or else c >= w or else r >= h or else c < 0 loop
                r := r - s;
                c := c + s;
              end loop;
            end if;
            if r = h - 2 and then c = 0 and then w rem 4 /= 0 then
              k :=  --  Corner B layout
                ((w - 1, 3 - h),
                 (w - 1, 2 - h),
                 (w - 2, 2 - h),
                 (w - 3, 2 - h),
                 (w - 4, 2 - h),
                     (0,     1),
                     (0,     0),
                     (0,    -1));

            elsif r = h - 2 and then c = 0 and then w rem 8 = 4 then
              k :=  --  Corner C layout
                ((w - 1, 5 - h),
                 (w - 1, 4 - h),
                 (w - 1, 3 - h),
                 (w - 1, 2 - h),
                 (w - 2, 2 - h),
                     (0,     1),
                     (0,     0),
                     (0,    -1));
            elsif r = 1 and then c = w - 1 and then (w rem 8) = 0 and then (h rem 8) = 6 then
              --  Omit corner D
              continue := True;
            else
              k := b;
            end if;
          end if;
        end if;

        if not continue then
          el := elem;
          for j in Layout'Range loop
            if (el and 1) /= 0 then
              x := c + k (j).x;
              y := r + k (j).y;

              --  Wrap around:
              if x < 0 then
                x := x + w;
                y := y + 4 - ((w + 4) rem 8);
              end if;
              if y < 0 then
                x := x + 4 - ((h + 4) rem 8);
                y := y + h;
              end if;

              --  Region gap
              bit (x + 2 * (x / fw) + 1, y + 2 * (y / fh) + 1);
            end if;

            el := el / 2;
          end loop;
        end if;

        --  Diagonal steps:
        r := r - s;
        c := c + s;
      end loop;

      --  Unfilled corner:
      for i in reverse 0 .. w loop
        exit when i rem 4 = 0;
        bit (i, i);
      end loop;

    end Draw_Data;

    procedure Output_to_Media is
    begin
      --  For vector graphics only: we want to squeeze the full 2D code
      --  into the bounding box. A "module" is the smallest square.
      bc.module_width  := bc.bounding.width / Real (border_size_x);
      bc.module_height := bc.bounding.height / Real (border_size_y);
      --
      for y in 0 .. border_size_y - 1 loop
        for x in 0 .. border_size_x - 1 loop
          if M (y, x) then
            Filled_Rectangle (Bar_Code'Class (bc), (x, border_size_y - 1 - y, 1, 1));
          end if;
        end loop;
      end loop;
    end Output_to_Media;

  begin
    Preparation;
    Layout_Perimeter_Finder_Pattern;
    Draw_Data;
    Output_to_Media;
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
