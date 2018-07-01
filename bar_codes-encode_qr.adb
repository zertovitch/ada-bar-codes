--
--  QR Code generator library (Ada)
--
--  Copyright (c) Gautier de Montmollin
--  http://ada-bar-codes.sf.net
--
--  Copyright (c) Project Nayuki
--  https://www.nayuki.io/page/qr-code-generator-library
--
--  (MIT License)
--  Permission is hereby granted, free of charge, to any person obtaining a copy of
--  this software and associated documentation files (the "Software"), to deal in
--  the Software without restriction, including without limitation the rights to
--  use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
--  the Software, and to permit persons to whom the Software is furnished to do so,
--  subject to the following conditions:
--  - The above copyright notice and this permission notice shall be included in
--    all copies or substantial portions of the Software.
--  - The Software is provided "as is", without warranty of any kind, express or
--    implied, including but not limited to the warranties of merchantability,
--    fitness for a particular purpose and noninfringement. In no event shall the
--    authors or copyright holders be liable for any claim, damages or other
--    liability, whether in an action of contract, tort or otherwise, arising from,
--    out of or in connection with the Software or the use or other dealings in the
--    Software.

--  !! To do: de-CamelCase-ize
--  !! To do: other places with "!!"...

with Ada.Containers.Vectors;
with Ada.Text_IO; use Ada.Text_IO;

with Interfaces; use Interfaces;
with Ada.Containers;

package body Bar_Codes.Encode_QR is

  subtype QR_version is Integer range 1 .. 40;

  --  Returns the number of data bits that can be stored in a QR Code of the given version number, after
  --  all function modules are excluded. This includes remainder bits, so it might not be a multiple of 8.
  --  The result is in the range [208, 29648].
  --
  function getNumRawDataModules (ver : QR_version) return Positive is
    result : Positive := (16 * ver + 128) * ver + 64;
    numAlign : constant Natural := ver / 7 + 2;
    num_alignment_pattern_modules : constant Natural := (25 * numAlign - 10) * numAlign - 55;
    num_version_information_modules : constant Natural := 18 * 2;
  begin
    if ver >= 2 then
      result := result - num_alignment_pattern_modules;
      if ver >= 7 then
        result := result - num_version_information_modules;
      end if;
    end if;
    if verbosity > 2 then
      Put_Line (
        "getNumRawDataModules: result" & Integer'Image (result) &
        " version" & QR_version'Image (ver)
      );
    end if;
    return result;
  end getNumRawDataModules;

  type Error_Correction_Level is (LOW, MEDIUM, QUARTILE, HIGH);

  type QR_param is array (Error_Correction_Level, QR_version) of Positive;

  ECC_CODEWORDS_PER_BLOCK : constant QR_param := (
    --  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40    Error correction level
       (7, 10, 15, 20, 26, 18, 20, 24, 30, 18, 20, 24, 26, 30, 22, 24, 28, 30, 28, 28, 28, 28, 30, 30, 26, 28, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30),  -- Low
      (10, 16, 26, 18, 24, 16, 18, 22, 22, 26, 30, 22, 22, 24, 24, 28, 28, 26, 26, 26, 26, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28),  -- Medium
      (13, 22, 18, 26, 18, 24, 18, 22, 20, 24, 28, 26, 24, 20, 30, 24, 28, 28, 26, 30, 28, 30, 30, 30, 30, 28, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30),  -- Quartile
      (17, 28, 22, 16, 22, 28, 26, 26, 24, 28, 24, 28, 22, 24, 24, 30, 28, 28, 26, 28, 30, 24, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30)   -- High
  );

  NUM_ERROR_CORRECTION_BLOCKS : constant QR_param := (
    --  1, 2, 3, 4, 5, 6, 7, 8, 9,10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40    Error correction level
       (1, 1, 1, 1, 1, 2, 2, 2, 2, 4,  4,  4,  4,  4,  6,  6,  6,  6,  7,  8,  8,  9,  9, 10, 12, 12, 12, 13, 14, 15, 16, 17, 18, 19, 19, 20, 21, 22, 24, 25),  -- Low
       (1, 1, 1, 2, 2, 4, 4, 4, 5, 5,  5,  8,  9,  9, 10, 10, 11, 13, 14, 16, 17, 17, 18, 20, 21, 23, 25, 26, 28, 29, 31, 33, 35, 37, 38, 40, 43, 45, 47, 49),  -- Medium
       (1, 1, 2, 2, 4, 4, 6, 6, 8, 8,  8, 10, 12, 16, 12, 17, 16, 18, 21, 20, 23, 23, 25, 27, 29, 34, 34, 35, 38, 40, 43, 45, 48, 51, 53, 56, 59, 62, 65, 68),  -- Quartile
       (1, 1, 2, 4, 4, 4, 5, 6, 8, 8, 11, 11, 16, 16, 18, 16, 19, 21, 25, 25, 25, 34, 30, 32, 35, 37, 40, 42, 45, 48, 51, 54, 57, 60, 63, 66, 70, 74, 77, 81)   -- High
  );

  --  Returns the number of 8-bit data (i.e. not error correction) codewords contained in any
  --  QR Code of the given version number and error correction level, with remainder bits discarded.
  --
  function getNumDataCodewords (ver : QR_version; ecl : Error_Correction_Level) return Positive is
  begin
    return getNumRawDataModules (ver) / 8 -
      ECC_CODEWORDS_PER_BLOCK (ecl, ver) *
      NUM_ERROR_CORRECTION_BLOCKS (ecl, ver);
  end;

  type Segment_mode is (NUMERIC, ALPHANUMERIC, BYTE, KANJI, ECI);

  type Char_count_bits is array (0 .. 2) of Natural;

  type Segment_mode_param is record
    modeBits : Positive;
    ccbits   : Char_count_bits;
  end record;

  segment_params : constant array (Segment_mode) of Segment_mode_param :=
    (NUMERIC      => (1, (10, 12, 14)),
     ALPHANUMERIC => (2,  (9, 11, 13)),
     BYTE         => (4,  (8, 16, 16)),
     KANJI        => (8,  (8, 10, 12)),
     ECI          => (7,  (0,  0,  0))
    );

  type Bit is range 0 .. 1;
  package Bit_vectors is new Ada.Containers.Vectors (Positive, Bit);
  subtype Bit_vector is Bit_vectors.Vector;  --  Just a simpler type name

  subtype U8 is Unsigned_8;
  subtype U16 is Unsigned_16;
  subtype U32 is Unsigned_32;

  procedure appendBits (bb : in out Bit_vector; value : U32; number_of_bits : Natural) is
  begin
    for pos in reverse 0 .. number_of_bits - 1 loop
      bb.Append (Bit (Shift_Right (value, pos) and 1));
    end loop;
  end appendBits;

  type Byte_array is array (Natural range <>) of U8;

  --  Packs this buffer's bits into bytes in big endian,
  --  padding with '0' bit values, and returns the new array.
  --
  function getBytes (bits : Bit_vector) return Byte_array is
    result : Byte_array (0 .. Natural (bits.Length) / 8 - 1) := (others => 0);
    idx : Integer;
  begin
    for i in 1 .. Integer (bits.Length) loop
      idx := (i - 1) / 8;
      result (idx) := result (idx) or
        Shift_Left (U8 (bits.Element (i)), Natural (7 - ((i - 1) mod 8)));
    end loop;
    if verbosity > 1 then
      for i in result'Range loop
        Put_Line ("getBytes: " & Integer'Image (i) & U8'Image (result (i)));
      end loop;
    end if;
    return result;
  end getBytes;

  type Segment is record
    mode     : Segment_mode;
    numChars : Natural;
    bitData  : Bit_vector;
  end record;

  type Segment_list is array (Positive range <>) of Segment;

  function numCharCountBits (mode : Segment_mode; ver : QR_version) return Natural is
  begin
    case ver is
      when 1 .. 9   => return segment_params (mode).ccbits (0);
      when 10 .. 26 => return segment_params (mode).ccbits (1);
      when 27 .. 40 => return segment_params (mode).ccbits (2);
    end case;
  end numCharCountBits;

  function getTotalBits (segs : Segment_list; version : QR_version) return Natural is
    result : Natural := 0;
    ccbits : Positive;
  begin
    for i in segs'Range loop
      ccbits := numCharCountBits (segs (i).mode, version);
      if verbosity > 2 then
        Put_Line (
          "getTotalBits: segment" & Integer'Image (i) &
          " mode " & Segment_mode'Image (segs (i).mode) &
          " version" & QR_version'Image (version) &
          " ccbits" & Integer'Image (ccbits)
        );
      end if;
      --  Fail if segment length value doesn't fit in the length field's bit-width
      if segs (i).numChars >= 2 ** ccbits then
        raise Cannot_Encode with "Segment data too long";
      end if;
      result := result + 4 + ccbits + Integer (segs (i).bitData.Length);
    end loop;
    return result;
  end getTotalBits;

  --  !! rename as: Compose_as_BYTE
  function makeBytes (text : String) return Segment is
    bit_soup : Bit_vector;
  begin
    if verbosity > 2 then
      Put_Line ("makeBytes start");
    end if;
    for i in text'Range loop
      appendBits (bit_soup, U32 (Character'Pos (text (i))), 8);
    end loop;
    if verbosity > 2 then
      Put_Line ("makeBytes done");
    end if;
    return (mode => BYTE, numChars => text'Length, bitData => bit_soup);
  end makeBytes;

  --  !! rename as: Compose_segments
  function makeSegments (text : String) return Segment_list is
  begin
    return (1 => makeBytes (text));
    --  !! To-do: split the text to make a smart mix of numeric,
    --     alphanumeric, etc. for having a compact encoding
  end makeSegments;

  function Get_min_version (ecl : Error_Correction_Level; text : String) return QR_version is
    dataUsedBits, dataCapacityBits : Positive;
    segs : constant Segment_list := makeSegments (text);
  begin
    for version in QR_version loop
      dataCapacityBits := getNumDataCodewords (version, ecl) * 8;
      begin
        dataUsedBits := getTotalBits (segs, version);
        if verbosity > 2 then
          Put_Line (
            "Get_min_version: test QR version" & Integer'Image (version) &
            " dataUsedBits="     & Integer'Image (dataUsedBits) &
            " dataCapacityBits=" & Integer'Image (dataCapacityBits));
        end if;
        if dataUsedBits <= dataCapacityBits then
          return version;
        end if;
      exception
        when Cannot_Encode =>
          null;  --  Skip this version: one segment's data would be too long
      end;
    end loop;
    raise Cannot_Encode with "Message to be encoded doesn't fit in any QR version";
  end Get_min_version;

  function Get_border_size (version : QR_version) return Positive is
  begin
    return version * 4 + 17;
  end Get_border_size;

  ---------------------------------------------------------------
  --  Error correction codes (could be in a separate package)  --
  ---------------------------------------------------------------

  --  Returns the product of the two given field elements modulo GF(2^8/16#11D#).
  function finiteFieldMultiply (x, y : U8) return U8 is
    z : U8 := 0;
  begin
    --  Russian peasant multiplication
    for i in reverse 0 .. 7 loop
      z := Shift_Left (z, 1) xor (Shift_Right (z, 7) * 16#1D#);
      z := z xor (Shift_Right (y, i) and 1) * x;
    end loop;
    return z;
  end finiteFieldMultiply;

  --  Calculates the Reed-Solomon generator polynomial of the given degree, storing in result[0 : degree].
  procedure calcReedSolomonGenerator (result : out Byte_array) is
    degree : constant Positive := result'Last + 1;
    root : U8;
  begin
    --  Start with the monomial x^0
    result := (others => 0);
    result (degree - 1) := 1;
    --  Compute the product polynomial (x - r^0) * (x - r^1) * (x - r^2) * ... * (x - r^{degree-1}),
    --  drop the highest term, and store the rest of the coefficients in order of descending powers.
    --  Note that r = 2, which is a generator element of this field GF(2^8/16#11D#).
    root := 1;
    for i in 0 .. degree - 1 loop
      --  Multiply the current product by (x - r^i)
      for j in 0 .. degree - 1 loop
        result (j) := finiteFieldMultiply (result (j), root);
        if j + 1 < degree then
          result (j) := result (j) xor result (j + 1);
        end if;
      end loop;
      root := finiteFieldMultiply (root, 2);
    end loop;
  end calcReedSolomonGenerator;

  --  Calculates the remainder of the polynomial data when divided by the generator, where all
  --  polynomials are in big endian and the generator has an implicit leading 1 term,
  --  storing the result in result[0 : degree].
  procedure calcReedSolomonRemainder (data, generator : Byte_array; result : out Byte_array) is
    factor : U8;
    degree : constant Natural := generator'Length;
    --  Perform polynomial division
  begin
    if result'Length /= degree then
      raise Constraint_Error with "result must have the generator's length (= polynomial degree)";
    end if;
    result := (others => 0);
    for i in data'Range loop
      factor := data (i) xor result (result'First);
      if verbosity > 2 then
        Put_Line (
          "calcReedSolomonRemainder: dumping data, factor: " &
          Integer'Image (i) & U8'Image (data (i)) & U8'Image (factor));
      end if;
      --  Shift.
      result (result'First .. result'First + degree - 2) :=
        result (result'First + 1 .. result'First + degree - 1);
      result (result'First + degree - 1) := 0;
      for j in 0 .. degree - 1 loop
        result (result'First + j) := result (result'First + j) xor
          finiteFieldMultiply (generator (generator'First + j), factor);
      end loop;
    end loop;
  end calcReedSolomonRemainder;

  qr_kind_to_ecl : constant array (Code_QR) of Error_Correction_Level :=
        (Code_QR_Low        =>   LOW,
         Code_QR_Medium     =>   MEDIUM,
         Code_QR_Quartile   =>   QUARTILE,
         Code_QR_High       =>   HIGH
    );

  procedure Draw (bc : in out Bar_Code; text : String; qr_kind : Code_QR) is
    selected_ecl : constant Error_Correction_Level := qr_kind_to_ecl (qr_kind);
    version : constant QR_version := Get_min_version (selected_ecl, text);
    border_size : constant Positive := Get_border_size (version);
    --  Coordinates in the QR square:
    subtype Module_range is Integer range 0 .. border_size - 1;
    --
    --  The grid y axis is top-down; coordinates are (y,x).
    type Grid is array (Module_range, Module_range) of Boolean;
    --
    modules, isFunction : Grid := (others => (others => False));
    --
    --  Sets the color of a module and marks it as a function module.
    --
    procedure setFunctionModule (x, y : Module_range; isBlack : Boolean) is
    begin
      modules (y, x)    := isBlack;
      isFunction (y, x) := True;  --  Cell is marked, be it black or white.
    end;
    --
    --  Table 23: Mask pattern generation (8 different ways of XOR masking).
    type Mask_pattern_reference is range 0 .. 7;
    --
    function Get_Bit (x : U16; bit : Natural) return Boolean is
    begin
      return (Shift_Right (x, bit) and 1) /= 0;
    end;
    --
    --  8.9 Format Information
    --
    procedure Draw_Format_Bits (mask_ref : Mask_pattern_reference) is
      --  The Format Information is a 15 bit sequence containing 5 data bits,
      --  with 10 error correction bits calculated using the (15, 5) BCH code.
      data, bch : U16;
    begin
      --  Table 25 - Error correction level indicators
      case selected_ecl is
        when LOW      => data := 1;
        when MEDIUM   => data := 0;
        when QUARTILE => data := 3;
        when HIGH     => data := 2;
      end case;
      data := Shift_Left (data, 3) + U16 (mask_ref);  --  5 bits data.
      --  Now we add 10 bits of an error-correcting code specific to the
      --  format bits only! Used: BCH (Bose-Chaudhuri-Hocquenghem) code.
      bch := data;
      for i in 1 .. 10 loop
        bch := Shift_Left (bch, 1) xor (Shift_Right (bch, 9) * 16#537#);
      end loop;
      data := Shift_Left (data, 10) + bch;
      --  Ensure that no combination of Error Correction Level and
      --  Mask Pattern Reference will result in an all-zero data string.
      data := data xor 2#101010000010010#;
      --
      --  Figure 19 - Format Information positioning
      --
      --  Draw first copy on top left corner
      for i in 0 .. 5 loop
        setFunctionModule (8, i, Get_Bit (data, i));
      end loop;
      setFunctionModule (8, 7, Get_Bit (data, 6));
      setFunctionModule (8, 8, Get_Bit (data, 7));
      setFunctionModule (7, 8, Get_Bit (data, 8));
      for i in 9 .. 14 loop
        setFunctionModule (14 - i, 8, Get_Bit (data, i));
      end loop;
      --  Draw second copy
      for i in 0 .. 7 loop
        setFunctionModule (border_size - 1 - i, 8, Get_Bit (data, i));
      end loop;
      for i in 8 .. 14 loop
        setFunctionModule (8, border_size - 15 + i, Get_Bit (data, i));
      end loop;
      --  The lonely Dark Module ("...shall always be dark and
      --  does not form part of the Format Information.")
      setFunctionModule (8, border_size - 8, True);
    end Draw_Format_Bits;
    --
    --  Draw patterns that do not belong to encoded data: the three
    --  big squares for finding the orientation and bounds, the small
    --  squares for alignment, etc. This is done before drawing the data.
    --  Function patterns are turned around when drawing data.
    --
    procedure Draw_Function_Patterns is
      --  7.3.2 - Draws a 7x7 finder pattern, plus the surrounding white
      --          border separator (7.3.3), with the center module at (x, y).
      --
      procedure Draw_Finder_Pattern (x, y : Module_range) is
        dist, xx, yy : Integer;
      begin
        for dx in -4 .. 4 loop
          for dy in -4 .. 4 loop
            dist := Integer'Max (abs dx, abs dy);  --  Chebyshev / infinity norm
            xx := x + dx;
            yy := y + dy;
            if xx in Module_range and then yy in Module_range then
              setFunctionModule (xx, yy, dist /= 2 and dist /= 4);
            end if;
          end loop;
        end loop;
      end Draw_Finder_Pattern;
      --
      --  Annex E - Position of Alignment Patterns - Table E.1
      --
      procedure Draw_Alignment_Patterns is
        --  Draws a 5x5 alignment pattern, with the center module at (x, y).
        procedure Draw_Alignment_Pattern (x, y : Module_range) is
          dist : Integer;
        begin
          for dx in -2 .. 2 loop
            for dy in -2 .. 2 loop
              dist := Integer'Max (abs dx, abs dy);  --  Chebyshev / infinity norm
              setFunctionModule (x + dx, y + dy, dist /= 1);
            end loop;
          end loop;
        end Draw_Alignment_Pattern;
        --
        num_align : Natural := 0;
        step : Integer := 26;
        pos : Integer := version * 4 + 10;
        align_pos : array (1 .. 7) of Integer;
      begin
        if version > 1 then
          num_align := version / 7 + 2;
          if version /= 32 then
            step := ((version * 4 + num_align * 2 + 1) / (2 * num_align - 2)) * 2;
          end if;
          align_pos (1) := 6;
          for i in reverse 2 .. num_align loop
            align_pos (i) := pos;
            pos := pos - step;
          end loop;
        end if;
        --  Draw the lattice
        for i in 1 .. num_align loop
          for j in 1 .. num_align loop
            if (i = 1 and j = 1) or
               (i = 1 and j = num_align) or
               (i = num_align and j = 1)
            then
              null;  --  Skip the three finder corners
            else
              Draw_Alignment_Pattern (align_pos (i), align_pos (j));
            end if;
          end loop;
        end loop;
      end Draw_Alignment_Patterns;
      --
      --  8.10 Version Information
      --
      --  Draws two copies of the version bits (with its own error correction code),
      --  based on this object's version field (which only has an effect for 7 <= version <= 40).
      procedure Draw_Version is
        --  The Version Information is an 18 bit sequence containing 6 data bits, with 12 error
        --  correction bits calculated using the (18, 6) BCH code.
        data, bch : U16;
        a, b : Module_range;
        bit : Boolean;
      begin
        if version < 7 then
          return;
        end if;
        --  Calculate error correction code and pack bits
        bch := U16 (version);
        for i in 1 .. 12 loop
          bch := Shift_Left (bch, 1) xor (Shift_Right (bch, 11) * 16#1F25#);
        end loop;
        data := Shift_Left (U16 (version), 12) + bch;
        --  Draw two copies
        for i in 0 .. 17 loop
          a := border_size - 11 + i mod 3;
          b := i / 3;
          bit := Get_Bit (data, i);
          setFunctionModule (a, b, bit);
          setFunctionModule (b, a, bit);
        end loop;
      end Draw_Version;
      --
    begin
      --  7.3.4 - Draw horizontal and vertical timing
      --          patterns (dotted lines).
      for i in Module_range loop
        setFunctionModule (6, i, i mod 2 = 0);
        setFunctionModule (i, 6, i mod 2 = 0);
      end loop;
      --  7.3.2 - Draw 3 finder patterns (all corners except bottom
      --          right; overwrites some timing modules)
      Draw_Finder_Pattern (3, 3);
      Draw_Finder_Pattern (border_size - 4, 3);
      Draw_Finder_Pattern (3, border_size - 4);
      --  7.3.5 - Draw alignment patterns
      Draw_Alignment_Patterns;
      --
      --  The mask ref. is fake; this is just for marking the modules
      --  as Function and avoid data being written there.
      Draw_Format_Bits (mask_ref => 0);
      --  8.10 - Draw Version Information
      Draw_Version;
    end Draw_Function_Patterns;
    --
    --  Appends error correction bytes to each block of the given data array, then interleaves bytes
    --  from the blocks and stores them in the result array. data (0 .. rawCodewords - totalEcc - 1) contains
    --  the input data. data (rawCodewords - totalEcc .. rawCodewords - 1) is used as a temporary work area.
    --  The final answer is stored in result.
    --
    function appendErrorCorrection (in_data : Byte_array) return Byte_array is
      numBlocks : constant Integer := NUM_ERROR_CORRECTION_BLOCKS (selected_ecl, version);
      blockEccLen : constant Integer := ECC_CODEWORDS_PER_BLOCK (selected_ecl, version);
      rawCodewords : constant Integer := getNumRawDataModules (version) / 8;
      dataLen : constant Integer := rawCodewords - blockEccLen * numBlocks;
      numShortBlocks : constant Integer := numBlocks - rawCodewords mod numBlocks;
      shortBlockDataLen : constant Integer := rawCodewords / numBlocks - blockEccLen;
      --
      data, result : Byte_array (0 .. rawCodewords - 1);
      generator : Byte_array (0 .. blockEccLen - 1);
      j, k, l, blockLen : Integer;
    begin
      data (0 .. dataLen - 1) := in_data;
      --
      --  8.5.2 Generating the error correction codeword
      --
      --  Split data into blocks and append ECC after all data
      calcReedSolomonGenerator (generator);
      if verbosity > 2 then
        for i in generator'Range loop
            Put_Line ("Dumping ECC generator: " & Integer'Image (i) & U8'Image (generator (i)));
        end loop;
      end if;
      --
      j := dataLen;
      k := 0;
      for i in 0 .. numBlocks - 1 loop
        blockLen := shortBlockDataLen;
        if i >= numShortBlocks then
          blockLen := blockLen + 1;
        end if;
        calcReedSolomonRemainder (data (k .. k + blockLen - 1), generator, data (j .. j + generator'Length - 1));
        j := j + blockEccLen;
        k := k + blockLen;
      end loop;
      --
      --  8.6 Constructing the final message codeword sequence
      --
      --  Interleave (not concatenate) the bytes from every block into a single sequence
      k := 0;
      for i in 0 .. numBlocks - 1 loop
        l := i;
        for j in 0 .. shortBlockDataLen - 1 loop
          result (l) := data (k);
          k := k + 1;
          l := l + numBlocks;
        end loop;
        if i >= numShortBlocks then
          k := k + 1;
        end if;
      end loop;
      k := (numShortBlocks + 1) * shortBlockDataLen;
      l := numBlocks * shortBlockDataLen;
      for i in numShortBlocks .. numBlocks - 1 loop
        result (l) := data (k);
        k := k + shortBlockDataLen + 1;
        l := l + 1;
      end loop;
      k := dataLen;
      for i in 0 .. numBlocks - 1 loop
        l := dataLen + i;
        for j in 0 .. blockEccLen - 1 loop
          result (l) := data (k);
          k := k + 1;
          l := l + numBlocks;
        end loop;
      end loop;
      return result;
    end appendErrorCorrection;
    --
    procedure Draw_Data is
      --
      --  Draw codewords (data with ecc) in zigzag
      --
      procedure Draw_Codewords (data_and_ecc_bytes : Byte_array) is
        i : Integer := 0;  --  Bit index into the data
        idx : Integer;  --  Codeword (byte) index
        right : Integer := border_size - 1;  --  Index of right column in each column pair
        x, y : Integer;
        upward : Boolean;
      begin
        loop
          if right = 6 then
            right := 5;
          end if;
          upward := (U32 (right + 1) and 2) = 0;
          for vert in Module_range loop
            for j in 0 .. 1 loop
              x := right - j;  --  Actual x coordinate
              --  Actual y coordinate:
              if upward then
                y := border_size - 1 - vert;
              else
                y := vert;
              end if;
              if not isFunction (y, x) then
                idx := data_and_ecc_bytes'First + i / 8;
                if idx > data_and_ecc_bytes'Last then
                  --  If there are any remainder bits (0 to 7), they are already
                  --  set to 0/false/white when the grid of modules was initialized
                  null;
                else
                  modules (y, x) := Get_Bit (U16 (data_and_ecc_bytes (idx)), 7 - (i mod 8));
                end if;
                i := i + 1;
              end if;
            end loop;
          end loop;
          right := right - 2;
          exit when right < 1;
        end loop;
      end Draw_Codewords;
      --
      --  Apply XOR mask
      --
      --  XORs the data modules in this QR Code with the given mask pattern. Due to XOR's mathematical
      --  properties, calling applyMask(m) twice with the same value is equivalent to no change at all.
      --  This means it is possible to apply a mask, undo it, and try another mask. Note that a final
      --  well-formed QR Code symbol needs exactly one mask applied (not zero, not two, etc.).
      --
      procedure Apply_mask (mask_ref : Mask_pattern_reference) is
        invert : Boolean;
      begin
        for y in Module_range loop
          for x in Module_range loop
            if not isFunction (y, x) then
              case mask_ref is
                when 0 => invert := (x + y) mod 2 = 0;
                when 1 => invert := y mod 2 = 0;
                when 2 => invert := x mod 3 = 0;
                when 3 => invert := (x + y) mod 3 = 0;
                when 4 => invert := (x / 3 + y / 2) mod 2 = 0;
                when 5 => invert := x * y mod 2 + x * y mod 3 = 0;
                when 6 => invert := (x * y mod 2 + x * y mod 3) mod 2 = 0;
                when 7 => invert := ((x + y) mod 2 + x * y mod 3) mod 2 = 0;
              end case;
              modules (y, x) := modules (y, x) xor invert;
            end if;
          end loop;
        end loop;
      end Apply_mask;
      --
      dataCapacityBits : constant Positive := getNumDataCodewords (version, selected_ecl) * 8;
      bb : Bit_vector;
      segs : constant Segment_list := makeSegments (text);
      padByte : U8;
    begin
      if verbosity > 2 then
        Put_Line ("encodeSegments: QR version:" & QR_version'Image (version));
        Put_Line ("encodeSegments: dataCapacityBits:" & Integer'Image (dataCapacityBits));
      end if;
      --  Create the data bit string by concatenating all segments
      for si in segs'Range loop
        if verbosity > 2 then
          Put_Line ("encodeSegments: one segment, modeBits:");
        end if;
        appendBits (bb, U32 (segment_params (segs (si).mode).modeBits), 4);
        if verbosity > 2 then
          Put_Line ("encodeSegments: one segment, numChars:");
        end if;
        appendBits (bb, U32 (segs (si).numChars), numCharCountBits (segs (si).mode, version));
        if verbosity > 2 then
          Put_Line ("encodeSegments: one segment, contents length:" & Ada.Containers.Count_Type'Image (segs (si).bitData.Length));
        end if;
        --  Copy bits into concatenated buffer
        bb.Append (segs (si).bitData);
      end loop;
      --  Add terminator and pad up to a byte if applicable
      if verbosity > 2 then
        Put_Line ("encodeSegments: terminator 1:");
      end if;
      appendBits (bb, 0, Integer'Min (4, dataCapacityBits - Integer (bb.Length)));
      if verbosity > 2 then
        Put_Line ("encodeSegments: terminator 2:");
      end if;
      appendBits (bb, 0, (8 - Integer (bb.Length) mod 8) mod 8);
      if verbosity > 2 then
        Put_Line ("encodeSegments: padding:");
      end if;
      --  Pad with alternate bytes until data capacity is reached
      padByte := 16#EC#;
      while Integer (bb.Length) < dataCapacityBits loop
        appendBits (bb, U32 (padByte), 8);
        padByte := padByte xor 16#EC# xor 16#11#;
      end loop;
      if verbosity > 2 then
        Put_Line ("encodeSegments: done padding");
      end if;
      if Integer (bb.Length) mod 8 /= 0 then
        raise Constraint_Error with "Wrong padding";
      end if;
      --
      --  Now bb contains the exact bit sequence to be drawn, turn it into a byte buffer
      --
      declare
        data_bytes : constant Byte_array := getBytes (bb);
        data_and_ecc_bytes : constant Byte_array := appendErrorCorrection (data_bytes);
        --  !!  to do: automatic mask selection (penalty etc.)
        mask_ref_chosen : constant Mask_pattern_reference := 0;
      begin
        if verbosity > 1 then
          for i in data_bytes'Range loop
            Put_Line ("Dumping data_bytes: " & Integer'Image (i) & U8'Image (data_bytes (i)));
          end loop;
          for i in data_and_ecc_bytes'Range loop
            Put_Line ("Dumping data_and_ecc_bytes: " & Integer'Image (i) & U8'Image (data_and_ecc_bytes (i)));
          end loop;
        end if;
        Draw_Codewords (data_and_ecc_bytes);
        Draw_Format_Bits (mask_ref_chosen);
        Apply_mask (mask_ref_chosen);
      end;
    end Draw_Data;
    --
    procedure Output_to_media is
    begin
      --  For vector graphics only: we need to squeeze the full 2D code
      --  into the bounding box. A "module" is the smallest square.
      bc.module_width  := bc.bounding.width / Real (border_size);
      bc.module_height := bc.bounding.height / Real (border_size);
      --
      for y in Module_range loop
        for x in Module_range loop
          if modules (y, x) then
            Filled_Rectangle (Bar_Code'Class (bc), (x, border_size - 1 - y, 1, 1));
          end if;
        end loop;
      end loop;
    end Output_to_media;
  begin
    if verbosity > 0 then
      Put_Line ("[QR code] version" & QR_version'Image (version));
    end if;
    Draw_Function_Patterns;
    Draw_Data;
    Output_to_media;
  end Draw;

  function Fitting (text : String; qr_kind : Code_QR) return Module_Box is
    border_size : constant Positive :=
      Get_border_size (Get_min_version (qr_kind_to_ecl (qr_kind), text));
  begin
    if verbosity > 0 then
      Put_Line ("[QR code] Fitting function");
    end if;
    return (0, 0, border_size, border_size);
  end Fitting;

end Bar_Codes.Encode_QR;
