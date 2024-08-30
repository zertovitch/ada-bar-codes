with Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;
with Interfaces;

package body Bar_Codes_Media is

  --------------------
  --  PDF_Bar_Code  --
  --------------------

  function PDF_Bar_Code
    (kind     : Bar_Codes.Kind_Of_Code;
     bounding : Bar_Codes.Box;  --  Box in the PDF page, containing the bar code
     text     : String)         --  Text to encode
  return String
  is
    use Ada.Strings.Unbounded, Bar_Codes;
    pdf_code : Unbounded_String;
    --
    type PDF_BC is new Bar_Code with null record;
    --
    overriding procedure Filled_Rectangle (bc : PDF_BC; shape : Module_Box) is
    begin
      pdf_code := pdf_code &
        "    " &
        Img (bounding.left   + bc.Get_Module_Width  * Real (shape.left))   & ' ' &
        Img (bounding.bottom + bc.Get_Module_Height * Real (shape.bottom)) & ' ' &
        Img (bc.Get_Module_Width  * Real (shape.width))  & ' ' &
        Img (bc.Get_Module_Height * Real (shape.height)) & " re" & ASCII.LF;
    end Filled_Rectangle;
    --
    bc : PDF_BC;
  begin
    bc.Set_Bounding_Box (bounding);
    bc.Draw (kind, text);
    return
      "%  Begin of Bar Code" & ASCII.LF &
      "%    Automatically generated by " & Bar_Codes.title &
      " version " & Bar_Codes.version &
      ", " & Bar_Codes.reference & ASCII.LF &
      "%    Web: " & Bar_Codes.web & ASCII.LF &
      "%    Requested bar code format: " & kind'Image & ASCII.LF &
      "%    Text to be encoded: [" & Make_Printable (text) & ']' & ASCII.LF &
      "%    This PDF snippet has to be included into a PDF document." & ASCII.LF &
      "%    For instance, use Insert_Graphics_PDF_Code of PDF_Out, http://apdf.sf.net/" & ASCII.LF &
      "q"   & ASCII.LF &  --  Save the current graphics state
      "0 g" & ASCII.LF &  --  Black
      To_String (pdf_code) &
      "f"   & ASCII.LF &  --  Paint the rectangles (fill)
      "Q"   & ASCII.LF &  --  Restore the graphics state
      "%  End of Bar Code" & ASCII.LF;
  end PDF_Bar_Code;

  --------------------
  --  SVG_Bar_Code  --
  --------------------

  function SVG_Bar_Code
    (kind          : Bar_Codes.Kind_Of_Code;
     width, height : Bar_Codes.Real;  --  Dimensions of the SVG bar code image
     unit          : String;          --  Length unit, for instance "mm" for millimeter
     text          : String)          --  Text to encode
  return String
  is
    use Ada.Strings.Unbounded, Bar_Codes;
    svg_code : Unbounded_String;
    --
    type SVG_BC is new Bar_Code with null record;
    --
    overriding procedure Filled_Rectangle (bc : SVG_BC; shape : Module_Box) is
    begin
      svg_code := svg_code &
        "    <rect style=""fill:#000000;""" &
        " x="""      & Img (bc.Get_Module_Width  * Real (shape.left))   & unit & """" &
        " y="""      & Img (height - bc.Get_Module_Height * Real (shape.bottom + shape.height)) & unit & """" &
        " width="""  & Img (bc.Get_Module_Width  * Real (shape.width))  & unit & """" &
        " height=""" & Img (bc.Get_Module_Height * Real (shape.height)) & unit & """/>" & ASCII.LF;
    end Filled_Rectangle;
    --
    bc : SVG_BC;
  begin
    bc.Set_Bounding_Box ((0.0, 0.0, width, height));
    bc.Draw (kind, text);
    return
      "<!--  Begin of Bar Code  -->" & ASCII.LF &
      "<!--      Automatically generated by " & Bar_Codes.title &
      " version " & Bar_Codes.version &
      ", " & Bar_Codes.reference & "  -->" & ASCII.LF &
      "<!--      Web: " & Bar_Codes.web & "  -->" & ASCII.LF &
      "<!--      Requested bar code format: " & kind'Image & "  -->" & ASCII.LF &
      "<!--      Text to be encoded: [" & Make_Printable (text) & "]  -->" & ASCII.LF &
      "<svg height=""" & Img (height) & unit &
        """ width=""" & Img (width) & unit &
        """ version=""1.1"" xmlns=""http://www.w3.org/2000/svg"">" & ASCII.LF &
      --  White rectangle as background
      "    <rect height=""100%"" width=""100%"" style=""fill:#FFFFFF""/>" & ASCII.LF &
      To_String (svg_code) &
      "</svg>"  & ASCII.LF &
      "<!--  End of Bar Code  -->" & ASCII.LF;
  end SVG_Bar_Code;

  ------------------------------------------------------------------------------------------
  --  Generic drawing of bar codes on a back & white bitmap, for raster graphics outputs  --
  ------------------------------------------------------------------------------------------

  generic
    scale_xx : Positive;
    scale_yy : Positive;
    width    : Positive;
    height   : Positive;
  package Bitmap_BC_Buffer is

    type Bit is range 0 .. 1;

    black : constant := 1;
    white : constant := 0;

    bitmap :
      array (0 .. scale_xx * width - 1,
             0 .. scale_yy * height - 1) of Bit :=
      (others => (others => white));

    type Bitmap_BC is new Bar_Codes.Bar_Code with null record;
    overriding procedure Filled_Rectangle (bc : Bitmap_BC; shape : Bar_Codes.Module_Box);

  end Bitmap_BC_Buffer;

  package body Bitmap_BC_Buffer is

    overriding procedure Filled_Rectangle (bc : Bitmap_BC; shape : Bar_Codes.Module_Box) is
    pragma Unreferenced (bc);
    begin
      for x in scale_xx * shape.left .. scale_xx * (shape.left + shape.width) - 1 loop
        for y in scale_yy * shape.bottom .. scale_yy * (shape.bottom + shape.height) - 1 loop
          bitmap (x, y) := black;
        end loop;
      end loop;
    end Filled_Rectangle;

  end Bitmap_BC_Buffer;

  --------------------
  --  PBM_Bar_Code  --
  --------------------

  function PBM_Bar_Code
    (kind             : Bar_Codes.Kind_Of_Code;
     scale_x, scale_y : Positive;      --  Scaling factors for the bitmap rendering
     text             : String)        --  Text to encode
  return String
  is
    fit : constant Bar_Codes.Module_Box := Bar_Codes.Fitting (kind, text);

    package BC_BMP is
      new Bitmap_BC_Buffer
            (scale_xx => scale_x,
             scale_yy => scale_y,
             width   => fit.width,
             height  => fit.height);

    pbm_code : String (1 .. (BC_BMP.bitmap'Length (1) * 2 + 1) * BC_BMP.bitmap'Length (2));
    pbm_i : Positive := 1;
    --
    bc : BC_BMP.Bitmap_BC;
  begin
    bc.Draw (kind, text);
    for y in reverse BC_BMP.bitmap'Range (2) loop
      for x in BC_BMP.bitmap'Range (1) loop
        pbm_code (pbm_i .. pbm_i + 1) := BC_BMP.bitmap (x, y)'Image;
        pbm_i := pbm_i + 2;
      end loop;
      pbm_code (pbm_i) := ASCII.LF;
      pbm_i := pbm_i + 1;
    end loop;
    return
      "P1" & ASCII.LF &
      "#      Automatically generated by " & Bar_Codes.title &
      " version " & Bar_Codes.version &
      ", " & Bar_Codes.reference & ASCII.LF &
      "#      Web: " & Bar_Codes.web & ASCII.LF &
      "#      Requested bar code format: " & kind'Image & ASCII.LF &
      "#      Text to be encoded: [" & Bar_Codes.Make_Printable (text) & "]" & ASCII.LF &
      BC_BMP.bitmap'Length (1)'Image &
      BC_BMP.bitmap'Length (2)'Image & ASCII.LF &
      pbm_code;
  end PBM_Bar_Code;

  --------------------
  --  PNG_Bar_Code  --
  --------------------

  procedure PNG_Bar_Code
    (kind             : in     Bar_Codes.Kind_Of_Code;
     scale_x, scale_y : in     Positive;      --  Scaling factors for the bitmap rendering
     text             : in     String;        --  Text to encode
     output           : in out Ada.Streams.Root_Stream_Type'Class)
  is
    fit : constant Bar_Codes.Module_Box := Bar_Codes.Fitting (kind, text);

    package BC_BMP is
      new Bitmap_BC_Buffer
            (scale_xx => scale_x,
             scale_yy => scale_y,
             width   => fit.width,
             height  => fit.height);

    --------------------------------------------------------------
    --  Captive copy of Dumb_PNG, that can be found elsewhere,  --
    --  for instance in the GID (Generic Image Decoder) tests.  --
    --------------------------------------------------------------
    package Dumb_PNG is

      type Byte_Array is array (Integer range <>) of Interfaces.Unsigned_8;
      type p_Byte_Array is access Byte_Array;
      procedure Dispose is new Ada.Unchecked_Deallocation (Byte_Array, p_Byte_Array);

      type Buffer_Mode is
        (packed,    --  Raw, packed, 8-bit-per-channel RGB data.
         padded);   --  Same but with a 0 byte at the beginning of each row (faster).

      procedure Write
        (data      : in     Byte_Array;
         data_mode : in     Buffer_Mode;
         width     : in     Integer;     --  Image width
         height    : in     Integer;     --  Image height
         s         : in out Ada.Streams.Root_Stream_Type'Class);

    end Dumb_PNG;

    package body Dumb_PNG is

      use Ada.Streams, Interfaces;

      procedure Deflate
        (data        : in     Byte_Array;
         defl_output :    out Byte_Array;
         last        :    out Integer);

      procedure Write_Chunk
        (chunk_type, chunk_data : in     Byte_Array;
         s                      : in out Root_Stream_Type'Class);

      --

      function To_Bytes_Big_Endian (x : Unsigned_32) return Byte_Array;

      procedure Write
        (data      : in     Byte_Array;
         data_mode : in     Buffer_Mode;
         width     : in     Integer;     --  Image width
         height    : in     Integer;     --  Image height
         s         : in out Ada.Streams.Root_Stream_Type'Class)
      is
        ihdr          : Byte_Array (0 .. 12);

        function To_Bytes (s : String) return Byte_Array is
          result : Byte_Array (s'Range);
        begin
          for i in s'Range loop
            result (i) := Character'Pos (s (i));
          end loop;
          return result;
        end To_Bytes;

        procedure Process_IDAT (idat : Byte_Array) is
          idat_deflated : p_Byte_Array;
          deflated_size : Integer;
          deflated_last : Integer;
        begin
          --  The "compressed" size is larger than the uncompressed one :-)
          --
          deflated_size := 6 + idat'Length + 5 * (1 + idat'Length / 16#FFFF#);

          idat_deflated := new Byte_Array (0 .. deflated_size);

          Deflate (idat, idat_deflated.all, deflated_last);
          Write_Chunk (To_Bytes ("IDAT"), idat_deflated (0 .. deflated_last), s);

          Dispose (idat_deflated);
        end Process_IDAT;

        idat_composed : p_Byte_Array;
        row_size      : Integer;
        index_data    : Integer;
        index         : Integer;

      begin
        --  PNG header
        Byte_Array'Write
          (s'Access,
           16#89# & To_Bytes ("PNG") & (13, 10, 16#1A#, 10));

        --  IHDR chunk
        ihdr  (0 .. 3) := To_Bytes_Big_Endian (Unsigned_32 (width));
        ihdr  (4 .. 7) := To_Bytes_Big_Endian (Unsigned_32 (height));
        ihdr  (8)      := 8;  --  Bit depth: 8 bits per sample
        ihdr  (9)      := 2;  --  Color type: True color RGB
        ihdr (10)      := 0;  --  Compression method: DEFLATE
        ihdr (11)      := 0;  --  Filter method: Adaptive
        ihdr (12)      := 0;  --  Interlace method: None
        Write_Chunk (To_Bytes ("IHDR"), ihdr, s);

        case data_mode is

          when packed =>
            --  IDAT chunk (pixel values and row filters)
            --  Note: One additional byte at the beginning of each
            --        row specifies the filtering method.
            row_size := width * 3 + 1;
            idat_composed := new Byte_Array (0 .. row_size * height - 1);
            --
            --  The extra buffer (idat_composed.all) differs from the data
            --  only for the additional 0 before each row.
            --
            for y in 0 .. height - 1 loop
              idat_composed (y * row_size) := 0;  --  Filter type: None
              for x in  0 .. width - 1 loop
                index := y * row_size + 1 + x * 3;
                index_data := data'First + y * width * 3 + x * 3;
                idat_composed (index + 0) := data (index_data + 0);  --  Red
                idat_composed (index + 1) := data (index_data + 1);  --  Green
                idat_composed (index + 2) := data (index_data + 2);  --  Blue
              end loop;
            end loop;
            --
            Process_IDAT (idat_composed.all);
            Dispose (idat_composed);

          when padded =>
            --  Under this form, the data is already fit for being sent directly.
            Process_IDAT (data);

        end case;

        Write_Chunk (To_Bytes ("IEND"), (1 .. 0 => 0), s);
      end Write;

      procedure Deflate
        (data        : in     Byte_Array;
         defl_output :    out Byte_Array;
         last        :    out Integer)
      is
        procedure Write (b : Unsigned_8) is
        begin
          last := last + 1;
          defl_output (last) := b;
        end Write;

        offset         : Integer := 0;
        start          : Integer;
        cur_block_size : Integer;
        adler_1        : Unsigned_32 := 1;
        adler_2        : Unsigned_32 := 0;
        modulus        : constant := 65521;
      begin
        last := defl_output'First - 1;
        --  zlib header
        Write (16#08#);
        Write (16#1D#);

        --  Deflate data
        loop
          cur_block_size := Integer'Min (data'Length - offset, 16#FFFF#);
          --  Block type: Store; final flag if last block.
          Write (if offset + cur_block_size = data'Length then 1 else 0);
          Write     (Unsigned_8  (cur_block_size        rem 256));
          Write     (Unsigned_8 ((cur_block_size / 256) rem 256));
          Write (not Unsigned_8  (cur_block_size        rem 256));
          Write (not Unsigned_8 ((cur_block_size / 256) rem 256));
          start := data'First + offset;
          for i in start .. start + cur_block_size - 1 loop
            Write (data (i));
          end loop;
          offset := offset + cur_block_size;
          exit when offset >= data'Length;
        end loop;

        for b of data loop
          adler_1 := (adler_1 + Unsigned_32 (b)) rem modulus;
          adler_2 := (adler_2 + adler_1) rem modulus;
        end loop;
        Write (Unsigned_8 (adler_2  /  256));
        Write (Unsigned_8 (adler_2 rem 256));
        Write (Unsigned_8 (adler_1  /  256));
        Write (Unsigned_8 (adler_1 rem 256));
      end Deflate;

      package CRC32 is
        procedure Init (crc : out Unsigned_32);
        function  Final (crc : Unsigned_32) return Unsigned_32;
        procedure Update (crc : in out Unsigned_32; in_buf : Byte_Array);
      end CRC32;

      procedure Write_U32
        (x : in     Unsigned_32;
         s : in out Root_Stream_Type'Class);

      procedure Write_Chunk
        (chunk_type, chunk_data : in     Byte_Array;
         s                      : in out Root_Stream_Type'Class)
      is
        c : Unsigned_32;
      begin
        CRC32.Init (c);
        CRC32.Update (c, chunk_type);
        CRC32.Update (c, chunk_data);
        Write_U32 (chunk_data'Length, s);
        Byte_Array'Write (s'Access, chunk_type);
        Byte_Array'Write (s'Access, chunk_data);
        Write_U32 (CRC32.Final (c), s);
      end Write_Chunk;

      procedure Write_U32
        (x : in     Unsigned_32;
         s : in out Root_Stream_Type'Class)
      is
      begin
        Byte_Array'Write (s'Access, To_Bytes_Big_Endian (x));
      end Write_U32;

      function To_Bytes_Big_Endian (x : Unsigned_32) return Byte_Array is
        result : Byte_Array (1 .. 4);
      begin
        result (1) := Unsigned_8 (Shift_Right (x, 24));
        result (2) := Unsigned_8 (Shift_Right (x, 16) and 255);
        result (3) := Unsigned_8 (Shift_Right (x,  8) and 255);
        result (4) := Unsigned_8              (x      and 255);
        return result;
      end To_Bytes_Big_Endian;

      package body CRC32 is

        CRC32_Table : array (Unsigned_32'(0) .. 255) of Unsigned_32;

        procedure Prepare_Table is
          --  CRC-32 algorithm, ISO-3309
          Seed : constant := 16#EDB88320#;
          l : Unsigned_32;
        begin
          for i in CRC32_Table'Range loop
            l := i;
            for bit in 0 .. 7 loop
              if (l and 1) = 0 then
                l := Shift_Right (l, 1);
              else
                l := Shift_Right (l, 1) xor Seed;
              end if;
            end loop;
            CRC32_Table (i) := l;
          end loop;
        end Prepare_Table;

        procedure Update (crc : in out Unsigned_32; in_buf : Byte_Array) is
          local_CRC : Unsigned_32;
        begin
          local_CRC := crc;
          for i in in_buf'Range loop
            local_CRC :=
              CRC32_Table (16#FF# and (local_CRC xor Unsigned_32 (in_buf (i))))
              xor
              Shift_Right (local_CRC, 8);
          end loop;
          crc := local_CRC;
        end Update;

        table_empty : Boolean := True;

        procedure Init (crc : out Unsigned_32) is
        begin
          if table_empty then
            Prepare_Table;
            table_empty := False;
          end if;
          crc := 16#FFFF_FFFF#;
        end Init;

        function Final (crc : Unsigned_32) return Unsigned_32 is
        begin
          return not crc;
        end Final;

      end CRC32;

    end Dumb_PNG;

    rgb_code : Dumb_PNG.Byte_Array (1 .. BC_BMP.bitmap'Length (1) * BC_BMP.bitmap'Length (2) * 3);
    rgb_i : Positive := 1;
    --
    bc : BC_BMP.Bitmap_BC;
    use Interfaces;
  begin
    bc.Draw (kind, text);
    for y in reverse BC_BMP.bitmap'Range (2) loop
      for x in BC_BMP.bitmap'Range (1) loop
        for channel in 1 .. 3 loop
          rgb_code (rgb_i) := 255 - 255 * Unsigned_8 (BC_BMP.bitmap (x, y));
          rgb_i := rgb_i + 1;
        end loop;
      end loop;
    end loop;
    Dumb_PNG.Write (rgb_code, Dumb_PNG.packed, BC_BMP.bitmap'Length (1), BC_BMP.bitmap'Length (2), output);
  end PNG_Bar_Code;

end Bar_Codes_Media;
