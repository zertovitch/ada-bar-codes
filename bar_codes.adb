with Bar_Codes.Encode_Code_128;

with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;
with Ada.Text_IO;

package body Bar_Codes is

  function PDF_Bar_Code (
    kind     : Kind_Of_Code;
    bounding : Box;           --  Box in which the bar code should fit
    text     : String         --  Text to encode
  )
  return String
  is
    pdf_code : Unbounded_String;
    --
    type PBC is new Bar_Code with null record;
    overriding procedure Filled_Rectangle (bc : PBC; shape : Box) is
    pragma Unreferenced (bc);
    begin
      pdf_code := pdf_code &
        "    " &
        Img (shape.left)   & ' ' &
        Img (shape.bottom) & ' ' &
        Img (shape.width)  & ' ' &
        Img (shape.height) & " re" & ASCII.LF;
    end Filled_Rectangle;
    --
    bc : PBC;
  begin
    bc.Draw (kind, bounding, text);
    return
      "%  Begin of Bar code" & ASCII.LF &
      "%    Automatically generated by " & Bar_Codes.title &
      " version " & Bar_Codes.version &
      ", " & Bar_Codes.reference &
      " @ " & Bar_Codes.web & ASCII.LF &
      "%    Bar code format: " & Bar_Codes.Kind_Of_Code'Image (kind) & ASCII.LF &
      "%    Text to be encoded: [" & Printable (text) & ']' & ASCII.LF &
      "%    This PDF snippet has to be included into a PDF document." & ASCII.LF &
      "%    For instance, use Insert_Graphics_PDF_Code of PDF_Out, http://apdf.sf.net/" & ASCII.LF &
      "q"   & ASCII.LF &  --  Save the current graphics state
      "0 g" & ASCII.LF &  --  Black
      To_String (pdf_code) &
      "f"   & ASCII.LF &  --  Paint the rectangles (fill)
      "Q"   & ASCII.LF &  --  Restore the graphics state
      "%  End of bar code" & ASCII.LF;
  end PDF_Bar_Code;

  function SVG_Bar_Code (
    kind          : Kind_Of_Code;
    width, height : Real;
    unit          : String;        --  Length unit, for instance "mm" for millimeter
    text          : String         --  Text to encode
  )
  return String
  is
    svg_code : Unbounded_String;
    --
    type SBC is new Bar_Code with null record;
    overriding procedure Filled_Rectangle (bc : SBC; shape : Box) is
    pragma Unreferenced (bc);
    begin
      svg_code := svg_code &
        "    <rect style=""fill:#000000;""" &
        " x="""      & Img (shape.left) & unit & """" &
        " y="""      & Img (shape.bottom) & unit & """" &
        " height=""" & Img (shape.height) & unit & """" &
        " width="""  & Img (shape.width) & unit & """/>" & ASCII.LF;
    end Filled_Rectangle;
    --
    bc : SBC;
  begin
    bc.Draw (kind, (0.0, 0.0, width, height), text);
    return
      "<!--  Begin of Bar code  -->" & ASCII.LF &
      "<!--      Automatically generated by " & Bar_Codes.title &
      " version " & Bar_Codes.version &
      ", " & Bar_Codes.reference &
      " @ " & Bar_Codes.web &
      "  -->" & ASCII.LF &
      "<!--      Bar code format: " & Bar_Codes.Kind_Of_Code'Image (kind) & "  -->" & ASCII.LF &
      "<!--      Text to be encoded: [" & Printable (text) & "]  -->" & ASCII.LF &
      "<svg height=""" & Img (height) & unit &
        """ width=""" & Img (width) & unit &
        """ version=""1.1"" xmlns=""http://www.w3.org/2000/svg"">" & ASCII.LF &
      --  White rectangle as background
      "    <rect height=""100%"" width=""100%"" style=""fill:#FFFFFF""/>" & ASCII.LF &
      To_String (svg_code) &
      "</svg>"  & ASCII.LF &
      "<!--  End of bar code  -->" & ASCII.LF;
  end SVG_Bar_Code;

  ----------
  -- Draw --
  ----------

  procedure Draw (bc : in out Bar_Code; kind : Kind_Of_Code; bounding : Box; text : String) is
  begin
    bc.bounding := bounding;
    case kind is
      when Code_128 =>
        Bar_Codes.Encode_Code_128.Draw (bc, text);
    end case;
  end Draw;

  ----------------------------------------------------
  -- Goodies that can be useful for implementations --
  ----------------------------------------------------

  package RIO is new Ada.Text_IO.Float_IO (Real);

  --  Compact real number image, taken from PDF_Out
  --
  function Img (x : Real; prec : Positive := Real'Digits) return String is
    s : String (1 .. 20 + prec);
    na : Natural := s'First;
    nb : Natural := s'Last;
    np : Natural := 0;
  begin
    RIO.Put (s, x, prec, 0);
    --  We will increase na and decrease nb
    --  to compact the string s (na .. nb);
    for i in s'Range loop
      case s (i) is
        when '.' => np := i; exit;    --   Find a decimal point
        when ' ' => na := i + 1;      -- * Trim spaces on left
        when others => null;
      end case;
    end loop;
    if np > 0 then
      while nb > np and then s (nb) = '0' loop
        nb := nb - 1;                 -- * Remove extra '0's after decimal point
      end loop;
      if nb = np then
        nb := nb - 1;                 -- * Remove '.' if it is at the end
      elsif s (na .. np - 1) = "-0" then
        na := na + 1;
        s (na) := '-';                -- * Reduce "-0.x" to "-.x"
      elsif s (na .. np - 1) = "0" then
        na := na + 1;                 -- * Reduce "0.x" to ".x"
      end if;
    end if;
    return s (na .. nb);
  end Img;

  function Printable (s : String) return String is
    t : String := s;
  begin
    for i in s'Range loop
      case s (i) is
        when ' ' .. '~' => null;
        when others     => t (i) := '*';
      end case;
    end loop;
    return t;
  end Printable;

end Bar_Codes;
