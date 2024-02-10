with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;

package body Bar_Codes.Impl is

  function PDF_Bar_Code (
    kind     : Kind_Of_Code;
    bounding : Box;           --  Box in which the bar code should fit
    text     : String         --  Text to encode
  )
  return String
  is
    pdf_code : Unbounded_String;
    --
    type PDF_BC is new Bar_Code with null record;
    overriding procedure Filled_Rectangle (bc : PDF_BC; shape : Module_Box) is
    begin
      pdf_code := pdf_code &
        "    " &
        Img (bc.bounding.left   + bc.module_width  * Real (shape.left))   & ' ' &
        Img (bc.bounding.bottom + bc.module_height * Real (shape.bottom)) & ' ' &
        Img (bc.module_width  * Real (shape.width))  & ' ' &
        Img (bc.module_height * Real (shape.height)) & " re" & ASCII.LF;
    end Filled_Rectangle;
    --
    bc : PDF_BC;
  begin
    bc.Set_Bounding_Box (bounding);
    bc.Draw (kind, text);
    return
      "%  Begin of Bar code" & ASCII.LF &
      "%    Automatically generated by " & Bar_Codes.title &
      " version " & Bar_Codes.version &
      ", " & Bar_Codes.reference & ASCII.LF &
      "%    Web: " & Bar_Codes.web & ASCII.LF &
      "%    Bar code format: " & Bar_Codes.Kind_Of_Code'Image (kind) & ASCII.LF &
      "%    Text to be encoded: [" & Make_Printable (text) & ']' & ASCII.LF &
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
    type SVG_BC is new Bar_Code with null record;
    overriding procedure Filled_Rectangle (bc : SVG_BC; shape : Module_Box) is
    begin
      svg_code := svg_code &
        "    <rect style=""fill:#000000;""" &
        " x="""      & Img (bc.module_width  * Real (shape.left))   & unit & """" &
        " y="""      & Img (height - bc.module_height * Real (shape.bottom + shape.height)) & unit & """" &
        " width="""  & Img (bc.module_width  * Real (shape.width))  & unit & """" &
        " height=""" & Img (bc.module_height * Real (shape.height)) & unit & """/>" & ASCII.LF;
    end Filled_Rectangle;
    --
    bc : SVG_BC;
  begin
    bc.Set_Bounding_Box ((0.0, 0.0, width, height));
    bc.Draw (kind, text);
    return
      "<!--  Begin of Bar code  -->" & ASCII.LF &
      "<!--      Automatically generated by " & Bar_Codes.title &
      " version " & Bar_Codes.version &
      ", " & Bar_Codes.reference & "  -->" & ASCII.LF &
      "<!--      Web: " & Bar_Codes.web & "  -->" & ASCII.LF &
      "<!--      Bar code format: " & Bar_Codes.Kind_Of_Code'Image (kind) & "  -->" & ASCII.LF &
      "<!--      Text to be encoded: [" & Make_Printable (text) & "]  -->" & ASCII.LF &
      "<svg height=""" & Img (height) & unit &
        """ width=""" & Img (width) & unit &
        """ version=""1.1"" xmlns=""http://www.w3.org/2000/svg"">" & ASCII.LF &
      --  White rectangle as background
      "    <rect height=""100%"" width=""100%"" style=""fill:#FFFFFF""/>" & ASCII.LF &
      To_String (svg_code) &
      "</svg>"  & ASCII.LF &
      "<!--  End of bar code  -->" & ASCII.LF;
  end SVG_Bar_Code;

  function PBM_Bar_Code (
    kind             : Kind_Of_Code;
    scale_x, scale_y : Positive;
    text             : String         --  Text to encode
  )
  return String
  is
    type Bit is range 0 .. 1;
    black : constant := 1;
    white : constant := 0;
    fit : constant Module_Box := Fitting (kind, text);
    bitmap : array (0 .. scale_x * fit.width - 1, 0 .. scale_y * fit.height - 1) of Bit :=
      (others => (others => white));
    pbm_code : String (1 .. (bitmap'Length (1) * 2 + 1) * bitmap'Length (2));
    pbm_i : Positive := 1;
    --
    type PBM_BC is new Bar_Code with null record;
    overriding procedure Filled_Rectangle (bc : PBM_BC; shape : Module_Box) is
    pragma Unreferenced (bc);
    begin
      for x in scale_x * shape.left .. scale_x * (shape.left + shape.width) - 1 loop
        for y in scale_y * shape.bottom .. scale_y * (shape.bottom + shape.height) - 1 loop
          bitmap (x, y) := black;
        end loop;
      end loop;
    end Filled_Rectangle;
    --
    bc : PBM_BC;
  begin
    bc.Draw (kind, text);
    for y in reverse bitmap'Range (2) loop
      for x in bitmap'Range (1) loop
        pbm_code (pbm_i .. pbm_i + 1) := Bit'Image (bitmap (x, y));
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
      "#      Bar code format: " & Bar_Codes.Kind_Of_Code'Image (kind) & ASCII.LF &
      "#      Text to be encoded: [" & Make_Printable (text) & "]" & ASCII.LF &
      Integer'Image (bitmap'Length (1)) &
      Integer'Image (bitmap'Length (2)) & ASCII.LF &
      pbm_code;
  end PBM_Bar_Code;

end Bar_Codes.Impl;
