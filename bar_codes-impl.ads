----------------------------------------------------------------------------
--  Ready-to-use implementations of the bar code generator:               --
--                                                                        --
--    - PDF_Bar_Code : PDF vector graphics for PDF documents              --
--    - SVG_Bar_Code : SVG vector graphics for Web contents               --
--    - PBM_Bar_Code : PBM bitmap image as an example of raster graphics  --
--                                                                        --
----------------------------------------------------------------------------

package Bar_Codes.Impl is

  --  The PDF_Bar_Code function produces a PDF (Portable Document Format) snippet
  --  to be included into a PDF document. For instance, you can use Insert_Graphics_PDF_Code
  --  of package PDF_Out (project Ada PDF Writer, http://apdf.sf.net/ ) for such an inclusion.
  --
  function PDF_Bar_Code (
    kind     : Kind_Of_Code;
    bounding : Box;           --  Box in which the bar code should fit
    text     : String         --  Text to encode
  )
  return String;

  --  The SVG_Bar_Code function produces a SVG (Scalable Vector Graphics) object.
  --  You can view directly a SVG image with most Web browsers, or include it in
  --  an HTML document.
  --
  function SVG_Bar_Code (
    kind          : Kind_Of_Code;
    width, height : Real;          --  (0,0)-based box in which the bar code should fit
    unit          : String;        --  Length unit, for instance "mm" for millimeter
    text          : String         --  Text to encode
  )
  return String;

  --  The PBM_Bar_Code function produces a PBM (Portable BitMap) image.
  --  This simple image format is supported by GIMP ( https://www.gimp.org/ )
  --  or GID ( https://gen-img-dec.sourceforge.io/ )
  --
  function PBM_Bar_Code (
    kind             : Kind_Of_Code;
    scale_x, scale_y : Positive;
    text             : String         --  Text to encode
  )
  return String;

end Bar_Codes.Impl;
