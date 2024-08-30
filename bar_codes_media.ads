----------------------------------------------------------------
--  Ready-to-use implementations of the bar code generator:   --
--                                                            --
--    - PDF_Bar_Code : PDF vector graphics for PDF documents  --
--    - SVG_Bar_Code : SVG vector graphics for Web contents   --
--    - PBM_Bar_Code : PBM bitmap image (raster graphics)     --
--    - PNG_Bar_Code : PNG bitmap image (raster graphics)     --
--                                                            --
----------------------------------------------------------------
--
--  Important note
--
--  Vector graphics and raster graphics are fundamentally different
--  regarding the production of bar codes:
--
--  -  On vector graphics, lengths are arbitrarily divisible. So it
--     is possible to define a rectangle and let Bar_Codes
--     fill that rectangle with the bar code.
--
--  -  On raster graphics, the bar code generator *has* to produce integer
--     amounts of pixels. The only freedom is the scaling (an integer amount
--     as well). Furthermore:
--        - On some screens or other devices, pixels are not
--            displayed as squares. Sad!
--        - Modules of certain 2D bar codes (such as QR) are best rendered
--            squared.
--        - Consequently, for such 2D bar codes, scale_x = scale_y is not
--            always, automatically, the appropriate setting.
--            Check the output media's aspect ratio.

with Bar_Codes;

with Ada.Streams;

package Bar_Codes_Media is

  -----------------------------------------------------------------------------------
  --  Vector Graphics - PDF                                                        --
  -----------------------------------------------------------------------------------
  --  The PDF_Bar_Code function produces a PDF (Portable Document Format) snippet  --
  --  to be included into a PDF document. For instance, you can use                --
  --  Insert_Graphics_PDF_Code of package PDF_Out (project Ada PDF Writer,         --
  --  http://apdf.sf.net/ ) for such an inclusion.                                 --
  -----------------------------------------------------------------------------------

  function PDF_Bar_Code
    (kind     : Bar_Codes.Kind_Of_Code;
     bounding : Bar_Codes.Box;  --  Box in the PDF page, containing the bar code
     text     : String)         --  Text to encode
  return String;

  -----------------------------------------------------------------------------------
  --  Vector Graphics - SVG                                                        --
  -----------------------------------------------------------------------------------
  --  The SVG_Bar_Code function produces a SVG (Scalable Vector Graphics) object.  --
  --  You can view directly a SVG image with most Web browsers, or include it in   --
  --  an HTML document.                                                            --
  -----------------------------------------------------------------------------------

  function SVG_Bar_Code
    (kind          : Bar_Codes.Kind_Of_Code;
     width, height : Bar_Codes.Real;  --  Dimensions of the SVG bar code image
     unit          : String;          --  Length unit, for instance "mm" for millimeter
     text          : String)          --  Text to encode
  return String;

  -------------------------------------------------------------------------------
  --  Raster Graphics - PBM                                                    --
  -------------------------------------------------------------------------------
  --  The PBM_Bar_Code function produces a PBM (Portable BitMap) image.        --
  --  This simple image format is supported by GIMP ( https://www.gimp.org/ )  --
  --  or GID ( https://gen-img-dec.sourceforge.io/ )                           --
  -------------------------------------------------------------------------------

  function PBM_Bar_Code
    (kind             : Bar_Codes.Kind_Of_Code;
     scale_x, scale_y : Positive;      --  Scaling factors for the bitmap rendering
     text             : String)        --  Text to encode
  return String;

  ----------------------------------------------
  --  Raster Graphics - PNG                   --
  ----------------------------------------------
  --  The PNG_Bar_Code procedure produces a   --
  --  PNG (Portable Network Graphics) image.  --
  ----------------------------------------------

  procedure PNG_Bar_Code
    (kind             : in     Bar_Codes.Kind_Of_Code;
     scale_x, scale_y : in     Positive;      --  Scaling factors for the bitmap rendering
     text             : in     String;        --  Text to encode
     output           : in out Ada.Streams.Root_Stream_Type'Class);

end Bar_Codes_Media;
