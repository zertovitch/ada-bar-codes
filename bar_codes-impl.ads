----------------------------------------------------------------------------
--  Ready-to-use implementations of the bar code generator:               --
--                                                                        --
--    - PDF_Bar_Code : PDF vector graphics for PDF documents              --
--    - SVG_Bar_Code : SVG vector graphics for Web contents               --
--    - PBM_Bar_Code : PBM bitmap image as an example of raster graphics  --
--                                                                        --
----------------------------------------------------------------------------
--
--  NB: vector graphics and raster graphics are fundamentally different
--      regarding the production of bar codes:
--
--        -  On vector graphics, lengths are arbitrarily divisible. So it
--           is possible to define a rectangle and let Bar_Codes
--           fill that rectangle with the bar code.
--
--        -  On raster graphics, the bar code generator needs to produce integer
--           amounts of pixels. The only freedom is the scaling (an integer amount
--           as well).
--           See the remark below the function PBM_Bar_Code for more details.

package Bar_Codes.Impl is

  -----------------------------------------------------------------------------------
  --  Vector Graphics - PDF                                                        --
  -----------------------------------------------------------------------------------
  --  The PDF_Bar_Code function produces a PDF (Portable Document Format) snippet  --
  --  to be included into a PDF document. For instance, you can use                --
  --  Insert_Graphics_PDF_Code of package PDF_Out (project Ada PDF Writer,         --
  --  http://apdf.sf.net/ ) for such an inclusion.                                 --
  -----------------------------------------------------------------------------------

  function PDF_Bar_Code
    (kind     : Kind_Of_Code;
     bounding : Box;           --  Box in the PDF page, containing the bar code
     text     : String)        --  Text to encode
  return String;

  -----------------------------------------------------------------------------------
  --  Vector Graphics - SVG                                                        --
  -----------------------------------------------------------------------------------
  --  The SVG_Bar_Code function produces a SVG (Scalable Vector Graphics) object.  --
  --  You can view directly a SVG image with most Web browsers, or include it in   --
  --  an HTML document.                                                            --
  -----------------------------------------------------------------------------------

  function SVG_Bar_Code
    (kind          : Kind_Of_Code;
     width, height : Real;          --  Dimensions of the SVG bar code image
     unit          : String;        --  Length unit, for instance "mm" for millimeter
     text          : String)        --  Text to encode
  return String;

  -------------------------------------------------------------------------------
  --  Raster Graphics - PBM                                                    --
  -------------------------------------------------------------------------------
  --  The PBM_Bar_Code function produces a PBM (Portable BitMap) image.        --
  --  This simple image format is supported by GIMP ( https://www.gimp.org/ )  --
  --  or GID ( https://gen-img-dec.sourceforge.io/ )                           --
  -------------------------------------------------------------------------------

  function PBM_Bar_Code
    (kind             : Kind_Of_Code;
     scale_x, scale_y : Positive;      --  Scaling factors for the bitmap rendering
     text             : String)        --  Text to encode
  return String;

  --  NB:
  --    - On some screens or other devices, pixels are not square.
  --    - Modules of certain 2D bar codes (such as QR) are best rendered square.
  --    - Consequently, for such 2D bar codes, scale_x = scale_y is not
  --        automatically the appropriate setting. Check the device's aspect ratio.

end Bar_Codes.Impl;
