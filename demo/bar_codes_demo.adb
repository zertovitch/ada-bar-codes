---------------------------------------------------
--  The big... Ada Bar Codes Demo (ABCD :-) ) !  --
---------------------------------------------------

with Ada.Streams.Stream_IO,
     Ada.Text_IO;

with Bar_Codes, Bar_Codes_Media;

procedure Bar_Codes_Demo is

  --  SVG files (bar_code_128.svg, dm_code.svg, qr_code_l.svg) can be viewed
  --       directly in a Web browser.
  --
  --  PDF snippets need to be included into a PDF document.
  --       For instance, use Insert_Graphics_PDF_Code of PDF_Out, http://apdf.sf.net/
  --
  --  PBM images demonstrate output as raster graphics. This could be another image
  --       format, or anything else involving pixels, like a screen, a printer, etc.
  --
  --  PNG images demonstrate output as raster graphics for the ubiquitous PNG format.
  --
  use Ada.Text_IO;
  svg, pdf, pbm : File_Type;

  package SIO renames Ada.Streams.Stream_IO;
  png : SIO.File_Type;

  procedure SVG_Header is
  --  NB: the SVG file can be viewed without this header.
  begin
    Put_Line (svg, "<?xml version=""1.0"" encoding=""UTF-8""?>");
    Put_Line (svg, "<!DOCTYPE svg");
    Put_Line (svg, "  PUBLIC '-//W3C//DTD SVG 1.1//EN'");
    Put_Line (svg, "  'http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd'>");
  end SVG_Header;
  --
  prefix : constant String := "Hello from " & Bar_Codes.title;
  hello_short : constant String := prefix & "! How's life?";
  hello_long  : constant String := prefix & " ( " & Bar_Codes.web & " ) ! My number is: 1234567890";
  --
  procedure Demo_Code_128 is
    use Bar_Codes, Bar_Codes_Media;
  begin
    Create (svg, Out_File, "bar_code_128.svg");
    SVG_Header;
    Put_Line (svg, SVG_Bar_Code (Code_128, 57.0, 23.0, "mm", hello_short));
    Close (svg);
    --
    Create (pdf, Out_File, "bar_code_128_pdf.txt");
    Put_Line (pdf, PDF_Bar_Code (Code_128, (150.0, 320.0, 600.0, 50.0), hello_short));
    Close (pdf);
    --
    Create (pbm, Out_File, "bar_code_128.pbm");
    Put_Line (pbm, PBM_Bar_Code (Code_128, 2, 100, hello_short));
    Close (pbm);
    --
    SIO.Create (png, SIO.Out_File, "bar_code_128.png");
    PNG_Bar_Code (Code_128, 2, 100, hello_short, SIO.Stream (png).all);
    SIO.Close (png);
  end Demo_Code_128;
  --
  procedure Demo_QR is
    use Bar_Codes, Bar_Codes_Media;
  begin
    Create (svg, Out_File, "qr_code_l.svg");
    SVG_Header;
    Put_Line (svg, SVG_Bar_Code (Code_QR_Low, 60.0, 60.0, "mm", hello_long));
    Close (svg);
    --
    Create (pdf, Out_File, "qr_code_q_pdf.txt");
    Put_Line (pdf, PDF_Bar_Code (Code_QR_Quartile, (150.0, 120.0, 100.0, 100.0), hello_long));
    Close (pdf);
    --
    Create (pbm, Out_File, "qr_code_h.pbm");
    Put_Line (pbm, PBM_Bar_Code (Code_QR_High, 5, 5, hello_long));
    Close (pbm);
    --
    SIO.Create (png, SIO.Out_File, "qr_code_h.png");
    PNG_Bar_Code (Code_QR_High, 5, 5, hello_long, SIO.Stream (png).all);
    SIO.Close (png);
  end Demo_QR;
  --
  procedure Demo_Data_Matrix is
    use Bar_Codes, Bar_Codes_Media;
  begin
    Create (svg, Out_File, "dm_code.svg");
    SVG_Header;
    Put_Line (svg, SVG_Bar_Code (Code_DM_Square, 80.0, 80.0, "mm", hello_long));
    Close (svg);
    --
    Create (pbm, Out_File, "dm_code_rect.pbm");
    Put_Line (pbm, PBM_Bar_Code (Code_DM_Rectangular, 10, 10, hello_short));
    Close (pbm);
    --
    SIO.Create (png, SIO.Out_File, "dm_code_rect.png");
    PNG_Bar_Code (Code_DM_Rectangular, 10, 10, hello_short, SIO.Stream (png).all);
    SIO.Close (png);
  end Demo_Data_Matrix;
  --
begin
  Demo_Code_128;
  Demo_QR;
  Demo_Data_Matrix;
end Bar_Codes_Demo;
