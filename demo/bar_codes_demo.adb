--  Ada Bar Codes Demo (ABCD :-) )

with Ada.Text_IO;
with Bar_Codes.Impl;

procedure Bar_Codes_Demo is

  --  SVG files (bar_code_128.svg, qr_code.svg) can be viewed directly in a Web browser.
  --  PDF snippets need to be included into a PDF document.
  --       For instance, use Insert_Graphics_PDF_Code of PDF_Out, http://apdf.sf.net/
  --  PBM images demonstrate output as raster graphics. This could be another image
  --       format, or anything else involving pixels, like a screen, a printer, etc.
  use Ada.Text_IO;
  svg, pdf, pbm : File_Type;
  --
  procedure SVG_header is
  begin
    Put_Line (svg, "<?xml version=""1.0"" encoding=""UTF-8""?>");
    Put_Line (svg, "<!DOCTYPE svg");
    Put_Line (svg, "  PUBLIC '-//W3C//DTD SVG 1.1//EN'");
    Put_Line (svg, "  'http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd'>");
  end SVG_header;
  --
  hello : constant String :=
    "Hello from " & Bar_Codes.title & " ( " & Bar_Codes.web & " ) ! 1234567890";
  --
  procedure Demo_Code_128 is
    use Bar_Codes, Bar_Codes.Impl;
  begin
    Create (svg, Out_File, "bar_code_128.svg");
    SVG_header;
    Put_Line (svg, SVG_Bar_Code (Code_128, 57.0, 23.0, "mm", hello));
    Close (svg);
    --
    Create (pdf, Out_File, "bar_code_128_pdf.txt");
    Put_Line (pdf, PDF_Bar_Code (Code_128, (150.0, 320.0, 600.0, 50.0), hello));
    Close (pdf);
    --
    Create (pbm, Out_File, "bar_code_128.pbm");
    Put_Line (pbm, PBM_Bar_Code (Code_128, 2, 100, hello));
    Close (pbm);
  end Demo_Code_128;
  --
  procedure Demo_QR is
    use Bar_Codes, Bar_Codes.Impl;
  begin
    Create (svg, Out_File, "qr_code.svg");
    SVG_header;
    Put_Line (svg, SVG_Bar_Code (Code_QR_Low, 23.0, 23.0, "mm", hello));
    Close (svg);
    --
    Create (pdf, Out_File, "qr_code_pdf.txt");
    Put_Line (pdf, PDF_Bar_Code (Code_QR_Quartile, (150.0, 120.0, 100.0, 100.0), hello));
    Close (pdf);
    --
    Create (pbm, Out_File, "qr_code.pbm");
    Put_Line (pbm, PBM_Bar_Code (Code_QR_Low, 3, 3, hello));
    Close (pbm);
  end Demo_QR;
  --
  procedure Demo_DM is
    use Bar_Codes, Bar_Codes.Impl;
  begin
    Create (svg, Out_File, "dm_code.svg");
    SVG_header;
    Put_Line (svg, SVG_Bar_Code (Code_DM_Square, 23.0, 23.0, "mm", hello));
    Close (svg);
    --
    Create (pbm, Out_File, "dm_code.pbm");
    Put_Line (pbm, PBM_Bar_Code (Code_DM_Rectangular, 3, 3, hello));
    Close (pbm);
  end Demo_DM;
  --
begin
  Demo_Code_128;
  Demo_QR;
  Demo_DM;
end Bar_Codes_Demo;
