--  Ada Bar Codes Demo (ABCD :-) )

with Bar_Codes;                         use Bar_Codes;

with Ada.Text_IO;                       use Ada.Text_IO;

procedure Bar_Codes_Demo is
  svg, pdf : File_Type;
  hello : constant String := "Hello from " & Bar_Codes.title & " ( " & Bar_Codes.web & " ) !";
begin
  --
  --  SVG demo. The resulting file (bar_code.svg) can be viewed directly in a Web browser.
  --
  Create (svg, Out_File, "bar_code.svg");
  Put_Line (svg, "<?xml version=""1.0"" encoding=""UTF-8""?>");
  Put_Line (svg, "<!DOCTYPE svg");
  Put_Line (svg, "  PUBLIC '-//W3C//DTD SVG 1.1//EN'");
  Put_Line (svg, "  'http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd'>");
  Put_Line (svg, SVG_Bar_Code (Code_128, (0.0, 0.0, 57.0, 23.0), "mm", hello));
  Close (svg);
  --
  --  PDF snippet to be included into a PDF document.
  --  For instance, use Insert_Graphics_PDF_Code of PDF_Out, http://apdf.sf.net/
  --
  Create (pdf, Out_File, "bar_code_pdf.txt");
  Put_Line (pdf, PDF_Bar_Code (Code_128, (100.0, 200.0, 600.0, 200.0), hello));
  Close (pdf);
end Bar_Codes_Demo;
