Ada Bar Codes
=============

The project Ada Bar Codes provides a package for generating
various types of bar codes on different output formats,
such as PDF, SVG or bitmaps.

The creation of a bar code is as simple as this small procedure:

  with Ada.Text_IO,
       Bar_Codes.Impl;

  procedure Small_Demo is
    use Ada.Text_IO;
    use Bar_Codes, Bar_Codes.Impl;
    svg : File_Type;
  begin
    Create (svg, Out_File, "bar_code.svg");
    Put_Line (svg, "<?xml version=""1.0"" encoding=""UTF-8""?>");
    Put_Line (svg, SVG_Bar_Code (Code_QR_Low, 25.0, 25.0, "mm", "Hello"));
    Close (svg);
  end;

====

Full description in: ada_bar_codes.txt
