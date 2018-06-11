Ada Bar Codes
=============

The project Ada Bar Codes provides a package for generating
various types of bar codes on different output formats,
such as PDF, SVG or bitmaps.

The creation of a bar code is as simple as this
small procedure:


  with Bar_Codes; use Bar_Codes;
  with Ada.Text_IO; use Ada.Text_IO;

  procedure Small_demo is 
    svg : File_Type;
  begin 
    Create (svg, Out_File, "bar_code.svg");
    Put_Line (svg, "<?xml version=""1.0"" encoding=""UTF-8""?>");
    Put_Line (svg, SVG_Bar_Code (Code_128, 57.0, 23.0, "mm", "Hello"));
    Close (svg);
  end;

====

Full description in: ada_bar_codes.txt
