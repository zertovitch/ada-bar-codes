# Ada Bar Codes

The project Ada Bar Codes provides a package for generating
various types of bar codes, including 2D bar codes like the QR code,
on different output formats, such as PDF, SVG vector graphics or PNG bitmaps.

The creation of a bar code is as simple as this small procedure:

```Ada
    with Ada.Text_IO, Bar_Codes, Bar_Codes_Media;

    procedure Small_Demo is
      use Ada.Text_IO;
      svg : File_Type;
    begin
      Create (svg, Out_File, "qr_code.svg");
      Put_Line
        (svg,
         Bar_Codes_Media.SVG_Bar_Code
           (Bar_Codes.Code_QR_Low, (5.0, 5.0, 100.0, 100.0), "mm", "Hello"));
      Close (svg);
    end;
```

**Full description in: `ada_bar_codes.txt`
