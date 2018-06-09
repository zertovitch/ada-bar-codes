with Bar_Codes;                         use Bar_Codes;

with Ada.Text_IO;                       use Ada.Text_IO;

procedure Bar_Codes_Test is
  svg : File_Type;
  procedure Test_128 is
    chunks : constant := 4;
    c : Character := ASCII.DEL;
    msg : String (1 .. 128 / chunks);
  begin
    for chunk in 1 .. chunks loop
      Create (svg, Out_File, "test code 128" & Integer'Image (chunk) & ".svg");
      Put_Line (svg, "<?xml version=""1.0"" encoding=""UTF-8""?>");
      Put_Line (svg, "<!DOCTYPE svg");
      Put_Line (svg, "  PUBLIC '-//W3C//DTD SVG 1.1//EN'");
      Put_Line (svg, "  'http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd'>");
      for i in msg'Range loop
        msg (i) := c;
        if c > ASCII.NUL then
          c := Character'Pred (c);
        end if;
      end loop;
      Put_Line (svg, SVG_Bar_Code (Code_128, 57.0, 23.0, "mm", msg));
      Close (svg);
    end loop;
  end;
begin
  Test_128;
end Bar_Codes_Test;
