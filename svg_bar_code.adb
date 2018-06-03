with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;

function SVG_Bar_Code (
  kind     : Kind_Of_Code;
  bounding : Box;
  text     : String
)
return String
is
  svg_code : Unbounded_String;
begin
  return
    "<svg height=""" &
     "mm"" width=""" &
     "mm"" version=""1.1"" xmlns=""http://www.w3.org/2000/svg"">" & ASCII.LF &
    "    <!-- Automatically generated by " & Bar_Codes.title &
    " version " & Bar_Codes.version &
    ", " & Bar_Codes.reference &
    " @ " & Bar_Codes.web &
    " -->" & ASCII.LF &
    --  White rectangle as background
    "    <rect height=""100%"" width=""100%"" style=""fill:#FFFFFF""/>" & ASCII.LF &
    To_String (svg_code) &
    "</svg>";
end;
