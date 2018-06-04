with Bar_Codes;

function SVG_Bar_Code (
  kind     : Bar_Codes.Kind_Of_Code;
  bounding : Bar_Codes.Box;           --  Box in which the bar code should fit
  unit     : String;                  --  length unit, for instance "mm" for millimeter
  text     : String                   --  Text to encode
)
return String;
