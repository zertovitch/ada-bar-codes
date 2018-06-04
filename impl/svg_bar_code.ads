with Bar_Codes;                         use Bar_Codes;

function SVG_Bar_Code (
  kind     : Kind_Of_Code;
  bounding : Box;           --  Box in which the bar code should fit
  unit     : String;        --  length unit, for instance "mm" for millimeter
  text     : String         --  Text to encode
)
return String;
