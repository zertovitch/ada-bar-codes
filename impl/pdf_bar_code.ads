with Bar_Codes;

function PDF_Bar_Code (
  kind     : Bar_Codes.Kind_Of_Code;
  bounding : Bar_Codes.Box;           --  Box in which the bar code should fit
  text     : String                   --  Text to encode
)
return String;
