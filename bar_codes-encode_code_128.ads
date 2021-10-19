private package Bar_Codes.Encode_Code_128 is
  procedure Draw (bc : in out Bar_Code; text : String);
  function Fitting (text : String) return Module_Box;
end Bar_Codes.Encode_Code_128;
