private package Bar_Codes.Encode_DM is
  procedure Draw (bc : in out Bar_Code; text : String; dm_kind : Code_DM);
  function Fitting (text : String; dm_kind : Code_DM) return Module_Box;
end Bar_Codes.Encode_DM;
