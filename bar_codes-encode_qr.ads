private package Bar_Codes.Encode_QR is
  procedure Draw (bc : in out Bar_Code; text : String; qr_kind : Code_QR);
  function Fitting (text : String; qr_kind : Code_QR) return Module_Box;
end Bar_Codes.Encode_QR;
