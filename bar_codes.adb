package body Bar_Codes is

  ----------------------
  -- Set_Bounding_Box --
  ----------------------

  procedure Set_Bounding_Box (code : in out Bar_Code; bounding : Box) is
  begin
    code.bounding := bounding;
  end Set_Bounding_Box;

  ----------
  -- Draw --
  ----------

  procedure Draw (code : Bar_Code; kind : Kind_Of_Code; text : String) is
  begin
    case kind is
      when Code_128 =>
        null; -- !!
    end case;
  end Draw;

end Bar_Codes;
