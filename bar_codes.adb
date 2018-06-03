with Bar_Codes.Draw_Code_128;

package body Bar_Codes is

  ----------------------
  -- Set_Bounding_Box --
  ----------------------

  procedure Set_Bounding_Box (bc : in out Bar_Code; bounding : Box) is
  begin
    bc.bounding := bounding;
  end Set_Bounding_Box;

  ----------
  -- Draw --
  ----------

  procedure Draw (bc : Bar_Code; kind : Kind_Of_Code; text : String) is
  begin
    case kind is
      when Code_128 =>
        Bar_Codes.Draw_Code_128 (bc, text);
    end case;
  end Draw;

end Bar_Codes;
