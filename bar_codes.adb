with Bar_Codes.Draw_Code_128;

package body Bar_Codes is

  ----------
  -- Draw --
  ----------

  procedure Draw (bc : in out Bar_Code; kind : Kind_Of_Code; bounding : Box; text : String) is
  begin
    bc.bounding := bounding;
    case kind is
      when Code_128 =>
        Bar_Codes.Draw_Code_128 (bc, text);
    end case;
  end Draw;

end Bar_Codes;
