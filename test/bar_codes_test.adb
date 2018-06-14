with Bar_Codes;                         use Bar_Codes;

with Ada.Text_IO;                       use Ada.Text_IO;

procedure Bar_Codes_Test is
  --
  procedure Spit (kind : Kind_Of_Code; file_name, text : String) is
    pbm : File_Type;
  begin
    if file_name = "" then
      Create (pbm, Out_File, text & ".pbm");
    else
      Create (pbm, Out_File, file_name & ".pbm");
    end if;
    if kind in Code_1D then
      Put_Line (pbm, PBM_Bar_Code (kind, 2, 30, text));
    else
      Put_Line (pbm, PBM_Bar_Code (kind, 2, 2, text));
    end if;
    Close (pbm);
  end Spit;
  --
  procedure Test_128 is
    chunks : constant := 2;
    c : Character := ASCII.DEL;
    msg : String (1 .. 128 / chunks);
  begin
    for chunk in 1 .. chunks loop
      for i in msg'Range loop
        msg (i) := c;
        if c > ASCII.NUL then
          c := Character'Pred (c);
        end if;
      end loop;
      Spit (Code_128, "test code 128" & Integer'Image (chunk), msg);
    end loop;
    Spit (Code_128, "vn1", "0520");
    Spit (Code_128, "vn2", "993512176004535560");
    Spit (Code_128, "", "12345abc1234abc1234567a123bcdef12345");
  end Test_128;
begin
  Test_128;
end Bar_Codes_Test;
