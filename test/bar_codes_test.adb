with Bar_Codes;                         use Bar_Codes;

with Ada.Text_IO;                       use Ada.Text_IO;

procedure Bar_Codes_Test is
  pbm : File_Type;
  procedure Test_128 is
    chunks : constant := 2;
    c : Character := ASCII.DEL;
    msg : String (1 .. 128 / chunks);
  begin
    for chunk in 1 .. chunks loop
      Create (pbm, Out_File, "test code 128" & Integer'Image (chunk) & ".pbm");
      for i in msg'Range loop
        msg (i) := c;
        if c > ASCII.NUL then
          c := Character'Pred (c);
        end if;
      end loop;
      Put_Line (pbm, PBM_Bar_Code (Code_128, 2, 50, msg));
      Close (pbm);
    end loop;
    Create (pbm, Out_File, "vn1.pbm");
    Put_Line (pbm, PBM_Bar_Code (Code_128, 2, 50, "0520"));
    Close (pbm);
    Create (pbm, Out_File, "vn2.pbm");
    Put_Line (pbm, PBM_Bar_Code (Code_128, 2, 50, "993512176004535560"));
    Close (pbm);
  end Test_128;
begin
  Test_128;
end Bar_Codes_Test;
