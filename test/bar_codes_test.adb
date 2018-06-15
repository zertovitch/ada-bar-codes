with Bar_Codes;                         use Bar_Codes;

with Ada.Characters.Handling;           use Ada.Characters.Handling;
with Ada.Numerics.Float_Random;         use Ada.Numerics.Float_Random;
with Ada.Text_IO;                       use Ada.Text_IO;

procedure Bar_Codes_Test is
  --
  procedure Spit (kind : Kind_Of_Code; file_name_part, text : String) is
    pbm : File_Type;
    prefix : constant String := "test " & To_Lower (Kind_Of_Code'Image (kind)) & ' ';
  begin
    if file_name_part = "" then
      Create (pbm, Out_File, prefix & text & ".pbm");
    else
      Create (pbm, Out_File, prefix & file_name_part & ".pbm");
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
    rnd : String (1 .. 50);
    gen : Generator;
    n : Positive;
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
    Reset (gen, 1);
    for iter in 1 .. 9 loop
      for i in rnd'Range loop
        rnd (i) := Character'Val (32 + Integer (Random (gen) * 95.0));
      end loop;
      --  Put a few non-printable characters...
      for i in rnd'Range loop
        if Random (gen) < 0.1 then
          rnd (i) := ASCII.ESC;
        end if;
      end loop;
      --  Put a few sequences of digits...
      for i in rnd'Range loop
        if Random (gen) < 0.08 then
          n := 1 + Integer (Random (gen) * 6.0);
          for j in i .. Integer'Min (rnd'Last, i + n) loop
            rnd (j) := Character'Val (Character'Pos ('0') + Integer (Random (gen) * 9.0));
          end loop;
        end if;
      end loop;
      Spit (Code_128, "rnd" & Integer'Image (iter), rnd);
      --  Digits only (must be all with subcode C):
      for i in rnd'Range loop
        rnd (i) := Character'Val (Character'Pos ('0') + Integer (Random (gen) * 9.0));
      end loop;
      Spit (Code_128, "rnd digits" & Integer'Image (iter), rnd);
    end loop;
  end Test_128;
begin
  Test_128;
end Bar_Codes_Test;
