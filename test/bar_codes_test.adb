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
    case kind is
      when Code_1D =>
        --  1D modules are as high as you wish.
        Put_Line (pbm, PBM_Bar_Code (kind, 2, 30, text));
      when Code_2D_Square =>
        --  Square 2D codes need square modules.
        Put_Line (pbm, PBM_Bar_Code (kind, 2, 2, text));
    end case;
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
  --
  procedure Test_QR is
    blabla : constant String :=
      "The Corporate Bullshit Generator " &
      " *** " &
      "Short URL (for bookmark and sharing): http://cbsg.sf.net " &
      " *** " &
      "A pre-integrated, non-deterministic and high-performance intellect " &
      "deepens mobility spaces. " &
      "Offshorings expediently generate our world-class and fast-paced brand image. " &
      "A segmentation influences the decision makers, while multi-divisional, " &
      "service-oriented, pipelines quickly streamline evolutions. " &
      "Above-average next steps incentivise the initiator; nevertheless " &
      "the enablers orchestrate the adjustments. A continual increase in " &
      "margins goes hand-in-hand with a measured gain in task efficiency." &
      " *** " &
      "The project Ada Bar Codes provides a package for generating " &
      "various types of bar codes on different output formats," &
      "such as PDF, SVG or bitmaps." &
      " *** " &
      "Zip-Ada is a programming library for dealing with the Zip compressed " &
      "archive file format. The full sources of Zip-Ada are in Ada, " &
      "compilable on every compiler and for every system. For more details, " &
      "read the files zipada.txt and zip.ads from the archive below." &
      " *** " &
      "GLOBE_3D stands for GL Object Based Engine for 3D." &
      "GL stands for Graphics Library, created by SGI. " &
      "SGI stands for Silicon Graphics, Inc. " &
      "Short description: GLOBE_3D is a free, open-source," &
      "real-time 3D Engine written in Ada, based on OpenGL.";
  begin
    for c in Code_QR loop
      Spit (c, "empty", "");
      Spit (c, "blabla 10",  blabla (1 .. 10));
      Spit (c, "blabla 100", blabla (1 .. 100));
      Spit (c, "blabla 500", blabla (1 .. 500));
      Spit (c, "blabla",     blabla);
    end loop;
  end Test_QR;
begin
  Test_128;
  Test_QR;
end Bar_Codes_Test;
