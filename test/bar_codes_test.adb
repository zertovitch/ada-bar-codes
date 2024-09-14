with Ada.Characters.Handling,
     Ada.Numerics.Float_Random,
     Ada.Streams.Stream_IO;

with Bar_Codes, Bar_Codes_Media;

procedure Bar_Codes_Test is

  procedure Spit (kind : Bar_Codes.Kind_Of_Code; file_name_part, text : String) is
    use Bar_Codes, Bar_Codes_Media;
    use Ada.Characters.Handling, Ada.Streams.Stream_IO;
    png : File_Type;
    prefix : constant String := "test " & To_Lower (kind'Image) & ' ';
  begin
    if file_name_part = "" then
      Create (png, Out_File, prefix & text & ".png");
    else
      Create (png, Out_File, prefix & file_name_part & ".png");
    end if;
    if Code_2D_Square (kind) then
      --  Square 2D codes need square modules.
      PNG_Bar_Code (kind, 2, 2, text, Stream (png).all);
    else
      case kind is
        when Code_1D =>
          --  1D modules are as high as you wish.
          PNG_Bar_Code (kind, 2, 30, text, Stream (png).all);
        when Code_DM_Rectangular =>
          PNG_Bar_Code (kind, 2, 2, text, Stream (png).all);
        when others =>
          pragma Assert (Code_2D_Square (kind));
      end case;
    end if;
    Close (png);
  end Spit;

  procedure Test_128 is
    use Bar_Codes;
    use Ada.Numerics.Float_Random;
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
      Spit (Code_128, chunk'Image, msg);
    end loop;
    Spit (Code_128, "vn1", "0520");
    Spit (Code_128, "vn2", "993512176004535560");
    Spit (Code_128,    "", "12345abc1234abc1234567a123bcdef12345");
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
      Spit (Code_128, "rnd" & iter'Image, rnd);
      --  Digits only (must be all with subcode C):
      for i in rnd'Range loop
        rnd (i) := Character'Val (Character'Pos ('0') + Integer (Random (gen) * 9.0));
      end loop;
      Spit (Code_128, "rnd digits" & iter'Image, rnd);
    end loop;
  end Test_128;

  procedure Test_MSI is
  begin
    Spit (Bar_Codes.Code_MSI, "", "1234567890");
    Spit (Bar_Codes.Code_MSI, "", "1234576890");
    Spit (Bar_Codes.Code_MSI, "", "12345678901");
    Spit (Bar_Codes.Code_MSI, "", "998877665544332211");
    Spit (Bar_Codes.Code_MSI, "", "97531");
    Spit (Bar_Codes.Code_MSI, "", "24680");
  end Test_MSI;

  procedure Test_EAN13 is
  begin
    for initial_digit in Character range '0' .. '9' loop
      Spit (Bar_Codes.Code_EAN13, (1 => initial_digit), initial_digit & "12345678901");
    end loop;
  end Test_EAN13;

  procedure Test_UPCA is
  begin
    Spit (Bar_Codes.Code_UPCA, "", "12345678901");
  end Test_UPCA;

  procedure Test_2D is
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
    for c in Bar_Codes.Code_2D loop
      Spit (c, "blabla 0001", blabla (1 .. 0001));
      Spit (c, "blabla 0010", blabla (1 .. 0010));
      Spit (c, "blabla 0035", blabla (1 .. 0035));
      Spit (c, "blabla 0100", blabla (1 .. 0100));
      Spit (c, "blabla 0250", blabla (1 .. 0250));
      Spit (c, "blabla 0500", blabla (1 .. 0500));
      Spit (c, "blabla full", blabla);
    end loop;
  end Test_2D;
begin
  Test_128;
  Test_MSI;
  Test_EAN13;
  Test_UPCA;
  Test_2D;
end Bar_Codes_Test;
