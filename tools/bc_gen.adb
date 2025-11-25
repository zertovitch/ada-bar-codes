--
--  BC_Gen * Generate bar codes from the command line.
--

with Bar_Codes, Bar_Codes_Media;

with Ada.Characters.Handling,
     Ada.Command_Line,
     Ada.Streams.Stream_IO,
     Ada.Strings.Unbounded,
     Ada.Text_IO;

procedure BC_Gen is

  use Ada.Characters.Handling, Ada.Streams.Stream_IO, Ada.Strings.Unbounded, Ada.Text_IO;

  procedure Blurb is
  begin
    Put_Line (Current_Error, "BC_Gen * Generate bar codes from the command line");
    Put_Line (Current_Error, "Using " &
      Bar_Codes.title & ' ' & Bar_Codes.version & " dated " & Bar_Codes.reference);
    Put_Line (Current_Error, "URL: " & Bar_Codes.web);
    New_Line (Current_Error);
    Put_Line (Current_Error, "Syntax:");
    Put_Line (Current_Error, "bc_gen [options] text");
    New_Line (Current_Error);
    Put_Line (Current_Error, "Options:");
    New_Line (Current_Error);
    Put_Line (Current_Error, "   -qr    :  QR code (default)");
    Put_Line (Current_Error, "   -dm    :  Data Matrix code");
    Put_Line (Current_Error, "   -ean   :  EAN13 code");
    Put_Line (Current_Error, "   -upca  :  UPCA code");
    Put_Line (Current_Error, "   -msi   :  MSI code");
    Put_Line (Current_Error, "   -128   :  code 128");
    New_Line (Current_Error);
    Put_Line (Current_Error, "   -png   :  Output as PNG image (default)");
    Put_Line (Current_Error, "   -svg   :  Output as SVG image");
    New_Line (Current_Error);
    Put_Line (Current_Error, "   -ofile :  Output to <file>");
    New_Line (Current_Error);
    Put (Current_Error, "Press Return");
    Skip_Line;
    return;
  end Blurb;

  kind : Bar_Codes.Kind_Of_Code := Bar_Codes.Code_QR_High;

  type Format_Type is (PNG, SVG);

  format : Format_Type := PNG;

  text : Unbounded_String := Null_Unbounded_String;
  file_name : Unbounded_String := Null_Unbounded_String;

  function Final_File_Name return String is
  (if file_name = Null_Unbounded_String then
     (case format is
        when PNG => "output.png",
        when SVG => "output.svg")
   else
     To_String (file_name));

  package SIO renames Ada.Streams.Stream_IO;
  package TIO renames Ada.Text_IO;

  stm_out : SIO.File_Type;
  txt_out : TIO.File_Type;

  procedure SVG_Header is
  --  NB: the SVG file can be viewed without this header.
  begin
    Put_Line (txt_out, "<?xml version=""1.0"" encoding=""UTF-8""?>");
    Put_Line (txt_out, "<!DOCTYPE svg");
    Put_Line (txt_out, "  PUBLIC '-//W3C//DTD SVG 1.1//EN'");
    Put_Line (txt_out, "  'http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd'>");
  end SVG_Header;

  use Ada.Command_Line, Bar_Codes;

begin
  if Argument_Count = 0 then
    Blurb;
    return;
  end if;

  for i in 1 .. Argument_Count loop
    declare
      arg : constant String := Argument (i);
    begin
      if arg'Length >= 2 and then arg (arg'First) = '-' then
        declare
          opt : constant String := To_Lower (arg (arg'First + 1 .. arg'Last));
        begin
          if opt = "qr" then
            kind := Bar_Codes.Code_QR_High;
          elsif opt = "dm" then
            kind := Bar_Codes.Code_DM_Square;
          elsif opt = "ean" then
            kind := Bar_Codes.Code_EAN13;
          elsif opt = "upca" then
            kind := Bar_Codes.Code_UPCA;
          elsif opt = "msi" then
            kind := Bar_Codes.Code_MSI;
          elsif opt = "128" then
            kind := Bar_Codes.Code_128;
          elsif opt = "png" then
            format := PNG;
          elsif opt = "svg" then
            format := SVG;
          elsif opt (opt'First) = 'o' then
            file_name := To_Unbounded_String (opt (opt'First + 1 .. opt'Last));
          else
            Put_Line (Current_Error, "Unknown option: " & arg);
          end if;
        end;
      else
        text := To_Unbounded_String (arg);
      end if;
    end;
  end loop;

  if text = Null_Unbounded_String then
    Blurb;
  else
    case format is
      when PNG =>
        SIO.Create (stm_out, SIO.Out_File, Final_File_Name);
        Bar_Codes_Media.PNG_Bar_Code
          (kind,
           5,
           (if kind in Code_1D then 100 else 5),
           To_String (text),
           SIO.Stream (stm_out).all);
        SIO.Close (stm_out);
      when SVG =>
        TIO.Create (txt_out, TIO.Out_File, Final_File_Name);
        SVG_Header;
        TIO.Put_Line
          (txt_out,
           Bar_Codes_Media.SVG_Bar_Code
             (kind,
              (3.0,
               3.0,
               57.0,
               (if kind in Code_1D then 23.0 else 57.0)),
              "mm",
              To_String (text)));
        TIO.Close (txt_out);
    end case;
  end if;
end BC_Gen;
