--
--  BC_Gen * Generate bar codes from the command line.
--

with Bar_Codes, Bar_Codes_Media;

with Ada.Calendar,
     Ada.Characters.Handling,
     Ada.Command_Line,
     Ada.Streams.Stream_IO,
     Ada.Text_IO;

with Interfaces;

procedure BC_Gen is

  use Ada.Characters.Handling, Ada.Streams.Stream_IO, Ada.Text_IO;

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

  use Ada.Command_Line;

  kind : Bar_Codes.Kind_Of_Code := Bar_Codes.Code_QR_High;

  type Format_Type is (PNG, SVG);

  format : Format_Type := PNG;

begin
  if Argument_Count = 0 then
    Blurb;
    return;
  end if;
  --  !!  Here: process arguments
end BC_Gen;
