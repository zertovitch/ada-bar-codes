with Bar_Codes.Encode_Code_128;
with Bar_Codes.Encode_QR;

with Ada.Text_IO;

package body Bar_Codes is

  ------------------------
  --  Bar_Code methods  --
  ------------------------

  procedure Set_Bounding_Box (bc : in out Bar_Code; bounding : Box) is
  begin
    bc.bounding := bounding;
  end Set_Bounding_Box;

  procedure Draw (bc : in out Bar_Code; kind : Kind_Of_Code; text : String) is
  begin
    case kind is
      when Code_128 =>
        Bar_Codes.Encode_Code_128.Draw (bc, text);
      when Code_QR =>
        Bar_Codes.Encode_QR.Draw (bc, text, kind);
    end case;
  end Draw;

  function Fitting (kind : Kind_Of_Code; text : String) return Module_Box is
  begin
    case kind is
      when Code_128 =>
        return Bar_Codes.Encode_Code_128.Fitting (text);
      when Code_QR =>
        return Bar_Codes.Encode_QR.Fitting (text, kind);
    end case;
  end Fitting;

  ----------------------------------------------------
  -- Goodies that can be useful for implementations --
  ----------------------------------------------------

  package RIO is new Ada.Text_IO.Float_IO (Real);

  --  Compact real number image, taken from PDF_Out
  --
  function Img (x : Real; prec : Positive := Real'Digits) return String is
    s : String (1 .. 20 + prec);
    na : Natural := s'First;
    nb : Natural := s'Last;
    np : Natural := 0;
  begin
    RIO.Put (s, x, prec, 0);
    --  We will increase na and decrease nb
    --  to compact the string s (na .. nb);
    for i in s'Range loop
      case s (i) is
        when '.' => np := i; exit;    --   Find a decimal point
        when ' ' => na := i + 1;      -- * Trim spaces on left
        when others => null;
      end case;
    end loop;
    if np > 0 then
      while nb > np and then s (nb) = '0' loop
        nb := nb - 1;                 -- * Remove extra '0's after decimal point
      end loop;
      if nb = np then
        nb := nb - 1;                 -- * Remove '.' if it is at the end
      elsif s (na .. np - 1) = "-0" then
        na := na + 1;
        s (na) := '-';                -- * Reduce "-0.x" to "-.x"
      elsif s (na .. np - 1) = "0" then
        na := na + 1;                 -- * Reduce "0.x" to ".x"
      end if;
    end if;
    return s (na .. nb);
  end Img;

  function Make_Printable (s : String) return String is
    t : String := s;
  begin
    for i in s'Range loop
      case s (i) is
        when ' ' .. '~' => null;
        when others     => t (i) := '*';
      end case;
    end loop;
    return t;
  end Make_Printable;

end Bar_Codes;
