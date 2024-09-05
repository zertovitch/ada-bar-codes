with Ada.Text_IO;

package body Bar_Codes is

  package Encode_Code_128 is
    procedure Draw (bc : in out Bar_Code; text : String);
    function Fitting (text : String) return Module_Box;
  end Encode_Code_128;

  package body Encode_Code_128 is separate;

  package Encode_DM is
    procedure Draw (bc : in out Bar_Code; text : String; dm_kind : Code_DM);
    function Fitting (text : String; dm_kind : Code_DM) return Module_Box;
  end Encode_DM;

  package body Encode_DM is separate;

  package Encode_QR is
    procedure Draw (bc : in out Bar_Code; text : String; qr_kind : Code_QR);
    function Fitting (text : String; qr_kind : Code_QR) return Module_Box;
  end Encode_QR;

  package body Encode_QR is separate;

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
      when Code_128 => Encode_Code_128.Draw (bc, text);
      when Code_DM  => Encode_DM.Draw       (bc, text, kind);
      when Code_QR  => Encode_QR.Draw       (bc, text, kind);
    end case;
  end Draw;

  function Fitting (kind : Kind_Of_Code; text : String) return Module_Box is
  (case kind is
     when Code_128 => Encode_Code_128.Fitting (text),
     when Code_DM  => Encode_DM.Fitting       (text, kind),
     when Code_QR  => Encode_QR.Fitting       (text, kind));

  function Get_Module_Width  (bc : Bar_Code) return Real is (bc.module_width);
  function Get_Module_Height (bc : Bar_Code) return Real is (bc.module_height);

  procedure Output_to_Media
    (bc            : in out Bar_Code'Class;
     border_size_x : in     Positive;
     border_size_y : in     Positive;
     module        : in     Grid)
  is
    done : Grid (0 .. border_size_y - 1, 0 .. border_size_x - 1) := (others => (others => False));
    size_x, size_y : Positive;
  begin
    --  For vector graphics only: we want to squeeze the full 2D code
    --  into the bounding box. A "module" is the smallest square.
    bc.module_width  := bc.bounding.width / Real (border_size_x);
    bc.module_height := bc.bounding.height / Real (border_size_y);
    --
    for y in done'Range (1) loop
      for x in done'Range (2) loop
        if module (y, x) and then not done (y, x) then
          --  We search for the largest "black" rectangle starting from
          --  the (y, x) point. On a vector graphics output, there are
          --  two advantages:
          --    - the output is much smaller (for SVG or PDF, the file
          --        is typically reduced to 1/4 of the "uncompressed" size)
          --    - many artefacts appearing between "black" modules are
          --        removed; it is appearent when you zoom a SVG file
          --        to the max on a Web browser.
          size_x := 1;
          size_y := 1;
          --  Try to extend the square to the right:
          for xh in x + 1 .. done'Last (2) loop
            exit when done (y, xh) or not module (y, xh);
            size_x := size_x + 1;
          end loop;
          --  Try to extend the rectangle vertically:
          Vertical_Extension :
          for yv in y + 1 .. done'Last (1) loop
            for xt in x .. x + size_x - 1 loop
              exit Vertical_Extension when done (yv, xt) or not module (yv, xt);
            end loop;
            size_y := size_y + 1;
          end loop Vertical_Extension;
          Filled_Rectangle (bc, (x, border_size_y - size_y - y, size_x, size_y));
          for yt in y .. y + size_y - 1 loop
            for xt in x .. x + size_x - 1 loop
              done (yt, xt) := True;
            end loop;
          end loop;
        end if;
      end loop;
    end loop;
  end Output_to_Media;

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
