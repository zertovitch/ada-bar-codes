-------------------------------------------------------------------------------------
--
--  BAR_CODES - A package for displaying bar codes (1D or 2D).
--                  Project name: Ada Bar Codes.
--
--  Pure Ada 2005 code, 100% portable: OS-, CPU- and compiler- independent.
--
--  Version / date / download info: see the version, reference, web strings
--    defined at the end of the public part of this package.

--  Legal licensing note:

--   Copyright (c) 2018 Gautier de Montmollin

--   Permission is hereby granted, free of charge, to any person obtaining a copy
--   of this software and associated documentation files (the "Software"), to deal
--   in the Software without restriction, including without limitation the rights
--   to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
--   copies of the Software, and to permit persons to whom the Software is
--   furnished to do so, subject to the following conditions:

--   The above copyright notice and this permission notice shall be included in
--   all copies or substantial portions of the Software.

--   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
--   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
--   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
--   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
--   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
--   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
--   THE SOFTWARE.

--  NB: this is the MIT License, as found 12-Sep-2007 on the site
--  http://www.opensource.org/licenses/mit-license.php

--  (*) All Trademarks mentioned are properties of their respective owners.
-------------------------------------------------------------------------------------

with System;

package Bar_Codes is

  type Kind_Of_Code is (
    --
    --  Code 128 is a 1D bar code that can encode the first 128 ASCII characters.
    --  Standard: ISO/IEC 15417:2007.
    --
    Code_128,
    --
    --  QR Code is a popular 2D bar code.
    --  Standard: ISO/IEC 18004:2015.
    --
    Code_QR_Low,        --  Level L (Low)       7% of codewords can be restored.
    Code_QR_Medium,     --  Level M (Medium)   15% of codewords can be restored.
    Code_QR_Quartile,   --  Level Q (Quartile) 25% of codewords can be restored.
    Code_QR_High        --  Level H (High)     30% of codewords can be restored.
  );

  subtype Code_1D is Kind_Of_Code range Code_128 .. Code_128;
  subtype Code_QR is Kind_Of_Code range Code_QR_Low  .. Code_QR_High;
  subtype Code_2D is Code_QR;
  subtype Code_2D_Square is Code_QR;

  type Real is digits System.Max_Digits;

  type Box is record left, bottom, width, height : Real; end record;

  -------------------------------------------------------------
  --  Here is what you need to implement the bar code on     --
  --  another device than PDF, SVG or PBM that are           --
  --  implemented in the package Bar_Codes.Impl .            --
  --                                                         --
  --  Bar_Code is the main type around bar code generation.  --
  --  The rendering of the bars is abstracted.               --
  -------------------------------------------------------------

  type Bar_Code is abstract tagged private;

  --  Set_Bounding_Box is useful only for vector graphics such as PDF or SVG (see those
  --  implementations to see why).
  --
  procedure Set_Bounding_Box (bc : in out Bar_Code; bounding : Box);

  procedure Draw (bc : in out Bar_Code; kind : Kind_Of_Code; text : String);

  --  A "module" is the thinnest bar (1D), or the smallest box (2D).
  --  The coordinates of a Module_Box are in "module" units.
  --  This is practical for raster graphics (typically on a screen) since the
  --  display can be done on a multiple of those units without rounding errors.
  type Module_Box is record left, bottom, width, height : Natural; end record;

  --  The Fitting function will return the exact box needed to fit the bar code
  --  for a given text. Fitting.left = Fitting.bottom = 0.
  --  For 1D codes Fitting.height = 1.
  --
  function Fitting (kind : Kind_Of_Code; text : String) return Module_Box;

  --  Callback method for filling a black bar (on PDF, SVG, etc.)
  --  For raster graphics, the shape variable can be used for pixel coordinates
  --  or eventually integer multiples of them. If multiples are not integral,
  --  the bar codes will be wrong.
  --
  procedure Filled_Rectangle (bc : Bar_Code; shape : Module_Box) is abstract;

  Cannot_Encode : exception;

  ------------------------------------------------------
  --  Goodies that can be useful for implementations  --
  ------------------------------------------------------

  --  Compact real number image
  function Img (x : Real; prec : Positive := Real'Digits) return String;

  --  Display a string (assumed 7-bit), with non-printable
  --  characters replaced by '*'.
  function Printable (s : String) return String;

  ----------------------------------------------------------------
  --  Information about this package - e.g. for an "about" box  --
  ----------------------------------------------------------------

  title     : constant String := "Ada Bar Codes";
  version   : constant String := "002 preview 1";
  reference : constant String := "> 18-Jun-2018";
  web       : constant String := "http://ada-bar-codes.sf.net/";
  --  Hopefully the latest version is at that URL ^

private

  type Bar_Code is abstract tagged record
    bounding      : Box := (0.0, 0.0, 1.0, 1.0);
    module_width  : Real;
    module_height : Real;
  end record;

  verbosity : constant Natural := 0;

  --  Controls diagnostic/debug output during all operations.
  --
  --    0: no output
  --    1: some output
  --  > 1: more output

end Bar_Codes;
