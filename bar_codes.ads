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

--   Copyright (c) 2018 .. 2025 Gautier de Montmollin

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

package Bar_Codes is

  type Kind_Of_Code is
     --
     --  Code 128 is a 1D bar code that can encode the first 128 ASCII characters.
     --  Standard: ISO/IEC 15417:2007.
     --
    (Code_128,
     --
     --  MSI 1D bar codes.
     --
     Code_MSI,
     --
     --  UPC-A / EAN-13 are 1D bar codes used on labels of retail products.
     --
     Code_UPCA,
     Code_EAN13,
     --
     --  Data Matrix is a 2D bar code popular for marking small items.
     --  Standard: ISO/IEC 16022:2006
     --
     Code_DM_Rectangular,
     Code_DM_Square,
     --
     --  QR (for "Quick Response") is a popular 2D bar code.
     --  Standard: ISO/IEC 18004:2015.
     --
     Code_QR_Low,        --  Level L (Low)       7% of codewords can be restored.
     Code_QR_Medium,     --  Level M (Medium)   15% of codewords can be restored.
     Code_QR_Quartile,   --  Level Q (Quartile) 25% of codewords can be restored.
     Code_QR_High);      --  Level H (High)     30% of codewords can be restored.

  --  Classify the bar codes by dimensions (1-dimensional or 2-dimensional):
  --
  subtype Code_1D is Kind_Of_Code range Kind_Of_Code'First .. Code_EAN13;
  subtype Code_2D is Kind_Of_Code range Code_DM_Rectangular .. Kind_Of_Code'Last;

  --  Classify the bar codes by family (Data Matrix, QR, ...):
  --
  subtype Code_DM is Kind_Of_Code range Code_DM_Rectangular .. Code_DM_Square;
  subtype Code_QR is Kind_Of_Code range Code_QR_Low .. Code_QR_High;
  subtype Code_UPCA_EAN13 is Kind_Of_Code range Code_UPCA .. Code_EAN13;

  function Code_2D_Square (kind : Kind_Of_Code) return Boolean is (kind in Code_DM_Square | Code_QR);

  type Real is digits 15;

  type Box is record left, bottom, width, height : Real; end record;

  ---------------------------------------------------------------
  --  Here is what you need to implement the bar code on       --
  --  any device. For an example, see the PDF, SVG or PBM      --
  --  implementations in the package `Bar_Codes_Media`.        --
  --                                                           --
  --  `Bar_Code` is the main type around bar code generation.  --
  --  The rendering of the bars is abstracted.                 --
  ---------------------------------------------------------------

  type Bar_Code is abstract tagged private;

  --  `Set_Bounding_Box` is meaningful only for a vector graphics implementation
  --  such as PDF or SVG (see those implementations in `Bar_Codes_Media` to see why).
  --
  procedure Set_Bounding_Box (bc : in out Bar_Code; bounding : Box);

  procedure Draw (bc : in out Bar_Code; kind : Kind_Of_Code; text : String);

  ---------------
  --  Modules  --
  ---------------

  --  A "module" is the thinnest bar (1D), or the smallest box (2D).
  --  The coordinates of a Module_Box are in "module" units.
  --  This is practical for raster graphics (typically on a screen) since the
  --  display can be done on a multiple of those units without rounding errors.
  --
  type Module_Box is record left, bottom, width, height : Natural; end record;

  --  The `Fitting` function will return the exact box, in terms of modules, needed
  --  to fit the bar code for a given text. Fitting.left = Fitting.bottom = 0.
  --  For 1D codes Fitting.height = 1.
  --  This function is helpful to calibrate a raster graphics bitmap.
  --
  function Fitting (kind : Kind_Of_Code; text : String) return Module_Box;

  function Get_Module_Width (bc : Bar_Code) return Real;
  function Get_Module_Height (bc : Bar_Code) return Real;

  --  Callback method for filling a black bar (on PDF, SVG, etc.).
  --  For raster graphics, the shape parameter can be used for pixel coordinates
  --  or possibly integer multiples of them. If multiples were not integers,
  --  the bar codes would be wrong on raster graphics.
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
  function Make_Printable (s : String) return String;

  ----------------------------------------------------------------
  --  Information about this package - e.g. for an "about" box  --
  ----------------------------------------------------------------

  title     : constant String := "Ada Bar Codes";
  version   : constant String := "006, preview 2";
  reference : constant String := "30-Nov-2025";
  web       : constant String := "http://ada-bar-codes.sf.net/";
  --  Hopefully the latest version is at that URL ^
  --
  --  There is a mirror too @ https://github.com/zertovitch/ada-bar-codes

private

  type Bar_Code is abstract tagged record
    bounding      : Box := (0.0, 0.0, 1.0, 1.0);
    module_width  : Real;
    module_height : Real;
  end record;

  --  Facilities for 2D bar codes

  type Grid is array (Natural range <>, Natural range <>) of Boolean;

  procedure Output_to_Media
    (bc            : in out Bar_Code'Class;
     border_size_x : in     Positive;
     border_size_y : in     Positive;
     module        : in     Grid);

  verbosity_level : constant Natural := 0;

  --  Controls diagnostic/debug output during all operations.
  --
  --    0: no output
  --    1: some output
  --  > 1: more output

end Bar_Codes;
