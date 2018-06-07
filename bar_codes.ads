-------------------------------------------------------------------------------------
--
--  BAR_CODES - A package for displaying bar codes (1D or 2D).
--                  Project name: Ada Bar Codes.
--
--  Pure Ada 95 code, 100% portable: OS-, CPU- and compiler- independent.
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
    --  Code 128 is a 1D bar code that can encode the 128
    --  first ASCII characters. Standard: ISO/IEC 15417:2007.
    Code_128
    --  Later: QR, ...
  );

  type Real is digits System.Max_Digits;

  type Box is record left, bottom, width, height : Real; end record;

  --------------------------------------------------------------
  --  Ready-to-use implementations of the bar code generator  --
  --------------------------------------------------------------

  --  The PDF_Bar_Code function produces a PDF (Portable Document Format) snippet
  --  to be included into a PDF document. For instance, use Insert_Graphics_PDF_Code
  --  of package PDF_Out (project Ada PDF Writer, http://apdf.sf.net/ ).

  function PDF_Bar_Code (
    kind     : Kind_Of_Code;
    bounding : Box;           --  Box in which the bar code should fit
    text     : String         --  Text to encode
  )
  return String;

  --  The SVG_Bar_Code function produces a SVG (Scalable Vector Graphics) object.

  function SVG_Bar_Code (
    kind     : Kind_Of_Code;
    bounding : Box;           --  Box in which the bar code should fit
    unit     : String;        --  Length unit, for instance "mm" for millimeter
    text     : String         --  Text to encode
  )
  return String;

  ------------------------------------------------------------
  --  Here is what you need to implement the bar code on    --
  --  another device than PDF or SVG.                       --
  --  Bar_Code: the main type around bar code generation.   --
  --  The rendering of the bars is abstracted.              --
  --  See functions SVG_Bar_Code or PDF_Bar_Code above for  --
  --  concrete implementations.                             --
  ------------------------------------------------------------

  type Bar_Code is abstract tagged private;

  procedure Draw (bc : in out Bar_Code; kind : Kind_Of_Code; bounding : Box; text : String);

  --  Callback method for filling a black bar (on PDF, SVG, etc.)
  procedure Filled_Rectangle (bc : Bar_Code; shape : Box) is abstract;

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
  version   : constant String := "001, preview 1";
  reference : constant String := "(2018)";
  web       : constant String := "http://ada-bar-codes.sf.net/";
  --  Hopefully the latest version is at that URL ^

private

  type Bar_Code is abstract tagged record
    bounding : Box;
  end record;

end Bar_Codes;
