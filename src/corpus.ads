-------------------------------------------------------------------------
--                           M L S E
--     M a r k o v  L o g i c  S e n t e n c e  E n c o d e r
--
--            Copyright 2012 Matteo Grella, Marco Nicola
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.
--
---------------------------------------------------------------------------

with Commons; use Commons;

package Corpus is

    function Ignored_Deprel(Deprel : String) return Boolean is
      (Deprel in "aux" | "auxpass" | "det"  | "complm"
       | "preconj" | "predet" | "punct" | "quantmod" | "expl" | "mark"); -- "num" | "number" | "parataxis" | "cop"

    function Ignored_Lemma(Lemma : String) return Boolean is
      (Lemma in "essere" | "sapere" |  "pensare" |  "dire" | "avere" | "fare" | "be" | "have" | "will" | "do");

    procedure Read_Articles
      (Directory_Name   : in     String;
       Articles         : in out Article_Sets.Set;
       Ignore_Deprel    : in      Boolean := False);

    procedure Print
      (Articles : in Article_Sets.Set);

    procedure Read_Sentences_From_Conll_File
      (File_Name      : in      String;
       Sentences      : in out  Sentence_Vector.Vector;
       Ignore_Deprel  : in      Boolean := False);

end Corpus;
