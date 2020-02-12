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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;

with Commons; use Commons;
with Corpus; use Corpus;
with USP_Eval; use USP_Eval;

procedure Eval is

    package T_IO renames Ada.Text_IO;

    Questions_Directory, Model_Directory, Articles_Directory : Unbounded_String;

    Articles : Article_Sets.Set;

begin

    -----
    -- Arguments
    -----

    if Ada.Command_Line.Argument_Count /= 3 then
        T_IO.Put_Line(T_IO.Standard_Error, "  Usage: ./eval <Questions_Directory> <Model_Directory> <Articles_Directory>");
    end if;

    Questions_Directory := To_Unbounded_String(Ada.Command_Line.Argument(1));
    Model_Directory     := To_Unbounded_String(Ada.Command_Line.Argument(2));
    Articles_Directory  := To_Unbounded_String(Ada.Command_Line.Argument(3));

    -- Read articles
    T_IO.Put_Line(T_IO.Standard_Error, "Read articles...");
    Read_Articles
      (Directory_Name => To_String(Articles_Directory),
       Articles       => Articles,
       Ignore_Deprel  => False);

    -- Read questions
    T_IO.Put_Line(T_IO.Standard_Error, "Read questions...");
    Read_Questions(Questions_Directory);

    -- Read morph: map form to lemma
    T_IO.Put_Line(T_IO.Standard_Error, "Map form to lemma...");
    Map_Form_To_Lemma(Articles);

    -- Identify clust for question rels
    T_IO.Put_Line(T_IO.Standard_Error, "Identify clust for question rels...");
    Read_Clusters(Path_Join(Str(Model_Directory), "model.mln"));

    -- Read parts
    T_IO.Put_Line(T_IO.Standard_Error, "Read parts...");
    Read_Parts(Path_Join(Str(Model_Directory), "model.parse"));

    -- Preprocess arguments
    T_IO.Put_Line(T_IO.Standard_Error, "Preprocess arguments...");
    Preprocess_Arguments;

    -- Match
    T_IO.Put_Line(T_IO.Standard_Error, "Match...");
    Match(Articles);

    -- Print answers
    T_IO.Put_Line(T_IO.Standard_Error, "Print answers...");
    Print_Answers(Articles);

    T_IO.Put_Line(T_IO.Standard_Error, "The End.");

end Eval;

