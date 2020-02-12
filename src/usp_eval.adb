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

with Ada.Text_IO;
with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants;
with Utils;

package body USP_Eval is

    package T_IO renames Ada.Text_IO;

    ------------------
    function To_String
    ------------------
      (Question : in Question_Type) return String is
    begin
        if Question.Deprel = "nsubj" then
            return "What does " & Str(Question.Argument) & " " & Str(Question.Relation) & "?";
        elsif Question.Deprel = "dobj" then
            return "What " & Str(Question.Relation) & "s " & Str(Question.Argument) & "?";
        else
            return Str(Question.Relation) & " ::: " & Str(Question.Deprel) & " ::: " & Str(Question.Argument);
        end if;
    end To_String;

    ------------------------
    procedure Read_Questions
    ------------------------
      (Questions_Directory : in Unbounded_String) is

        What_Does_Length : constant Positive := 10; -- Length of "What does "
    begin

        -----
        -- Question SBJ
        -----

        declare
            File_Name : constant String := Path_Join(Str(Questions_Directory), "question.sbj.txt");
            File      : Ada.Text_IO.File_Type;
        begin
            T_IO.Open(File, T_IO.In_File, File_Name);
            while not T_IO.End_Of_File(File) loop
                declare
                    Line : constant String := Str(Trim(UStr(T_IO.Get_Line(File)), Ada.Strings.Both));
                begin
                    if Line'Length > 0 then
                        declare
                            Space_Index : constant Natural := Ada.Strings.Fixed.Index(Line, " ", Ada.Strings.Backward);

                            Relation    : constant String  := Line(Space_Index + 1 .. Line'Last - 1);
                            Argument    : constant String  := Ada.Strings.Fixed.Translate(Str(Trim(UStr(Line(What_Does_Length + 1 .. Space_Index)), Ada.Strings.Both)), Ada.Strings.Maps.Constants.Lower_Case_Map);
                            -- FIXME: To_Lower_Case estremamente rozzo! Accertarsi di farlo in UTF-8
                        begin

                            -- Update: Relation_To_Question_Vector_Maps
                            declare
                                Question    : Question_Type;

                                Cursor      : Relation_To_Question_Vector_Maps.Cursor
                                  := Relation_To_Question_Vector_Map.Find(UStr(Relation));
                            begin
                                if not Relation_To_Question_Vector_Maps.Has_Element(Cursor) then
                                    Insert
                                      (Map      => Relation_To_Question_Vector_Map,
                                       Key      => UStr(Relation),
                                       Position => Cursor);
                                end if;

                                Question.Relation := UStr(Relation);
                                Question.Argument := UStr(Argument);
                                Question.Deprel   := UStr("nsubj");

                                Add_To_Vector(Relation_To_Question_Vector_Map, Cursor, Question);
                            end;

                            -- Update: Relation_To_Question_Vector_Maps
                            declare
                                Tokenizer               : Utils.String_Tokenizer_Type;
                                Token_First, Token_Last : Natural;
                            begin
                                Tokenizer.Tokenizer_Start(Argument);
                                while Tokenizer.Tokenizer_Has_Next(Argument) loop
                                    Tokenizer.Tokenizer_Next
                                      (Str       => Argument,
                                       Separator => " ",
                                       First     => Token_First,
                                       Last      => Token_Last);

                                    Insert(Forms_Set, UStr(Argument(Token_First .. Token_Last)));
                                end loop;
                            end;
                            Insert(Forms_Set, UStr(Relation));
                        end;
                    end if;
                end;
            end loop;
            T_IO.Close(File);
        end;

        -----
        -- Question OBJ
        -----

        declare
            File_Name : constant String := Path_Join(Str(Questions_Directory), "question.obj.txt");
            File      : Ada.Text_IO.File_Type;
        begin
            T_IO.Open(File, T_IO.In_File, File_Name);
            while not T_IO.End_Of_File(File) loop
                declare
                    Line : constant String := Str(Trim(UStr(T_IO.Get_Line(File)), Ada.Strings.Both));
                begin
                    if Line'Length > 0 then
                        declare
                            Space_1 : constant Natural := Ada.Strings.Fixed.Index(Line, " ");
                            Line_2  : constant String  := Str(Trim(Ustr(Line(Space_1 + 1 .. Line'Last)), Ada.Strings.Both));

                            Space_2 : constant Natural := Ada.Strings.Fixed.Index(Line_2, " ");

                            Argument : constant String := Ada.Strings.Fixed.Translate(Str(Trim(UStr(Line_2(Space_2 + 1 .. Line_2'Last - 1)), Ada.Strings.Both)), Ada.Strings.Maps.Constants.Lower_Case_Map); -- remove .
                            Relation : constant String := Remove_Third_Person(Line_2(Line_2'First .. Space_2 - 1));
                            -- FIXME: To_Lower_Case estremamente rozzo! Accertarsi di farlo in UTF-8
                        begin

                            -- Update: Relation_To_Question_Vector_Maps
                            declare
                                Question    : Question_Type;

                                Cursor      : Relation_To_Question_Vector_Maps.Cursor
                                  := Relation_To_Question_Vector_Map.Find(UStr(Relation));
                            begin
                                if not Relation_To_Question_Vector_Maps.Has_Element(Cursor) then
                                    Insert
                                      (Map      => Relation_To_Question_Vector_Map,
                                       Key      => UStr(Relation),
                                       Position => Cursor);
                                end if;

                                Question.Relation := UStr(Relation);
                                Question.Argument := UStr(Argument);
                                Question.Deprel   := UStr("dobj");

                                Add_To_Vector(Relation_To_Question_Vector_Map, Cursor, Question);
                            end;

                            -- Update: Relation_To_Question_Vector_Maps
                            declare
                                Tokenizer               : Utils.String_Tokenizer_Type;
                                Token_First, Token_Last : Natural;
                            begin
                                Tokenizer.Tokenizer_Start(Argument);
                                while Tokenizer.Tokenizer_Has_Next(Argument) loop
                                    Tokenizer.Tokenizer_Next
                                      (Str       => Argument,
                                       Separator => " ",
                                       First     => Token_First,
                                       Last      => Token_Last);

                                    Insert(Forms_Set, UStr(Argument(Token_First .. Token_Last)));
                                end loop;
                            end;
                            Insert(Forms_Set, UStr(Relation));
                        end;
                    end if;
                end;
            end loop;
            T_IO.Close(File);
        end;
    end Read_Questions;

    ---------------------------
    procedure Map_Form_To_Lemma
    ---------------------------
      (Articles : in Article_Sets.Set) is
    begin
        for Article of Articles loop
            for Sentence of Article.Sentences loop
                for Word of Sentence loop

                    if Word.ID /= 0 and then Forms_Set.Contains(Word.Form) then
                        declare
                            Cursor : Form_To_Lemmas_Maps.Cursor
                              := Form_To_Lemmas_Map.Find(Word.Form);
                        begin

                            if not Form_To_Lemmas_Maps.Has_Element(Cursor) then
                                Insert
                                  (Map      => Form_To_Lemmas_Map,
                                   Key      => Word.Form,
                                   Position => Cursor);
                            end if;

                            Add_To_Set(Form_To_Lemmas_Map, Cursor, Word.Lemma);

                            Insert(Lemmas_Set, Word.Lemma);
                        end;
                    end if;

                end loop;
            end loop;
        end loop;

    end Map_Form_To_Lemma;

    -----------------------
    procedure Read_Clusters
    -----------------------
      (MLN_Filename : in String) is

        File      : Ada.Text_IO.File_Type;

        Cur_Map_Cursor : Cluster_Index_To_Deprel_To_Argument_Cluster_Index_Maps.Cursor
          := Cluster_Index_To_Deprel_To_Argument_Cluster_Index_Maps.No_Element;
    begin

        T_IO.Open(File, T_IO.In_File, MLN_Filename);

        while not T_IO.End_Of_File(File) loop
            declare
                Line : constant String := T_IO.Get_Line(File);
            begin
                if not Is_Digit(Line(Line'First)) then

                    -- aci/argnum
                    declare
                        First_HT                   : Natural renames Ada.Strings.Fixed.Index(Line, ASCII.HT&"");
                        Second_HT                  : Natural renames Ada.Strings.Fixed.Index(Line, ASCII.HT&"", First_HT + 1);
                        Argument_Cluster_Index     : constant Argument_Cluster_Index_Type
                          := Argument_Cluster_Index_Type'Value(Line(First_HT + 1 .. Second_HT - 1));

                        ATI_Full_Line                   : constant String := T_IO.Get_Line(File);
                        ATI_Line                        : String renames ATI_Full_Line(ATI_Full_Line'First + 2 .. ATI_Full_Line'Last); -- Remove '\t\t'
                        ChdCl_Line                      : constant String := T_IO.Get_Line(File);
                        pragma Unreferenced(ChdCl_Line); -- Skipped

                        Tokenizer                  : Utils.String_Tokenizer_Type;
                        Token_First, Token_Last    : Natural;
                    begin
                        Tokenizer.Tokenizer_Start(ATI_Line);
                        while Tokenizer.Tokenizer_Has_Next(ATI_Line) loop
                            Tokenizer.Tokenizer_Next
                              (Str       => ATI_Line,
                               Separator => ASCII.HT&"",
                               First     => Token_First,
                               Last      => Token_Last);
                            declare
                                Token : String renames ATI_Line(Token_First .. Token_Last);

                                First_S  : Natural renames Ada.Strings.Fixed.Index(Token, ":");
                                Second_S : Natural renames Ada.Strings.Fixed.Index(Token, ":", First_S + 1);
                                Deprel   : String  renames Token(First_S + 1 .. Second_S - 1);
                            begin
                                -- TODO: essendoci duplicati, sembrerebbe greedy anche questo!?
                                Insert_Replace_To_Map
                                  (Map              => Cluster_Index_To_Deprel_To_Argument_Cluster_Index_Map,
                                   Map_Cursor       => Cur_Map_Cursor,
                                   Sub_Map_Key      => UStr(Deprel),
                                   Sub_Map_New_Item => Argument_Cluster_Index);
                            end;
                        end loop;
                    end;

                else -- Is_Digit(Line(Line'First))
                    declare
                        Tab_Index      : Natural renames Ada.Strings.Fixed.Index(Line, ASCII.HT&"");
                        Cluster_Index  : constant Cluster_Index_Type := Cluster_Index_Type'Value(Line(Line'First .. Tab_Index - 1));
                        Next_Index     : Natural := Ada.Strings.Fixed.Index(Line, "(");
                    begin
                        while Next_Index >= Line'First loop
                            declare
                                Mid_Index  : constant Natural := Ada.Strings.Fixed.Index(Line, ":", Next_Index);
                                Last_Index : constant Natural := Ada.Strings.Fixed.Index(Line, "):", Mid_Index);
                                POS        : String renames Line(Next_Index + 1 .. Mid_Index - 1);
                                Rel_Type   : String renames Line(Mid_Index + 1  .. Last_Index - 1);
                            begin
                                --  process multiple piece
                                Process_Rel_Type(Cluster_Index, UStr(POS), UStr(Rel_Type));

                                Next_Index := Ada.Strings.Fixed.Index(Line, "(", Last_Index);
                            end;
                        end loop;

                        -- New
                        Insert(Cluster_Index_To_Deprel_To_Argument_Cluster_Index_Map, Cluster_Index, Cur_Map_Cursor);
                    end;
                end if;
            end;
        end loop;

        T_IO.Close(File);

    end Read_Clusters;

    --------------------------
    procedure Process_Rel_Type
    --------------------------
      (Cluster_Index : in Cluster_Index_Type;
       POS           : in Unbounded_String;
       Rel_Type      : in Unbounded_String) is

    begin
        -- 19508	[(N:b (nn (N:nf-kappa))):1466,	(N:b (dep (N:nf-kappa))):2]

        -- rel only
        if POS = "V" and then Relation_To_Question_Vector_Map.Contains(Rel_Type) then
            Rel_Type_To_Cluster_Index_Map.Insert(Rel_Type, Cluster_Index);
        end if;

        -- arbitrary str
        if Lemmas_Set.Contains(Rel_Type) then
            declare
                Cursor : Lemma_To_Cluster_Index_Set_Maps.Cursor
                  := Lemma_To_Cluster_Index_Set_Map.Find(Rel_Type);
            begin
                if not Lemma_To_Cluster_Index_Set_Maps.Has_Element(Cursor) then
                    Insert
                      (Map      => Lemma_To_Cluster_Index_Set_Map,
                       Key      => Rel_Type,
                       Position => Cursor);
                end if;

                Add_To_Set(Lemma_To_Cluster_Index_Set_Map, Cursor, Cluster_Index);
            end;

        else
            -- probably multiple?
            declare
                Space_Index : constant Natural := Index(Rel_Type, " ");
            begin
                if Space_Index >= 1 then
                    declare
                        Open_Par_Index  : constant Natural := Index(Rel_Type, "(", Space_Index);
                        Sep_Index       : constant Natural := Index(Rel_Type, ":", Open_Par_Index);
                        Close_Par_Index : constant Natural := Index(Rel_Type, ")", Sep_Index);

                        Gov_Dep_Lemma_Link : constant Gov_Dep_Lemma_Link_Type
                          := (Governor_Lemma  => Unbounded_Slice(Rel_Type, 1, Space_Index - 1),
                              Dependent_Lemma => Unbounded_Slice(Rel_Type, Sep_Index + 1, Close_Par_Index - 1));
                    begin
                        if Lemmas_Set.Contains(Gov_Dep_Lemma_Link.Governor_Lemma)
                          and then Lemmas_Set.Contains(Gov_Dep_Lemma_Link.Dependent_Lemma) then
                            -- TODO: essendoci duplicati, sembrerebbe greedy anche questo!?
                            Insert_Replace
                              (Map      => Gov_Dep_Lemma_Link_To_Cluster_Index_Map,
                               Key      => Gov_Dep_Lemma_Link,
                               New_Item => Cluster_Index);
                        end if;
                    end;
                end if;
            end;
        end if;

    end Process_Rel_Type;

    --------------------
    procedure Read_Parts
    --------------------
      (Parse_Filename : in String) is

        File : Ada.Text_IO.File_Type;
    begin
        T_IO.Open(File, T_IO.In_File, Parse_Filename);

        while not T_IO.End_Of_File(File) loop
            declare
                Line_1      : constant String := T_IO.Get_Line(File);
                Line_2_Full : constant String := T_IO.Get_Line(File);
                Line_2      : String renames Line_2_Full(Line_2_Full'First + 1 .. Line_2_Full'Last); -- Skip "\t"
                Line_3_Full : constant String := T_IO.Get_Line(File);
                Line_3      : String renames Line_3_Full(Line_3_Full'First + 1 .. Line_3_Full'Last); -- Skip "\t"
                Line_4_Full : constant String := T_IO.Get_Line(File);
                Line_4      : String renames Line_4_Full(Line_4_Full'First + 1 .. Line_4_Full'Last); -- Skip "\t"

                -- id/str
                Tab_1            : constant Natural := Ada.Strings.Fixed.Index(Line_1, ASCII.HT&"");
                Atom_ID          : constant Atom_ID_Type := From_String(Line_1(Line_1'First .. Tab_1 - 1));
                Atom_Tree_Str    : String renames Line_1(Tab_1 + 1 .. Line_1'Last);

                -- clustIdx/clust
                Tab_2       : constant Natural := Ada.Strings.Fixed.Index(Line_2, ASCII.HT&"");
                Cluster_Index : constant Cluster_Index_Type := Cluster_Index_Type'Value(Line_2(Line_2'First .. Tab_2 - 1));

            begin

                declare
                    Cursor : Cluster_Index_To_Atom_ID_Set_Maps.Cursor
                      := Cluster_Index_To_Atom_ID_Set_Map.Find(Cluster_Index);
                begin
                    if not Cluster_Index_To_Atom_ID_Set_Maps.Has_Element(Cursor) then
                        Insert
                          (Map      => Cluster_Index_To_Atom_ID_Set_Map,
                           Key      => Cluster_Index,
                           Position => Cursor);
                    end if;
                    Add_To_Set(Cluster_Index_To_Atom_ID_Set_Map, Cursor, Atom_ID);
                end;

                Atom_ID_To_Cluster_Index_And_Atom_Tree_Str_Link_Map.Insert
                  (Atom_ID, (Cluster_Index => Cluster_Index,
                             Atom_Tree_Str => UStr(Atom_Tree_Str)));

                if Line_3'Length > 0 then
                    -- parid/clust
                    declare
                        Governor_Atom_ID : constant Atom_ID_Type
                          := From_String(Line_3(Line_3'First .. Ada.Strings.Fixed.Index(Line_3, ASCII.HT&"") - 1));

                        Argument_Cluster_Index : constant Argument_Cluster_Index_Type
                          := Argument_Cluster_Index_Type'Value(Line_4(Line_4'First .. Ada.Strings.Fixed.Index(Line_4, ASCII.HT&"") - 1));

                        Deprel           : constant String
                          := Line_4(Ada.Strings.Fixed.Index(Line_4, ASCII.HT&"", Ada.Strings.Backward) + 1 .. Line_4'Last);

                        procedure Update_1(K : in Atom_ID_Type; Argument_Cluster_Index_To_Atom_ID_Set_Map : in out Argument_Cluster_Index_To_Atom_ID_Set_Maps.Map) is
                            pragma Unreferenced (K);
                            Cursor_2         : Argument_Cluster_Index_To_Atom_ID_Set_Maps.Cursor
                              := Argument_Cluster_Index_To_Atom_ID_Set_Map.Find(Argument_Cluster_Index);
                        begin
                            if not Argument_Cluster_Index_To_Atom_ID_Set_Maps.Has_Element(Cursor_2) then
                                Insert
                                  (Map      => Argument_Cluster_Index_To_Atom_ID_Set_Map,
                                   Key      => Argument_Cluster_Index,
                                   Position => Cursor_2);
                            end if;

                            Add_To_Set(Argument_Cluster_Index_To_Atom_ID_Set_Map, Cursor_2, Atom_ID);
                        end Update_1;

                        Cursor_1         : Atom_ID_To_Argument_Cluster_Index_To_Atom_ID_Set_Map_Maps.Cursor
                          := Atom_ID_To_Argument_Cluster_Index_To_Atom_ID_Set_Map_Map.Find(Governor_Atom_ID);
                    begin

                        Atom_ID_To_Deprel_Map.Insert(Atom_ID, UStr(Deprel));

                        if not Atom_ID_To_Argument_Cluster_Index_To_Atom_ID_Set_Map_Maps.Has_Element(Cursor_1) then
                            Insert
                              (Map      => Atom_ID_To_Argument_Cluster_Index_To_Atom_ID_Set_Map_Map,
                               Key      => Governor_Atom_ID,
                               Position => Cursor_1);
                        end if;

                        Atom_ID_To_Argument_Cluster_Index_To_Atom_ID_Set_Map_Map.Update_Element(Cursor_1, Update_1'Access);
                    end;
                end if;
            end;
        end loop;

        T_IO.Close(File);
    end Read_Parts;

    ---------------------------------
    procedure Preprocess_Arguments is
    ---------------------------------

        Relation_To_Question_Cursor : Relation_To_Question_Vector_Maps.Cursor
          := Relation_To_Question_Vector_Map.First;
    begin
        while Relation_To_Question_Vector_Maps.Has_Element(Relation_To_Question_Cursor) loop
            declare
                Question_Vector   : Question_Vectors.Vector;
                Ignored_Questions : Question_Sets.Set; -- ignore due to missing form
            begin

                -- TODO: Copia forse inefficiente.
                Question_Vector := Relation_To_Question_Vector_Maps.Element(Relation_To_Question_Cursor);
                for Question of Question_Vector loop
                    if not Argument_To_Str_Cluster_Indexes_Double_Vector_Map.Contains(Question.Argument) then
                        declare
                            Argument   : constant String := Str(Question.Argument);

                            Is_Ignored : Boolean := False;

                            Str_Cluster_Indexes_Vector        : Str_Cluster_Indexes_Vectors.Vector;
                            Str_Cluster_Indexes_Double_Vector : Str_Cluster_Indexes_Double_Vectors.Vector;

                            Tokenizer  : Utils.String_Tokenizer_Type;
                            Tok_First, Tok_Last : Natural;
                            Form_Tokens : UString_Vectors.Vector;
                        begin

                            -- Find Lemmas
                            Tokenizer.Tokenizer_Start(Argument);
                            while Tokenizer.Tokenizer_Has_Next(Argument) loop
                                Tokenizer.Tokenizer_Next(Argument, " ", Tok_First, Tok_Last);
                                declare
                                    Form                  : constant Unbounded_String := UStr(Argument(Tok_First .. Tok_Last));
                                    Cluster_Index_Set     : Cluster_Index_Sets.Set;
                                    Form_To_Lemmas_Cursor : Form_To_Lemmas_Maps.Cursor;
                                    Lemma_Set             : Lemmas_Sets.Set;
                                    Str_Cluster_Indexes   : Unbounded_String;
                                begin
                                    Form_Tokens.Append(Form);

                                    -- TO-DO: match rel
                                    if Str(Form) not in "the" | "of" | "in" then

                                        Form_To_Lemmas_Cursor := Form_To_Lemmas_Map.Find(Form);
                                        if not Form_To_Lemmas_Maps.Has_Element(Form_To_Lemmas_Cursor) then
                                            Is_Ignored := True;
                                            exit; -- break
                                        end if;
                                        -- TODO: copia forse inefficiente
                                        Lemma_Set := Form_To_Lemmas_Maps.Element(Form_To_Lemmas_Cursor);

                                        for Lemma of Lemma_Set loop
                                            declare
                                                Cursor : constant Lemma_To_Cluster_Index_Set_Maps.Cursor
                                                  := Lemma_To_Cluster_Index_Set_Map.Find(Lemma);
                                            begin
                                                if Lemma_To_Cluster_Index_Set_Maps.Has_Element(Cursor) then
                                                    Cluster_Index_Set.Union(Lemma_To_Cluster_Index_Set_Maps.Element(Cursor));
                                                end if;
                                            end;
                                        end loop;

                                        -- FIXME: ma cosa sarebbe questa porcata?
                                        for Cluster_Index of Cluster_Index_Set loop
                                            if Length(Str_Cluster_Indexes) > 0 then
                                                Append(Str_Cluster_Indexes, " ");
                                            end if;
                                            Append(Str_Cluster_Indexes, Str(Cluster_Index));
                                        end loop;

                                        Str_Cluster_Indexes_Vector.Append(Str_Cluster_Indexes);

                                    end if;
                                end;
                            end loop;

                            if Is_Ignored then
                                Insert(Ignored_Questions, Question);
                            else
                                -- generate all possible matches
                                Str_Cluster_Indexes_Double_Vector.Append(Str_Cluster_Indexes_Vector);

                                -- sort all possible match: TO-DO only check last two for now
                                if Natural(Form_Tokens.Length) >= 2 then
                                    declare
                                        Cluster_Index_Set : Cluster_Index_Sets.Set;

                                        -- TODO: copie forse inefficienti
                                        Lemma_Set_1 : constant Lemmas_Sets.Set
                                          := Form_To_Lemmas_Map.Element(Form_Tokens.Element(Form_Tokens.Last_Index));
                                        Lemma_Set_2 : constant Lemmas_Sets.Set
                                          := Form_To_Lemmas_Map.Element(Form_Tokens.Element(Form_Tokens.Last_Index - 1));
                                    begin

                                        -- check if last two are together
                                        for Lemma_1 of Lemma_Set_1 loop
                                            for Lemma_2 of Lemma_Set_2 loop
                                                declare
                                                    Cursor : constant Gov_Dep_Lemma_Link_To_Cluster_Index_Maps.Cursor
                                                      := Gov_Dep_Lemma_Link_To_Cluster_Index_Map.Find
                                                        ((Governor_Lemma  => Lemma_1,
                                                          Dependent_Lemma => Lemma_2));
                                                begin
                                                    if Gov_Dep_Lemma_Link_To_Cluster_Index_Maps.Has_Element(Cursor) then
                                                        Cluster_Index_Set.Insert(Gov_Dep_Lemma_Link_To_Cluster_Index_Maps.Element(Cursor));
                                                    end if;
                                                end;
                                            end loop;
                                        end loop;

                                        if not Cluster_Index_Set.Is_Empty then
                                            declare
                                                Cur_Cluster_Indexes : Str_Cluster_Indexes_Vectors.Vector;
                                                Str_Cluster_Indexes : Unbounded_String;
                                            begin
                                                -- TO-DO: may have delete the/of/ before last?
                                                for I in Str_Cluster_Indexes_Vector.First_Index .. Str_Cluster_Indexes_Vector.Last_Index - 2 loop
                                                    Cur_Cluster_Indexes.Append
                                                      (Str_Cluster_Indexes_Vector.Element(I));
                                                end loop;

                                                for Cluster_Index of Cluster_Index_Set loop
                                                    if Length(Str_Cluster_Indexes) > 0 then
                                                        Append(Str_Cluster_Indexes, " ");
                                                    end if;
                                                    Append(Str_Cluster_Indexes, Str(Cluster_Index));
                                                end loop;
                                                Cur_Cluster_Indexes.Append(Str_Cluster_Indexes);

                                                Str_Cluster_Indexes_Double_Vector.Append(Cur_Cluster_Indexes);
                                            end;
                                        end if;
                                    end;
                                end if;

                                Argument_To_Str_Cluster_Indexes_Double_Vector_Map.Insert
                                  (Question.Argument, Str_Cluster_Indexes_Double_Vector);
                            end if;
                        end;
                    end if;
                end loop; -- Question

                Remove_All(Relation_To_Question_Vector_Map, Relation_To_Question_Cursor, Ignored_Questions);

                Ignored_Questions.Clear; -- ignore due to missing form
                -- TODO: Copia forse inefficiente.
                Question_Vector := Relation_To_Question_Vector_Maps.Element(Relation_To_Question_Cursor);

                for Question of Question_Vector loop
                    -- find aci
                    declare
                        Cluster_Index : constant Cluster_Index_Type := Rel_Type_To_Cluster_Index_Map.Element(Question.Relation);
                        Deprel_2      : constant Unbounded_String   := (if Question.Deprel = "nsubj" then UStr("dobj") else Ustr("nsubj"));
                    begin
                        if not Cluster_Index_To_Deprel_To_Argument_Cluster_Index_Map.Element(Cluster_index).Contains(Question.Deprel)
                          or else not Cluster_Index_To_Deprel_To_Argument_Cluster_Index_Map.Element(Cluster_index).Contains(Deprel_2) then
                            Insert(Ignored_Questions, Question);
                        end if;
                    end;
                end loop;

                Remove_All(Relation_To_Question_Vector_Map, Relation_To_Question_Cursor, Ignored_Questions);
            end;
            Relation_To_Question_Vector_Maps.Next(Relation_To_Question_Cursor);
        end loop;
    end Preprocess_Arguments;

    ---------------
    procedure Match
    ---------------
      (Articles : in Article_Sets.Set) is
        Cursor : Relation_To_Question_Vector_Maps.Cursor
          := Relation_To_Question_Vector_Map.First;
    begin
        while Relation_To_Question_Vector_Maps.Has_Element(Cursor) loop
            declare
                Relation        : constant Unbounded_String        := Relation_To_Question_Vector_Maps.Key(Cursor);
                -- TODO: copia forse inefficiente.
                Question_Vector : constant Question_Vectors.Vector := Relation_To_Question_Vector_Maps.Element(Cursor);

                Cluster_Index   : constant Cluster_Index_Type      := Rel_Type_To_Cluster_Index_Map.Element(Relation);
                -- TODO: copia forse inefficiente.
                Atom_ID_Set     : constant Atom_ID_Sets.Set        := Cluster_Index_To_Atom_ID_Set_Map.Element(Cluster_Index);
            begin

                for Question of Question_Vector loop
                    -- find aci
                    declare
                        Deprel_2 : constant Unbounded_String := (if Question.Deprel = "nsubj" then UStr("dobj") else Ustr("nsubj"));

                        Argument_Cluster_Index : constant Argument_Cluster_Index_Type
                          := Cluster_Index_To_Deprel_To_Argument_Cluster_Index_Map.Element(Cluster_Index).Element(Question.Deprel);
                        Argument_Cluster_Index_2 : constant Argument_Cluster_Index_Type
                          := Cluster_Index_To_Deprel_To_Argument_Cluster_Index_Map.Element(Cluster_Index).Element(Deprel_2);
                    begin
                        for Atom_ID of Atom_ID_Set loop
                            Match(Articles, Question, Atom_ID, Argument_Cluster_Index, Argument_Cluster_Index_2);
                        end loop;
                    end;
                end loop;
            end;
            Relation_To_Question_Vector_Maps.Next(Cursor);
        end loop;
    end Match;

    ---------------
    procedure Match
    ---------------
      (Articles                 : in Article_Sets.Set;
       Question                 : in Question_Type;
       Atom_ID                  : in Atom_ID_Type;
       Argument_Cluster_Index   : in Argument_Cluster_Index_Type;
       Argument_Cluster_Index_2 : in Argument_Cluster_Index_Type) is

        Match_Flag : Boolean := False;

        Atom_ID_Cursor : Atom_ID_To_Argument_Cluster_Index_To_Atom_ID_Set_Map_Maps.Cursor;
        Argument_Cluster_Index_To_Atom_ID_Set_Map : Argument_Cluster_Index_To_Atom_ID_Set_Maps.Map;

        Argument_Cluster_Index_1_Cursor : Argument_Cluster_Index_To_Atom_ID_Set_Maps.Cursor;
        Argument_Cluster_Index_2_Cursor : Argument_Cluster_Index_To_Atom_ID_Set_Maps.Cursor;
        Atom_ID_Set_1 : Atom_ID_Sets.Set;
        Atom_ID_Set_2 : Atom_ID_Sets.Set;

    begin

        Atom_ID_Cursor := Atom_ID_To_Argument_Cluster_Index_To_Atom_ID_Set_Map_Map.Find(Atom_ID);
        if not Atom_ID_To_Argument_Cluster_Index_To_Atom_ID_Set_Map_Maps.Has_Element(Atom_ID_Cursor) then
            return;
        end if;
        Argument_Cluster_Index_To_Atom_ID_Set_Map := Atom_ID_To_Argument_Cluster_Index_To_Atom_ID_Set_Map_Maps.Element(Atom_ID_Cursor);

        Argument_Cluster_Index_1_Cursor := Argument_Cluster_Index_To_Atom_ID_Set_Map.Find(Argument_Cluster_Index);
        Argument_Cluster_Index_2_Cursor := Argument_Cluster_Index_To_Atom_ID_Set_Map.Find(Argument_Cluster_Index_2);
        if not Argument_Cluster_Index_To_Atom_ID_Set_Maps.Has_Element(Argument_Cluster_Index_1_Cursor)
          or else not Argument_Cluster_Index_To_Atom_ID_Set_Maps.Has_Element(Argument_Cluster_Index_2_Cursor) then
            return;
        end if;
        Atom_ID_Set_1 := Argument_Cluster_Index_To_Atom_ID_Set_Maps.Element(Argument_Cluster_Index_1_Cursor);
        Atom_ID_Set_2 := Argument_Cluster_Index_To_Atom_ID_Set_Maps.Element(Argument_Cluster_Index_2_Cursor);

        -- negation
        declare
            Cursor : Argument_Cluster_Index_To_Atom_ID_Set_Maps.Cursor
              := Argument_Cluster_Index_To_Atom_ID_Set_Map.First;
        begin
            while Argument_Cluster_Index_To_Atom_ID_Set_Maps.Has_Element(Cursor) loop
                declare
                    Cur_ACI         : Argument_Cluster_Index_Type renames Argument_Cluster_Index_To_Atom_ID_Set_Maps.Key(Cursor);
                    Cur_Atom_ID_Set : Atom_ID_Sets.Set            renames Argument_Cluster_Index_To_Atom_ID_Set_Maps.Element(Cursor);
                begin
                    if Cur_ACI not in Argument_Cluster_Index | Argument_Cluster_Index_2 then
                        for Cur_Atom_ID of Cur_Atom_ID_Set loop
                            declare
                                Deprel : Unbounded_String renames Atom_ID_To_Deprel_Map.Element(Cur_Atom_ID);
                            begin
                                if Deprel = "neg" then
                                    return;
                                end if;
                            end;
                        end loop;
                    end if;
                end;
                Argument_Cluster_Index_To_Atom_ID_Set_Maps.Next(Cursor);
            end loop;
        end;

        -- match aci w. arg
        for Cur_Atom_ID of Atom_ID_Set_1 loop
            if Is_Match(Cur_Atom_ID, Question.Argument) then
                Match_Flag := True;
                exit; -- break
            end if;
        end loop;

        -- retrieve aci2
        if Match_Flag then
            for Cur_Atom_ID of Atom_ID_Set_2 loop
                -- recursively construct ans; take care of and/appos
                Find_Answer(Articles, Question, Cur_Atom_ID);
            end loop;
        end if;

    end Match;

    -----------------
    function Is_Match
    -----------------
      (Dependent_Atom_ID : in Atom_ID_Type;
       Argument          : in Unbounded_String) return Boolean is

    -- TODO: copia forse inefficiente
        Str_Cluster_Indexes_Double_Vector : constant Str_Cluster_Indexes_Double_Vectors.Vector
          := Argument_To_Str_Cluster_Indexes_Double_Vector_Map.Element(Argument);

        Atom_ID_Cursor : Atom_ID_To_Argument_Cluster_Index_To_Atom_ID_Set_Map_Maps.Cursor;
        Argument_Cluster_Index_To_Atom_ID_Set_Map : Argument_Cluster_Index_To_Atom_ID_Set_Maps.Map;
        ACI_Cursor : Argument_Cluster_Index_To_Atom_ID_Set_Maps.Cursor;
    begin

        for Str_Cluster_Indexes_Vector of Str_Cluster_Indexes_Double_Vector loop
            if Is_Match_From_Head(Dependent_Atom_ID, Str_Cluster_Indexes_Vector) then
                return True;
            end if;
        end loop;

        Atom_ID_Cursor := Atom_ID_To_Argument_Cluster_Index_To_Atom_ID_Set_Map_Map.Find(Dependent_Atom_ID);
        if not Atom_ID_To_Argument_Cluster_Index_To_Atom_ID_Set_Map_Maps.Has_Element(Atom_ID_Cursor) then
            return False;

        else
            -- TODO: copia forse inefficiente
            Argument_Cluster_Index_To_Atom_ID_Set_Map := Atom_ID_To_Argument_Cluster_Index_To_Atom_ID_Set_Map_Maps.Element(Atom_ID_Cursor);
            ACI_Cursor := Argument_Cluster_Index_To_Atom_ID_Set_Map.First;
            while Argument_Cluster_Index_To_Atom_ID_Set_Maps.Has_Element(ACI_Cursor) loop
                for Atom_ID of Argument_Cluster_Index_To_Atom_ID_Set_Maps.Element(ACI_Cursor) loop
                    declare
                        Deprel : constant Unbounded_String := Atom_ID_To_Deprel_Map.Element(Atom_ID);
                    begin
                        if (Index(Deprel, "conj_") = 1 and then Deprel /= "conj_negcc") or else Deprel = "appos" then
                            for Str_Cluster_Indexes_Vector of Str_Cluster_Indexes_Double_Vector loop
                                if Is_Match_From_Head(Atom_ID, Str_Cluster_Indexes_Vector) then
                                    return True;
                                end if;
                            end loop;
                        end if;
                    end;
                end loop;
                Argument_Cluster_Index_To_Atom_ID_Set_Maps.Next(ACI_Cursor);
            end loop;
        end if;

        return False;
    end Is_Match;

    ---------------------------
    function Is_Match_From_Head
    ---------------------------
      (Dependent_Atom_ID          : in Atom_ID_Type;
       Str_Cluster_Indexes_Vector : Str_Cluster_Indexes_Vectors.Vector) return Boolean is

        Governor_Cluster_Index : Cluster_Index_Type renames
          Atom_ID_To_Cluster_Index_And_Atom_Tree_Str_Link_Map.Element(Dependent_Atom_ID).Cluster_Index;

        Tree_Cluster_Index_Set : Cluster_Index_Sets.Set;
    begin

        if not Contains(Str_Cluster_Indexes_Vector, Governor_Cluster_Index) then
            return False;
        end if;

        Tree_Cluster_Index_Set := Get_Tree_Cluster_Indexes(Dependent_Atom_ID);

        for Str_Cluster_Indexes of Str_Cluster_Indexes_Vector loop
            declare
                Ok              : Boolean := False;
                Tokenizer       : Utils.String_Tokenizer_Type;
                Cluster_Indexes : constant String := Str(Str_Cluster_Indexes); -- FIXME: ALLARME STRINGA SCHIFEZZA!!
                First, Last     : Natural;
            begin
                Tokenizer.Tokenizer_Start(Cluster_Indexes);

                while Tokenizer.Tokenizer_Has_Next(Cluster_Indexes) loop
                    Tokenizer.Tokenizer_Next(Cluster_Indexes, " ", First, Last);
                    if Tree_Cluster_Index_Set.Contains(Cluster_Index_Type'Value(Cluster_Indexes(First .. Last))) then
                        Ok := True;
                        exit;
                    end if;
                end loop;

                if not Ok then
                    return False;
                end if;
            end;
        end loop;

        return True;
    end Is_Match_From_Head;

    ---------------------------------
    function Get_Tree_Cluster_Indexes
    ---------------------------------
      (Atom_ID : in Atom_ID_Type) return Cluster_Index_Sets.Set is

        Cluster_Index_Set : Cluster_Index_Sets.Set;
        Atom_ID_Cursor    : Atom_ID_To_Argument_Cluster_Index_To_Atom_ID_Set_Map_Maps.Cursor;
    begin

        Insert
          (Set      => Cluster_Index_Set,
           New_Item => Atom_ID_To_Cluster_Index_And_Atom_Tree_Str_Link_Map.Element(Atom_ID).Cluster_Index);

        Atom_ID_Cursor := Atom_ID_To_Argument_Cluster_Index_To_Atom_ID_Set_Map_Map.Find(Atom_ID);
        if Atom_ID_To_Argument_Cluster_Index_To_Atom_ID_Set_Map_Maps.Has_Element(Atom_ID_Cursor) then
            for Atom_ID_Set of Atom_ID_To_Argument_Cluster_Index_To_Atom_ID_Set_Map_Maps.Element(Atom_ID_Cursor) loop
                for Cur_Atom_ID of Atom_ID_Set loop
                    declare
                        Deprel : Unbounded_String renames Atom_ID_To_Deprel_Map.Element(Cur_Atom_ID);
                    begin
                        if Deprel_Is_Allowed(Deprel) then
                            Cluster_Index_Set.Union(Get_Tree_Cluster_Indexes(Cur_Atom_ID));
                        end if;
                    end;
                end loop;
            end loop;

        end if;

        return Cluster_Index_Set;
    end Get_Tree_Cluster_Indexes;

    ---------------------
    procedure Find_Answer
    ---------------------
      (Articles : in Article_Sets.Set;
       Question : in Question_Type;
       Atom_ID  : in Atom_ID_Type) is

        Sentence_ID : constant Unbounded_String -- FIXME: rimpiazzare con una struttura
          := UStr(Str(Natural(Atom_ID.Article_ID)) & ":" & Str(Natural(Atom_ID.Sentence_ID)));

        -- TODO: essendo Article solo di passaggio per arrivare a Sentence, si puo' forse ottimizzare
        Article  : constant Article_Type := Get_Article(Articles, Atom_ID.Article_ID);
        Sentence : constant Word_Vector.Vector := Article.Sentences.Element(Atom_ID.Sentence_ID);

        Atom_ID_To_Min_Atom_ID : Atom_ID_To_Atom_ID_Maps.Map;

        Answers                : constant Atom_ID_Ordered_Set_Vectors.Vector
          := Find_Answer(Atom_ID, Atom_ID_To_Min_Atom_ID);
    begin

        for Answer of Answers loop
            declare
                Word_Index_Set         : Word_Index_Sets.Set; -- tknIdx; may add some prep/det
                Word_Index_To_Str_Prep : Word_Index_To_Str_Prep_Maps.Map;
                Str_Preps_Sequence     : Unbounded_String;
            begin
                for Answer_Atom_ID of Answer loop
                    -- look up dependency
                    declare
                        Word_Index           : Word_Index_Type renames Answer_Atom_ID.Word_ID;
                        Dependent_Word       : constant Word_Type := Sentence.Element(Word_Index);
                        Dependent_Word_Index : Word_Index_Type := -1; -- det
                    begin
                        Word_Index_Set.Insert(Word_Index);

                        -- det
                        for Dependent_Index of Dependent_Word.Dependents loop
                            declare
                                Cur_Dependent : constant Word_Type := Sentence.Element(Dependent_Index);
                            begin
                                if Cur_Dependent.Deprel = "det" then
                                    Dependent_Word_Index := Cur_Dependent.ID;
                                    Word_Index_Set.Insert(Dependent_Word_Index);
                                    exit; -- break
                                end if;
                            end;
                        end loop;

                        -- prep?
                        if Dependent_Word.Head /= -1 and then Index(Dependent_Word.Deprel, "prep_") = 1 then
                            declare
                                Governor_Atom_ID : constant Atom_ID_Type
                                  := (Article_ID  => Atom_ID.Article_ID,
                                      Sentence_ID => Atom_ID.Sentence_ID,
                                      Word_ID     => Dependent_Word.Head);
                            begin
                                if Answer.Contains(Governor_Atom_ID) then
                                    -- should include the preposition in the answer string
                                    declare
                                        Str_Prep : constant Unbounded_String
                                          := UStr(Slice(Dependent_Word.Deprel, 6, Length(Dependent_Word.Deprel))); -- Remove "prep_"

                                        Min_Atom_ID : constant Atom_ID_Type
                                          := Atom_ID_To_Min_Atom_ID.Element(Answer_Atom_ID);

                                        Min_Word_ID : Word_Index_Type := Min_Atom_ID.Word_ID;
                                    begin
                                        if Dependent_Word_Index >= 0 and then Dependent_Word_Index < Min_Word_ID then
						Min_Word_ID := Dependent_Word_Index;
					end if;
                                        Word_Index_To_Str_Prep.Insert(Min_Word_ID, Str_Prep);
                                    end;
                                end if;
                            end;
                        end if;
                    end;
                end loop; -- Answer_Atom_ID

                -- Build Str_Preps_Sequence
                for Word_Index of Word_Index_Set loop

                    -- prep
                    if not Word_Index_To_Str_Prep.Is_Empty then
                        declare
                            First_Word_Index : constant Word_Index_Type
                              := Word_Index_To_Str_Prep.First_Key;
                        begin
                            if Word_Index >= First_Word_Index then
                                if Length(Str_Preps_Sequence) > 0 then
                                    Append(Str_Preps_Sequence, " ");
                                end if;

                                Append(Str_Preps_Sequence, Word_Index_To_Str_Prep.Element(first_Word_Index));
                                Word_Index_To_Str_Prep.Delete(first_Word_Index);
                            end if;
                        end;
                    end if;

                    -- word
                    declare
                        Word : Unbounded_String
                          := Sentence.Element(Word_Index).Form;

                        Cur_Atom_ID : constant Atom_ID_Type
                          := (Article_ID  => Atom_ID.Article_ID,
                              Sentence_ID => Atom_ID.Sentence_ID,
                              Word_ID     => Word_Index);

                        Atom_ID_Cursor : constant Atom_ID_To_Cluster_Index_And_Atom_Tree_Str_Link_Maps.Cursor
                          := Atom_ID_To_Cluster_Index_And_Atom_Tree_Str_Link_Map.Find(Cur_Atom_ID);
                    begin
                        if Atom_ID_To_Cluster_Index_And_Atom_Tree_Str_Link_Maps.Has_Element(Atom_ID_Cursor) then
                            declare
                                Atom_Tree_Str : constant Unbounded_String
                                  := Atom_ID_To_Cluster_Index_And_Atom_Tree_Str_Link_Maps.Element(Atom_ID_Cursor).Atom_Tree_Str;
                            begin
                                if Index(Atom_Tree_Str, " ") > 1 then
                                    Word := Atom_Tree_Str;
                                end if;
                            end;
                        end if;

                        if Length(Str_Preps_Sequence) > 0 then
                            Append(Str_Preps_Sequence, " ");
                        end if;

                        Append(Str_Preps_Sequence, Word);
                    end;
                end loop; -- Word_Index

                declare
                    New_Answer : constant Answer_Type
                      := (Sentence_ID        => Sentence_ID,
                          Str_Preps_Sequence => Str_Preps_Sequence);

                    Cursor     : Question_To_Answer_Set_Maps.Cursor
                      := Question_To_Answer_Set_Map.Find(Question);
                begin
                    if not Question_To_Answer_Set_Maps.Has_Element(Cursor) then
                        Insert
                          (Map      => Question_To_Answer_Set_Map,
                           Key      => Question,
                           Position => Cursor);
                    end if;

                    Add_To_Set(Question_To_Answer_Set_Map, Cursor, New_Answer);
                end;
            end;
        end loop; -- Answer

    end Find_Answer;

    --------------------
    function Find_Answer
    --------------------
      (Atom_ID                : in     Atom_ID_Type;
       Atom_ID_To_Min_Atom_ID : in out Atom_ID_To_Atom_ID_Maps.Map) return Atom_ID_Ordered_Set_Vectors.Vector is

        Answers     : Atom_ID_Ordered_Set_Vectors.Vector;
        Cur_Answers : Atom_ID_Ordered_Set_Vectors.Vector;

        Atom_ID_Cursor : Atom_ID_To_Argument_Cluster_Index_To_Atom_ID_Set_Map_Maps.Cursor;
    begin

        declare
            Atom_ID_Set : Atom_ID_Ordered_Sets.Set;
        begin
            Atom_ID_Set.Insert(Atom_ID);
            Cur_Answers.Append(Atom_ID_Set);
        end;

        Atom_ID_To_Min_Atom_ID.Insert(Atom_ID, Atom_ID);

        Atom_ID_Cursor := Atom_ID_To_Argument_Cluster_Index_To_Atom_ID_Set_Map_Map.Find(Atom_ID);
        if Atom_ID_To_Argument_Cluster_Index_To_Atom_ID_Set_Map_Maps.Has_Element(Atom_ID_Cursor) then
            for Atom_ID_Set of Atom_ID_To_Argument_Cluster_Index_To_Atom_ID_Set_Map_Maps.Element(Atom_ID_Cursor) loop
                for Dependent_Atom_ID of Atom_ID_Set loop
                    declare
                        Deprel : constant Unbounded_String := Atom_ID_To_Deprel_Map(Dependent_Atom_ID);
                    begin
                        if (Index(Deprel, "conj_") = 1 and then Deprel /= "conj_negcc") or else Deprel = "appos" then
                            declare
                                Ret : constant Atom_ID_Ordered_Set_Vectors.Vector
                                  := Find_Answer(Dependent_Atom_ID, Atom_ID_To_Min_Atom_ID);
                            begin
                                Answers.Append(Ret);
                                if Atom_ID_To_Min_Atom_ID.Element(Dependent_Atom_ID) < Atom_ID_To_Min_Atom_ID.Element(Atom_ID) then
                                    Atom_ID_To_Min_Atom_ID.Replace(Atom_ID, Atom_ID_To_Min_Atom_ID.Element(Dependent_Atom_ID));
                                end if;
                            end;

                        elsif Deprel_Is_Allowed(Deprel) then
                            declare
                                Ret           : constant Atom_ID_Ordered_Set_Vectors.Vector
                                  := Find_Answer(Dependent_Atom_ID, Atom_ID_To_Min_Atom_ID);
                                Local_Answers : Atom_ID_Ordered_Set_Vectors.Vector;
                            begin
                                if Atom_ID_To_Min_Atom_ID.Element(Dependent_Atom_ID) < Atom_ID_To_Min_Atom_ID.Element(Atom_ID) then
                                    Atom_ID_To_Min_Atom_ID.Replace(Atom_ID, Atom_ID_To_Min_Atom_ID.Element(Dependent_Atom_ID));
                                end if;

                                for Cur_Answer of Cur_Answers loop
                                    for Ret_Answer of Ret loop
                                        Local_Answers.Append(Atom_ID_Ordered_Sets.Union(Cur_Answer, Ret_Answer));
                                    end loop;
                                end loop;

                                Cur_Answers := Local_Answers;
                            end;
                        end if;
                    end;
                end loop;
            end loop;
        end if;

        Answers.Append(Cur_Answers);
        return Answers;
    end Find_Answer;

    -----------------------
    procedure Print_Answers
    -----------------------
      (Articles : in Article_Sets.Set) is

        function Get_Str_Sentence
          (Str_Sentence_ID : in String) return Unbounded_String is

            Sep_Index : constant Natural := Ada.Strings.Fixed.Index(Str_Sentence_ID, ":");

            Article_Index  : constant Article_Index_Type  := Article_Index_Type'Value(Str_Sentence_ID(Str_Sentence_ID'First .. Sep_Index - 1));
            Sentence_Index : constant Sentence_Index_Type := Sentence_Index_Type'Value(Str_Sentence_ID(Sep_Index + 1 .. Str_Sentence_ID'Last));

            -- TODO: essendo Article solo di passaggio per arrivare a Sentence, si puo' forse ottimizzare
            Article  : constant Article_Type       := Get_Article(Articles, Article_Index);
            Sentence : constant Word_Vector.Vector := Article.Sentences.Element(Sentence_Index);

            Ret : Unbounded_String;
        begin

            for Word of Sentence loop
                if Word.ID /= 0 then
                    if Length(Ret) > 0 then
                        Append(Ret, " ");
                    end if;
                    Append(Ret, Word.Form);
                end if;
            end loop;

            return Ret;
        end Get_Str_Sentence;

        Cursor : Question_To_Answer_Set_Maps.Cursor
          := Question_To_Answer_Set_Map.First;
    begin

        while Question_To_Answer_Set_Maps.Has_Element(Cursor) loop
            declare
                Question   : constant Question_Type   := Question_To_Answer_Set_Maps.Key(Cursor);
                Answer_Set : constant Answer_Sets.Set := Question_To_Answer_Set_Maps.Element(Cursor);
            begin
                for Answer of Answer_Set loop
                    declare
                        Str_Sentence : constant Unbounded_String
                          := Get_Str_Sentence(Str(Answer.Sentence_ID));
                    begin
                        T_IO.Put_Line("<question str=""" & To_String(Question) & """>");
                        T_IO.Put_Line("<label></label>");
                        T_IO.Put_Line("<answer>" & Str(Answer.Str_Preps_Sequence) & "</answer>");
                        T_IO.Put_Line("<sentence id=""" & Str(Answer.Sentence_ID) & """>" & Str(Str_Sentence) & "</sentence>");
                        T_IO.Put_Line("</question>");
                        T_IO.New_Line;
                    end;
                end loop;
            end;
            Question_To_Answer_Set_Maps.Next(Cursor);
        end loop;

    end Print_Answers;

    --------------------
    function Get_Article
    --------------------
      (Articles   : in Article_Sets.Set;
       Article_ID : in Article_Index_Type) return Article_Type is
    begin
        for Article of Articles loop
            if Article.ID = Article_ID then
                return Article;
            end if;
        end loop;
        raise MLSE_Error with "Article not found.";
    end Get_Article;

    -----------------
    function Contains
    -----------------
      (Str_Cluster_Indexes : in Unbounded_String;
       Str_Cluster_Index   : in String) return Boolean is
    -- FIXME: non viglio piu' vedere questa porcata!!!

        Cluster_Indexes : constant String := Str(Str_Cluster_Indexes);
        Tokenizer : Utils.String_Tokenizer_Type;
        First, Last : Natural;
    begin
        Tokenizer.Tokenizer_Start(Cluster_Indexes);
        while Tokenizer.Tokenizer_Has_Next(Cluster_Indexes) loop
            Tokenizer.Tokenizer_Next(Cluster_Indexes, " ", First, Last);
            if Cluster_Indexes(First .. Last) = Str_Cluster_Index then
                return True;
            end if;
        end loop;

        return False;
    end Contains;

    -----------------
    function Contains
    -----------------
      (Str_Cluster_Indexes_Vector : in Str_Cluster_Indexes_Vectors.Vector;
       Cluster_Index              : in Cluster_Index_Type) return Boolean is
    -- FIXME: non viglio piu' vedere questa porcata!!!

        Str_Cluster_Index : constant String := Str(Cluster_Index);
    begin
        for Str_Cluster_Indexes of Str_Cluster_Indexes_Vector loop
            if Contains(Str_Cluster_Indexes, Str_Cluster_Index) then
                return True;
            end if;
        end loop;

        return False;
    end Contains;

    ----------------------------
    function Remove_Third_Person
    ----------------------------
      (Relation : String) return String is
        F : constant Natural  := Relation'First;
        L : constant Positive := Relation'Last;
    begin
        if Relation'Length <= 3 then
            T_IO.Put_Line("*** " & Relation);
        end if;

        if Relation(L - 1) /= 'e' then
            return Relation(F .. L - 1);
        elsif Relation(L - 2) = 'i' then
            return Relation(F .. L - 3) & "y";
        elsif Relation(L - 2) = 's' and then Relation(L - 3) = 's' then
            return Relation(F .. L - 2);
        elsif Relation(L - 2) = 'h' and then Relation(L - 3) = 's' then
            return Relation(F .. L - 2);
        else
            return Relation(F .. L - 1);
        end if;
    end Remove_Third_Person;

    -------------------------------------------------------------------------

    ----------------
    procedure Insert
    ----------------
      (Map      : in out Relation_To_Question_Vector_Maps.Map;
       Key      : in     Unbounded_String;
       Position : out    Relation_To_Question_Vector_Maps.Cursor) is

        Inserted : Boolean;
    begin
        Map.Insert
          (Key      => Key,
           Position => Position,
           Inserted => Inserted);

        if not Inserted then
            raise MLSE_Insertion_Error;
        end if;
    end Insert;

    --------------------
    procedure Remove_All
    --------------------
      (Map      : in out Relation_To_Question_Vector_Maps.Map;
       Position : in     Relation_To_Question_Vector_Maps.Cursor;
       Elements : in     Question_Sets.Set) is

        procedure Update(Relation : in Unbounded_String; Question_Vector : in out Question_Vectors.Vector) is
            pragma Unreferenced (Relation);
        begin
            for Question of Elements loop
                while True loop
                    declare
                        Position : Question_Vectors.Cursor
                          := Question_Vector.Find(Question);
                    begin
                        if Question_Vectors.Has_Element(Position) then
                            Question_Vector.Delete(Position);
                        else
                            exit; -- break
                        end if;
                    end;
                end loop;
            end loop;
        end Update;
        pragma Inline(Update);
    begin
        Map.Update_Element(Position, Update'Access);
    end Remove_All;

    ----------------
    procedure Insert
    ----------------
      (Map      : in out Form_To_Lemmas_Maps.Map;
       Key      : in     Unbounded_String;
       Position : out    Form_To_Lemmas_Maps.Cursor) is

        Inserted : Boolean;
    begin
        Map.Insert
          (Key      => Key,
           Position => Position,
           Inserted => Inserted);

        if not Inserted then
            raise MLSE_Insertion_Error;
        end if;
    end Insert;

    --------------------
    procedure Add_To_Set
    --------------------
      (Map          : in out Form_To_Lemmas_Maps.Map;
       Map_Cursor   : in     Form_To_Lemmas_Maps.Cursor;
       Set_New_Item : in     Unbounded_String) is

        procedure Update
          (Form       : in     Unbounded_String;
           Lemmas_Set : in out Lemmas_Sets.Set) is
            pragma Unreferenced (Form);
            Position : Lemmas_Sets.Cursor;
            Inserted : Boolean;
        begin
            Lemmas_Set.Insert
              (New_Item => Set_New_Item,
               Position => Position,
               Inserted => Inserted);
        end Update;
        pragma Inline(Update);

    begin
        Map.Update_Element(Map_Cursor, Update'Access);
    end Add_To_Set;

    -----------------------
    procedure Add_To_Vector
    -----------------------
      (Map             : in out Relation_To_Question_Vector_Maps.Map;
       Map_Cursor      : in     Relation_To_Question_Vector_Maps.Cursor;
       Vector_New_Item : in     Question_Type) is

        procedure Update
          (Relation        : in     Unbounded_String;
           Question_Vector : in out Question_Vectors.Vector) is
            pragma Unreferenced (Relation);
        begin
            Question_Vector.Append(Vector_New_Item);
        end Update;
        pragma Inline(Update);

    begin
        Map.Update_Element(Map_Cursor, Update'Access);
    end Add_To_Vector;

    ----------------
    procedure Insert
    ----------------
      (Set      : in out Forms_Sets.Set;
       New_Item : in     Unbounded_String) is

        Position : Forms_Sets.Cursor;
        Inserted : Boolean;
    begin
        Set.Insert
          (New_Item => New_Item,
           Position => Position,
           Inserted => Inserted);
    end Insert;

    ----------------
    procedure Insert
    ----------------
      (Set      : in out Lemmas_Sets.Set;
       New_Item : in     Unbounded_String) is

        Position : Lemmas_Sets.Cursor;
        Inserted : Boolean;
    begin
        Set.Insert
          (New_Item => New_Item,
           Position => Position,
           Inserted => Inserted);
    end Insert;

    ------------------------
    procedure Insert_Replace
    ------------------------
      (Map      : in out Deprel_To_Argument_Cluster_Index_Maps.Map;
       Key      : in     Unbounded_String;
       New_Item : in     Argument_Cluster_Index_Type) is

        Position : constant Deprel_To_Argument_Cluster_Index_Maps.Cursor := Map.Find(Key);
    begin
        if Deprel_To_Argument_Cluster_Index_Maps.Has_Element(Position) then
            Map.Replace_Element(Position, New_Item);
        else
            Map.Insert(Key, New_Item);
        end if;
    end Insert_Replace;

    ----------------
    procedure Insert
    ----------------
      (Map      : in out Cluster_Index_To_Deprel_To_Argument_Cluster_Index_Maps.Map;
       Key      : in     Cluster_Index_Type;
       Position : out    Cluster_Index_To_Deprel_To_Argument_Cluster_Index_Maps.Cursor) is

        Inserted : Boolean;
    begin
        Map.Insert
          (Key      => Key,
           Position => Position,
           Inserted => Inserted);

        if not Inserted then
            raise MLSE_Insertion_Error;
        end if;
    end Insert;

    -------------------------------
    procedure Insert_Replace_To_Map
    -------------------------------
      (Map              : in out Cluster_Index_To_Deprel_To_Argument_Cluster_Index_Maps.Map;
       Map_Cursor       : in     Cluster_Index_To_Deprel_To_Argument_Cluster_Index_Maps.Cursor;
       Sub_Map_Key      : in     Unbounded_String;
       Sub_Map_New_Item : in     Argument_Cluster_Index_Type) is

        procedure Update
          (Cluster_Index                    : in Cluster_Index_Type;
           Deprel_To_Argument_Cluster_Index : in out Deprel_To_Argument_Cluster_Index_Maps.Map) is
            pragma Unreferenced (Cluster_Index);
        begin
            Insert_Replace(Deprel_To_Argument_Cluster_Index, Sub_Map_Key, Sub_Map_New_Item);
        end Update;
        pragma Inline(Update);
    begin
        Map.Update_Element(Map_Cursor, Update'Access);
    end Insert_Replace_To_Map;

    ----------------
    procedure Insert
    ----------------
      (Map      : in out Lemma_To_Cluster_Index_Set_Maps.Map;
       Key      : in     Unbounded_String;
       Position : out    Lemma_To_Cluster_Index_Set_Maps.Cursor) is

        Inserted : Boolean;
    begin
        Map.Insert
          (Key      => Key,
           Position => Position,
           Inserted => Inserted);

        if not Inserted then
            raise MLSE_Insertion_Error;
        end if;
    end Insert;

    --------------------
    procedure Add_To_Set
    --------------------
      (Map          : in out Lemma_To_Cluster_Index_Set_Maps.Map;
       Map_Cursor   : in     Lemma_To_Cluster_Index_Set_Maps.Cursor;
       Set_New_Item : in     Cluster_Index_Type) is

        procedure Update
          (Lemma             : in     Unbounded_String;
           Cluster_Index_Set : in out Cluster_Index_Sets.Set) is
            pragma Unreferenced (Lemma);
            Position : Cluster_Index_Sets.Cursor;
            Inserted : Boolean;
        begin
            Cluster_Index_Set.Insert
              (New_Item => Set_New_Item,
               Position => Position,
               Inserted => Inserted);
        end Update;
        pragma Inline(Update);

    begin
        Map.Update_Element(Map_Cursor, Update'Access);
    end Add_To_Set;

    ------------------------
    procedure Insert_Replace
    ------------------------
      (Map      : in out Gov_Dep_Lemma_Link_To_Cluster_Index_Maps.Map;
       Key      : in     Gov_Dep_Lemma_Link_Type;
       New_Item : in     Cluster_Index_Type) is

        Position : constant Gov_Dep_Lemma_Link_To_Cluster_Index_Maps.Cursor := Map.Find(Key);
    begin
        if Gov_Dep_Lemma_Link_To_Cluster_Index_Maps.Has_Element(Position) then
            Map.Replace_Element(Position, New_Item);
        else
            Map.Insert(Key, New_Item);
        end if;
    end Insert_Replace;

    ----------------
    procedure Insert
    ----------------
      (Map      : in out Argument_Cluster_Index_To_Atom_ID_Set_Maps.Map;
       Key      : in     Argument_Cluster_Index_Type;
       Position : out    Argument_Cluster_Index_To_Atom_ID_Set_Maps.Cursor) is

        Inserted : Boolean;
    begin
        Map.Insert
          (Key      => Key,
           Position => Position,
           Inserted => Inserted);

        if not Inserted then
            raise MLSE_Insertion_Error;
        end if;
    end Insert;

    --------------------
    procedure Add_To_Set
    --------------------
      (Map          : in out Argument_Cluster_Index_To_Atom_ID_Set_Maps.Map;
       Map_Cursor   : in     Argument_Cluster_Index_To_Atom_ID_Set_Maps.Cursor;
       Set_New_Item : in     Atom_ID_Type) is

        procedure Update
          (Argument_Cluster_Index       : in     Argument_Cluster_Index_Type;
           Atom_ID_Set                  : in out Atom_ID_Sets.Set) is
            pragma Unreferenced (Argument_Cluster_Index);
            Position : Atom_ID_Sets.Cursor;
            Inserted : Boolean;
        begin
            Atom_ID_Set.Insert
              (New_Item => Set_New_Item,
               Position => Position,
               Inserted => Inserted);
        end Update;
        pragma Inline(Update);

    begin
        Map.Update_Element(Map_Cursor, Update'Access);
    end Add_To_Set;

    ----------------
    procedure Insert
    ----------------
      (Map      : in out Atom_ID_To_Argument_Cluster_Index_To_Atom_ID_Set_Map_Maps.Map;
       Key      : in     Atom_ID_Type;
       Position : out    Atom_ID_To_Argument_Cluster_Index_To_Atom_ID_Set_Map_Maps.Cursor) is

        Inserted : Boolean;
    begin
        Map.Insert
          (Key      => Key,
           Position => Position,
           Inserted => Inserted);

        if not Inserted then
            raise MLSE_Insertion_Error;
        end if;
    end Insert;

    ----------------
    procedure Insert
    ----------------
      (Set      : in out Question_Sets.Set;
       New_Item : in     Question_Type) is

        Position : Question_Sets.Cursor;
        Inserted : Boolean;
    begin
        Set.Insert
          (New_Item => New_Item,
           Position => Position,
           Inserted => Inserted);
    end Insert;

    ----------------
    procedure Insert
    ----------------
      (Map      : in out Question_To_Answer_Set_Maps.Map;
       Key      : in     Question_Type;
       Position : out    Question_To_Answer_Set_Maps.Cursor) is

        Inserted : Boolean;
    begin
        Map.Insert
          (Key      => Key,
           Position => Position,
           Inserted => Inserted);

        if not Inserted then
            raise MLSE_Insertion_Error;
        end if;
    end Insert;

    --------------------
    procedure Add_To_Set
    --------------------
      (Map          : in out Question_To_Answer_Set_Maps.Map;
       Map_Cursor   : in     Question_To_Answer_Set_Maps.Cursor;
       Set_New_Item : in     Answer_Type) is

        procedure Update
          (Question       : in     Question_Type;
           Answer_Set     : in out Answer_Sets.Set) is
            pragma Unreferenced (Question);
            Position : Answer_Sets.Cursor;
            Inserted : Boolean;
        begin
            Answer_Set.Insert
              (New_Item => Set_New_Item,
               Position => Position,
               Inserted => Inserted);
        end Update;
        pragma Inline(Update);

    begin
        Map.Update_Element(Map_Cursor, Update'Access);
    end Add_To_Set;

end USP_Eval;
