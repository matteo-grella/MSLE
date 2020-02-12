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
with Ada.Directories; use Ada.Directories;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Utils; use Utils;

package body Corpus is

    package T_IO renames Ada.Text_IO;
    function UStr(S : String) return Unbounded_String renames To_Unbounded_String;

    procedure Read_Articles
      (Directory_Name   : in     String;
       Articles         : in out Article_Sets.Set;
       Ignore_Deprel    : in      Boolean := False) is

        Pattern : constant String := ""; -- empty pattern = all file names/subdirectory names
        Search  : Search_Type;
        Dir_Ent : Directory_Entry_Type;
    begin

        Start_Search (Search, Directory_Name, Pattern);

        while More_Entries (Search) loop
            Get_Next_Entry (Search, Dir_Ent);
            if Simple_Name(Dir_Ent)(Simple_Name(Dir_Ent)'First) /= '.' then
                declare
                    Article   : Article_Type;
                    File_Name : constant String := Simple_Name(Dir_Ent);
                begin
                    T_Io.Put_Line(T_Io.Standard_Error, ">"&Full_Name(Dir_Ent));
                    Article.ID := Article_Index_Type'Value(File_Name(File_Name'First .. File_Name'Last - 4)); -- .txt
                    Read_Sentences_From_Conll_File(Full_Name(Dir_Ent), Article.Sentences, Ignore_Deprel);
                    Articles.Insert(Article);

                end;
            end if;
        end loop;

        End_Search (Search);

    end Read_Articles;

    procedure Print (Articles : Article_Sets.Set) is

        function To_String(Word_Index_Set : in Word_Index_Sets.Set) return String is
            Ret : Unbounded_String;
        begin
            for WI of Word_Index_Set loop
                Ret := Ret & " " & Str(Integer(WI));
            end loop;
            return Trim(To_String(Ret), Ada.Strings.Both);
        end To_String;
    begin
        for Article of Articles loop
            T_IO.Put_Line("-- Article: " & Article.ID'Img);

            for Sentence of Article.Sentences loop
                for Word of Sentence loop
                    T_IO.Put_Line
                      (Str(Natural(Word.ID)) & ASCII.HT
                       & To_String(Word.Form) & ASCII.HT
                       & To_String(Word.Lemma) & ASCII.HT
                       & To_String(Word.POS) & ASCII.HT
                       & Str(Natural(Word.Head)) & ASCII.HT
                       & To_String(Word.Deprel) & ASCII.HT
                       & "[" & To_String(Word.Dependents) & "]");
                end loop;
                T_IO.New_Line;
            end loop;

            T_IO.New_Line;
        end loop;
    end Print;

    procedure Read_Sentences_From_Conll_File
      (File_Name      : in      String;
       Sentences      : in out  Sentence_Vector.Vector;
       Ignore_Deprel  : in      Boolean := False) is

        File       : Ada.Text_IO.File_Type;
        Line_Count : Natural := 0;
        Sentence   : Word_Vector.Vector;

        procedure Compute_Dependents(Sentence : in out Word_Vector.Vector) is
        begin
            for I in Sentence.First_Index .. Sentence.Last_Index loop
                declare
                    Word  : constant Word_Type := Sentence.Element(I);

                    procedure Add_Dependents(Callback_Word : in out Word_Type) is
                    begin
                        Callback_Word.Dependents.Insert(Word.ID);
                    end;
                begin
                    if (Word.Head /= -1) then
                        Sentence.Update_Element(Word.Head, Add_Dependents'Access);
                    end if;
                end;
            end loop;
        end Compute_Dependents;
        pragma Inline(Compute_Dependents);

    begin

        Sentence.Append((ID => 0, Form => Ustr("-"), Lemma => Ustr("ROOT"), POS => Ustr("-"), Deprel => Ustr("-"), Head => -1, Dependents => Word_Index_Sets.Empty_Set));

        T_IO.Open(File, T_IO.In_File, File_Name);

        while not T_IO.End_Of_File(File) loop
            declare
                Line                    : constant String := T_IO.Get_Line(File);
                TAB_SEPARATOR           : constant String := ASCII.HT & "";
                Tab_Tokenizer           : String_Tokenizer_Type;
                Token_First, Token_Last : Natural;
                Token_Counter           : Natural := 0;
            begin
                Line_Count := Line_Count + 1;


                --T_IO.Put_Line ("Reading " & Line_Count'Img & " line");

                if Line'Length = 0 then
                    Compute_Dependents(Sentence);
                    Sentences.Append(Sentence);
                    Sentence := Word_Vector.Empty_Vector;

                    Sentence.Append((ID => 0, Form => Ustr("-"), Lemma => Ustr("ROOT"), POS => Ustr("-"), Deprel => Ustr("-"), Head => -1, Dependents => Word_Index_Sets.Empty_Set));


                    if Natural(Sentences.Length) mod 50_000 = 0 then
                        T_IO.Put_Line("Reading " & Sentences.Length'Img & " sentences");
                    end if;

                    --if Natural(Sentences.Length) = 150_000 then
                    --    T_IO.Put_Line("HARD CODED STOP: " & Sentences.Length'Img);
                    --    exit; -- break
                    --end if;

                else

                    declare
                        Word : Word_Type;
                    begin
                        Tab_Tokenizer.Tokenizer_Start(Line);
                        while Tab_Tokenizer.Tokenizer_Has_Next(Line) loop
                            Tab_Tokenizer.Tokenizer_Next(Line, TAB_SEPARATOR, Token_First, Token_Last);
                            Token_Counter := Token_Counter + 1;

                            declare
                                Str : constant String := Line(Token_First .. Token_Last);
                            begin

                                case Token_Counter is

                                when 1 => -- ID
                                    Word.ID := Word_Index_Type'Value(Str);

                                when 2 => -- Form
                                    Word.Form := To_Unbounded_String(Str);

                                when 3 => -- Lemma
                                    Word.Lemma := To_Unbounded_String(Str);

                                when 4 => -- POS
                                    if Is_Content(Str) then
                                        Word.POS := To_Unbounded_String(Str(Str'First)&"");
                                    else
                                        Word.POS := To_Unbounded_String(Str);
                                    end if;

                                when 5 => -- Head
                                    if Str = "-" then
                                        Word.Head := -1;
                                    else
                                        Word.Head := Word_Index_Type'Value(Str);
                                    end if;

                                when 6 => -- Deprel
                                    Word.Deprel := To_Unbounded_String(Str);

                                when others => null;

                                end case;
                            end;
                        end loop;

                        if Ignore_Deprel and then Ignored_Deprel(To_String(Word.Deprel)) then
                            Word.Head   := -1;
                            Word.Deprel := UStr("-");
                        end if;

                        if Ignore_Deprel and then Ignored_Lemma(To_String(Word.Lemma)) then
                            Word.Head   := -1;
                            Word.Deprel := UStr("-");
                        end if;


                        if Word.Form = "__trace__" then
                            Word.Head   := -1;
                            Word.Deprel := UStr("-");
                        end if;

                        if Index(Word.POS, "P") = 1 then -- skip Pronoun
                            Word.Head   := -1;
                            Word.Deprel := UStr("-");
                        end if;

                        Sentence.Append(Word);
                    end;
                end if;
            end;
        end loop;

        if not Sentence.Is_Empty then
            Compute_Dependents(Sentence);
            Sentences.Append(Sentence);
            Sentence := Word_Vector.Empty_Vector;
        end if;

        T_IO.Close(File);

    end Read_Sentences_From_Conll_File;

end Corpus;
