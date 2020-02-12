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

with Ada.Directories;
with Ada.Direct_IO;

with Ada.Text_IO;
with Ada.Float_Text_IO;
with Ada.Strings.Fixed;

--  with Unicode.CES.Utf8;

with GNAT.Regpat;   use GNAT.Regpat;


package body Utils is

    function Is_Null(Acc_Str : Access_To_String) return Boolean is
    begin
        return Acc_Str = null;
    end Is_Null;

    function String_Equal(String_A_Access : Access_To_String; String_B_Access : Access_To_String) return Boolean is
    begin
        return not Is_Null(String_A_Access) and then not Is_Null(String_B_Access) and then String_A_Access.all = String_B_Access.all;
    end String_Equal;

    function String_Equal(String_A_Access : Access_To_String; String_B_Fixed : String) return Boolean is
    begin
        return not Is_Null(String_A_Access) and then String_A_Access.all = String_B_Fixed;
    end String_Equal;

--      function To_Lowercase
--        (Str : in String) return String is
--      begin
--          return Strings_Edit.UTF8.Mapping.To_Lowercase(Str);
--      exception
--          when ADA.IO_EXCEPTIONS.DATA_ERROR =>
--              raise ADA.IO_EXCEPTIONS.DATA_ERROR with "Str = """ & Str & """";
--      end To_Lowercase;

    function In_Str(Source : String; Pattern : String) return Boolean is
    begin
        return Ada.Strings.Fixed.Index(Source => Source, Pattern => Pattern) > 0;
    end In_Str;

--      function "+"(S: String) return U_String renames
--        Ada.Strings.Unbounded.To_Unbounded_String;
--      function "-"(U: U_String) return String renames
--        Ada.Strings.Unbounded.To_String;

    function Natural_Hash(N : in Natural) return Ada.Containers.Hash_Type is
    begin
        return Ada.Containers.Hash_Type(N);
    end Natural_Hash;

    procedure String_Replace(S: in out U_String; Pattern, Replacement: String) is
    -- example: if S is "Mary had a XX lamb", then String_Replace(S, "X", "little");
    --          will turn S into "Mary had a littlelittle lamb"
    --          and String_Replace(S, "Y", "small"); will not change S

        Index : Natural;
    begin
        loop
            Index := Ada.Strings.Unbounded.Index(Source => S, Pattern => Pattern);
            exit when Index = 0;
            Ada.Strings.Unbounded.Replace_Slice
              (Source => S, Low => Index, High => Index+Pattern'Length-1,
               By     => Replacement);
        end loop;
    end String_Replace;

    function Search_Regexp (Pattern : String; Search_In : String) return Boolean is
        Re      : constant Pattern_Matcher := Compile (Pattern);
        Matches : Match_Array (0 .. 1);
    begin
        Match (Re, Search_In, Matches);
        if Matches (0) = No_Match then
            return False;
            --Put_Line ("The pattern did not match");
        else
            return True;
            --Put_Line ("The application really is " & Search_In (Matches (1).First .. Matches (1).Last));
        end if;
    end Search_Regexp;

    function File_To_String(File_Name : in String) return Access_To_String is
        subtype File_String    is String (1 .. Natural(Ada.Directories.Size(File_Name)));
        package File_String_IO is new Ada.Direct_IO (File_String);
        File : File_String_IO.File_Type;
        Str  : Access_To_String;
    begin
        Str := new String(1 .. File_String'Last);
        File_String_IO.Open (File, File_String_IO.In_File, File_Name);
        File_String_IO.Read (File, Str.all);
        File_String_IO.Close(File);
        return Str;
    end File_To_String;

    function File_To_String(File_Name : in String) return String is
        subtype File_String    is String (1 .. Natural(Ada.Directories.Size(File_Name)));
        package File_String_IO is new Ada.Direct_IO (File_String);
        File : File_String_IO.File_Type;
        Str  : String(1 .. File_String'Last);
    begin
        File_String_IO.Open (File, File_String_IO.In_File, File_Name);
        File_String_IO.Read (File, Str);
        File_String_IO.Close(File);
        return Str;
    end File_To_String;

    function Does_File_Exist (File_Name : String) return Boolean is
        File : Ada.Text_IO.File_Type;
    begin
        Ada.Text_IO.Open(File, Ada.Text_IO.In_File, File_Name);
        Ada.Text_IO.Close(File);
        return True;
    exception
        when Ada.Text_IO.Name_Error => return False;
    end Does_File_Exist;

    procedure Print_Percent_Value
      (Label     : in String;
       Cur_Value : in Natural;
       Max_Value : in Natural) is
    begin
        Ada.Text_IO.Put(Ada.Text_IO.Standard_Error, Label);
        Ada.Float_Text_IO.Put(Ada.Text_IO.Standard_Error, Float(Cur_Value) * 100.0 / Float(Max_Value), 3, 3, 0);
        Ada.Text_IO.Put_Line(Ada.Text_IO.Standard_Error, "% (" & Nat_Img(Cur_Value) & "/" & Nat_Img(Max_Value) & ")");
    end Print_Percent_Value;

    procedure Print_Percent_Value
      (Label     : in String;
       Cur_Value : in Float;
       Max_Value : in Float) is
    begin
        Ada.Text_IO.Put(Ada.Text_IO.Standard_Error, Label);
        Ada.Float_Text_IO.Put(Ada.Text_IO.Standard_Error, Cur_Value * 100.0 / Max_Value, 3, 3, 0);
        Ada.Text_IO.Put_Line(Ada.Text_IO.Standard_Error, "% (" & Float_Img(Cur_Value) & "/" & Float_Img(Max_Value) & ")");
    end Print_Percent_Value;

    function Int_Img (I : Integer) return String is
        Str : constant String := Integer'Image(I);
    begin
        if Str(Str'First) = ' ' then
            declare
                Str1 : constant String(1 .. Str'Last - 1) := Str (Str'First + 1 .. Str'Last);
            begin
                return Str1;
            end;
        else
            return Str;
        end if;
    end Int_Img;

    function Nat_Img (I : Integer) return String is
        Str : constant String := Natural'Image(I);
    begin
        if Str(Str'First) = ' ' then
            declare
                Str1 : constant String(1 .. Str'Last - 1) := Str (Str'First + 1 .. Str'Last);
            begin
                return Str1;
            end;
        else
            return Str;
        end if;
    end Nat_Img;

    function Float_Img (F : Float) return String is
        Str : constant String := Float'Image(F);
    begin
        if Str(Str'First) = ' ' then
            declare
                Str1 : constant String(1 .. Str'Last - 1) := Str (Str'First + 1 .. Str'Last);
            begin
                return Str1;
            end;
        else
            return Str;
        end if;
    end Float_Img;

    -- Chronometer
    --------------

    procedure Chronometer_Start(C : out Chronometer_Type) is
    begin
        C.Start_Time := Ada.Real_Time.Clock;
    end Chronometer_Start;


    procedure Chronometer_Stop (C : out Chronometer_Type) is
    begin
        C.End_Time := Ada.Real_Time.Clock;
    end Chronometer_Stop;

    procedure Chronometer_Print(C : in Chronometer_Type; S : in String := "Elapsed time: ") is
        Dur : constant Duration
          := Ada.Real_Time.To_Duration(Ada.Real_Time."-"(C.End_Time, C.Start_Time));
    begin
        Ada.Text_IO.Put(Ada.Text_IO.Standard_Error, S);
        Ada.Float_Text_IO.Put(Ada.Text_IO.Standard_Error, Float(Dur), 1, 9, 0);
        Ada.Text_IO.Put(Ada.Text_IO.Standard_Error, " s");

        if Dur > 60.0 then
            Ada.Text_IO.Put(Ada.Text_IO.Standard_Error, " = ");
            Ada.Float_Text_IO.Put(Ada.Text_IO.Standard_Error, Float(Dur/60.0), 1, 9, 0);
            Ada.Text_IO.Put(Ada.Text_IO.Standard_Error, " m");
        end if;

        Ada.Text_IO.New_Line(Ada.Text_IO.Standard_Error);

    end Chronometer_Print;

    procedure Chronometer_Stop_And_Print(C : in out Chronometer_Type; S : in String := "Elapsed time: ") is
    begin
        Chronometer_Stop(C);
        Chronometer_Print(C, S);
    end Chronometer_Stop_And_Print;

    function Chronometer_Get_Time(C : in Chronometer_Type) return Float is
    begin
        return Float(Ada.Real_Time.To_Duration(Ada.Real_Time."-"(C.End_Time, C.Start_Time)));
    end Chronometer_Get_Time;

    -------------------
    -- String Tokenizer
    -------------------

    procedure Tokenizer_Start(String_Tokenizer : out String_Tokenizer_Type; Str : in String) is
    begin
        String_Tokenizer.Last_Index := Str'First - 1;
    end Tokenizer_Start;

    function Tokenizer_Has_Next(String_Tokenizer : in String_Tokenizer_Type; Str : in String) return Boolean is
    begin
        return String_Tokenizer.Last_Index <= Str'Last;
    end Tokenizer_Has_Next;

    procedure Tokenizer_Next(String_Tokenizer : in out String_Tokenizer_Type; Str, Separator : in String; First, Last : out Natural) is
        Pos : Natural;
    begin
        if String_Tokenizer.Last_Index > Str'Last then
            raise Tokenizer_Cant_Next;
        end if;

        First := String_Tokenizer.Last_Index + 1;
        Pos := Ada.Strings.Fixed.Index(Str(First .. Str'Last), Separator);
        if Pos < First then
            Last := Str'Last;
            String_Tokenizer.Last_Index := Str'Last + 1;
        else
            Last := Pos - 1;
            String_Tokenizer.Last_Index := Pos + Separator'Length - 1;
        end if;
    end Tokenizer_Next;

    ------------------
    -- UTF-8 utilities
    ------------------

--      function Uni2Str
--        (C : in Unicode.Unicode_Char) return String is
--          S : String(1 .. 10);
--          I : Natural;
--      begin
--          I := S'First - 1;
--          Unicode.CES.Utf8.Encode(C, S, I);
--          return S(1 .. I);
--      end Uni2Str;

    -------------------
    -- String utils
    -------------------

    function Levenshtein_Edit_List(Left, Right : String) return String is
        D                 : array(0 .. Left'Last, 0 .. Right'Last) of Natural;
        Cur_X, Cur_Y      : Natural;
        Str_Out           : String(1 .. ((Left'Last + Right'Last) * 2));
        SO_Start          : Natural := 0;
        SO_Last_Not_Equal : Natural := 0;
    begin

        if Left = Right then
            return "";
        end if;

        for I in D'range(1) loop
            D(I, 0) := I;
        end loop;

        for J in D'range(2) loop
            D(0, J) := J;
        end loop;

        for I in Left'range loop
            for J in Right'range loop
                D(I, J) := Natural'Min(D(I - 1, J), D(I, J - 1)) + 1;
                D(I, J) := Natural'Min(D(I, J), D(I - 1, J - 1) + Boolean'Pos(Left(I) /= Right(J)));
            end loop;
        end loop;

        Cur_X := D'Last(1);
        Cur_Y := D'Last(2);
        SO_Start := Str_Out'Last + 1;

        while Cur_X > 0 or Cur_Y > 0 loop

            SO_Start := SO_Start - 2;

            if (Cur_X > 0 and Cur_Y > 0) and then (D(Cur_X - 1, Cur_Y - 1) <= D(Cur_X - 1, Cur_Y) and D(Cur_X - 1, Cur_Y - 1) <= D(Cur_X, Cur_Y - 1)) then
                -- Diag
                if Left(Cur_X) = Right(Cur_Y) then
                    Str_Out(SO_Start .. SO_Start + 1) := "=" & Right(Cur_Y);
                else
                    Str_Out(SO_Start .. SO_Start + 1) := "#" & Right(Cur_Y);
                    SO_Last_Not_Equal := SO_Start;
                end if;
                Cur_X := Cur_X - 1;
                Cur_Y := Cur_Y - 1;

            elsif Cur_X > 0 and then (Cur_Y = 0 or else (D(Cur_X - 1, Cur_Y) <= D(Cur_X, Cur_Y - 1) and D(Cur_X - 1, Cur_Y) <= D(Cur_X - 1, Cur_Y - 1))) then
                -- Left
                Str_Out(SO_Start .. SO_Start + 1) := "--";
                SO_Last_Not_Equal := SO_Start;
                Cur_X := Cur_X - 1;

            elsif Cur_Y > 0 and then (Cur_X = 0 or else (D(Cur_X, Cur_Y - 1) <= D(Cur_X - 1, Cur_Y) and D(Cur_X, Cur_Y - 1) <= D(Cur_X - 1, Cur_Y - 1))) then
                -- Up
                Str_Out(SO_Start .. SO_Start + 1) := "+" & Right(Cur_Y);
                SO_Last_Not_Equal := SO_Start;
                Cur_Y := Cur_Y - 1;
            else
                raise Constraint_Error with "Levenshtein_Edit_List: no condition satisfied.";
                --T_IO.Put_Line("Un po' un problema...");
            end if;
        end loop;

        return Str_Out(SO_Last_Not_Equal .. Str_Out'Last) & D(D'Last(1), D'Last(2))'Img;
    end Levenshtein_Edit_List;

    function Levenshtein_Reverse_Edit_List(Left, Right : String) return String is
        D                 : array(0 .. Left'Last, 0 .. Right'Last) of Natural;
        Cur_X, Cur_Y      : Natural;
        Str_Out           : String(1 .. ((Left'Last + Right'Last) * 2));
        SO_Last           : Natural;
        SO_Last_Not_Equal : Natural := 0;
    begin

        if Left = Right then
            return "";
        end if;

        for I in D'range(1) loop
            D(I, 0) := I;
        end loop;

        for J in D'range(2) loop
            D(0, J) := J;
        end loop;

        for I in Left'range loop
            for J in Right'range loop
                D(I, J) := Natural'Min(D(I - 1, J), D(I, J - 1)) + 1;
                D(I, J) := Natural'Min(D(I, J), D(I - 1, J - 1) + Boolean'Pos(Left(I) /= Right(J)));
            end loop;
        end loop;

        Cur_X := D'Last(1);
        Cur_Y := D'Last(2);
        SO_Last := Str_Out'First - 1;

        while Cur_X > 0 or Cur_Y > 0 loop

            SO_Last := SO_Last + 2;

            if (Cur_X > 0 and Cur_Y > 0) and then (D(Cur_X - 1, Cur_Y - 1) <= D(Cur_X - 1, Cur_Y) and D(Cur_X - 1, Cur_Y - 1) <= D(Cur_X, Cur_Y - 1)) then
                -- Diag
                if Left(Cur_X) = Right(Cur_Y) then
                    Str_Out(SO_Last - 1 .. SO_Last) := "=" & Right(Cur_Y);
                else
                    Str_Out(SO_Last - 1 .. SO_Last) := "#" & Right(Cur_Y);
                    SO_Last_Not_Equal := SO_Last;
                end if;
                Cur_X := Cur_X - 1;
                Cur_Y := Cur_Y - 1;

            elsif Cur_X > 0 and then (Cur_Y = 0 or else (D(Cur_X - 1, Cur_Y) <= D(Cur_X, Cur_Y - 1) and D(Cur_X - 1, Cur_Y) <= D(Cur_X - 1, Cur_Y - 1))) then
                -- Left
                Str_Out(SO_Last - 1 .. SO_Last) := "--";
                SO_Last_Not_Equal := SO_Last;
                Cur_X := Cur_X - 1;

            elsif Cur_Y > 0 and then (Cur_X = 0 or else (D(Cur_X, Cur_Y - 1) <= D(Cur_X - 1, Cur_Y) and D(Cur_X, Cur_Y - 1) <= D(Cur_X - 1, Cur_Y - 1))) then
                -- Up
                Str_Out(SO_Last - 1 .. SO_Last) := "+" & Right(Cur_Y);
                SO_Last_Not_Equal := SO_Last;
                Cur_Y := Cur_Y - 1;
            else
                raise Constraint_Error with "Levenshtein_Reverse_Edit_List: no condition satisfied.";
                --T_IO.Put_Line("Un po' un problema...");
            end if;
        end loop;

        return Str_Out(Str_Out'First .. SO_Last_Not_Equal) & D(D'Last(1), D'Last(2))'Img;
    end Levenshtein_Reverse_Edit_List;

    --	declare
    --		S1 : String := "frollini";
    --		--S2 : String := "forlilni";
    --		S2 : String := "frolilni";
    --	begin
    --
    --		T_IO.Put_Line(Utils.Levenshtein_Edit_List(S1, S2));
    --		T_IO.Put_Line(Utils.Levenshtein_Reverse_Edit_List(S1, S2));
    --
    --	end;

--      procedure Permute_String (Str : in String; K : Positive := 1) is
--          S    : String(Str'Range) := Str;
--          N    : constant Positive := S'Length;
--          temp : Character;
--      begin
--          if K = N then
--              --T_IO.Put_Line(S); -- TODO: metto in hashtable da restituire?
--              null;
--          else
--              for i in K .. N loop
--                  temp := S(i);
--                  S(i) := S(K);
--                  S(K) := temp;
--                  Permute_String(S, K + 1);
--              end loop;
--          end if;
--      end Permute_String;

    function Get_Sequential_Number return Natural is
    begin
        Sequential_Number := Sequential_Number + 1;
        return Sequential_Number;
    end Get_Sequential_Number;

    procedure String_To_Float(S : in String; F: out Float; Error : out Boolean) is
        After_Decimal : Boolean := False;
        Multiplier    : Float   := 0.1;
    begin

        F := 0.0;

        if S'Length = 0
          or else S(S'First) in '.' | ','
          or else S(S'Last) in '.' | ','then

            Error := True;
            return;
        end if;

        for I in S'Range loop

            if S(I) in '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' then

                if not After_Decimal then
                    F := (F * 10.0) + Float'Value(S(I..I));
                else
                    F := F + (Float'Value(S(I..I)) * Multiplier);
                    Multiplier := Multiplier * 0.1;
                end if;

            elsif S(I) in '.' | ',' then
                if After_Decimal then
                    F := 0.0;
                    Error := True;
                    return;
                else
                    After_Decimal := True;
                end if;
            else
                F := 0.0;
                Error := True;
                return;
            end if;
        end loop;

        Error := False;
    end String_To_Float;

    procedure Print_Map(Map : in String_To_Integer_Hashed_Maps.Map) is
        use String_To_Integer_Hashed_Maps;
        Index : Cursor;
    begin
        Index := Map.First;
        while Index /= No_Element loop
            Ada.Text_IO.Put_Line (Key (Index) & Integer'Image (Element (Index)));
            Index := Next (Index);
        end loop;
    end Print_Map;

    function Long_Float_Img(N : in Long_Float; Dec : in Natural := 5) return String is
        use Ada.Strings.Unbounded;
        package F_IO is new Ada.Text_IO.Float_IO(Long_Float);
        S : String(1 .. 50);
    begin
        F_IO.Put(To   => S,
                 Item => N,
                 Aft  => Dec,
                 Exp  => 0);
        return To_String(Trim(To_Unbounded_String(S), Ada.Strings.Both));
    end Long_Float_Img;

    ------------------------------------
    procedure Press_A_Key_To_Continue is
    ------------------------------------
        C : Character;
        pragma Unreferenced(C);
    begin
        Ada.Text_IO.Put_Line("Press a key to continue...");
        Ada.Text_IO.Get_Immediate(C);
    end Press_A_Key_To_Continue;

    ----------------------------
    protected body Mutex_Type is
    ----------------------------
        entry Seize when not Owned is
        begin
            Owned := True;
        end Seize;
        procedure Release is
        begin
            Owned := False;
        end Release;
    end Mutex_Type;

end Utils;

