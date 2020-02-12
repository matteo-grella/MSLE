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

--  with Gnat.IO; use Gnat.IO;
--  with Gnat.Debug_Pools;
--  with Ada.IO_Exceptions;

--with Strings_Edit.UTF8.Mapping;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Unchecked_Deallocation;
with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Vectors;
with Ada.Strings.Hash;
with Ada.Strings.Unbounded;

private with Ada.Real_Time;

--with Unicode;

--  with GNAT.Debug_Pools; use GNAT.Debug_Pools;

package Utils is

    type Access_To_String is access all String;
        -- Debug Pool
    --Pool : GNAT.Debug_Pools.Debug_Pool;
    --for Access_To_String'Storage_Pool use Pool;
    --procedure Debug_Pool_Info is new GNAT.Debug_Pools.Print_Info(Put_Line);
    -- catch exception GNAT.Debug_Pools.Accessing_Not_Allocated_Storage


    procedure Unchecked_String_Deallocation is
      new Ada.Unchecked_Deallocation(String, Access_To_String);

    subtype U_String is Ada.Strings.Unbounded.Unbounded_String;


    package Hashed_String_Maps is
      new Ada.Containers.Indefinite_Hashed_Maps
        (Key_Type => String,
         Element_Type => Access_To_String,
         Hash => Ada.Strings.Hash,
         Equivalent_Keys => "=");

    package Access_To_String_Vectors is
      new Ada.Containers.Vectors
        (Index_Type   => Natural,
         Element_Type => Access_To_String);

    package String_Vectors is
      new Ada.Containers.Indefinite_Vectors
        (Index_Type   => Natural,
         Element_Type => String);

    package Integer_Vectors is new Ada.Containers.Vectors(Natural, Integer);

    ---

    package String_To_Natural_Hashed_Maps is
      new Ada.Containers.Indefinite_Hashed_Maps
        (Key_Type        => String,
         Element_Type    => Natural,
         Hash            => Ada.Strings.Hash,
         Equivalent_Keys => "=");

    package String_To_Integer_Hashed_Maps is
      new Ada.Containers.Indefinite_Hashed_Maps
        (Key_Type        => String,
         Element_Type    => Integer,
         Hash            => Ada.Strings.Hash,
         Equivalent_Keys => "=");

    procedure Print_Map(Map : in String_To_Integer_Hashed_Maps.Map);
    ---

    function Natural_Hash(N : in Natural) return Ada.Containers.Hash_Type;

    package Natural_To_Natural_Hashed_Maps is
      new Ada.Containers.Hashed_Maps
        (Key_Type        => Natural,
         Element_Type    => Natural,
         Hash            => Natural_Hash, -- Simple cast
         Equivalent_Keys => "=");

    package Natural_Natural_Vectors is
      new Ada.Containers.Indefinite_Vectors
        (Index_Type   => Natural,
         Element_Type => Natural);

    ---

    function Is_Null(Acc_Str : Access_To_String) return Boolean;

    function In_Str(Source : String; Pattern : String) return Boolean;
    function String_Equal(String_A_Access : Access_To_String; String_B_Fixed : String) return Boolean;
    function String_Equal(String_A_Access : Access_To_String; String_B_Access : Access_To_String) return Boolean;
    procedure String_Replace(S: in out U_String; Pattern, Replacement: String);

    function File_To_String(File_Name : in String) return Access_To_String;
    function File_To_String(File_Name : in String) return String;

    function Does_File_Exist(File_Name : String) return Boolean;

    procedure Print_Percent_Value
      (Label     : in String;
       Cur_Value : in Natural;
       Max_Value : in Natural);

    procedure Print_Percent_Value
      (Label     : in String;
       Cur_Value : in Float;
       Max_Value : in Float);

    function Int_Img (I : Integer) return String;
    function Nat_Img (I : Integer) return String;
    function Float_Img (F : Float) return String;

	--------------
	-- Chronometer
 	--------------
    type Chronometer_Type is tagged private;
    procedure Chronometer_Start(C : out Chronometer_Type);
    procedure Chronometer_Stop (C : out Chronometer_Type);
    procedure Chronometer_Print(C : in Chronometer_Type; S : in String := "Elapsed time: ");
    procedure Chronometer_Stop_And_Print(C : in out Chronometer_Type; S : in String := "Elapsed time: ");
    function Chronometer_Get_Time(C : in Chronometer_Type) return Float;

	-------------------
	-- String Tokenizer
	-------------------
    type String_Tokenizer_Type is tagged private;
    procedure Tokenizer_Start(String_Tokenizer : out String_Tokenizer_Type; Str : in String);
    function Tokenizer_Has_Next(String_Tokenizer : in String_Tokenizer_Type; Str : in String) return Boolean;
    procedure Tokenizer_Next(String_Tokenizer : in out String_Tokenizer_Type; Str, Separator : in String; First, Last : out Natural);
    Tokenizer_Cant_Next : exception;

	------------------
	-- UTF-8 utilities
	------------------

--      function Uni2Str
--        (C : in Unicode.Unicode_Char) return String;

    function Levenshtein_Reverse_Edit_List(Left, Right : String) return String;
    function Levenshtein_Edit_List(Left, Right : String) return String;

    function Search_Regexp (Pattern : String; Search_In : String) return Boolean;


--      function To_Lowercase
--        (Str : in String) return String;
--      -- Returns the lowercase utf-8 string version of Form.

    --------------------

    function Get_Sequential_Number return Natural;

    procedure String_To_Float(S : in String; F: out Float; Error : out Boolean);

    function Long_Float_Img(N : in Long_Float; Dec : in Natural := 5) return String;

    procedure Press_A_Key_To_Continue;

     -- Mutex

    protected type Mutex_Type is
        entry Seize;
        procedure Release;
    private
        Owned : Boolean := False;
    end Mutex_Type;

    type Mutex_Access_Type is access all Mutex_Type;

private

    Sequential_Number : Natural := 0;

    pragma Inline(Is_Null);

    type Chronometer_Type is tagged record
        Start_Time : Ada.Real_Time.Time;
        End_Time   : Ada.Real_Time.Time;
    end record;


    type String_Tokenizer_Type is tagged record
        Last_Index : Natural;
    end record;

    pragma Inline(Tokenizer_Start);
    pragma Inline(Tokenizer_Has_Next);
    pragma Inline(Tokenizer_Next);

--      pragma Inline(Uni2Str);

end Utils;
