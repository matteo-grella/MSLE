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

with Ada.Containers; use Ada.Containers;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;
with Ada.Strings.Hash;
with Ada.Strings.Unbounded.Hash;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Sets;
with Ada.Text_IO;
with Ada.Streams;
with Ada.Unchecked_Deallocation;

package Commons is

    --------
    -- Utils
    --------

    function Hash(Key : String) return Hash_Type renames Ada.Strings.Hash;
    -- String Hash function.

    function Hash(Key : Unbounded_String) return Hash_Type renames Ada.Strings.Unbounded.Hash;
    -- Ubnounded_String Hash function.

    function UStr(S : String) return Unbounded_String renames To_Unbounded_String;
    -- Transforms a String into an Unbounded_String.

    function Str(S : Unbounded_String) return String renames To_String;
    -- Transforms an Unbounded_String into a String.

    function Str(I : Integer) return String;
    pragma Inline(Str);
    -- Returns a String representation of an Integer, without leading spaces.

    function Str(C : Count_Type) return String is
      (Str(Integer(C)));
    pragma Inline(Str);
    -- String representation of a Count_Type

    function Is_Content(POS : String) return Boolean is
      (POS'Length > 0 and then POS(POS'First) in 'J' | 'R' | 'V' | 'N');
    pragma Inline(Is_Content);
    -- Returns True when the specified POS is "content" (starts with 'J' | 'R' | 'V' | 'N')

    function Is_Content(POS : Unbounded_String) return Boolean is
      (Is_Content(Str(POS)));
    pragma Inline(Is_Content);
    -- Returns True when the specified POS is "content" (starts with 'J' | 'R' | 'V' | 'N')

    Max_Integer_Digits  : constant Hash_Type
      := Hash_Type(Float'Floor(Log(X => Float(Hash_Type'Last)))) + 1;
    -- Max number of digits of an Integer number.

    Half_Integer_Digits : constant Hash_Type
      := Hash_Type(Float'Floor(Float(Max_Integer_Digits)) / 2.0);
    -- Max_Integer_Digits / 2

    Half_Digits_Power   : constant Hash_Type
      := 10**Natural(Half_Integer_Digits);
    -- 10 ^ Half_Integer_Digits

    function Path_Join(A, B: in String) return String is
      (if A(A'Last) = '/' then A & B else A & '/' & B);
    pragma Inline(Path_Join);

    --------
    -- Score
    --------

    subtype Score_Type is Long_Float;
    -- Real type for USP Scores.

    package Score_Elementary_Functions is
      new Ada.Numerics.Generic_Elementary_Functions (Score_Type);
    -- Math elementary functions for Score_Type.

    function XlogX(X : in Float)      return Score_Type is
      (if X <= 0.0 then 0.0 else Score_Type(X) * Score_Elementary_Functions.Log(Score_Type(X)));
    pragma Inline(XlogX);
    -- X * Log(X)

    function XlogX(X : in Long_Float) return Score_Type is
      (if X <= 0.0 then 0.0 else X * Score_Elementary_Functions.Log(X));
    pragma Inline(XlogX);
    -- X * Log(X)

    function XlogX(X : in Integer)    return Score_Type is
      (if X <= 0 then 0.0 else Score_Type(X) * Score_Elementary_Functions.Log(Score_Type(X)));
    pragma Inline(XlogX);
    -- X * Log(X)

    package Score_Text_IO is
      new Ada.Text_IO.Float_IO (Score_Type);
    -- Test_IO for Score_Type.

    -------
    -- Word
    -------

    type Word_Index_Type is new Integer;
    -- Index of Word_Type.

    subtype Word_Index_Natural_Type is Word_Index_Type range 0 .. Word_Index_Type'Last;
    -- Word_Index_Type starting from 0.


    function Str(I : Word_Index_Type) return String is
      (Str(Integer(I)));
    pragma Inline(Str);
    -- Word_Index_Type to String.

    package Word_Index_Sets is new
      Ordered_Sets
        (Element_Type => Word_Index_Type);
    -- Ordered Sets of Word_Index_Type.

    type Word_Type is record
        ID         : Word_Index_Type;
        --
        Form       : Unbounded_String;
        --
        Lemma      : Unbounded_String;
        --
        POS        : Unbounded_String;
        --
        Deprel     : Unbounded_String;
        --
        Head       : Word_Index_Type;
        --
        Dependents : Word_Index_Sets.Set;
        --
    end record;
    -- A Word (or token) from a parsed corpus, with standard informations and
    -- a set of Dependents Indexes.

    function To_String(Word : Word_Type) return String is
      (To_String(Word.POS & ":" & Word.Lemma));
    pragma Inline(To_String);
    -- String representation of Word_Type: "POS:LEMMA". Word Form is ignored.
    -- TODO: Si potrebbe rendere piu' efficiente?

    function Hash(Word : Word_Type) return Hash_Type is
      (Ada.Strings.Hash(To_String(Word)));
    pragma Inline(Hash);
    -- Hash function for Word_Type.
    -- TODO: Si potrebbe rendere piu' efficiente?

    function "=" (Left, Right : Word_Type) return Boolean is
      (Left.Lemma = Right.Lemma and then Left.POS = Right.POS);
    pragma Inline("=");
    -- Compares Lemma and POS only.

    function "<" (Left, Right : Word_Type) return Boolean is
      (if Left.POS = Right.POS then Left.Lemma < Right.Lemma else Left.POS < Right.POS);
    pragma Inline("<");
    -- Compares Lemma and POS only.

    -----------
    -- Sentence
    -----------

    package Word_Vector is
      new Ada.Containers.Vectors
        (Index_Type   => Word_Index_Natural_Type,
         Element_Type => Word_Type);
    -- A vector of words.

    type Sentence_Index_Type is new Integer;
    -- Index of a sentence.

    subtype Sentence_Index_Natural_Type is Sentence_Index_Type range 0 .. Sentence_Index_Type'Last;
    -- Sentence_Index_Type starting from 0.

    -----------
    -- Article
    -----------

    package Sentence_Vector is
      new Ada.Containers.Vectors
        (Index_Type   => Sentence_Index_Natural_Type,
         Element_Type => Word_Vector.Vector,
         "="          => Word_Vector."=");
    -- A vector of sentences (word vectors).

    type Article_Index_Type is new Integer;
    -- Index of an Article

    type Article_Type is record
        ID        : Article_Index_Type;
        --
        Sentences : Sentence_Vector.Vector;
        --
    end record;
    -- An Article

    function "=" (Left, Right : Article_Type) return Boolean is
      (Left.ID = Right.ID);
    pragma Inline("=");
    -- Compares articles' IDs.

    function "<" (Left, Right : Article_Type) return Boolean is
      (Left.ID < Right.ID);
    pragma Inline("<");
    -- Compares articles' IDs.

    package Article_Sets is
      new Ada.Containers.Ordered_Sets
        (Element_Type => Article_Type);
    -- Sets of Article_Type

    package Article_Index_To_Article_Maps is
      new Ada.Containers.Ordered_Maps
        (Key_Type     => Article_Index_Type,
         Element_Type => Article_Type);
    -- Article_Index => Article_Type maps.

    -------------
    --- Atom
    -------------

    type Atom_Index_Type is new Integer;
    -- Index of an Atom.

    type Atom_Type;
    -- An Atom.

    type Atom_Access_Type is access Atom_Type;
    -- Atom_Type access.

    function "=" (Left, Right : not null Atom_Access_Type) return Boolean;
    -- Compares the values of two Atom_Access_Types.

    function "<" (Left, Right : not null Atom_Access_Type) return Boolean;
    -- Compares the values of two Atom_Access_Types.

    package Atom_Sets is new
      Ada.Containers.Ordered_Sets
        (Element_Type => Atom_Access_Type);
    -- Sets of Atom_Access_Type.

    package Deprel_To_Atom_Set_Maps is
      new Ada.Containers.Ordered_Maps
        (Key_Type     => Unbounded_String,
         Element_Type => Atom_Sets.Set,
         "="          => Atom_Sets."=");
    -- Deprel (Unbounded_String) => Atom_Sets maps.

    procedure Insert
      (Map      : in out Deprel_To_Atom_Set_Maps.Map;
       Key      : in     Unbounded_String;
       Position : out    Deprel_To_Atom_Set_Maps.Cursor);
    pragma Inline(Insert);
    -- Insert a new item. Raise USP_Insertion_Error if not inserted.

    procedure Add_To_Set
      (Map          : in out Deprel_To_Atom_Set_Maps.Map;
       Map_Cursor   : in     Deprel_To_Atom_Set_Maps.Cursor;
       Set_New_Item : in     Atom_Access_Type);
    pragma Inline(Add_To_Set);
    -- Insert a new item in the set at Map_Cursor position. Ignore duplicates.

    type Atom_ID_Type is record
        Article_ID  : Article_Index_Type  := -1;
        --
        Sentence_ID : Sentence_Index_Type := -1;
        --
        Word_ID     : Word_Index_Type     := -1;
        --
    end record;
    -- Atom ID = Article_ID + Sentence_ID + Word_ID

    function To_String (Atom_ID : Atom_ID_Type) return String is
      (Str(Integer(Atom_ID.Article_ID)) & ":" & Str(Integer(Atom_ID.Sentence_ID)) & ":" & Str(Integer(Atom_ID.Word_ID)));

    --      function To_String
    --        (Atom_ID : Atom_ID_Type) return String;
    --      pragma Inline(To_String);
    --      -- String representation of Atom_ID.

    function From_String
      (S : String) return Atom_ID_Type;
    pragma Inline(From_String);
    -- String representation of Atom_ID.

    function Hash(Atom_ID : Atom_ID_Type) return Hash_Type;
    pragma Inline(Hash);
    -- Hash function for Atom_ID.

    function "<" (Left, Right : Atom_ID_Type) return Boolean is
      (if Left.Article_ID = Right.Article_ID then
         (if Left.Sentence_ID = Right.Sentence_ID then
          Left.Word_ID < Right.Word_ID
          else
          Left.Sentence_ID < Right.Sentence_ID)
       else
       Left.Article_ID < Right.Article_ID);
    pragma Inline("<");
    -- Compares Article ID, Sentence ID and Word ID.

    --        (if Left.Article_ID = Right.Article_ID and then Left.Sentence_ID = Right.Sentence_ID then
    --         Left.Word_ID < Right.Word_ID
    --         elsif Left.Article_ID = Right.Article_ID then
    --         Left.Sentence_ID < Right.Sentence_ID
    --         else
    --         Left.Article_ID < Right.Article_ID);

    function ">" (Left, Right : Atom_ID_Type) return Boolean is
      (Right < Left);
    pragma Inline(">");
    -- Compares Article ID, Sentence ID and Word ID.

    function ">=" (Left, Right : Atom_ID_Type) return Boolean is
      (Left > Right or else Left = Right);
    pragma Inline(">=");

    function "<=" (Left, Right : Atom_ID_Type) return Boolean is
      (Left < Right or else Left = Right);
    pragma Inline("<=");

    type Atom_Type is record
        ID             : Atom_ID_Type := (-1, -1, -1);
        --
        Form           : Unbounded_String;
        --
        Lemma          : Unbounded_String;
        --
        POS            : Unbounded_String;
        --
        Dependents     : Deprel_To_Atom_Set_Maps.Map;
        --
    end record;
    -- An Atom.

    procedure Free is new Ada.Unchecked_Deallocation
      (Atom_Type, Atom_Access_Type);

    function To_String(Atom : Atom_Type) return String is
      (To_String(Atom.POS & ":" & Atom.Lemma));
    pragma Inline(To_String);
    -- String representation of Atom_Type: "POS:LEMMA". Atom Form is ignored.
    -- TODO: Si potrebbe rendere piu' efficiente?

    function Hash(Atom : Atom_Type) return Hash_Type is
      (Ada.Strings.Hash(To_String(Atom)));
    pragma Inline(Hash);
    -- Hash function for Atom_Type.

    function "=" (Left, Right : not null Atom_Access_Type) return Boolean is
      (Left.Lemma = Right.Lemma and then Left.POS = Right.POS);
    pragma Inline("=");
    -- Compares Atom Lemma and POS.

    function "<" (Left, Right : not null Atom_Access_Type) return Boolean is
      (Left.Lemma < Right.Lemma or else Left.POS < Right.POS);
    pragma Inline("<");
    -- Compares Atom Lemma and POS.

    package Atom_ID_To_Atom_Maps is new
      Ada.Containers.Hashed_Maps
        (Key_Type        => Atom_ID_Type,
         Element_Type    => Atom_Access_Type,
         Hash            => Hash,
         Equivalent_Keys => "=");
    -- Atom_ID_Type => Atom_Access_Type maps.

    package Atom_Index_Sets is new
      Ada.Containers.Ordered_Sets
        (Element_Type => Atom_Index_Type);
    -- Sets of Atom_Index_Type.

    package Atom_ID_Sets is new
      Ada.Containers.Ordered_Sets
        (Element_Type => Atom_ID_Type);
    -- Sets if Atom_ID_Type.

    procedure Insert
      (Set      : in out Atom_ID_Sets.Set;
       New_Item : in     Atom_ID_Type);
    pragma Inline(Insert);
    -- Insert a new item. Ignores duplicates.

    type Atom_Link_Type is record
        Governor_Atom_ID  : Atom_ID_Type;
        --
        Dependent_Atom_ID : Atom_ID_Type;
        --
    end record;
    -- Pairs Governor-Dependent Atom_IDs.

    function Hash(Atom_Link : Atom_Link_Type) return Hash_Type is
      (Hash(Atom_Link.Governor_Atom_ID) * 101 + Hash(Atom_Link.Dependent_Atom_ID));
    pragma Inline(Hash);
    -- Hash function for Atom_Link_Type.

    function "<"(Left, Right : in Atom_Link_Type) return Boolean is
      (if Left.Governor_Atom_ID = Right.Governor_Atom_ID then Left.Dependent_Atom_ID < Right.Dependent_Atom_ID else Left.Governor_Atom_ID < Right.Governor_Atom_ID);
    pragma Inline("<");
    -- Compare Atom_Link_Type's Governor and Dependent Atom IDs.

    package Atom_Link_Sets_Ordered is new
      Ada.Containers.Ordered_Sets
        (Element_Type => Atom_Link_Type);
    package Atom_Link_Sets_Hashed is new
      Ada.Containers.Hashed_Sets
        (Element_Type        => Atom_Link_Type,
         Hash                => Hash,
         Equivalent_Elements => "=",
         "="                 => "=");
    package Atom_Link_Sets renames Atom_Link_Sets_Hashed;
    -- Sets of Atom_Link_Type.

    -----------
    -- Rel Type
    -----------

    type Rel_Type_Index_Type is new Integer;
    -- Index of a Rel_Type.

    subtype Rel_Type_Index_Natural_Type is Rel_Type_Index_Type range 0 .. Rel_Type_Index_Type'Last;
    -- Rel_Type_Index_Type starting from 0.

    function Hash(Key : Rel_Type_Index_Type) return Hash_Type is (Hash_Type(Key));
    pragma Inline(Hash);
    -- Hash function for Rel_Type_Index_Type.

    package Rel_Type_Str_Vectors is new
      Ada.Containers.Vectors
        (Index_Type   => Rel_Type_Index_Natural_Type,
         Element_Type => Unbounded_String);
    -- Vectors of Rel_Types (Unbounded_String).

    package Rel_Type_Str_To_Index_Maps is new
      Ada.Containers.Hashed_Maps
        (Key_Type        => Unbounded_String,
         Element_Type    => Rel_Type_Index_Type,
         Hash            => Ada.Strings.Unbounded.Hash,
         Equivalent_Keys => "=");
    -- Unbounded_String Rel_Type to Rel_Type_Index_Type maps.

    procedure Insert
      (Map      : in out Rel_Type_Str_To_Index_Maps.Map;
       Key      : in     Unbounded_String;
       New_Item : in     Rel_Type_Index_Type;
       Position : out    Rel_Type_Str_To_Index_Maps.Cursor);
    pragma Inline(Insert);
    -- Insert a new item. Raise USP_Insertion_Error if not inserted.

    package Rel_Type_Index_To_Count_Maps_Ordered is new
      Ada.Containers.Ordered_Maps
        (Key_Type     => Rel_Type_Index_Type,
         Element_Type => Integer); -- TODO: Natural
    package Rel_Type_Index_To_Count_Maps_Hashed is new
      Ada.Containers.Hashed_Maps
        (Key_Type        => Rel_Type_Index_Type,
         Element_Type    => Integer, -- TODO: Natural
         Hash            => Hash,
         Equivalent_Keys => "=");
    package Rel_Type_Index_To_Count_Maps renames Rel_Type_Index_To_Count_Maps_Hashed;
    -- Rel_Type_Index_Type to count (Natural) maps.

    procedure Increment
      (Map      : in out Rel_Type_Index_To_Count_Maps.Map;
       Position : in     Rel_Type_Index_To_Count_Maps.Cursor);
    pragma Inline(Increment);
    -- Count +1 at Position.

    procedure Decrement
      (Map      : in out Rel_Type_Index_To_Count_Maps.Map;
       Position : in     Rel_Type_Index_To_Count_Maps.Cursor);
    pragma Inline(Decrement);
    -- Count -1 at Position.

    type Rel_Type_Index_Link_Type is record
        Governor_Rel_Type_Index  : Rel_Type_Index_Type := -1;
        --
        Dependent_Rel_Type_Index : Rel_Type_Index_Type := -1;
        --
    end record;
    -- Pairs Governor-Dependent Rel_Type_Index

    function Hash(Rel_Type_Index_Link : Rel_Type_Index_Link_Type) return Hash_Type is
      (Hash_Type(Rel_Type_Index_Link.Governor_Rel_Type_Index) * Half_Digits_Power + Hash_Type(Rel_Type_Index_Link.Dependent_Rel_Type_Index));
    pragma Precondition(Hash_Type(Rel_Type_Index_Link.Governor_Rel_Type_Index) < Half_Digits_Power and then Hash_Type(Rel_Type_Index_Link.Dependent_Rel_Type_Index) < Half_Digits_Power);
    pragma Inline(Hash);
    -- Hash function for Rel_Type_Index_Link_Type

    package Rel_Type_Index_Link_To_Count_Maps is new
      Ada.Containers.Hashed_Maps
        (Key_Type        => Rel_Type_Index_Link_Type,
         Element_Type    => Natural,
         Hash            => Hash,
         Equivalent_Keys => "=");
    -- Rel_Type_Index_Link_Type to count (Natural) maps.

    procedure Increment
      (Map      : in out Rel_Type_Index_Link_To_Count_Maps.Map;
       Position : in     Rel_Type_Index_Link_To_Count_Maps.Cursor);
    pragma Inline(Increment);
    -- Count +1 at Position.

    ---------------------------------------------
    --- Part, Cluster, Argument_Cluster
    ---------------------------------------------

    type Part_Type;
    type Cluster_Type;
    type Argument_Cluster_Type;

    type Part_Access_Type is access Part_Type;
    -- Access to Part_Type.

    type Argument_Cluster_Access_Type is access Argument_Cluster_Type;
    -- Access to Argument_Cluster_Type.

    procedure Stream_Write
      (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
       Item   : in  Argument_Cluster_Access_Type);
    pragma Inline(Stream_Write);
    for Argument_Cluster_Access_Type'Write use Stream_Write;

    procedure Stream_Output
      (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
       Item   : in  Argument_Cluster_Access_Type);
    pragma Inline(Stream_Output);
    for Argument_Cluster_Access_Type'Output use Stream_Output;

    procedure Stream_Read
      (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
       Item   : out Argument_Cluster_Access_Type);
    pragma Inline(Stream_Read);
    for Argument_Cluster_Access_Type'Read use Stream_Read;

    function Stream_Input
      (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
       return Argument_Cluster_Access_Type;
    pragma Inline(Stream_Input);
    for Argument_Cluster_Access_Type'Input use Stream_Input;

    type Part_Index_Type             is new Integer;
    -- Index of a Part.

    type Cluster_Index_Type          is new Integer;
    -- Index of a Cluster.

    function Str(I : Cluster_Index_Type) return String is
      (Str(Integer(I)));
    pragma Inline(Str);
    -- Cluster_Index_Type to String.

    type Argument_Cluster_Index_Type is new Integer;
    -- Index of an Argument_Cluster.

    function Str(I : Argument_Cluster_Index_Type) return String is
      (Str(Integer(I)));
    pragma Inline(Str);
    -- Argument_Cluster_Index_Type to String.

    function Hash(Key : Part_Index_Type) return Hash_Type is (Hash_Type(Key));
    pragma Inline(Hash);
    -- Hash function for Part_Index_Type.

    function Hash(Key : Cluster_Index_Type) return Hash_Type is (Hash_Type(Key));
    pragma Inline(Hash);
    -- Hash function for Cluster_Index_Type.

    function Hash(Key : Argument_Cluster_Index_Type) return Hash_Type is (Hash_Type(Key));
    pragma Inline(Hash);
    -- Hash function for Argument_Cluster_Index_Type.

    package Part_Index_To_Argument_Cluster_Index_Maps is new
      Ada.Containers.Hashed_Maps
        (Key_Type        => Part_Index_Type,
         Element_Type    => Argument_Cluster_Index_Type,
         Hash            => Hash,
         Equivalent_Keys => "=");
    -- Part_Index_Type to Argument_Cluster_Index_Type maps.

    package Part_Maps is
      new Ada.Containers.Ordered_Maps
        (Key_Type     => Part_Index_Type,
         Element_Type => Part_Access_Type);
    -- Part_Index_Type to Part_Access_Type maps.

    package Part_Index_Sets is new
      Ada.Containers.Ordered_Sets
        (Element_Type => Part_Index_Type);
    -- Sets of Part_Index_Type.

    package Argument_Cluster_Index_To_Part_Indexes_Maps is new
      Ada.Containers.Hashed_Maps
        (Key_Type        => Argument_Cluster_Index_Type,
         Element_Type    => Part_Index_Sets.Set,
         Hash            => Hash,
         Equivalent_Keys => "=",
         "="             => Part_Index_Sets."=");
    -- Argument_Cluster_Index_Type to Part_Index_Sets maps.

    procedure Insert
      (Map      : in out Argument_Cluster_Index_To_Part_Indexes_Maps.Map;
       Key      : in     Argument_Cluster_Index_Type;
       Position : out    Argument_Cluster_Index_To_Part_Indexes_Maps.Cursor);
    pragma Inline(Insert);
    -- Insert a new item. Raise USP_Insertion_Error if not inserted.

    procedure Add_To_Set
      (Map          : in out Argument_Cluster_Index_To_Part_Indexes_Maps.Map;
       Map_Cursor   : in     Argument_Cluster_Index_To_Part_Indexes_Maps.Cursor;
       Set_New_Item : in     Part_Index_Type);
    pragma Inline(Add_To_Set);
    -- Insert a new item in the set at Map_Cursor position. Ignore duplicates.

    type Deprel_Index_Type is new Integer;
    -- Index of Deprel.

    function Str(N : Deprel_Index_Type) return String is
      (Str(Integer(N)));
    -- Deprel_Index_Type to String.

    subtype Deprel_Index_Natural_Type is Deprel_Index_Type range 0 .. Deprel_Index_Type'Last;
    -- Deprel_Index_Type starting from 0.

    function Hash(Key : Deprel_Index_Natural_Type) return Hash_Type is (Hash_Type(Key));
    pragma Inline(Hash);
    -- Hash function for Deprel_Index_Natural_Type

    package Deprel_Vectors is
      new Ada.Containers.Vectors
        (Index_Type   => Deprel_Index_Natural_Type,
         Element_Type => Unbounded_String);
    -- Vectors of Deprels (Unbounded_Strings).

    package Deprel_To_Index_Maps is new
      Ada.Containers.Hashed_Maps
        (Key_Type        => Unbounded_String,
         Element_Type    => Deprel_Index_Type,
         Hash            => Hash,
         Equivalent_Keys => "=");
    -- Deprel (Unbounded_String) to Deprel_Index_Type maps.

    procedure Insert
      (Map      : in out Deprel_To_Index_Maps.Map;
       Key      : in     Unbounded_String;
       New_Item : in     Deprel_Index_Type;
       Position : out    Deprel_To_Index_Maps.Cursor);
    pragma Inline(Insert);
    -- Insert a new item. Raise USP_Insertion_Error if not inserted.

    package Deprel_Index_Sets is new
      Ada.Containers.Hashed_Sets
        (Element_Type        => Deprel_Index_Type,
         Hash                => Hash,
         Equivalent_Elements => "=");
    -- Sets of Deprel_Index_Type.

    procedure Insert
      (Set      : in out Deprel_Index_Sets.Set;
       New_Item : in     Deprel_Index_Type);
    pragma Inline(Insert);
    -- Insert a new item. Ignores duplicates.

    -------------------
    -- Argument_Cluster
    -------------------

    type Num_Arg_Type is new Natural;
    -- Num_Arg (a simple counter)

    function Str(N : Num_Arg_Type) return String is
      (Str(Integer(N)));
    -- Num_Arg_Type to String.

    function Hash(Key : Num_Arg_Type) return Hash_Type is (Hash_Type(Key));
    pragma Inline(Hash);
    -- Hash function for Num_Arg_Type.

    package Num_Arg_To_Count_Maps_Ordered is new
      Ada.Containers.Ordered_Maps
        (Key_Type     => Num_Arg_Type,
         Element_Type => Integer);
    package Num_Arg_To_Count_Maps_Hashed is new
      Ada.Containers.Hashed_Maps
        (Key_Type        => Num_Arg_Type,
         Element_Type    => Integer,
         Hash            => Hash,
         Equivalent_Keys => "=");
    package Num_Arg_To_Count_Maps renames Num_Arg_To_Count_Maps_Hashed;
    -- Num_Arg_Type to count maps.
    -- FIXME: come mai va a meno di zero?

    procedure Increment
      (Map      : in out Num_Arg_To_Count_Maps.Map;
       Position : in     Num_Arg_To_Count_Maps.Cursor);
    pragma Inline(Increment);
    -- Count +1 at Position.

    procedure Decrement
      (Map      : in out Num_Arg_To_Count_Maps.Map;
       Position : in     Num_Arg_To_Count_Maps.Cursor);
    pragma Inline(Decrement);
    -- Count -1 at Position.

    package Deprel_Index_To_Count_Maps_Ordered is new
      Ada.Containers.Ordered_Maps
        (Key_Type     => Deprel_Index_Type,
         Element_Type => Integer);
    package Deprel_Index_To_Count_Maps_Hashed is new
      Ada.Containers.Hashed_Maps
        (Key_Type        => Deprel_Index_Type,
         Element_Type    => Integer,
         Hash            => Hash,
         Equivalent_Keys => "=");
    package Deprel_Index_To_Count_Maps renames Deprel_Index_To_Count_Maps_Hashed;
    -- Deprel_Index_Type to count.
    -- FIXME: come mai va a meno di zero?

    procedure Increment
      (Map      : in out Deprel_Index_To_Count_Maps.Map;
       Position : in     Deprel_Index_To_Count_Maps.Cursor);
    pragma Inline(Increment);
    -- Count +1 at Position.

    procedure Decrement
      (Map      : in out Deprel_Index_To_Count_Maps.Map;
       Position : in     Deprel_Index_To_Count_Maps.Cursor);
    pragma Inline(Decrement);
    -- Count -1 at Position.

    package Argument_Cluster_Index_To_Deprel_Index_To_Count_Maps is new
      Ada.Containers.Hashed_Maps
        (Key_Type        => Argument_Cluster_Index_Type,
         Element_Type    => Deprel_Index_To_Count_Maps.Map,
         Hash            => Hash,
         Equivalent_Keys => "=",
         "="             => Deprel_Index_To_Count_Maps."=");
    -- Argument_Cluster_Index_Type to Deprel_Index_To_Count_Maps maps.

    procedure Insert
      (Map      : in out Argument_Cluster_Index_To_Deprel_Index_To_Count_Maps.Map;
       Key      : in     Argument_Cluster_Index_Type;
       Position : out    Argument_Cluster_Index_To_Deprel_Index_To_Count_Maps.Cursor);
    pragma Inline(Insert);
    -- Insert a new item. Raise USP_Insertion_Error if not inserted.

    package Cluster_Index_To_Count_Maps_Ordered is new
      Ada.Containers.Ordered_Maps
        (Key_Type     => Cluster_Index_Type,
         Element_Type => Integer);
    package Cluster_Index_To_Count_Maps_Hashed is new
      Ada.Containers.Hashed_Maps
        (Key_Type        => Cluster_Index_Type,
         Element_Type    => Integer, -- FIXME: come mai va a meno di zero?
         Hash            => Hash,
         Equivalent_Keys => "=");
    package Cluster_Index_To_Count_Maps renames Cluster_Index_To_Count_Maps_Hashed;
    -- Cluster_Index_Type to count maps.
    -- FIXME: come mai va a meno di zero?

    procedure Increment
      (Map      : in out Cluster_Index_To_Count_Maps.Map;
       Position : in     Cluster_Index_To_Count_Maps.Cursor);
    pragma Inline(Increment);
    -- Count +1 at Position.

    procedure Decrement
      (Map      : in out Cluster_Index_To_Count_Maps.Map;
       Position : in     Cluster_Index_To_Count_Maps.Cursor);
    pragma Inline(Decrement);
    -- Count -1 at Position.

    package Argument_Cluster_Index_To_Cluster_Index_To_Count_Maps is new
      Ada.Containers.Hashed_Maps
        (Key_Type        => Argument_Cluster_Index_Type,
         Element_Type    => Cluster_Index_To_Count_Maps.Map,
         Hash            => Hash,
         Equivalent_Keys => "=",
         "="             => Cluster_Index_To_Count_Maps."=");
    -- Argument_Cluster_Index_Type to Cluster_Index_To_Count_Maps maps.

    procedure Insert
      (Map      : in out Argument_Cluster_Index_To_Cluster_Index_To_Count_Maps.Map;
       Key      : in     Argument_Cluster_Index_Type;
       Position : out    Argument_Cluster_Index_To_Cluster_Index_To_Count_Maps.Cursor);
    pragma Inline(Insert);
    -- Insert a new item. Raise USP_Insertion_Error if not inserted.

    package Argument_Cluster_Index_To_Num_Arg_To_Count_Maps is new
      Ada.Containers.Hashed_Maps
        (Key_Type        => Argument_Cluster_Index_Type,
         Element_Type    => Num_Arg_To_Count_Maps.Map,
         Hash            => Hash,
         Equivalent_Keys => "=",
         "="             => Num_Arg_To_Count_Maps."=");
    -- Argument_Cluster_Index_Type to Num_Arg_To_Count_Maps maps.

    procedure Insert
      (Map      : in out Argument_Cluster_Index_To_Num_Arg_To_Count_Maps.Map;
       Key      : in     Argument_Cluster_Index_Type;
       Position : out    Argument_Cluster_Index_To_Num_Arg_To_Count_Maps.Cursor);
    pragma Inline(Insert);
    -- Insert a new item. Raise USP_Insertion_Error if not inserted.

    package Argument_Cluster_Index_To_Root_Node_Count_Maps is new
      Ada.Containers.Hashed_Maps
        (Key_Type        => Argument_Cluster_Index_Type,
         Element_Type    => Integer, -- FIXME: Come mai minore di 0??
         Hash            => Hash,
         Equivalent_Keys => "=");
    -- Argument_Cluster_Index_Type to count maps.

    procedure Increment
      (Map      : in out Argument_Cluster_Index_To_Root_Node_Count_Maps.Map;
       Position : in     Argument_Cluster_Index_To_Root_Node_Count_Maps.Cursor);
    pragma Inline(Increment);
    -- Count +1 at Position.

    procedure Decrement
      (Map      : in out Argument_Cluster_Index_To_Root_Node_Count_Maps.Map;
       Position : in     Argument_Cluster_Index_To_Root_Node_Count_Maps.Cursor);
    pragma Inline(Decrement);
    -- Count -1 at Position.

    package Argument_Cluster_Index_To_Dependents_Count_Maps is new
      Ada.Containers.Hashed_Maps
        (Key_Type        => Argument_Cluster_Index_Type,
         Element_Type    => Integer, -- FIXME: Come mai minore di 0??
         Hash            => Hash,
         Equivalent_Keys => "=");
    -- Argument_Cluster_Index_Type to count maps.

    procedure Increment
      (Map      : in out Argument_Cluster_Index_To_Dependents_Count_Maps.Map;
       Position : in     Argument_Cluster_Index_To_Dependents_Count_Maps.Cursor);
    pragma Inline(Increment);
    -- Count +1 at Position.

    procedure Decrement
      (Map      : in out Argument_Cluster_Index_To_Dependents_Count_Maps.Map;
       Position : in     Argument_Cluster_Index_To_Dependents_Count_Maps.Cursor);
    pragma Inline(Decrement);
    -- Count -1 at Position.

    -----
    -- Argument Cluster
    -----

    type Argument_Cluster_Type is record
        Deprel_Index_To_Count_Map            : Deprel_Index_To_Count_Maps.Map;
        --
        Dependent_Cluster_Index_To_Count_Map : Cluster_Index_To_Count_Maps.Map;
        --
        Dependents_Count                     : Integer := 0;
        --
        Num_Arg_Count_Map                    : Num_Arg_To_Count_Maps.Map;
        --
        Root_Node_ID_Set                     : Atom_ID_Sets.Set;
        --
    end record;
    -- An Argument_Cluster

    procedure Free is new Ada.Unchecked_Deallocation
      (Argument_Cluster_Type, Argument_Cluster_Access_Type);

    package Argument_Cluster_Maps_Ordered is
      new Ada.Containers.Ordered_Maps
        (Key_Type     => Argument_Cluster_Index_Type,
         Element_Type => Argument_Cluster_Access_Type);
    package Argument_Cluster_Maps_Hashed is
      new Ada.Containers.hashed_Maps
        (Key_Type        => Argument_Cluster_Index_Type,
         Element_Type    => Argument_Cluster_Access_Type,
         Hash            => Hash,
         Equivalent_Keys => "=");
    package Argument_Cluster_Maps renames Argument_Cluster_Maps_Hashed;
    -- Argument_Cluster_Index_Type to Argument_Cluster_Access_Type maps.

    package Argument_Cluster_Index_Sets is new
      Ada.Containers.Hashed_Sets
        (Element_Type        => Argument_Cluster_Index_Type,
         Hash                => Hash,
         Equivalent_Elements => "=");
    -- Sets of Argument_Cluster_Index_Type.

    procedure Insert
      (Set      : in out Argument_Cluster_Index_Sets.Set;
       New_Item : in     Argument_Cluster_Index_Type);
    pragma Inline(Insert);
    -- Insert a new item. Ignores duplicates.

    package Deprel_Index_To_Argument_Cluster_Index_Set_Maps is new
      Ada.Containers.Hashed_Maps
        (Key_Type        => Deprel_Index_Type,
         Element_Type    => Argument_Cluster_Index_Sets.Set,
         Hash            => Hash,
         Equivalent_Keys => "=",
         "="             => Argument_Cluster_Index_Sets."=");
    -- Deprel_Index_Type to Argument_Cluster_Index_Sets maps.

    package Argument_Cluster_Index_To_Argument_Cluster_Index_Maps_Ordered is new
      Ada.Containers.Ordered_Maps
        (Key_Type     => Argument_Cluster_Index_Type,
         Element_Type => Argument_Cluster_Index_Type);
    package Argument_Cluster_Index_To_Argument_Cluster_Index_Maps_Hashed is new
      Ada.Containers.Hashed_Maps
        (Key_Type        => Argument_Cluster_Index_Type,
         Element_Type    => Argument_Cluster_Index_Type,
         Hash            => Hash,
         Equivalent_Keys => "=");
    package Argument_Cluster_Index_To_Argument_Cluster_Index_Maps renames Argument_Cluster_Index_To_Argument_Cluster_Index_Maps_Hashed;
    -- Argument_Cluster_Index_Type to Argument_Cluster_Index_Type maps.

    ----------
    -- Cluster
    ----------

    type Cluster_Type is record
        Index                                           : Cluster_Index_Type := -1;
        --
        Argument_Cluster_Map                            : Argument_Cluster_Maps.Map;
        --
        Next_Argument_Cluster_Index                     : Argument_Cluster_Index_Type := 0;
        --
        Is_Content                                      : Boolean := False;
        --
        Is_Stop                                         : Boolean := False;
        --
        Part_Count                                      : Natural := 0;
        -- Number of Part linked at Cluster
        Rel_Type_Index_To_Count_Map                     : Rel_Type_Index_To_Count_Maps.Map;
        --
        Deprel_Index_To_Argument_Cluster_Index_Set_Map  : Deprel_Index_To_Argument_Cluster_Index_Set_Maps.Map;
        --
    end record;
    -- A Cluster.

    type Cluster_Access_Type is access Cluster_Type;
    -- Access to Cluster_Type.

    procedure Stream_Write
      (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
       Item   : in  Cluster_Access_Type);
    pragma Inline(Stream_Write);
    for Cluster_Access_Type'Write use Stream_Write;

    procedure Stream_Output
      (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
       Item   : in  Cluster_Access_Type);
    pragma Inline(Stream_Output);
    for Cluster_Access_Type'Output use Stream_Output;

    procedure Stream_Read
      (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
       Item   : out Cluster_Access_Type);
    pragma Inline(Stream_Read);
    for Cluster_Access_Type'Read use Stream_Read;

    function Stream_Input
      (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
       return Cluster_Access_Type;
    pragma Inline(Stream_Input);
    for Cluster_Access_Type'Input use Stream_Input;

    procedure Free is new Ada.Unchecked_Deallocation
      (Cluster_Type, Cluster_Access_Type);

    package Cluster_Maps_Ordered is new
      Ada.Containers.Ordered_Maps
        (Key_Type     => Cluster_Index_Type,
         Element_Type => Cluster_Access_Type);
    package Cluster_Maps_Hashed is new
      Ada.Containers.Hashed_Maps
        (Key_Type        => Cluster_Index_Type,
         Element_Type    => Cluster_Access_Type,
         Hash            => Hash,
         Equivalent_Keys => "=");
    package Cluster_Maps renames Cluster_Maps_Hashed;
    -- Cluster_Index_Type to Cluster_Access_Type maps.

    package Cluster_Index_Sets is
      new Ada.Containers.Ordered_Sets
        (Element_Type => Cluster_Index_Type);
    -- Sets of Cluster_Index_Type.

    procedure Insert
      (Set      : in out Cluster_Index_Sets.Set;
       New_Item : in     Cluster_Index_Type);
    pragma Inline(Insert);
    -- Insert a new item. Ignores duplicates.

    package Rel_Type_Index_To_Cluster_Index_Set_Maps is new
      Ada.Containers.Hashed_Maps
        (Key_Type        => Rel_Type_Index_Type,
         Element_Type    => Cluster_Index_Sets.Set,
         Hash            => Hash,
         Equivalent_Keys => "=",
         "="             => Cluster_Index_Sets."=");
    -- Rel_Type_Index_Type to Cluster_Index_Sets maps.

    procedure Insert
      (Map      : in out Rel_Type_Index_To_Cluster_Index_Set_Maps.Map;
       Key      : in     Rel_Type_Index_Type;
       Position : out    Rel_Type_Index_To_Cluster_Index_Set_Maps.Cursor);
    pragma Inline(Insert);
    -- Insert a new item. Raise USP_Insertion_Error if not inserted.

    procedure Add_To_Set
      (Map          : in out Rel_Type_Index_To_Cluster_Index_Set_Maps.Map;
       Map_Cursor   : in     Rel_Type_Index_To_Cluster_Index_Set_Maps.Cursor;
       Set_New_Item : in     Cluster_Index_Type);
    pragma Inline(Add_To_Set);
    -- Insert a new item in the set at Map_Cursor position. Ignore duplicates.

    package Cluster_Index_To_Atom_ID_Set_Maps_Ordered is new
      Ada.Containers.Ordered_Maps
        (Key_Type     => Cluster_Index_Type,
         Element_Type => Atom_ID_Sets.Set,
         "="          => Atom_ID_Sets."=");
    package Cluster_Index_To_Atom_ID_Set_Maps_Hashed is new
      Ada.Containers.Hashed_Maps
        (Key_Type        => Cluster_Index_Type,
         Element_Type    => Atom_ID_Sets.Set,
         Hash            => Hash,
         Equivalent_Keys => "=",
         "="             => Atom_ID_Sets."=");
    package Cluster_Index_To_Atom_ID_Set_Maps renames Cluster_Index_To_Atom_ID_Set_Maps_Hashed;
    -- Cluster_Index_Type to Atom_ID_Sets maps.

    procedure Insert
      (Map      : in out Cluster_Index_To_Atom_ID_Set_Maps.Map;
       Key      : in     Cluster_Index_Type;
       Position : out    Cluster_Index_To_Atom_ID_Set_Maps.Cursor);
    pragma Inline(Insert);
    -- Insert a new item. Raise USP_Insertion_Error if not inserted.

    procedure Add_To_Set
      (Map          : in out Cluster_Index_To_Atom_ID_Set_Maps.Map;
       Map_Cursor   : in     Cluster_Index_To_Atom_ID_Set_Maps.Cursor;
       Set_New_Item : in     Atom_ID_Type);
    pragma Inline(Add_To_Set);
    -- Insert a new item in the set at Map_Cursor position. Ignore duplicates.

    procedure Delete_From_Set
      (Map          : in out Cluster_Index_To_Atom_ID_Set_Maps.Map;
       Map_Position : in     Cluster_Index_To_Atom_ID_Set_Maps.Cursor;
       Set_Item     : in     Atom_ID_Type);
    pragma Inline(Delete_From_Set);
    -- If Set_Item exists, deletes it from the set at Map_Position.

    package Cluster_Index_Root_Count_Maps is new
      Ada.Containers.Hashed_Maps
        (Key_Type        => Cluster_Index_Type,
         Element_Type    => Integer, -- FIXME: come mai va a meno di zero?
         Hash            => Hash,
         Equivalent_Keys => "=");
    -- Cluster_Index_Type to count maps.

    procedure Increment
      (Map      : in out Cluster_Index_Root_Count_Maps.Map;
       Position : in     Cluster_Index_Root_Count_Maps.Cursor);
    pragma Inline(Increment);
    -- Count +1 at Position.

    procedure Decrement
      (Map      : in out Cluster_Index_Root_Count_Maps.Map;
       Position : in     Cluster_Index_Root_Count_Maps.Cursor);
    pragma Inline(Decrement);
    -- Count -1 at Position.

    type Cluster_Link_Type is record
        Governor_Cluster_Index  : Cluster_Index_Type := -1;
        --
        Dependent_Cluster_Index : Cluster_Index_Type := -1;
        --
    end record;
    -- Pairs Governor-Dependent Cluster Indexes.

    function Hash(Cluster_Link : Cluster_Link_Type) return Hash_Type is
      (Hash_Type(Cluster_Link.Governor_Cluster_Index) * Half_Digits_Power + Hash_Type(Cluster_Link.Dependent_Cluster_Index));
    pragma Precondition(Hash_Type(Cluster_Link.Governor_Cluster_Index) < Half_Digits_Power and then Hash_Type(Cluster_Link.Dependent_Cluster_Index) < Half_Digits_Power);
    pragma Inline(Hash);
    -- Hash function for Cluster_Link_Type.

    type Cluster_And_Argument_Cluster_Link_Type is record
        Cluster_Index          : Cluster_Index_Type          := -1;
        --
        Argument_Cluster_Index : Argument_Cluster_Index_Type := -1;
        --
    end record;
    -- Paris Cluster_Index and Argument_Cluster_Index.

    function "<"(Left, Right : in Cluster_And_Argument_Cluster_Link_Type) return Boolean is
      (if Left.Cluster_Index = Right.Cluster_Index then Left.Argument_Cluster_Index < Right.Argument_Cluster_Index else Left.Cluster_Index < Right.Cluster_Index);
    pragma Inline("<");
    -- Compares Cluster_Index and Argument_Cluster_Index.

    function Hash(Cluster_And_Argument_Cluster_Link : Cluster_And_Argument_Cluster_Link_Type) return Hash_Type is
      (Hash_Type(Cluster_And_Argument_Cluster_Link.Cluster_Index) * Half_Digits_Power + Hash_Type(Cluster_And_Argument_Cluster_Link.Argument_Cluster_Index));
    pragma Precondition(Hash_Type(Cluster_And_Argument_Cluster_Link.Cluster_Index) < Half_Digits_Power and then Hash_Type(Cluster_And_Argument_Cluster_Link.Argument_Cluster_Index) < Half_Digits_Power);
    pragma Inline(Hash);
    -- Hash function for Cluster_And_Argument_Cluster_Link_Type.

    package Cluster_Link_Sets is new
      Ada.Containers.Hashed_Sets
        (Element_Type        => Cluster_Link_Type,
         Hash                => Hash,
         Equivalent_Elements => "=");
    -- Sets of Cluster_Link_Type

    package Cluster_Index_To_Cluster_Link_Set_Maps is new
      Ada.Containers.Hashed_Maps
        (Key_Type        => Cluster_Index_Type,
         Element_Type    => Cluster_Link_Sets.Set,
         Hash            => Hash,
         Equivalent_Keys => "=",
         "="             => Cluster_Link_Sets."=");
    -- Cluster_Index_Type to Cluster_Link_Sets maps.

    procedure Add_To_Set
      (Map          : in out Cluster_Index_To_Cluster_Link_Set_Maps.Map;
       Map_Cursor   : in     Cluster_Index_To_Cluster_Link_Set_Maps.Cursor;
       Set_New_Item : in     Cluster_Link_Type);
    pragma Inline(Add_To_Set);
    -- Insert a new item in the set at Map_Cursor position. Ignore duplicates.

    procedure Insert
      (Map      : in out Cluster_Index_To_Cluster_Link_Set_Maps.Map;
       Key      : in     Cluster_Index_Type;
       Position : out    Cluster_Index_To_Cluster_Link_Set_Maps.Cursor);
    pragma Inline(Insert);
    -- Insert a new item. Raise USP_Insertion_Error if not inserted.

    procedure Delete_From_Set
      (Map          : in out Cluster_Index_To_Cluster_Link_Set_Maps.Map;
       Map_Position : in     Cluster_Index_To_Cluster_Link_Set_Maps.Cursor;
       Set_Item     : in     Cluster_Link_Type);
    pragma Inline(Delete_From_Set);
    -- If Set_Item exists, deletes it from the set at Map_Position.

    package Cluster_Link_To_Atom_Link_Set_Maps is new
      Ada.Containers.Hashed_Maps
        (Key_Type        => Cluster_Link_Type,
         Element_Type    => Atom_Link_Sets.Set,
         Hash            => Hash,
         Equivalent_Keys => "=",
         "="             => Atom_Link_Sets."=");
    -- Cluster_Link_Type to Atom_Link_Sets maps.

    procedure Insert
      (Map      : in out Cluster_Link_To_Atom_Link_Set_Maps.Map;
       Key      : in     Cluster_Link_Type;
       Position : out    Cluster_Link_To_Atom_Link_Set_Maps.Cursor);
    pragma Inline(Insert);
    -- Insert a new item. Raise USP_Insertion_Error if not inserted.

    procedure Add_To_Set
      (Map          : in out Cluster_Link_To_Atom_Link_Set_Maps.Map;
       Map_Cursor   : in     Cluster_Link_To_Atom_Link_Set_Maps.Cursor;
       Set_New_Item : in     Atom_Link_Type);
    pragma Inline(Add_To_Set);
    -- Insert a new item in the set at Map_Cursor position. Ignore duplicates.

    procedure Delete_From_Set
      (Map          : in out Cluster_Link_To_Atom_Link_Set_Maps.Map;
       Map_Position : in     Cluster_Link_To_Atom_Link_Set_Maps.Cursor;
       Set_Item     : in     Atom_Link_Type);
    pragma Inline(Delete_From_Set);
    -- Deletes Set_Item from the set at Map_Position.
    -- Raise error if it does not exist.

    type Cluster_Index_Pair_Type is record
        Max_Cluster_Index  : Cluster_Index_Type := -1;
        --
        Min_Cluster_Index  : Cluster_Index_Type := -1;
        --
    end record;
    -- Pairs Max and Min cluster indexes.

    function Hash(Cluster_Index_Pair : Cluster_Index_Pair_Type) return Hash_Type is (Hash_Type(Cluster_Index_Pair.Max_Cluster_Index) * Half_Digits_Power + Hash_Type(Cluster_Index_Pair.Min_Cluster_Index));
    pragma Precondition(Hash_Type(Cluster_Index_Pair.Max_Cluster_Index) < Half_Digits_Power and then Hash_Type(Cluster_Index_Pair.Max_Cluster_Index) < Half_Digits_Power);
    pragma Inline(Hash);
    -- Hash function for Cluster_Index_Pair_Type.

    package Cluster_Index_Pair_To_Conj_Count_Maps is new
      Ada.Containers.Hashed_Maps
        (Key_Type        => Cluster_Index_Pair_Type,
         Element_Type    => Natural,
         Hash            => Hash,
         Equivalent_Keys => "=");
    -- Cluster_Index_Pair_Type to count of conj.

    procedure Increment
      (Map      : in out Cluster_Index_Pair_To_Conj_Count_Maps.Map;
       Position : in     Cluster_Index_Pair_To_Conj_Count_Maps.Cursor);
    pragma Inline(Increment);
    -- Count +1 at Position.

    procedure Decrement
      (Map      : in out Cluster_Index_Pair_To_Conj_Count_Maps.Map;
       Position : in     Cluster_Index_Pair_To_Conj_Count_Maps.Cursor);
    pragma Inline(Decrement);
    -- Count -1 at Position.

    package Cluster_And_Argument_Cluster_Link_Type_To_Count_Maps_Ordered is new
      Ada.Containers.Ordered_Maps
        (Key_Type     => Cluster_And_Argument_Cluster_Link_Type,
         Element_Type => Natural);
    package Cluster_And_Argument_Cluster_Link_Type_To_Count_Maps_Hashed is new
      Ada.Containers.Hashed_Maps
        (Key_Type        => Cluster_And_Argument_Cluster_Link_Type,
         Element_Type    => Natural,
         Hash            => Hash,
         Equivalent_Keys => "=");
    package Cluster_And_Argument_Cluster_Link_Type_To_Count_Maps renames Cluster_And_Argument_Cluster_Link_Type_To_Count_Maps_Hashed;
    -- Cluster_And_Argument_Cluster_Link_Type to count maps.

    procedure Increment
      (Map      : in out Cluster_And_Argument_Cluster_Link_Type_To_Count_Maps.Map;
       Position : in     Cluster_And_Argument_Cluster_Link_Type_To_Count_Maps.Cursor);
    pragma Inline(Increment);
    -- Count +1 at Position.

    procedure Decrement
      (Map      : in out Cluster_And_Argument_Cluster_Link_Type_To_Count_Maps.Map;
       Position : in     Cluster_And_Argument_Cluster_Link_Type_To_Count_Maps.Cursor);
    pragma Inline(Decrement);
    -- Count +1 at Position.

    package Cluster_Index_To_Cluster_And_Argument_Cluster_Link_Type_To_Count_Map_Maps is new
      Ada.Containers.Hashed_Maps
        (Key_Type        => Cluster_Index_Type,
         Element_Type    => Cluster_And_Argument_Cluster_Link_Type_To_Count_Maps.Map,
         Hash            => Hash,
         Equivalent_Keys => "=",
         "="             => Cluster_And_Argument_Cluster_Link_Type_To_Count_Maps."=");
    -- Cluster_Index_Type to Cluster_And_Argument_Cluster_Link_Type_To_Count_Maps maps.

    procedure Insert
      (Map      : in out Cluster_Index_To_Cluster_And_Argument_Cluster_Link_Type_To_Count_Map_Maps.Map;
       Key      : in     Cluster_Index_Type;
       Position : out    Cluster_Index_To_Cluster_And_Argument_Cluster_Link_Type_To_Count_Map_Maps.Cursor);
    pragma Inline(Insert);
    -- Insert a new item. Raise USP_Insertion_Error if not inserted.

    -------
    -- Part
    -------

    type Part_Type is record
        Root_Node                                       : Atom_Access_Type    := null;
        --
        Rel_Type_Index                                  : Rel_Type_Index_Type := -1;
        --

        Cluster_Index                                   : Cluster_Index_Type  := -1;
        -- Index of Cluster_Access

        Cluster_Access                                  : Cluster_Access_Type := null;
        -- Cluster pointed by Cluster_Index.

        Governor_Part                                   : Part_Access_Type    := null;
        --

        Deprel                                          : Unbounded_String;
        --
        Deprel_Index                                    : Deprel_Index_Type   := -1;
        --
        Argument_Index_In_Governor_Part                 : Part_Index_Type     := -1;
        --
        Argument_Part_Map                               : Part_Maps.Map;
        --
        Next_Argument_Index                             : Part_Index_Type     := 0;
        --
        Argument_Index_To_Argument_Cluster_Index_Map    : Part_Index_To_Argument_Cluster_Index_Maps.Map;
        --
        Argument_Cluster_Index_To_Dependent_Indexes_Map : Argument_Cluster_Index_To_Part_Indexes_Maps.Map;
        --
    end record;
    -- A Part.

    procedure Free is new Ada.Unchecked_Deallocation
      (Part_Type, Part_Access_Type);

    package Atom_ID_To_Part_Maps is new
      Ada.Containers.Ordered_Maps
        (Key_Type     => Atom_ID_Type,
         Element_Type => Part_Access_Type);
    -- Atom_ID_Type to Part_Access_Type maps.

    -------------------
    -- Search_Operation
    -------------------

    type Integer_Array_Type is array (Positive range <>) of Integer;
    type Integer_Array_Access_Type is access all Integer_Array_Type;

    procedure Stream_Write
      (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
       Item   : in  Integer_Array_Access_Type);
    pragma Inline(Stream_Write);
    for Integer_Array_Access_Type'Write use Stream_Write;

    procedure Stream_Output
      (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
       Item   : in  Integer_Array_Access_Type);
    pragma Inline(Stream_Output);
    for Integer_Array_Access_Type'Output use Stream_Output;

    procedure Stream_Read
      (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
       Item   : out Integer_Array_Access_Type);
    pragma Inline(Stream_Read);
    for Integer_Array_Access_Type'Read use Stream_Read;

    function Stream_Input
      (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
       return Integer_Array_Access_Type;
    pragma Inline(Stream_Input);
    for Integer_Array_Access_Type'Input use Stream_Input;

    procedure Free is new Ada.Unchecked_Deallocation(Integer_Array_Type, Integer_Array_Access_Type);

    type Search_Operation_Command_Type is (Op_Merge_Clust, Op_Compose, Op_NIL);
    -- Search Operation labels.

    type Search_Operation_Type is record --new Ada.Finalization.Controlled with record
        Operation               : Search_Operation_Command_Type := Op_NIL;
        -- The operation label.

        Cluster_Index_1_Or_Governor  : Cluster_Index_Type := -1;
        -- merge clust: Cluster_Index_1; absorb: Governor_Cluster_Index

        Cluster_Index_2_Or_Dependent : Cluster_Index_Type := -1;
        -- merge clust: Cluster_Index_2; absorb: Dependent_Cluster_Index

        --Str                     : Unbounded_String;
        -- String representation.

        Repr                    : Integer_Array_Access_Type := null;

        Hash_Value              : Hash_Type;
        -- Hash of Str
    end record;

    function "<"(Left, Right : in Search_Operation_Type) return Boolean is
      (Left.Repr.all < Right.Repr.all);
    pragma Precondition(Left.Repr /= null and then Right.Repr /= null);
    pragma Inline("<");
    -- Compares Search_Operation_Types' Str.

    function "="(Left, Right : in Search_Operation_Type) return Boolean is
      (Left.Repr.all = Right.Repr.all);
    pragma Precondition(Left.Repr /= null and then Right.Repr /= null);
    pragma Inline("=");
    -- Compares Search_Operation_Types' Str.

    function Hash (Search_Operation : in Search_Operation_Type) return Hash_Type is
      (Search_Operation.Hash_Value);
    pragma Precondition(Search_Operation.Repr /= null);
    pragma Inline(Hash);
    -- Hash function for Search_Operation_Type (it's the Hash of Str)

    package Search_Operation_Sets_Ordered is new
      Ada.Containers.Ordered_Sets
        (Element_Type => Search_Operation_Type);
    package Search_Operation_Sets_hashed is new
      Ada.Containers.Hashed_Sets
        (Element_Type        => Search_Operation_Type,
         Hash                => Hash,
         Equivalent_Elements => "=");
    package Search_Operation_Sets renames Search_Operation_Sets_Hashed;
    -- Sets of Search_Operation_Type.

    procedure Insert
      (Set                         : in out Search_Operation_Sets.Set;
       New_Item                    : in out Search_Operation_Type;
       Free_Repr_When_Not_Inserted : in     Boolean);
    pragma Inline(Insert);
    -- Insert a new item. Ignores duplicates.

    -------------------------------
    -- Agenda and Search_Operations
    -------------------------------

    type Neigh_Type_Type is new Integer;
    -- Numeric Neigh_Type.

    function Hash(Key : Neigh_Type_Type) return Hash_Type is (Hash_Type(Key));
    pragma Inline(Hash);
    -- Hash function for Neigh_Type_Type.

    package Neighs_Sets is new
      Ada.Containers.Hashed_Sets
        (Element_Type        => Neigh_Type_Type,
         Hash                => Hash,
         Equivalent_Elements => "=");
    -- Sets of Neigh_Type_Type.

    procedure Insert
      (Set      : in out Neighs_Sets.Set;
       New_Item : in     Neigh_Type_Type);
    pragma Inline(Insert);
    -- Insert a new item. Ignores duplicates.

    package Search_Operation_To_Neighs_Set_Maps is new
      Ada.Containers.Hashed_Maps
        (Key_Type        => Search_Operation_Type,
         Element_Type    => Neighs_Sets.Set,
         Hash            => Hash,
         Equivalent_Keys => "=",
         "="             => Neighs_Sets."=");
    -- Search_Operation_Type to Neighs_Sets maps.

    procedure Insert
      (Map      : in out Search_Operation_To_Neighs_Set_Maps.Map;
       Key      : in     Search_Operation_Type;
       Position : out    Search_Operation_To_Neighs_Set_Maps.Cursor);
    pragma Inline(Insert);
    -- Insert a new item. Raise USP_Insertion_Error if not inserted.

    package Compose_To_Count_Maps is new
      Ada.Containers.Hashed_Maps
        (Key_Type        => Search_Operation_Type,
         Element_Type    => Natural,
         Hash            => Hash,
         Equivalent_Keys => "=");
    -- Search_Operation_Type to count maps.

    procedure Increment
      (Map      : in out Compose_To_Count_Maps.Map;
       Position : in     Compose_To_Count_Maps.Cursor);
    pragma Inline(Increment);
    -- Count +1 at Position.

    package Search_Operation_To_Score_Maps is new
      Ada.Containers.Hashed_Maps
        (Key_Type        => Search_Operation_Type,
         Element_Type    => Score_Type,
         Hash            => Hash,
         Equivalent_Keys => "=");
    -- Search_Operation_Type to Score_Type maps.

    type Score_And_Search_Operation_Link_Type is record
        Score            : Score_Type;
        --
        Search_Operation : Search_Operation_Type;
        --
    end record;
    -- Paris Search_Operation and Score.

    function "<"(Left, Right : in Score_And_Search_Operation_Link_Type) return Boolean is
      (if Left.Score = Right.Score then Left.Search_Operation < Right.Search_Operation else Left.Score < Right.Score);
    pragma Inline("<");
    -- Compares Score and Search_Operation.

    package Score_And_Search_Operation_Link_Type_Sets is new
      Ada.Containers.Ordered_Sets
        (Element_Type => Score_And_Search_Operation_Link_Type);
    -- Sets of Score_And_Search_Operation_Link_Type

    package Cluster_Index_To_Search_Operation_Set_Maps is new
      Ada.Containers.Hashed_Maps
        (Key_Type        => Cluster_Index_Type,
         Element_Type    => Search_Operation_Sets.Set,
         Hash            => Hash,
         Equivalent_Keys => "=",
         "="             => Search_Operation_Sets."=");
    -- Cluster_Index_Type to Search_Operation_Sets maps.

    procedure Insert
      (Map      : in out Cluster_Index_To_Search_Operation_Set_Maps.Map;
       Key      : in     Cluster_Index_Type;
       Position : out    Cluster_Index_To_Search_Operation_Set_Maps.Cursor);
    pragma Inline(Insert);
    -- Insert a new item. Raise USP_Insertion_Error if not inserted.

    procedure Add_To_Set
      (Map          : in out Cluster_Index_To_Search_Operation_Set_Maps.Map;
       Map_Cursor   : in     Cluster_Index_To_Search_Operation_Set_Maps.Cursor;
       Set_New_Item : in     Search_Operation_Type);
    pragma Inline(Add_To_Set);
    -- Insert a new item in the set at Map_Cursor position. Ignore duplicates.

    procedure Delete_From_Set
      (Map          : in out Cluster_Index_To_Search_Operation_Set_Maps.Map;
       Map_Position : in     Cluster_Index_To_Search_Operation_Set_Maps.Cursor;
       Set_Item     : in     Search_Operation_Type);
    pragma Inline(Delete_From_Set);
    -- If Set_Item exists, deletes it from the set at Map_Position.

    type Agenda_Type is record
        Search_Operation_To_Neighs_Set_Map        : Search_Operation_To_Neighs_Set_Maps.Map;
        --
        Agenda_To_Score_Set                       : Search_Operation_Sets.Set;
        --
        Compose_To_Count_Map                      : Compose_To_Count_Maps.Map;
        --
        Inactive_Agenda_To_Score_Map              : Search_Operation_To_Score_Maps.Map;
        -- Inactive
        Active_Agenda_To_Score_Map                : Search_Operation_To_Score_Maps.Map;
        -- Active
        Score_Active_Agenda                       : Score_And_Search_Operation_Link_Type_Sets.Set; -- heap for active list
        --
        Cluster_Index_To_Search_Operation_Set_Map : Cluster_Index_To_Search_Operation_Set_Maps.Map;
        --
    end record;
    -- An Agenda.

    type Agenda_Access_Type is access all Agenda_Type;
    procedure Free is new Ada.Unchecked_Deallocation(Agenda_Type, Agenda_Access_Type);

    ------------------

    type Argument_Cluster_Index_And_Count_Type is record
        Argument_Cluster_Index : Argument_Cluster_Index_Type := -1;
        --
        Dependents_Count       : Integer                     := -1;
        --
    end record;
    -- Pairs Argument_Cluster_Index and Dependents_Count.

    function ">" (Left, Right : in Argument_Cluster_Index_And_Count_Type) return Boolean is
      (if Left.Dependents_Count = Right.Dependents_Count then Left.Argument_Cluster_Index > Right.Argument_Cluster_Index else Left.Dependents_Count > Right.Dependents_Count);
    pragma Inline(">");
    -- Compares Dependents_Count and Argument_Cluster_Index.

    package Ordered_Argument_Cluster_Index_And_Count_Type_Sets is new
      Ada.Containers.Ordered_Sets
        (Element_Type => Argument_Cluster_Index_And_Count_Type,
         "<"          => ">");
    -- Sets of Argument_Cluster_Index_And_Count_Type in reverse ordering.

    ------------------------------------------------------------------------

    package ID_To_String_Maps is new
      Ada.Containers.Ordered_Maps
        (Key_Type     => Atom_ID_Type,
         Element_Type => Unbounded_String);
    -- Atom_ID_Type to Unbounded_String maps (used by Print_Model).

    ------------------------------------
    -- Specific functions and procedures
    ------------------------------------

    function Check_Word_Ancestor
      (Sentence : in Word_Vector.Vector;
       Word_ID  : in Word_Index_Type) return Boolean;
    pragma Inline(Check_Word_Ancestor);
    -- Returns True when the top-ancestor of Word_ID in the Sentence is
    -- the ROOT element (ID 0).

    function Gen_Rel_Type
      (Atom : Atom_Type) return Unbounded_String;
    pragma Inline(Gen_Rel_Type);

    function Get_Tree_Str
      (Atom : in Atom_Type) return Unbounded_String;
    pragma Inline(Get_Tree_Str);

    -------------
    -- Exceptions
    -------------

    MLSE_Error,
    MLSE_Insertion_Error,
    MLSE_Invalid_Search_Operation_Error: exception;

end Commons;
