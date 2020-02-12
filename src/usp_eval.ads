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
with Ada.Strings.Hash;
with Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Containers.Ordered_Maps;
with Ada.Strings.Unbounded.Hash;
with Ada.Containers.Hashed_Sets;
with Ada.Containers.Hashed_Maps;

with Commons; use Commons;
with Ada.Containers.Ordered_Sets;

package USP_Eval is

    -----
    -- Question
    -----

    type Question_Type is record
        Relation            : Unbounded_String;
        --
        Argument            : Unbounded_String;
        --
        Deprel              : Unbounded_String;
        --
    end record;
    -- dep = the arg given in question

    function To_String(Question : in Question_Type) return String;
    pragma Inline(To_String);
    -- String representation of a Question.

    function Hash(Question : in Question_Type) return Ada.Containers.Hash_Type is
      (Ada.Strings.Hash(To_String(Question)));
    pragma Inline(Hash);
    -- Hash function for Question_Type.

    function "<"(Left,Right : in Question_Type) return Boolean is
      (if Left.Deprel = Right.Deprel and then Left.Relation = Right.Relation then
       Left.Argument < Right.Argument
       elsif Left.Deprel = Right.Deprel then
       Left.Relation < Right.Relation
       else
       Left.Deprel < Right.Deprel);
    pragma Inline("<");
    -- Compares Deprel, Relation and Argument

    package Question_Vectors is new
      Ada.Containers.Vectors
        (Index_Type   => Natural,
         Element_Type => Question_Type);
    --

    package Question_Sets is new
      Ada.Containers.Hashed_Sets
        (Element_Type        => Question_Type,
         Hash                => Hash,
         Equivalent_Elements => "=");

    procedure Insert
      (Set      : in out Question_Sets.Set;
       New_Item : in     Question_Type);
    pragma Inline(Insert);
    -- Insert a new item. Ignores duplicates.

    package Relation_To_Question_Vector_Maps is new
      Ada.Containers.Ordered_Maps
        (Key_Type     => Unbounded_String,
         Element_Type => Question_Vectors.Vector,
         "="          => Question_Vectors."=");
    --

    procedure Remove_All
      (Map      : in out Relation_To_Question_Vector_Maps.Map;
       Position : in     Relation_To_Question_Vector_Maps.Cursor;
       Elements : in     Question_Sets.Set);
    pragma Inline(Remove_All);
    --

    procedure Insert
      (Map      : in out Relation_To_Question_Vector_Maps.Map;
       Key      : in     Unbounded_String;
       Position : out    Relation_To_Question_Vector_Maps.Cursor);
    pragma Inline(Insert);
    --

    procedure Add_To_Vector
      (Map             : in out Relation_To_Question_Vector_Maps.Map;
       Map_Cursor      : in     Relation_To_Question_Vector_Maps.Cursor;
       Vector_New_Item : in     Question_Type);
    pragma Inline(Add_To_Vector);
    -- Insert a new item in the set at Map_Cursor position. Ignore duplicates.

    -----
    -- Forms & Lemmas
    -----

    package Forms_Sets_Ordered is new
      Ada.Containers.Ordered_Sets
        (Element_Type => Unbounded_String);
    package Forms_Sets_Hashed is new
      Ada.Containers.Hashed_Sets
        (Element_Type        => Unbounded_String,
         Hash                => Ada.Strings.Unbounded.Hash,
         Equivalent_Elements => "=");
    package Forms_Sets renames Forms_Sets_Hashed;
    -- Sets of Forms (Unbounded_String)

    procedure Insert
      (Set      : in out Forms_Sets.Set;
       New_Item : in     Unbounded_String);
    pragma Inline(Insert);
    -- Insert a new item. Ignores duplicates.

    package Lemmas_Sets_Ordered is new
      Ada.Containers.Ordered_Sets
        (Element_Type => Unbounded_String);
    package Lemmas_Sets_Hashed is new
      Ada.Containers.Hashed_Sets
        (Element_Type        => Unbounded_String,
         Hash                => Ada.Strings.Unbounded.Hash,
         Equivalent_Elements => "=");
    package Lemmas_Sets renames Lemmas_Sets_Hashed;
    -- Sets of Lemmas (Unbounded_String)

    procedure Insert
      (Set      : in out Lemmas_Sets.Set;
       New_Item : in     Unbounded_String);
    pragma Inline(Insert);
    -- Insert a new item. Ignores duplicates.

    package Form_To_Lemmas_Maps_Ordered is new
      Ada.Containers.Ordered_Maps
        (Key_Type     => Unbounded_String,
         Element_Type => Lemmas_Sets.Set,
         "="          => Lemmas_Sets."=");
    package Form_To_Lemmas_Maps_Hashed is new
      Ada.Containers.Hashed_Maps
        (Key_Type        => Unbounded_String,
         Element_Type    => Lemmas_Sets.Set,
         Hash            => Ada.Strings.Unbounded.Hash,
         Equivalent_Keys => "=",
         "="             => Lemmas_Sets."=");
    package Form_To_Lemmas_Maps renames Form_To_Lemmas_Maps_Hashed;
    --

    procedure Insert
      (Map      : in out Form_To_Lemmas_Maps.Map;
       Key      : in     Unbounded_String;
       Position : out    Form_To_Lemmas_Maps.Cursor);
    pragma Inline(Insert);
    --

    procedure Add_To_Set
      (Map          : in out Form_To_Lemmas_Maps.Map;
       Map_Cursor   : in     Form_To_Lemmas_Maps.Cursor;
       Set_New_Item : in     Unbounded_String);
    pragma Inline(Add_To_Set);
    -- Insert a new item in the set at Map_Cursor position. Ignore duplicates.

    -----
    -- Answer
    -----

    type Answer_Type is record
        Sentence_ID        : Unbounded_String; -- TODO: it could be replaced with a new structure (ArticleID, SentenceID)
        Str_Preps_Sequence : Unbounded_String;
    end record;
    --

    function "<"(Left, Right : in Answer_Type) return Boolean is
      (if Left.Str_Preps_Sequence = Right.Str_Preps_Sequence then
       Left.Sentence_ID < Right.Sentence_ID
       else
       Left.Str_Preps_Sequence < Right.Str_Preps_Sequence);
    --        (if Left.Sentence_ID = Right.Sentence_ID then
    --         Left.Str_Preps_Sequence < Right.Str_Preps_Sequence
    --         else
    --         Left.Sentence_ID < Right.Sentence_ID);
    --

    function To_String(Answer : in Answer_Type) return Unbounded_String is
      (Answer.Sentence_ID & " " & Answer.Str_Preps_Sequence);
    --

    function Hash(Answer : in Answer_Type) return Ada.Containers.Hash_Type is
      (Ada.Strings.Unbounded.Hash(To_String(Answer)));
    pragma Inline(Hash);
    -- Hash function for Answer_Type.

    package Answer_Sets is new
      Ada.Containers.Ordered_Sets
        (Element_Type => Answer_Type);
    --

    package Question_To_Answer_Set_Maps is new
      Ada.Containers.Ordered_Maps
        (Key_Type     => Question_Type,
         Element_Type => Answer_Sets.Set,
         "="          => Answer_Sets."=");
    --

    procedure Insert
      (Map      : in out Question_To_Answer_Set_Maps.Map;
       Key      : in     Question_Type;
       Position : out    Question_To_Answer_Set_Maps.Cursor);
    pragma Inline(Insert);
    -- Insert a new item. Raise USP_Insertion_Error if not inserted.

    procedure Add_To_Set
      (Map          : in out Question_To_Answer_Set_Maps.Map;
       Map_Cursor   : in     Question_To_Answer_Set_Maps.Cursor;
       Set_New_Item : in     Answer_Type);
    pragma Inline(Add_To_Set);
    -- Insert a new item in the set at Map_Cursor position. Ignore duplicates.

    -----
    -- Misc
    -----

    package Deprel_To_Argument_Cluster_Index_Maps_Ordered is new
      Ada.Containers.Ordered_Maps
        (Key_Type     => Unbounded_String,
         Element_Type => Argument_Cluster_Index_Type);
    package Deprel_To_Argument_Cluster_Index_Maps_Hashed is new
      Ada.Containers.Hashed_Maps
        (Key_Type        => Unbounded_String,
         Element_Type    => Argument_Cluster_Index_Type,
         Hash            => Ada.Strings.Unbounded.Hash,
         Equivalent_Keys => "=");
    package Deprel_To_Argument_Cluster_Index_Maps renames Deprel_To_Argument_Cluster_Index_Maps_Hashed;
    --

    procedure Insert_Replace
      (Map      : in out Deprel_To_Argument_Cluster_Index_Maps.Map;
       Key      : in     Unbounded_String;
       New_Item : in     Argument_Cluster_Index_Type);
    pragma Inline(Insert_Replace);
    -- Insert a new item. Rewrite new value if key already esists.

    package Cluster_Index_To_Deprel_To_Argument_Cluster_Index_Maps_Ordered is new
      Ada.Containers.Ordered_Maps
        (Key_Type     => Cluster_Index_Type,
         Element_Type => Deprel_To_Argument_Cluster_Index_Maps.Map,
         "="          => Deprel_To_Argument_Cluster_Index_Maps."=");
    package Cluster_Index_To_Deprel_To_Argument_Cluster_Index_Maps_Hashed is new
      Ada.Containers.Hashed_Maps
        (Key_Type        => Cluster_Index_Type,
         Element_Type    => Deprel_To_Argument_Cluster_Index_Maps.Map,
         Hash            => Hash,
         Equivalent_Keys => "=",
         "="             => Deprel_To_Argument_Cluster_Index_Maps."=");
    package Cluster_Index_To_Deprel_To_Argument_Cluster_Index_Maps renames Cluster_Index_To_Deprel_To_Argument_Cluster_Index_Maps_Hashed;
    --

    procedure Insert
      (Map      : in out Cluster_Index_To_Deprel_To_Argument_Cluster_Index_Maps.Map;
       Key      : in     Cluster_Index_Type;
       Position : out    Cluster_Index_To_Deprel_To_Argument_Cluster_Index_Maps.Cursor);
    pragma Inline(Insert);
    -- Insert a new item. Raise USP_Insertion_Error if not inserted.

    procedure Insert_Replace_To_Map
      (Map              : in out Cluster_Index_To_Deprel_To_Argument_Cluster_Index_Maps.Map;
       Map_Cursor       : in     Cluster_Index_To_Deprel_To_Argument_Cluster_Index_Maps.Cursor;
       Sub_Map_Key      : in     Unbounded_String;
       Sub_Map_New_Item : in     Argument_Cluster_Index_Type);
    pragma Inline(Insert_Replace_To_Map);
    --

    package Rel_Type_To_Cluster_Index_Maps_Ordered is new
      Ada.Containers.Ordered_Maps
        (Key_Type     => Unbounded_String,
         Element_Type => Cluster_Index_Type);
    package Rel_Type_To_Cluster_Index_Maps_Hashed is new
      Ada.Containers.Hashed_Maps
        (Key_Type        => Unbounded_String,
         Element_Type    => Cluster_Index_Type,
         Hash            => Ada.Strings.Unbounded.Hash,
         Equivalent_Keys => "=");
    package Rel_Type_To_Cluster_Index_Maps renames Rel_Type_To_Cluster_Index_Maps_Hashed;
    --

    package Lemma_To_Cluster_Index_Set_Maps_Ordered is new
      Ada.Containers.Ordered_Maps
        (Key_Type     => Unbounded_String,
         Element_Type => Cluster_Index_Sets.Set,
         "="          => Cluster_Index_Sets."=");
    package Lemma_To_Cluster_Index_Set_Maps_Hashed is new
      Ada.Containers.hashed_Maps
        (Key_Type        => Unbounded_String,
         Element_Type    => Cluster_Index_Sets.Set,
         Hash            => Ada.Strings.Unbounded.Hash,
         Equivalent_Keys => "=",
         "="             => Cluster_Index_Sets."=");
    package Lemma_To_Cluster_Index_Set_Maps renames Lemma_To_Cluster_Index_Set_Maps_Hashed;
    --

    procedure Insert
      (Map      : in out Lemma_To_Cluster_Index_Set_Maps.Map;
       Key      : in     Unbounded_String;
       Position : out    Lemma_To_Cluster_Index_Set_Maps.Cursor);
    pragma Inline(Insert);
    -- Insert a new item. Raise USP_Insertion_Error if not inserted.

    procedure Add_To_Set
      (Map          : in out Lemma_To_Cluster_Index_Set_Maps.Map;
       Map_Cursor   : in     Lemma_To_Cluster_Index_Set_Maps.Cursor;
       Set_New_Item : in     Cluster_Index_Type);
    pragma Inline(Add_To_Set);
    -- Insert a new item in the set at Map_Cursor position. Ignore duplicates.

    type Gov_Dep_Lemma_Link_Type is record
        Governor_Lemma  : Unbounded_String;
        Dependent_Lemma : Unbounded_String;
    end record;
    -- Governor and Dependent Lemmas

    function Hash(Gov_Dep_Lemma_Link : in Gov_Dep_Lemma_Link_Type) return Ada.Containers.Hash_Type is
      (Ada.Strings.Unbounded.Hash(Gov_Dep_Lemma_Link.Governor_Lemma & " " & Gov_Dep_Lemma_Link.Dependent_Lemma));
    pragma Inline(Hash);
    -- Hash function for Gov_Dep_Lemma_Link_Type.

    function "<"(Left, Right : in Gov_Dep_Lemma_Link_Type) return Boolean is
      (if Left.Governor_Lemma = Right.Governor_Lemma then Left.Dependent_Lemma < Right.Dependent_Lemma else Left.Governor_Lemma < Right.Governor_Lemma);
    pragma Inline("<");
    -- "<" function for Gov_Dep_Lemma_Link_Type.

    package Gov_Dep_Lemma_Link_To_Cluster_Index_Maps_Ordered is new
      Ada.Containers.Ordered_Maps
        (Key_Type     => Gov_Dep_Lemma_Link_Type,
         Element_Type => Cluster_Index_Type);
    package Gov_Dep_Lemma_Link_To_Cluster_Index_Maps_Hashed is new
      Ada.Containers.Hashed_Maps
        (Key_Type        => Gov_Dep_Lemma_Link_Type,
         Element_Type    => Cluster_Index_Type,
         Hash            => Hash,
         Equivalent_Keys => "=");
    package Gov_Dep_Lemma_Link_To_Cluster_Index_Maps renames Gov_Dep_Lemma_Link_To_Cluster_Index_Maps_Hashed;
    --

    procedure Insert_Replace
      (Map      : in out Gov_Dep_Lemma_Link_To_Cluster_Index_Maps.Map;
       Key      : in     Gov_Dep_Lemma_Link_Type;
       New_Item : in     Cluster_Index_Type);
    pragma Inline(Insert_Replace);
    -- Insert a new item. Rewrite new value if key already esists.


    type Cluster_Index_And_Atom_Tree_Str_Link_Type is record
        Cluster_Index : Cluster_Index_Type;
        Atom_Tree_Str : Unbounded_String;
    end record;
    --

    package Atom_ID_To_Cluster_Index_And_Atom_Tree_Str_Link_Maps is new
      Ada.Containers.Hashed_Maps
        (Key_Type        => Atom_ID_Type,
         Element_Type    => Cluster_Index_And_Atom_Tree_Str_Link_Type,
         Hash            => Hash,
         Equivalent_Keys => "=");
    --

    package Atom_ID_To_Deprel_Maps_Ordered is new
      Ada.Containers.Ordered_Maps
        (Key_Type     => Atom_ID_Type,
         Element_Type => Unbounded_String);
    package Atom_ID_To_Deprel_Maps_Hashed is new
      Ada.Containers.Hashed_Maps
        (Key_Type        => Atom_ID_Type,
         Element_Type    => Unbounded_String,
         Hash            => Hash,
         Equivalent_Keys => "=");
    package Atom_ID_To_Deprel_Maps renames Atom_ID_To_Deprel_Maps_Hashed;
    --

    package Argument_Cluster_Index_To_Atom_ID_Set_Maps_Ordered is new
      Ada.Containers.Ordered_Maps
        (Key_Type     => Argument_Cluster_Index_Type,
         Element_Type => Atom_ID_Sets.Set,
         "="          => Atom_ID_Sets."=");
    package Argument_Cluster_Index_To_Atom_ID_Set_Maps_Hashed is new
      Ada.Containers.Hashed_Maps
        (Key_Type        => Argument_Cluster_Index_Type,
         Element_Type    => Atom_ID_Sets.Set,
         Hash            => Hash,
         Equivalent_Keys => "=",
         "="             => Atom_ID_Sets."=");
    package Argument_Cluster_Index_To_Atom_ID_Set_Maps renames Argument_Cluster_Index_To_Atom_ID_Set_Maps_Hashed;
    --

    procedure Insert
      (Map      : in out Argument_Cluster_Index_To_Atom_ID_Set_Maps.Map;
       Key      : in     Argument_Cluster_Index_Type;
       Position : out    Argument_Cluster_Index_To_Atom_ID_Set_Maps.Cursor);
    pragma Inline(Insert);
    -- Insert a new item. Raise USP_Insertion_Error if not inserted.

    procedure Add_To_Set
      (Map          : in out Argument_Cluster_Index_To_Atom_ID_Set_Maps.Map;
       Map_Cursor   : in     Argument_Cluster_Index_To_Atom_ID_Set_Maps.Cursor;
       Set_New_Item : in     Atom_ID_Type);
    pragma Inline(Add_To_Set);
    -- Insert a new item in the set at Map_Cursor position. Ignore duplicates.

    package Atom_ID_To_Argument_Cluster_Index_To_Atom_ID_Set_Map_Maps_Ordered is new
      Ada.Containers.Ordered_Maps
        (Key_Type     => Atom_ID_Type,
         Element_Type => Argument_Cluster_Index_To_Atom_ID_Set_Maps.Map,
         "="          => Argument_Cluster_Index_To_Atom_ID_Set_Maps."=");
    package Atom_ID_To_Argument_Cluster_Index_To_Atom_ID_Set_Map_Maps_Hashed is new
      Ada.Containers.Hashed_Maps
        (Key_Type        => Atom_ID_Type,
         Element_Type    => Argument_Cluster_Index_To_Atom_ID_Set_Maps.Map,
         Hash            => Hash,
         Equivalent_Keys => "=",
         "="              => Argument_Cluster_Index_To_Atom_ID_Set_Maps."=");
    package Atom_ID_To_Argument_Cluster_Index_To_Atom_ID_Set_Map_Maps renames Atom_ID_To_Argument_Cluster_Index_To_Atom_ID_Set_Map_Maps_Hashed;
    --

    procedure Insert
      (Map      : in out Atom_ID_To_Argument_Cluster_Index_To_Atom_ID_Set_Map_Maps.Map;
       Key      : in     Atom_ID_Type;
       Position : out    Atom_ID_To_Argument_Cluster_Index_To_Atom_ID_Set_Map_Maps.Cursor);
    pragma Inline(Insert);
    -- Insert a new item. Raise USP_Insertion_Error if not inserted.

    package Str_Cluster_Indexes_Vectors is new
      Ada.Containers.Vectors
        (Index_Type   => Natural,
         Element_Type => Unbounded_String);
    --

    package Str_Cluster_Indexes_Double_Vectors is new
      Ada.Containers.Vectors
        (Index_Type   => Natural,
         Element_Type => Str_Cluster_Indexes_Vectors.Vector,
         "="          => Str_Cluster_Indexes_Vectors."=");
    --

    package Argument_To_Str_Cluster_Indexes_Double_Vector_Maps_Ordered is new
      Ada.Containers.Ordered_Maps
        (Key_Type     => Unbounded_String,
         Element_Type => Str_Cluster_Indexes_Double_Vectors.Vector,
         "="          => Str_Cluster_Indexes_Double_Vectors."=");
    package Argument_To_Str_Cluster_Indexes_Double_Vector_Maps_Hashed is new
      Ada.Containers.Hashed_Maps
        (Key_Type        => Unbounded_String,
         Element_Type    => Str_Cluster_Indexes_Double_Vectors.Vector,
         Hash            => Ada.Strings.Unbounded.Hash,
         Equivalent_Keys => "=",
         "="             => Str_Cluster_Indexes_Double_Vectors."=");
    package Argument_To_Str_Cluster_Indexes_Double_Vector_Maps renames Argument_To_Str_Cluster_Indexes_Double_Vector_Maps_Hashed;
    --

    package Atom_ID_To_Atom_ID_Maps is new
      Ada.Containers.Hashed_Maps
        (Key_Type        => Atom_ID_Type,
         Element_Type    => Atom_ID_Type,
         Hash            => Hash,
         Equivalent_Keys => "=");
    --

    package Atom_ID_Ordered_Sets is new
      Ada.Containers.Ordered_Sets
        (Element_Type => Atom_ID_Type);
    --

    package Atom_ID_Ordered_Set_Vectors is new
      Ada.Containers.Vectors
        (Index_Type   => Natural,
         Element_Type => Atom_ID_Ordered_Sets.Set,
         "="          => Atom_ID_Ordered_Sets."=");
    --

    package Word_Index_To_Str_Prep_Maps is new
      Ada.Containers.Ordered_Maps
        (Key_Type     => Word_Index_Type,
         Element_Type => Unbounded_String);

    -----
    -- Utils
    -----

    function Is_Digit(C : in Character) return Boolean is
      (C in '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9');
    pragma Inline(Is_Digit);
    --

    function Remove_Third_Person(Relation : String) return String;
    pragma Inline(Remove_Third_Person);
    --

    package UString_Vectors is new
      Ada.Containers.Vectors
        (Index_Type   => Natural,
         Element_Type => Unbounded_String);
    --

    function Contains
      (Str_Cluster_Indexes : in Unbounded_String;
       Str_Cluster_Index   : in String) return Boolean;
    pragma Inline(Contains);
    --

    function Contains
      (Str_Cluster_Indexes_Vector : in Str_Cluster_Indexes_Vectors.Vector;
       Cluster_Index              : in Cluster_Index_Type) return Boolean;
    pragma Inline(Contains);
    --

    function Deprel_Is_Allowed(Deprel : in Unbounded_String) return Boolean is
      (Str(Deprel) in "nn" | "amod" | "prep_of" | "num" | "appos");
    pragma Inline(Deprel_Is_Allowed);
    --

    function Get_Article
      (Articles   : in Article_Sets.Set;
       Article_ID : in Article_Index_Type) return Article_Type;
    --

    ------------------
    -- Shared Memories
    ------------------

    Relation_To_Question_Vector_Map : Relation_To_Question_Vector_Maps.Map;
    -- TODO: in realta' Relation coincide con Rel_Type?

    Forms_Set                                             : Forms_Sets.Set;
    Lemmas_Set                                            : Lemmas_Sets.Set;
    Form_To_Lemmas_Map                                    : Form_To_Lemmas_Maps.Map;
    Cluster_Index_To_Deprel_To_Argument_Cluster_Index_Map : Cluster_Index_To_Deprel_To_Argument_Cluster_Index_Maps.Map;

    Rel_Type_To_Cluster_Index_Map                         : Rel_Type_To_Cluster_Index_Maps.Map;
    -- identify verbs: dep->multiple argclust

    Lemma_To_Cluster_Index_Set_Map                        : Lemma_To_Cluster_Index_Set_Maps.Map;

    Gov_Dep_Lemma_Link_To_Cluster_Index_Map               : Gov_Dep_Lemma_Link_To_Cluster_Index_Maps.Map;
    -- AB: B->ci,A

    Cluster_Index_To_Atom_ID_Set_Map                      : Cluster_Index_To_Atom_ID_Set_Maps.Map;
    Atom_ID_To_Cluster_Index_And_Atom_Tree_Str_Link_Map   : Atom_ID_To_Cluster_Index_And_Atom_Tree_Str_Link_Maps.Map;
    Atom_ID_To_Deprel_Map                                 : Atom_ID_To_Deprel_Maps.Map;
    Atom_ID_To_Argument_Cluster_Index_To_Atom_ID_Set_Map_Map : Atom_ID_To_Argument_Cluster_Index_To_Atom_ID_Set_Map_Maps.Map;

    Argument_To_Str_Cluster_Indexes_Double_Vector_Map     : Argument_To_Str_Cluster_Indexes_Double_Vector_Maps.Map;
    -- multiple cis: each a possible parse
    -- cis: multiple node, each w. multiple possible cis

    Question_To_Answer_Set_Map                            : Question_To_Answer_Set_Maps.Map;

    -----
    -- Functions and procedures
    -----

    procedure Read_Questions
      (Questions_Directory : in Unbounded_String);

    procedure Map_Form_To_Lemma
      (Articles : in Article_Sets.Set);

    procedure Read_Clusters
      (MLN_Filename : in String);
    -- Find clustIdx for rel in questions

    procedure Process_Rel_Type
      (Cluster_Index : in Cluster_Index_Type;
       POS           : in Unbounded_String;
       Rel_Type      : in Unbounded_String);
    -- TO-DO: only handle 2 layer for now; ignore rt

    procedure Read_Parts
      (Parse_Filename : in String);

    procedure Preprocess_Arguments;

    procedure Match
      (Articles : in Article_Sets.Set);

    procedure Match
      (Articles                 : in Article_Sets.Set;
       Question                 : in Question_Type;
       Atom_ID                  : in Atom_ID_Type;
       Argument_Cluster_Index   : in Argument_Cluster_Index_Type;
       Argument_Cluster_Index_2 : in Argument_Cluster_Index_Type);
    -- aci: given; aci2: ans

    function Is_Match
      (Dependent_Atom_ID : in Atom_ID_Type;
       Argument          : in Unbounded_String) return Boolean;

    procedure Find_Answer
      (Articles : in Article_Sets.Set;
       Question : in Question_Type;
       Atom_ID  : in Atom_ID_Type);

    function Find_Answer
      (Atom_ID                : in     Atom_ID_Type;
       Atom_ID_To_Min_Atom_ID : in out Atom_ID_To_Atom_ID_Maps.Map) return Atom_ID_Ordered_Set_Vectors.Vector;
    --  minPid for inserting prep, if any

    function Is_Match_From_Head
      (Dependent_Atom_ID          : in Atom_ID_Type;
       Str_Cluster_Indexes_Vector : Str_Cluster_Indexes_Vectors.Vector) return Boolean;

    function Get_Tree_Cluster_Indexes
      (Atom_ID : in Atom_ID_Type) return Cluster_Index_Sets.Set;

    procedure Print_Answers
      (Articles : in Article_Sets.Set);

end USP_Eval;
