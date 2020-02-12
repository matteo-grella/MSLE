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

with Ada.Strings.Fixed;

package body Commons is

    ------------
    function Str
    ------------
      (I : Integer) return String is

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
    end Str;

    --      ------------------
    --      function To_String
    --      ------------------
    --        (Atom_ID : Atom_ID_Type) return String is
    --          A : Unbounded_String := Ustr(Str(Integer(Atom_ID.Article_ID)));
    --          S : Unbounded_String := Ustr(Str(Integer(Atom_ID.Sentence_ID)));
    --          W : Unbounded_String := Ustr(Str(Integer(Atom_ID.Word_ID)));
    --      begin
    --          while Length(A) < 4 loop A := "0" & A; end loop;
    --          while Length(S) < 3 loop S := "0" & S; end loop;
    --          while Length(W) < 3 loop W := "0" & W; end loop;
    --          return Str(A & ":" & S & ":" & W);
    --      end To_String;

    --------------------
    function From_String
    --------------------
      (S : String) return Atom_ID_Type is
        Index_1 : constant Natural := Ada.Strings.Fixed.Index(S, ":");
        Index_2 : constant Natural := Ada.Strings.Fixed.Index(S, ":", Index_1 + 1);

        Atom_ID : Atom_ID_Type;
    begin
        Atom_ID.Article_ID  := Article_Index_Type'Value(S(S'First .. Index_1 - 1));
        Atom_ID.Sentence_ID := Sentence_Index_Type'Value(S(Index_1 + 1 .. Index_2 - 1));
        Atom_ID.Word_ID     := Word_Index_Type'Value(S(Index_2 + 1 .. S'Last));
        return Atom_ID;
    end From_String;

    -------------
    function Hash
    -------------
      (Atom_ID : Atom_ID_Type) return Ada.Containers.Hash_Type is
        Ret : Hash_Type := 0; -- Seed
    begin
        Ret := Ret * 101 + Hash_Type(Atom_ID.Article_ID);
        Ret := Ret * 101 + Hash_Type(Atom_ID.Sentence_ID);
        Ret := Ret * 101 + Hash_Type(Atom_ID.Word_ID);
        return Ret;
    end Hash;

    -----------------------------
    function Check_Word_Ancestor
    -----------------------------
      (Sentence : in Word_Vector.Vector;
       Word_ID  : in Word_Index_Type) return Boolean is

        Ancestor : Word_Index_Type := Word_ID;
    begin
        while Sentence.Element(Ancestor).Head /= -1 loop
            Ancestor := Sentence.Element(Ancestor).Head;
        end loop;
        return Ancestor = 0;
    end Check_Word_Ancestor;

    ---------------------
    function Gen_Rel_Type
    ---------------------
      (Atom : Atom_Type) return Unbounded_String is

        use Deprel_To_Atom_Set_Maps;

        Rel_Type : Unbounded_String;

        Deprel_Cursor : Cursor := Atom.Dependents.First;
    begin

        Rel_Type := Ustr("(");
        Append(Rel_Type, To_String(Atom));

        while Has_Element(Deprel_Cursor) loop
            declare
                Deprel  : constant Unbounded_String := Key(Deprel_Cursor);
                Deps    : constant Atom_Sets.Set    := Element(Deprel_Cursor);
            begin
                Append(Rel_Type, " (");
                Append(Rel_Type, Deprel);

                for Dep of Deps loop
                    Append(Rel_Type, " ");
                    Append(Rel_Type, Gen_Rel_Type(Dep.all));
                end loop;
                Append(Rel_Type, ")");
            end;
            Next(Deprel_Cursor);
        end loop;

        Append(Rel_Type, ")");

        return Rel_Type;
    end Gen_Rel_Type;

    ---------------------
    function Get_Tree_Str
    ---------------------
      (Atom : in Atom_Type) return Unbounded_String is

        ID_Str        : ID_To_String_Maps.Map;
        ID_Str_Cursor : ID_To_String_Maps.Cursor;

        Dependent_Cursor : Deprel_To_Atom_Set_Maps.Cursor;
        Ret              : Unbounded_String;
    begin

        if not Atom.Dependents.Is_Empty then
            Dependent_Cursor := Atom.Dependents.First;
            while Deprel_To_Atom_Set_Maps.Has_Element(Dependent_Cursor) loop
                declare
                    Deprel   : constant Unbounded_String := Deprel_To_Atom_Set_Maps.Key(Dependent_Cursor);
                    Atom_Set : constant Atom_Sets.Set    := Deprel_To_Atom_Set_Maps.Element(Dependent_Cursor);
                    S_Deprel : constant String           := Str(Deprel);
                begin

                    for Dependent_Atom of Atom_Set loop
                        declare
                            S : Unbounded_String;
                        begin
                            if Index(Deprel, "prep_") = S_Deprel'First then
                                Append(S, S_Deprel(S_Deprel'First + 5 .. S_Deprel'Last));
                                Append(S, " ");
                            elsif Index(Deprel, "conj_") = Str(Deprel)'First then
                                Append(S, S_Deprel(S_Deprel'First + 5 .. S_Deprel'Last));
                                Append(S, " ");
                            end if;
                            Append(S, Get_Tree_Str(Dependent_Atom.all));
                            ID_Str.Insert(Dependent_Atom.ID, S);
                        end;
                    end loop;
                end;
                Deprel_To_Atom_Set_Maps.Next(Dependent_Cursor);
            end loop;
        end if;
        ID_Str.Insert(Atom.ID, Atom.Lemma);

        ID_Str_Cursor := ID_Str.First;
        while ID_To_String_Maps.Has_Element(ID_Str_Cursor) loop
            if Length(Ret) > 0 then
                Append(Ret, " ");
            end if;
            Append(Ret, ID_To_String_Maps.Element(ID_Str_Cursor));

            ID_To_String_Maps.Next(ID_Str_Cursor);
        end loop;

        return Ret;
    end Get_Tree_Str;

    ----------------
    procedure Insert
    ----------------
      (Map      : in out Rel_Type_Str_To_Index_Maps.Map;
       Key      : in     Unbounded_String;
       New_Item : in     Rel_Type_Index_Type;
       Position : out    Rel_Type_Str_To_Index_Maps.Cursor) is

        Inserted : Boolean;
    begin
        Map.Insert
          (Key      => Key,
           New_Item => New_Item,
           Position => Position,
           Inserted => Inserted);

        if not Inserted then
            raise MLSE_Insertion_Error;
        end if;
    end Insert;

    ----------------
    procedure Insert
    ----------------
      (Map      : in out Rel_Type_Index_To_Cluster_Index_Set_Maps.Map;
       Key      : in     Rel_Type_Index_Type;
       Position : out    Rel_Type_Index_To_Cluster_Index_Set_Maps.Cursor) is

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
      (Map          : in out Rel_Type_Index_To_Cluster_Index_Set_Maps.Map;
       Map_Cursor   : in     Rel_Type_Index_To_Cluster_Index_Set_Maps.Cursor;
       Set_New_Item : in     Cluster_Index_Type) is

        procedure Update
          (Rel_Type_Index    : in     Rel_Type_Index_Type;
           Cluster_Index_Set : in out Cluster_Index_Sets.Set) is
            pragma Unreferenced (Rel_Type_Index);
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

    ----------------
    procedure Insert
    ----------------
      (Map      : in out Cluster_Index_To_Atom_ID_Set_Maps.Map;
       Key      : in     Cluster_Index_Type;
       Position : out    Cluster_Index_To_Atom_ID_Set_Maps.Cursor) is

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
      (Map          : in out Cluster_Index_To_Atom_ID_Set_Maps.Map;
       Map_Cursor   : in     Cluster_Index_To_Atom_ID_Set_Maps.Cursor;
       Set_New_Item : in     Atom_ID_Type) is

        procedure Update
          (Cluster_Index  : in     Cluster_Index_Type;
           Atom_ID_Set    : in out Atom_ID_Sets.Set) is
            pragma Unreferenced (Cluster_Index);
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

    -------------------------
    procedure Delete_From_Set
    -------------------------
      (Map          : in out Cluster_Index_To_Atom_ID_Set_Maps.Map;
       Map_Position : in     Cluster_Index_To_Atom_ID_Set_Maps.Cursor;
       Set_Item     : in     Atom_ID_Type) is

        procedure Update
          (Cluster_Index  : in     Cluster_Index_Type;
           Atom_ID_Set    : in out Atom_ID_Sets.Set) is
            pragma Unreferenced (Cluster_Index);
            Position : Atom_ID_Sets.Cursor
              := Atom_ID_Set.Find(Set_Item);
        begin
            if Atom_ID_Sets.Has_Element(Position) then
                Atom_ID_Set.Delete(Position);
            end if;
        end Update;
        pragma Inline(Update);
   begin


         Map.Update_Element(Map_Position, Update'Access);

    end Delete_From_Set;

    -------------------
    procedure Increment
    -------------------
      (Map      : in out Rel_Type_Index_To_Count_Maps.Map;
       Position : in     Rel_Type_Index_To_Count_Maps.Cursor) is

        procedure Update
          (Rel_Type_Index : in     Rel_Type_Index_Type;
           Count          : in out Integer) is -- TODO: Natural
            pragma Unreferenced (Rel_Type_Index);
        begin
            Count := Count + 1;
        end Update;
        pragma Inline(Update);
    begin
        Map.Update_Element(Position, Update'Access);
    end Increment;

    -------------------
    procedure Decrement
    -------------------
      (Map      : in out Rel_Type_Index_To_Count_Maps.Map;
       Position : in     Rel_Type_Index_To_Count_Maps.Cursor) is

        procedure Update
          (Rel_Type_Index : in     Rel_Type_Index_Type;
           Count          : in out Integer) is -- TODO: Natural
            pragma Unreferenced (Rel_Type_Index);
        begin
            Count := Count - 1;
        end Update;
        pragma Inline(Update);
    begin
        Map.Update_Element(Position, Update'Access);
    end Decrement;

    ----------------
    procedure Insert
    ----------------
      (Set      : in out Argument_Cluster_Index_Sets.Set;
       New_Item : in     Argument_Cluster_Index_Type) is

        Position : Argument_Cluster_Index_Sets.Cursor;
        Inserted : Boolean;
    begin
        Set.Insert
          (New_Item => New_Item,
           Position => Position,
           Inserted => Inserted);
    end Insert;

    -------------------------
    procedure Delete_From_Set
    -------------------------
      (Map          : in out Cluster_Index_To_Cluster_Link_Set_Maps.Map;
       Map_Position : in     Cluster_Index_To_Cluster_Link_Set_Maps.Cursor;
       Set_Item     : in     Cluster_Link_Type) is

        procedure Update
          (Cluster_Index    : in     Cluster_Index_Type;
           Cluster_Link_Set : in out Cluster_Link_Sets.Set) is
            pragma Unreferenced (Cluster_Index);
            Position : Cluster_Link_Sets.Cursor
              := Cluster_Link_Set.Find(Set_Item);
        begin
            if Cluster_Link_Sets.Has_Element(Position) then
                Cluster_Link_Set.Delete(Position);
            end if;
        end Update;
        pragma Inline(Update);
    begin
        Map.Update_Element(Map_Position, Update'Access);
    end Delete_From_Set;

    -------------------------
    procedure Delete_From_Set
    -------------------------
      (Map          : in out Cluster_Link_To_Atom_Link_Set_Maps.Map;
       Map_Position : in     Cluster_Link_To_Atom_Link_Set_Maps.Cursor;
       Set_Item     : in     Atom_Link_Type) is

        procedure Update
          (Cluster_Link  : in     Cluster_Link_Type;
           Atom_Link_Set : in out Atom_Link_Sets.Set) is
            pragma Unreferenced (Cluster_Link);
        begin
            Atom_Link_Set.Delete(Set_Item);
        end Update;
        pragma Inline(Update);
    begin
        Map.Update_Element(Map_Position, Update'Access);
    end Delete_From_Set;

    -------------------
    procedure Increment
    -------------------
      (Map      : in out Cluster_Index_Pair_To_Conj_Count_Maps.Map;
       Position : in     Cluster_Index_Pair_To_Conj_Count_Maps.Cursor) is

        procedure Update
          (Cluster_Index_Pair : in     Cluster_Index_Pair_Type;
           Count              : in out Natural) is
            pragma Unreferenced (Cluster_Index_Pair);
        begin
            Count := Count + 1;
        end Update;
        pragma Inline(Update);
    begin
        Map.Update_Element(Position, Update'Access);
    end Increment;

    -------------------
    procedure Decrement
    -------------------
      (Map      : in out Cluster_Index_Pair_To_Conj_Count_Maps.Map;
       Position : in     Cluster_Index_Pair_To_Conj_Count_Maps.Cursor) is

        procedure Update
          (Cluster_Index_Pair : in     Cluster_Index_Pair_Type;
           Count              : in out Natural) is
            pragma Unreferenced (Cluster_Index_Pair);
        begin
            Count := Count - 1;
        end Update;
        pragma Inline(Update);
    begin
        Map.Update_Element(Position, Update'Access);
    end Decrement;

    ----------------
    procedure Insert
    ----------------
      (Map      : in out Cluster_Index_To_Cluster_Link_Set_Maps.Map;
       Key      : in     Cluster_Index_Type;
       Position : out    Cluster_Index_To_Cluster_Link_Set_Maps.Cursor) is

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
      (Map          : in out Cluster_Index_To_Cluster_Link_Set_Maps.Map;
       Map_Cursor   : in     Cluster_Index_To_Cluster_Link_Set_Maps.Cursor;
       Set_New_Item : in     Cluster_Link_Type) is

        procedure Update
          (Cluster_Index    : in     Cluster_Index_Type;
           Cluster_Link_Set : in out Cluster_Link_Sets.Set) is
            pragma Unreferenced (Cluster_Index);
            Position : Cluster_Link_Sets.Cursor;
            Inserted : Boolean;
        begin
            Cluster_Link_Set.Insert
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
      (Map      : in out Cluster_Link_To_Atom_Link_Set_Maps.Map;
       Key      : in     Cluster_Link_Type;
       Position : out    Cluster_Link_To_Atom_Link_Set_Maps.Cursor) is

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
      (Map          : in out Cluster_Link_To_Atom_Link_Set_Maps.Map;
       Map_Cursor   : in     Cluster_Link_To_Atom_Link_Set_Maps.Cursor;
       Set_New_Item : in     Atom_Link_Type) is

        procedure Update
          (Cluster_Link  : in     Cluster_Link_Type;
           Atom_Link_Set : in out Atom_Link_Sets.Set) is
            pragma Unreferenced (Cluster_Link);
            Position : Atom_Link_Sets.Cursor;
            Inserted : Boolean;
        begin
            Atom_Link_Set.Insert
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
      (Map      : in out Deprel_To_Index_Maps.Map;
       Key      : in     Unbounded_String;
       New_Item : in     Deprel_Index_Type;
       Position : out    Deprel_To_Index_Maps.Cursor) is

        Inserted : Boolean;
    begin
        Map.Insert
          (Key      => Key,
           New_Item => New_Item,
           Position => Position,
           Inserted => Inserted);

        if not Inserted then
            raise MLSE_Insertion_Error;
        end if;
    end Insert;

    ----------------
    procedure Insert
    ----------------
      (Map      : in out Argument_Cluster_Index_To_Part_Indexes_Maps.Map;
       Key      : in     Argument_Cluster_Index_Type;
       Position : out    Argument_Cluster_Index_To_Part_Indexes_Maps.Cursor) is

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
      (Map          : in out Argument_Cluster_Index_To_Part_Indexes_Maps.Map;
       Map_Cursor   : in     Argument_Cluster_Index_To_Part_Indexes_Maps.Cursor;
       Set_New_Item : in     Part_Index_Type) is

        procedure Update
          (Argument_Cluster_Index  : in     Argument_Cluster_Index_Type;
           Part_Index_Set          : in out Part_Index_Sets.Set) is
            pragma Unreferenced (Argument_Cluster_Index);
            Position : Part_Index_Sets.Cursor;
            Inserted : Boolean;
        begin
            Part_Index_Set.Insert
              (New_Item => Set_New_Item,
               Position => Position,
               Inserted => Inserted);
        end Update;
        pragma Inline(Update);

    begin
        Map.Update_Element(Map_Cursor, Update'Access);
    end Add_To_Set;

    -------------------
    procedure Increment
    -------------------
      (Map      : in out Deprel_Index_To_Count_Maps.Map;
       Position : in     Deprel_Index_To_Count_Maps.Cursor) is

        procedure Update
          (Deprel_Index : in     Deprel_Index_Type;
           Count        : in out Integer) is
            pragma Unreferenced (Deprel_Index);
        begin
            Count := Count + 1;
        end Update;
        pragma Inline(Update);
    begin
        Map.Update_Element(Position, Update'Access);
    end Increment;

    -------------------
    procedure Decrement
    -------------------
      (Map      : in out Deprel_Index_To_Count_Maps.Map;
       Position : in     Deprel_Index_To_Count_Maps.Cursor) is

        procedure Update
          (Deprel_Index : in     Deprel_Index_Type;
           Count        : in out Integer) is
            pragma Unreferenced (Deprel_Index);
        begin
            Count := Count - 1;
        end Update;
        pragma Inline(Update);
    begin
        Map.Update_Element(Position, Update'Access);
    end Decrement;

    -------------------
    procedure Increment
    -------------------
      (Map      : in out Cluster_Index_To_Count_Maps.Map;
       Position : in     Cluster_Index_To_Count_Maps.Cursor) is

        procedure Update
          (Cluster_Index : in     Cluster_Index_Type;
           Count         : in out Integer) is
            pragma Unreferenced (Cluster_Index);
        begin
            Count := Count + 1;
        end Update;
        pragma Inline(Update);
    begin
        Map.Update_Element(Position, Update'Access);
    end Increment;

    -------------------
    procedure Decrement
    -------------------
      (Map      : in out Cluster_Index_To_Count_Maps.Map;
       Position : in     Cluster_Index_To_Count_Maps.Cursor) is

        procedure Update
          (Cluster_Index : in     Cluster_Index_Type;
           Count         : in out Integer) is
            pragma Unreferenced (Cluster_Index);
        begin
            Count := Count - 1;
        end Update;
        pragma Inline(Update);
    begin
        Map.Update_Element(Position, Update'Access);
    end Decrement;

    -------------------
    procedure Increment
    -------------------
      (Map      : in out Cluster_And_Argument_Cluster_Link_Type_To_Count_Maps.Map;
       Position : in     Cluster_And_Argument_Cluster_Link_Type_To_Count_Maps.Cursor) is

        procedure Update
          (Cluster_And_Argument_Cluster_Link : in     Cluster_And_Argument_Cluster_Link_Type;
           Count                             : in out Natural) is
            pragma Unreferenced (Cluster_And_Argument_Cluster_Link);
        begin
            Count := Count + 1;
        end Update;
        pragma Inline(Update);
    begin
        Map.Update_Element(Position, Update'Access);
    end Increment;

    -------------------
    procedure Decrement
    -------------------
      (Map      : in out Cluster_And_Argument_Cluster_Link_Type_To_Count_Maps.Map;
       Position : in     Cluster_And_Argument_Cluster_Link_Type_To_Count_Maps.Cursor) is

        procedure Update
          (Cluster_And_Argument_Cluster_Link : in     Cluster_And_Argument_Cluster_Link_Type;
           Count                             : in out Natural) is
            pragma Unreferenced (Cluster_And_Argument_Cluster_Link);
        begin
            Count := Count - 1;
        end Update;
        pragma Inline(Update);
    begin
        Map.Update_Element(Position, Update'Access);
    end Decrement;

    ----------------
    procedure Insert
    ----------------
      (Map      : in out Cluster_Index_To_Cluster_And_Argument_Cluster_Link_Type_To_Count_Map_Maps.Map;
       Key      : in     Cluster_Index_Type;
       Position : out    Cluster_Index_To_Cluster_And_Argument_Cluster_Link_Type_To_Count_Map_Maps.Cursor) is

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

    -------------------
    procedure Increment
    -------------------
      (Map      : in out Num_Arg_To_Count_Maps.Map;
       Position : in     Num_Arg_To_Count_Maps.Cursor) is

        procedure Update
          (Num_Arg : in     Num_Arg_Type;
           Count   : in out Integer) is
            pragma Unreferenced (Num_Arg);
        begin
            Count := Count + 1;
        end Update;
        pragma Inline(Update);
    begin
        Map.Update_Element(Position, Update'Access);
    end Increment;

    -------------------
    procedure Decrement
    -------------------
      (Map      : in out Num_Arg_To_Count_Maps.Map;
       Position : in     Num_Arg_To_Count_Maps.Cursor) is

        procedure Update
          (Num_Arg : in     Num_Arg_Type;
           Count   : in out Integer) is
            pragma Unreferenced (Num_Arg);
        begin
            Count := Count - 1;
        end Update;
        pragma Inline(Update);
    begin
        Map.Update_Element(Position, Update'Access);
    end Decrement;

    ----------------
    procedure Insert
    ----------------
      (Set      : in out Atom_ID_Sets.Set;
       New_Item : in     Atom_ID_Type) is

        Position : Atom_ID_Sets.Cursor;
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
      (Set      : in out Deprel_Index_Sets.Set;
       New_Item : in     Deprel_Index_Type) is

        Position : Deprel_Index_Sets.Cursor;
        Inserted : Boolean;
    begin
        Set.Insert
          (New_Item => New_Item,
           Position => Position,
           Inserted => Inserted);
    end Insert;

    -------------------
    procedure Increment
    -------------------
      (Map      : in out Cluster_Index_Root_Count_Maps.Map;
       Position : in     Cluster_Index_Root_Count_Maps.Cursor) is

        procedure Update
          (Cluster_Index : in     Cluster_Index_Type;
           Count         : in out Integer) is
            pragma Unreferenced (Cluster_Index);
        begin
            Count := Count + 1;
        end Update;
        pragma Inline(Update);
    begin
        Map.Update_Element(Position, Update'Access);
    end Increment;

    -------------------
    procedure Decrement
    -------------------
      (Map      : in out Cluster_Index_Root_Count_Maps.Map;
       Position : in     Cluster_Index_Root_Count_Maps.Cursor) is

        procedure Update
          (Cluster_Index : in     Cluster_Index_Type;
           Count         : in out Integer) is
            pragma Unreferenced (Cluster_Index);
        begin
            Count := Count - 1;
        end Update;
        pragma Inline(Update);
    begin
        Map.Update_Element(Position, Update'Access);
    end Decrement;

    ----------------
    procedure Insert
    ----------------
      (Set                         : in out Search_Operation_Sets.Set;
       New_Item                    : in out Search_Operation_Type;
       Free_Repr_When_Not_Inserted : in     Boolean) is

        Position : Search_Operation_Sets.Cursor;
        Inserted : Boolean;
    begin
        Set.Insert
          (New_Item => New_Item,
           Position => Position,
           Inserted => Inserted);
        if not Inserted and then Free_Repr_When_Not_Inserted then
            Free(New_Item.Repr);
        end if;
    end Insert;

    ----------------
    procedure Insert
    ----------------
      (Set      : in out Neighs_Sets.Set;
       New_Item : in     Neigh_Type_Type) is

        Position : Neighs_Sets.Cursor;
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
      (Map      : in out Search_Operation_To_Neighs_Set_Maps.Map;
       Key      : in     Search_Operation_Type;
       Position : out    Search_Operation_To_Neighs_Set_Maps.Cursor) is

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

    -------------------
    procedure Increment
    -------------------
      (Map      : in out Compose_To_Count_Maps.Map;
       Position : in     Compose_To_Count_Maps.Cursor) is

        procedure Update
          (Search_Operation : in     Search_Operation_Type;
           Count            : in out Natural) is
            pragma Unreferenced (Search_Operation);
        begin
            Count := Count + 1;
        end Update;
        pragma Inline(Update);
    begin
        Map.Update_Element(Position, Update'Access);
    end Increment;

    --------------------
    procedure Add_To_Set
    --------------------
      (Map          : in out Cluster_Index_To_Search_Operation_Set_Maps.Map;
       Map_Cursor   : in     Cluster_Index_To_Search_Operation_Set_Maps.Cursor;
       Set_New_Item : in     Search_Operation_Type) is

        procedure Update
          (Cluster_Index        : in     Cluster_Index_Type;
           Search_Operation_Set : in out Search_Operation_Sets.Set) is
            pragma Unreferenced (Cluster_Index);
            Position : Search_Operation_Sets.Cursor;
            Inserted : Boolean;
        begin
            Search_Operation_Set.Insert
              (New_Item => Set_New_Item,
               Position => Position,
               Inserted => Inserted);
        end Update;
        pragma Inline(Update);

    begin
        Map.Update_Element(Map_Cursor, Update'Access);
    end Add_To_Set;

    procedure Delete_From_Set
      (Map          : in out Cluster_Index_To_Search_Operation_Set_Maps.Map;
       Map_Position : in     Cluster_Index_To_Search_Operation_Set_Maps.Cursor;
       Set_Item     : in     Search_Operation_Type)  is

        procedure Update
          (Cluster_Index        : in     Cluster_Index_Type;
           Search_Operation_Set : in out Search_Operation_Sets.Set) is
            pragma Unreferenced (Cluster_Index);
            Position : Search_Operation_Sets.Cursor
              := Search_Operation_Set.Find(Set_Item);
        begin
            if Search_Operation_Sets.Has_Element(Position) then
                Search_Operation_Set.Delete(Position);
            end if;
        end Update;
        pragma Inline(Update);
    begin
        Map.Update_Element(Map_Position, Update'Access);
    end Delete_From_Set;

    ----------------
    procedure Insert
    ----------------
      (Map      : in out Cluster_Index_To_Search_Operation_Set_Maps.Map;
       Key      : in     Cluster_Index_Type;
       Position : out    Cluster_Index_To_Search_Operation_Set_Maps.Cursor) is

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
      (Map          : in out Deprel_To_Atom_Set_Maps.Map;
       Map_Cursor   : in     Deprel_To_Atom_Set_Maps.Cursor;
       Set_New_Item : in     Atom_Access_Type) is

        procedure Update
          (Deprel   : in Unbounded_String;
           Atom_Set : in out Atom_Sets.Set) is
            pragma Unreferenced (Deprel);
            Position : Atom_Sets.Cursor;
            Inserted : Boolean;
        begin
            Atom_Set.Insert
              (New_Item => Set_New_Item,
               Position => Position,
               Inserted => Inserted);
        end Update;
        pragma Inline(Update);

    begin
        Map.Update_Element(Map_Cursor, Update'Access);
    end Add_To_Set;

    procedure Insert
      (Map      : in out Deprel_To_Atom_Set_Maps.Map;
       Key      : in     Unbounded_String;
       Position : out    Deprel_To_Atom_Set_Maps.Cursor) is

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

    -------------------
    procedure Increment
    -------------------
      (Map      : in out Rel_Type_Index_Link_To_Count_Maps.Map;
       Position : in     Rel_Type_Index_Link_To_Count_Maps.Cursor) is

        procedure Update
          (Rel_Type_Index_Link : in     Rel_Type_Index_Link_Type;
           Count               : in out Natural) is
            pragma Unreferenced (Rel_Type_Index_Link);
        begin
            Count := Count + 1;
        end Update;
        pragma Inline(Update);
    begin
        Map.Update_Element(Position, Update'Access);
    end Increment;

    -------------------
    procedure Increment
    -------------------
      (Map      : in out Argument_Cluster_Index_To_Root_Node_Count_Maps.Map;
       Position : in     Argument_Cluster_Index_To_Root_Node_Count_Maps.Cursor) is

        procedure Update
          (Argument_Cluster_Index : in     Argument_Cluster_Index_Type;
           Count                  : in out Integer) is
            pragma Unreferenced (Argument_Cluster_Index);
        begin
            Count := Count + 1;
        end Update;
        pragma Inline(Update);
    begin
        Map.Update_Element(Position, Update'Access);
    end Increment;

    -------------------
    procedure Decrement
    -------------------
      (Map      : in out Argument_Cluster_Index_To_Root_Node_Count_Maps.Map;
       Position : in     Argument_Cluster_Index_To_Root_Node_Count_Maps.Cursor) is

        procedure Update
          (Argument_Cluster_Index : in     Argument_Cluster_Index_Type;
           Count                  : in out Integer) is
            pragma Unreferenced (Argument_Cluster_Index);
        begin
            Count := Count - 1;
        end Update;
        pragma Inline(Update);
    begin
        Map.Update_Element(Position, Update'Access);
    end Decrement;

    -------------------
    procedure Increment
    -------------------
      (Map      : in out Argument_Cluster_Index_To_Dependents_Count_Maps.Map;
       Position : in     Argument_Cluster_Index_To_Dependents_Count_Maps.Cursor) is

        procedure Update
          (Argument_Cluster_Index : in     Argument_Cluster_Index_Type;
           Count                  : in out Integer) is
            pragma Unreferenced (Argument_Cluster_Index);
        begin
            Count := Count + 1;
        end Update;
        pragma Inline(Update);
    begin
        Map.Update_Element(Position, Update'Access);
    end Increment;

    -------------------
    procedure Decrement
    -------------------
      (Map      : in out Argument_Cluster_Index_To_Dependents_Count_Maps.Map;
       Position : in     Argument_Cluster_Index_To_Dependents_Count_Maps.Cursor) is

        procedure Update
          (Argument_Cluster_Index : in     Argument_Cluster_Index_Type;
           Count                  : in out Integer) is
            pragma Unreferenced (Argument_Cluster_Index);
        begin
            Count := Count - 1;
        end Update;
        pragma Inline(Update);
    begin
        Map.Update_Element(Position, Update'Access);
    end Decrement;

    ----------------
    procedure Insert
    ----------------
      (Map      : in out Argument_Cluster_Index_To_Num_Arg_To_Count_Maps.Map;
       Key      : in     Argument_Cluster_Index_Type;
       Position : out    Argument_Cluster_Index_To_Num_Arg_To_Count_Maps.Cursor) is

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
      (Map      : in out Argument_Cluster_Index_To_Deprel_Index_To_Count_Maps.Map;
       Key      : in     Argument_Cluster_Index_Type;
       Position : out    Argument_Cluster_Index_To_Deprel_Index_To_Count_Maps.Cursor) is

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
      (Map      : in out Argument_Cluster_Index_To_Cluster_Index_To_Count_Maps.Map;
       Key      : in     Argument_Cluster_Index_Type;
       Position : out    Argument_Cluster_Index_To_Cluster_Index_To_Count_Maps.Cursor) is

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
      (Set      : in out Cluster_Index_Sets.Set;
       New_Item : in     Cluster_Index_Type) is

        Position : Cluster_Index_Sets.Cursor;
        Inserted : Boolean;
    begin
        Set.Insert
          (New_Item => New_Item,
           Position => Position,
           Inserted => Inserted);
    end Insert;

    -----
    -- Cluster_Access_Type Streams
    -----

    ----------------------
    procedure Stream_Write
    ----------------------
      (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
       Item   : in  Cluster_Access_Type) is
    begin
        Cluster_Type'Write(Stream, Item.all);
    end Stream_Write;

    -----------------------
    procedure Stream_Output
    -----------------------
      (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
       Item   : in  Cluster_Access_Type) is
    begin
        Cluster_Type'Output(Stream, Item.all);
    end Stream_Output;

    ---------------------
    procedure Stream_Read
    ---------------------
      (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
       Item   : out Cluster_Access_Type) is
    begin
        Item := new Cluster_Type;
        Cluster_Type'Read(Stream, Item.all);
    end Stream_Read;

    ---------------------
    function Stream_Input
    ---------------------
      (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
       return Cluster_Access_Type is
    begin
        return new Cluster_Type'(Cluster_Type'Input(Stream));
    end Stream_Input;

    -----
    -- Argument_Cluster_Access_Type Streams
    -----

    ----------------------
    procedure Stream_Write
    ----------------------
      (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
       Item   : in  Argument_Cluster_Access_Type) is
    begin
        Argument_Cluster_Type'Write(Stream, Item.all);
    end Stream_Write;

    -----------------------
    procedure Stream_Output
    -----------------------
      (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
       Item   : in  Argument_Cluster_Access_Type) is
    begin
        Argument_Cluster_Type'Output(Stream, Item.all);
    end Stream_Output;

    ---------------------
    procedure Stream_Read
    ---------------------
      (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
       Item   : out Argument_Cluster_Access_Type) is
    begin
        Item := new Argument_Cluster_Type;
        Argument_Cluster_Type'Read(Stream, Item.all);
    end Stream_Read;

    ---------------------
    function Stream_Input
    ---------------------
      (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
       return Argument_Cluster_Access_Type is
    begin
        return new Argument_Cluster_Type'(Argument_Cluster_Type'Input(Stream));
    end Stream_Input;

    -----
    -- Integer_Array_Access_Type Streams
    -----

    ----------------------
    procedure Stream_Write
    ----------------------
      (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
       Item   : in  Integer_Array_Access_Type) is
    begin
        Positive'Write(Stream, Item'First);
        Positive'Write(Stream, Item'Last);
        Integer_Array_Type'Write(Stream, Item.all);
    end Stream_Write;

    -----------------------
    procedure Stream_Output
    -----------------------
      (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
       Item   : in  Integer_Array_Access_Type) is
    begin
        Positive'Output(Stream, Item'First);
        Positive'Output(Stream, Item'Last);
        Integer_Array_Type'Output(Stream, Item.all);
    end Stream_Output;

    ---------------------
    procedure Stream_Read
    ---------------------
      (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
       Item   : out Integer_Array_Access_Type) is
        First, Last : Positive;
    begin
        Positive'Read(Stream, First);
        Positive'Read(Stream, Last);

        Item := new Integer_Array_Type(First .. Last);
        Integer_Array_Type'Read(Stream, Item.all);
    end Stream_Read;

    ---------------------
    function Stream_Input
    ---------------------
      (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
       return Integer_Array_Access_Type is
        First, Last : Positive;
    begin
        return Item : Integer_Array_Access_Type := null do
            First := Positive'Input(Stream);
            Last  := Positive'Input(Stream);

            Item := new Integer_Array_Type(First .. Last);
            Item.all := Integer_Array_Type'Input(Stream);

            --Ada.Text_IO.Put_Line(">>>>>>>>>>Stream Input");
            --return new Integer_Array_Type'(Integer_Array_Type'Input(Stream));
        end return;
    end Stream_Input;

end Commons;
