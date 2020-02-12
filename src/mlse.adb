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
with Ada.Streams.Stream_IO;
with Create_Agenda_Manager; use Create_Agenda_Manager;
with Ada.Containers;

package body MLSE is

    package T_IO renames Ada.Text_IO;
    use type Ada.Containers.Count_Type;

    ----------------------------
    function Get_Rel_Type_Index
    ----------------------------
      (Atom : Atom_Type) return Rel_Type_Index_Type is

        Rel_Type : constant Unbounded_String
          := Gen_Rel_Type(Atom); -- generate unique tree type string

        Cursor   : Rel_Type_Str_To_Index_Maps.Cursor
          := Rel_Type_Str_To_Index_Map.Find(Rel_Type);
    begin
        if not Rel_Type_Str_To_Index_Maps.Has_Element(Cursor) then
            Rel_Type_Str_Vector.Append(Rel_Type);

            Insert
              (Map      => Rel_Type_Str_To_Index_Map,
               Key      => Rel_Type,
               New_Item => Rel_Type_Str_Vector.Last_Index,
               Position => Cursor);
        end if;

        return Rel_Type_Str_To_Index_Maps.Element(Cursor);
    end Get_Rel_Type_Index;

    -----------------------------------
    function Get_Clusters_With_Rel_Type
    -----------------------------------
      (Rel_Type_Index : Rel_Type_Index_Type) return Cluster_Index_Sets.Set is
    begin
        if Rel_Type_Index_To_Cluster_Index_Set_Map.Contains(Rel_Type_Index) then
            return Rel_Type_Index_To_Cluster_Index_Set_Map.Element(Rel_Type_Index);
        else
            return Cluster_Index_Sets.Empty_Set;
        end if;
    end Get_Clusters_With_Rel_Type;

    ------------------------
    function Create_Cluster
    ------------------------
      (Rel_Type_Index : Rel_Type_Index_Type;
       Is_Content     : Boolean := False) return Cluster_Index_Type is

        Cluster  : constant Cluster_Access_Type
          := new Cluster_Type;

        Rel_Type : constant Unbounded_String
          := Rel_Type_Str_Vector.Element(Rel_Type_Index);

        Cursor   : Rel_Type_Index_To_Cluster_Index_Set_Maps.Cursor
          := Rel_Type_Index_To_Cluster_Index_Set_Map.Find(Rel_Type_Index);
    begin
        Cluster.Index      := Next_Cluster_Index;
        Next_Cluster_Index := Next_Cluster_Index + 1;

        Cluster.Is_Content := Is_Content;

        if Str(Rel_Type) in "(V:be)" | "(N:%)" | "(V:say)" | "($:$)" then
            Cluster.Is_Stop := True;
        end if;

        Cluster_Map.Insert(Cluster.Index, Cluster);

        if not Rel_Type_Index_To_Cluster_Index_Set_Maps.Has_Element(Cursor) then
            Insert
              (Map      => Rel_Type_Index_To_Cluster_Index_Set_Map,
               Key      => Rel_Type_Index,
               Position => Cursor);
        end if;

        Add_To_Set
          (Map          => Rel_Type_Index_To_Cluster_Index_Set_Map,
           Map_Cursor   => Cursor,
           Set_New_Item => Cluster.Index);

        return Cluster.Index;
    end Create_Cluster;

    --------------------------------
    procedure Link_Part_And_Cluster
    --------------------------------
      (Part           : in out Part_Type;
       Cluster_Access : in     Cluster_Access_Type) is
    begin

        -- Update Part

        Part.Cluster_Index  := Cluster_Access.Index;
        Part.Cluster_Access := Cluster_Access;

        declare
            Cursor   : Cluster_Index_To_Atom_ID_Set_Maps.Cursor
              := Cluster_Index_To_Atom_ID_Set_Map.Find(Cluster_Access.Index);

        begin
            if not Cluster_Index_To_Atom_ID_Set_Maps.Has_Element(Cursor) then
                Insert
                  (Map      => Cluster_Index_To_Atom_ID_Set_Map,
                   Key      => Cluster_Access.Index,
                   Position => Cursor);
            end if;

            Add_To_Set
              (Map          => Cluster_Index_To_Atom_ID_Set_Map,
               Map_Cursor   => Cursor,
               Set_New_Item => Part.Root_Node.ID);
        end;

        -- Update Cluster

        Cluster_Access.Part_Count := Cluster_Access.Part_Count + 1;

        declare
            Cursor   : constant Rel_Type_Index_To_Count_Maps.Cursor
              := Cluster_Access.Rel_Type_Index_To_Count_Map.Find(Part.Rel_Type_Index);
        begin
            if not Rel_Type_Index_To_Count_Maps.Has_Element(Cursor) then
                Cluster_Access.Rel_Type_Index_To_Count_Map.Insert
                  (Key      => Part.Rel_Type_Index,
                   New_Item => 1);
            else
                Increment
                  (Map      => Cluster_Access.Rel_Type_Index_To_Count_Map,
                   Position => Cursor);
            end if;
        end;
    end Link_Part_And_Cluster;

    ---------------------
    function Add_Argument
    ---------------------
      (Governor_Part         : in out Part_Type;
       Dependent_Part_Access : in     Part_Access_Type;
       Deprel                : in     Unbounded_String;
       Deprel_Index          : in     Deprel_Index_Type) return Part_Index_Type is

        Argument_Index : constant Part_Index_Type := Governor_Part.Next_Argument_Index;
    begin
        Governor_Part.Argument_Part_Map.Insert(Argument_Index, Dependent_Part_Access);
        Governor_Part.Next_Argument_Index := Governor_Part.Next_Argument_Index + 1;

        if Length(Deprel) /= 0 then
            Dependent_Part_Access.Deprel         := Deprel;
            Dependent_Part_Access.Deprel_Index   := Deprel_Index;
        end if;

        return Argument_Index;
    end Add_Argument;

    -----------------------------------
    procedure Remove_Argument_From_Part
    -----------------------------------
      (Part                                      : in out Part_Type;
       Dependent_Argument_Index_In_Governor_Part : in     Part_Index_Type) is

        Old_Argument_Cluster_Index : constant Argument_Cluster_Index_Type
          := Part.Argument_Index_To_Argument_Cluster_Index_Map.Element(Dependent_Argument_Index_In_Governor_Part);

        Cluster : Cluster_Access_Type renames Part.Cluster_Access;

        Argument_Part    : constant Part_Access_Type
          := Part.Argument_Part_Map.Element(Dependent_Argument_Index_In_Governor_Part);
    begin

        Part.Argument_Index_To_Argument_Cluster_Index_Map.Delete(Dependent_Argument_Index_In_Governor_Part);

        declare
            Part_Index_Set_Size : Integer := -1;

            procedure Remove_Arg(Argument_Cluster_Index : in Argument_Cluster_Index_Type; Part_Index_Set : in out Part_Index_Sets.Set) is
                pragma Unreferenced (Argument_Cluster_Index);
            begin
                Part_Index_Set.Delete(Dependent_Argument_Index_In_Governor_Part);
                Part_Index_Set_Size := Natural(Part_Index_Set.Length);
            end Remove_Arg;

            Cursor : Argument_Cluster_Index_To_Part_Indexes_Maps.Cursor
              := Part.Argument_Cluster_Index_To_Dependent_Indexes_Map.Find(Old_Argument_Cluster_Index);
        begin
            Part.Argument_Cluster_Index_To_Dependent_Indexes_Map.Update_Element(Cursor, Remove_Arg'Access);

            if Part_Index_Set_Size = 0 then
                Part.Argument_Cluster_Index_To_Dependent_Indexes_Map.Delete(Cursor);
            end if;
        end;

        On_Part_Unset_Argument(Cluster.all, Part, Argument_Part.all, Old_Argument_Cluster_Index);

        Part.Argument_Part_Map.Delete(Dependent_Argument_Index_In_Governor_Part);

        Argument_Part.Deprel       := Null_Unbounded_String;
        Argument_Part.Deprel_Index := -1;

    end Remove_Argument_From_Part;

    ---------------------------------
    function Create_Argument_Cluster
    ---------------------------------
      (Cluster        : in out Cluster_Type;
       Deprel_Index   : in     Deprel_Index_Type) return Argument_Cluster_Index_Type is

        Argument_Cluster_Index     : constant Argument_Cluster_Index_Type  := Cluster.Next_Argument_Cluster_Index;
        Argument_Cluster_Access    : constant Argument_Cluster_Access_Type := new Argument_Cluster_Type;
        Argument_Cluster_Index_Set : Argument_Cluster_Index_Sets.Set;
    begin

        Cluster.Next_Argument_Cluster_Index := Cluster.Next_Argument_Cluster_Index + 1;
        Cluster.Argument_Cluster_Map.Insert(Argument_Cluster_Index, Argument_Cluster_Access);

        Insert
          (Set      => Argument_Cluster_Index_Set,
           New_Item => Argument_Cluster_Index);

        declare
            Position : constant Deprel_Index_To_Argument_Cluster_Index_Set_Maps.Cursor
              := Cluster.Deprel_Index_To_Argument_Cluster_Index_Set_Map.Find(Deprel_Index);
        begin
            if Deprel_Index_To_Argument_Cluster_Index_Set_Maps.Has_Element(Position) then
                Cluster.Deprel_Index_To_Argument_Cluster_Index_Set_Map.Replace_Element(Position, Argument_Cluster_Index_Set);
            else
                Cluster.Deprel_Index_To_Argument_Cluster_Index_Set_Map.Insert(Deprel_Index, Argument_Cluster_Index_Set);
            end if;
        end;

        return Argument_Cluster_Index;

    end Create_Argument_Cluster;

    ------------------------------
    procedure Unset_Governor_Part
    ------------------------------
      (Part : in out Part_Type) is

        Cluster_Link : Cluster_Link_Type;
    begin

        if Part.Governor_Part = null then
            return;
        end if;

        Cluster_Link :=
          (Governor_Cluster_Index  => Part.Governor_Part.Cluster_Index,
           Dependent_Cluster_Index => Part.Cluster_Index);

        Delete_From_Set
          (Map          => Cluster_Index_To_Cluster_Link_Set_Map,
           Map_Position => Cluster_Index_To_Cluster_Link_Set_Map.Find(Part.Governor_Part.Cluster_Index),
           Set_Item     => Cluster_Link);

        Delete_From_Set
          (Map          => Cluster_Index_To_Cluster_Link_Set_Map,
           Map_Position => Cluster_Index_To_Cluster_Link_Set_Map.Find(Part.Cluster_Index),
           Set_Item     => Cluster_Link);

        Delete_From_Set
          (Map          => Cluster_Link_To_Atom_Link_Set_Map,
           Map_Position => Cluster_Link_To_Atom_Link_Set_Map.Find(Cluster_Link),
           Set_Item     => (Governor_Atom_ID  => Part.Governor_Part.Root_Node.ID,
                            Dependent_Atom_ID => Part.Root_Node.ID));

        -------
        -- Conj
        -------

        declare
            Argument : constant Part_Access_Type := Part.Governor_Part.Argument_Part_Map(Part.Argument_Index_In_Governor_Part);
            Deprel   : constant Unbounded_String := Argument.Deprel;
        begin
            if Part.Governor_Part.Cluster_Index /= Part.Cluster_Index and then Index(Deprel, "conj_") = Str(Deprel)'First then
                declare
                    Cluster_Index_Pair : Cluster_Index_Pair_Type;
                    Cursor             : Cluster_Index_Pair_To_Conj_Count_Maps.Cursor;
                begin
                    if Part.Governor_Part.Cluster_Index < Part.Cluster_Index then
                        Cluster_Index_Pair :=
                          (Max_Cluster_Index => Part.Cluster_Index,
                           Min_Cluster_Index => Part.Governor_Part.Cluster_Index);
                    else
                        Cluster_Index_Pair :=
                          (Max_Cluster_Index => Part.Governor_Part.Cluster_Index,
                           Min_Cluster_Index => Part.Cluster_Index);
                    end if;

                    Cursor := Cluster_Index_Pair_To_Conj_Count_Map.Find(Cluster_Index_Pair);

                    if Cluster_Index_Pair_To_Conj_Count_Maps.Has_Element(Cursor) then
                        if Cluster_Index_Pair_To_Conj_Count_Maps.Element(Cursor) = 1 then
                            Cluster_Index_Pair_To_Conj_Count_Map.Delete(Cursor);
                        else
                            Decrement
                              (Map      => Cluster_Index_Pair_To_Conj_Count_Map,
                               Position => Cursor);
                        end if;
                    end if;
                end;
            end if;
        end;

        Part.Governor_Part                   := null;
        Part.Argument_Index_In_Governor_Part := -1;

    end Unset_Governor_Part;

    ----------------------------
    procedure Set_Governor_Part
    ----------------------------
      (Dependent_Part        : in out Part_Type;
       Governor_Part_Access  : in     Part_Access_Type;
       Argument_Index        : in     Part_Index_Type) is
    begin

        if Dependent_Part.Governor_Part /= null then
            Unset_Governor_Part(Dependent_Part);
        end if;

        if Governor_Part_Access = null then
            raise MLSE_Error with "Governor_Part_Access = NULL";
        end if;

        Dependent_Part.Governor_Part                   := Governor_Part_Access;
        Dependent_Part.Argument_Index_In_Governor_Part := Argument_Index;

        if Governor_Part_Access.Cluster_Index < 0 or else Dependent_Part.Cluster_Index < 0 then
            raise MLSE_Error with "Governor_Part_Access.Cluster_Index < 0 or else Dependent_Part.Cluster_Index < 0";
        end if;

        declare
            Cluster_Link : constant Cluster_Link_Type :=
                             (Governor_Cluster_Index  => Dependent_Part.Governor_Part.Cluster_Index,
                              Dependent_Cluster_Index => Dependent_Part.Cluster_Index);
        begin

            -- Dependent_Part.Governor_Part.Cluster_Index --> Cluster_Link
            declare
                Cursor : Cluster_Index_To_Cluster_Link_Set_Maps.Cursor
                  := Cluster_Index_To_Cluster_Link_Set_Map.Find(Dependent_Part.Governor_Part.Cluster_Index);
            begin
                if not Cluster_Index_To_Cluster_Link_Set_Maps.Has_Element(Cursor) then
                    Insert
                      (Map      => Cluster_Index_To_Cluster_Link_Set_Map,
                       Key      => Dependent_Part.Governor_Part.Cluster_Index,
                       Position => Cursor);
                end if;

                Add_To_Set
                  (Map          => Cluster_Index_To_Cluster_Link_Set_Map,
                   Map_Cursor   => Cursor,
                   Set_New_Item => Cluster_Link);
            end;

            -- Dependent_Part.Cluster_Index --> Cluster_Link
            declare
                Cursor : Cluster_Index_To_Cluster_Link_Set_Maps.Cursor
                  := Cluster_Index_To_Cluster_Link_Set_Map.Find(Dependent_Part.Cluster_Index);
            begin
                if not Cluster_Index_To_Cluster_Link_Set_Maps.Has_Element(Cursor) then
                    Insert
                      (Map      => Cluster_Index_To_Cluster_Link_Set_Map,
                       Key      => Dependent_Part.Cluster_Index,
                       Position => Cursor);
                end if;

                Add_To_Set
                  (Map          => Cluster_Index_To_Cluster_Link_Set_Map,
                   Map_Cursor   => Cursor,
                   Set_New_Item => Cluster_Link);
            end;

            declare
                Cursor   : Cluster_Link_To_Atom_Link_Set_Maps.Cursor
                  := Cluster_Link_To_Atom_Link_Set_Map.Find(Cluster_Link);
            begin
                if not Cluster_Link_To_Atom_Link_Set_Maps.Has_Element(Cursor) then
                    Insert
                      (Map      => Cluster_Link_To_Atom_Link_Set_Map,
                       Key      => Cluster_Link,
                       Position => Cursor);
                end if;

                Add_To_Set
                  (Map          => Cluster_Link_To_Atom_Link_Set_Map,
                   Map_Cursor   => Cursor,
                   Set_New_Item => (Governor_Atom_ID  => Dependent_Part.Governor_Part.Root_Node.ID,
                                    Dependent_Atom_ID => Dependent_Part.Root_Node.ID));
            end;
        end;

        -------
        -- Conj
        -------

        declare
            Argument : constant Part_Access_Type := Dependent_Part.Governor_Part.Argument_Part_Map(Dependent_Part.Argument_Index_In_Governor_Part);
            Deprel   : constant Unbounded_String := Argument.Deprel;
        begin
            if Dependent_Part.Governor_Part.Cluster_Index /= Dependent_Part.Cluster_Index and then Index(Deprel, "conj_") = Str(Deprel)'First then
                declare
                    Cluster_Index_Pair : Cluster_Index_Pair_Type;
                    Cursor             : Cluster_Index_Pair_To_Conj_Count_Maps.Cursor;
                begin
                    if Dependent_Part.Governor_Part.Cluster_Index < Dependent_Part.Cluster_Index then
                        Cluster_Index_Pair :=
                          (Max_Cluster_Index => Dependent_Part.Cluster_Index,
                           Min_Cluster_Index => Dependent_Part.Governor_Part.Cluster_Index);
                    else
                        Cluster_Index_Pair :=
                          (Max_Cluster_Index => Dependent_Part.Governor_Part.Cluster_Index,
                           Min_Cluster_Index => Dependent_Part.Cluster_Index);
                    end if;

                    Cursor := Cluster_Index_Pair_To_Conj_Count_Map.Find(Cluster_Index_Pair);

                    if not Cluster_Index_Pair_To_Conj_Count_Maps.Has_Element(Cursor) then
                        Cluster_Index_Pair_To_Conj_Count_Map.Insert
                          (Key         => Cluster_Index_Pair,
                           New_Item    => 1);
                    else
                        Increment
                          (Map      => Cluster_Index_Pair_To_Conj_Count_Map,
                           Position => Cursor);
                    end if;
                end;
            end if;
        end;

    end Set_Governor_Part;

    ---------------------
    procedure Create_Args
    ---------------------
      (Atom_ID  : in Atom_ID_Type;
       Sentence : in Word_Vector.Vector) is

        function Get_Deprel_Index (Deprel : Unbounded_String) return Deprel_Index_Type is
            Cursor   : Deprel_To_Index_Maps.Cursor := Deprel_To_Index_Map.Find(Deprel);
        begin

            if not Deprel_To_Index_Maps.Has_Element(Cursor) then
                Deprel_Vector.Append(Deprel);

                Insert
                  (Map      => Deprel_To_Index_Map,
                   Key      => Deprel,
                   Position => Cursor,
                   New_Item => Deprel_Vector.Last_Index);
            end if;

            return Deprel_To_Index_Maps.Element(Cursor);
        end Get_Deprel_Index;

        Part    : constant Part_Access_Type
          := Atom_ID_To_Part_Map.Element(Atom_ID);

        Cluster : Cluster_Access_Type renames Part.Cluster_Access;
    begin

        for Word_Index of Sentence.Element(Atom_ID.Word_ID).Dependents loop
            declare
                Word           : Word_Type renames Sentence.Element(Word_Index);
                Dep_Part       : constant Part_Access_Type  := Atom_ID_To_Part_Map.Element((Atom_ID.Article_ID, Atom_ID.Sentence_ID, Word.ID));
                Deprel_Index   : constant Deprel_Index_Type := Get_Deprel_Index(Word.Deprel);

                Argument_Index : constant Part_Index_Type   :=
                                   Add_Argument(Governor_Part         => Part.all,
                                                Dependent_Part_Access => Dep_Part,
                                                Deprel                => Word.Deprel,
                                                Deprel_Index          => Deprel_Index);
            begin

                Set_Governor_Part
                  (Dependent_Part        => Dep_Part.all,
                   Governor_Part_Access  => Part,
                   Argument_Index        => Argument_Index);

                -------------------
                -- Argument_Cluster
                -------------------

                declare
                    Cursor : constant Deprel_Index_To_Argument_Cluster_Index_Set_Maps.Cursor
                      := Cluster.Deprel_Index_To_Argument_Cluster_Index_Set_Map.Find(Deprel_Index);

                    Argument_Cluster_Index : Argument_Cluster_Index_Type := -1;
                begin
                    if not Deprel_Index_To_Argument_Cluster_Index_Set_Maps.Has_Element(Cursor) then
                        Argument_Cluster_Index := Create_Argument_Cluster(Cluster => Cluster.all, Deprel_Index => Deprel_Index);
                    else
                        Argument_Cluster_Index := Argument_Cluster_Index_Sets.Element(Deprel_Index_To_Argument_Cluster_Index_Set_Maps.Element(Cursor).First);  -- TODO: multiple ones?
                    end if;

                    Set_Argument_Cluster(Part.all, Argument_Index, Argument_Cluster_Index);
                end;

                Create_Args -- recursive
                  (Atom_ID  => (Atom_ID.Article_ID, Atom_ID.Sentence_ID, Word.ID),
                   Sentence => Sentence);
            end;
        end loop;

    end Create_Args;

    ------------------------------
    procedure Set_Argument_Cluster
    ------------------------------
      (Part                   : in out Part_Type;
       Argument_Index         : in     Part_Index_Type;
       Argument_Cluster_Index : in     Argument_Cluster_Index_Type) is

        Old_Argument_Cluster_Index : Argument_Cluster_Index_Type := -1;
    begin
        declare
            Cursor : constant Part_Index_To_Argument_Cluster_Index_Maps.Cursor
              := Part.Argument_Index_To_Argument_Cluster_Index_Map.Find(Argument_Index);
        begin
            if Part_Index_To_Argument_Cluster_Index_Maps.Has_Element(Cursor) then
                Old_Argument_Cluster_Index := Part_Index_To_Argument_Cluster_Index_Maps.Element(Cursor);
            end if;
        end;

        if Old_Argument_Cluster_Index = Argument_Cluster_Index then
            return;
        end if;

        declare
            Cursor : constant Part_Index_To_Argument_Cluster_Index_Maps.Cursor
              := Part.Argument_Index_To_Argument_Cluster_Index_Map.Find(Argument_Index);
        begin
            if Part_Index_To_Argument_Cluster_Index_Maps.Has_Element(Cursor) then
                Part.Argument_Index_To_Argument_Cluster_Index_Map.Replace_Element(Cursor, Argument_Cluster_Index);
            else
                Part.Argument_Index_To_Argument_Cluster_Index_Map.Insert(Argument_Index, Argument_Cluster_Index);
            end if;
        end;

        declare
            Cursor   : Argument_Cluster_Index_To_Part_Indexes_Maps.Cursor
              := Part.Argument_Cluster_Index_To_Dependent_Indexes_Map.Find(Argument_Cluster_Index);
        begin
            if not Argument_Cluster_Index_To_Part_Indexes_Maps.Has_Element(Cursor) then
                Insert
                  (Map      => Part.Argument_Cluster_Index_To_Dependent_Indexes_Map,
                   Key      => Argument_Cluster_Index,
                   Position => Cursor);
            end if;

            Add_To_Set
              (Map          => Part.Argument_Cluster_Index_To_Dependent_Indexes_Map,
               Map_Cursor   => Cursor,
               Set_New_Item => Argument_Index);
        end;

        declare
            Argument_Part : constant Part_Access_Type
              := Part.Argument_Part_Map.Element(Argument_Index);

            Cluster       : Cluster_Access_Type renames Part.Cluster_Access;
        begin
            if Old_Argument_Cluster_Index /= -1 then
                declare
                    Part_Index_Set_Size : Integer := -1;

                    procedure Update_Argument_Cluster_Index_To_Part_Indexes_Map
                      (Argument_Cluster_Index  : in     Argument_Cluster_Index_Type;
                       Part_Index_Set          : in out Part_Index_Sets.Set) is
                        pragma Unreferenced (Argument_Cluster_Index);
                    begin
                        Part_Index_Set.Delete(Argument_Index);
                        Part_Index_Set_Size := Natural(Part_Index_Set.Length);
                    end Update_Argument_Cluster_Index_To_Part_Indexes_Map;

                    Cursor : Argument_Cluster_Index_To_Part_Indexes_Maps.Cursor
                      := Part.Argument_Cluster_Index_To_Dependent_Indexes_Map.Find(Old_Argument_Cluster_Index);
                begin
                    Part.Argument_Cluster_Index_To_Dependent_Indexes_Map.Update_Element
                      (Cursor, Update_Argument_Cluster_Index_To_Part_Indexes_Map'Access);

                    if Part_Index_Set_Size = 0 then
                        Part.Argument_Cluster_Index_To_Dependent_Indexes_Map.Delete(Cursor);
                    end if;
                end;
            end if;

            On_Part_Set_Argument(Cluster.all, Part, Argument_Part.all, Argument_Cluster_Index, Old_Argument_Cluster_Index);
        end;

    end Set_Argument_Cluster;

    --------------------------------
    procedure Unset_Argument_Cluster
    --------------------------------
      (Part                   : in out Part_Type;
       Argument_Index         : in     Part_Index_Type) is

        Old_Argument_Cluster_Index : constant Argument_Cluster_Index_Type
          := Part.Argument_Index_To_Argument_Cluster_Index_Map.Element(Argument_Index);

        Argument_Part : constant Part_Access_Type
          := Part.Argument_Part_Map.Element(Argument_Index);

        Cluster : Cluster_Access_Type renames Part.Cluster_Access;
    begin

        if not Part.Argument_Index_To_Argument_Cluster_Index_Map.Contains(Argument_Index) then

            return; -- TODO:...
        end if;


        Part.Argument_Index_To_Argument_Cluster_Index_Map.Delete(Argument_Index);

        declare
            Part_Index_Set_Size : Integer := -1;

            procedure Update_Argument_Cluster_Index_To_Dependent_Indexes_Map(Argument_Cluster_Index : in Argument_Cluster_Index_Type; Part_Index_Set : in out Part_Index_Sets.Set) is
                pragma Unreferenced (Argument_Cluster_Index);
            begin
                Part_Index_Set.Delete(Argument_Index);
                Part_Index_Set_Size := Natural(Part_Index_Set.Length);
            end Update_Argument_Cluster_Index_To_Dependent_Indexes_Map;

            Cursor  : Argument_Cluster_Index_To_Part_Indexes_Maps.Cursor
              := Part.Argument_Cluster_Index_To_Dependent_Indexes_Map.Find(Old_Argument_Cluster_Index);
        begin
            Part.Argument_Cluster_Index_To_Dependent_Indexes_Map.Update_Element(Cursor, Update_Argument_Cluster_Index_To_Dependent_Indexes_Map'Access);

            if Part_Index_Set_Size = 0 then
                Part.Argument_Cluster_Index_To_Dependent_Indexes_Map.Delete(Cursor);
            end if;
        end;

        On_Part_Unset_Argument(Cluster.all, Part, Argument_Part.all, Old_Argument_Cluster_Index);

    end Unset_Argument_Cluster;

    ------------------------------
    procedure On_Part_Set_Argument
    ------------------------------
      (Cluster                    : in out Cluster_Type;
       Governor_Part              : in     Part_Type;
       Argument_Part              : in     Part_Type;
       Argument_Cluster_Index     : in     Argument_Cluster_Index_Type;
       Old_Argument_Cluster_Index : in     Argument_Cluster_Index_Type) is

        Deprel_Index            : Deprel_Index_Type  renames Argument_Part.Deprel_Index;
        Dependent_Cluster_Index : Cluster_Index_Type renames Argument_Part.Cluster_Index;

        Argument_Cluster : Argument_Cluster_Access_Type;

    begin

        if not Cluster.Argument_Cluster_Map.Contains(Argument_Cluster_Index) then
            return; -- TODO: ...
        end if;

        Argument_Cluster := Cluster.Argument_Cluster_Map.Element(Argument_Cluster_Index);

        if Old_Argument_Cluster_Index < 0 and then Old_Argument_Cluster_Index /= -1 then
            raise MLSE_Error with "Invalid Old_Argument_Cluster_Index value.";
        end if;

        -- Update: Argument_Cluster.Deprel_Index_To_Count_Map

        declare
            Cursor   : constant Deprel_Index_To_Count_Maps.Cursor
              := Argument_Cluster.Deprel_Index_To_Count_Map.Find(Deprel_Index);
        begin
            if not Deprel_Index_To_Count_Maps.Has_Element(Cursor) then
                Argument_Cluster.Deprel_Index_To_Count_Map.Insert(Deprel_Index, 1);
            else
                Increment(Argument_Cluster.Deprel_Index_To_Count_Map, Cursor);
            end if;
        end;

        -- Update: Argument_Cluster.Dependent_Cluster_Index_To_Count_Map

        declare
            Cursor   : constant Cluster_Index_To_Count_Maps.Cursor
              := Argument_Cluster.Dependent_Cluster_Index_To_Count_Map.Find(Dependent_Cluster_Index);
        begin
            if not Cluster_Index_To_Count_Maps.Has_Element(Cursor) then
                Argument_Cluster.Dependent_Cluster_Index_To_Count_Map.Insert(Dependent_Cluster_Index, 1);
            else
                Increment(Argument_Cluster.Dependent_Cluster_Index_To_Count_Map, Cursor);
            end if;
        end;

        Argument_Cluster.Dependents_Count := Argument_Cluster.Dependents_Count + 1;

        -- Update: Cluster_Index_To_Cluster_And_Argument_Cluster_Link_Type_To_Count_Map_Map

        declare

            procedure Update_Cluster_Index_To_Cluster_And_Argument_Cluster_Link_Type_To_Count_Map_Map(Cluster_Index : in Cluster_Index_Type; Cluster_And_Argument_Cluster_Link_Type_To_Count_Map : in out Cluster_And_Argument_Cluster_Link_Type_To_Count_Maps.Map) is
                pragma Unreferenced (Cluster_Index);

                Cluster_And_Argument_Cluster_Link : constant Cluster_And_Argument_Cluster_Link_Type
                  := (Cluster_Index          => Cluster.Index,
                      Argument_Cluster_Index => Argument_Cluster_Index);

                Sub_Cursor   : constant Cluster_And_Argument_Cluster_Link_Type_To_Count_Maps.Cursor
                  := Cluster_And_Argument_Cluster_Link_Type_To_Count_Map.Find(Cluster_And_Argument_Cluster_Link);
            begin
                if not Cluster_And_Argument_Cluster_Link_Type_To_Count_Maps.Has_Element(Sub_Cursor) then
                    Cluster_And_Argument_Cluster_Link_Type_To_Count_Map.Insert(Cluster_And_Argument_Cluster_Link, 1);
                else
                    Increment(Cluster_And_Argument_Cluster_Link_Type_To_Count_Map, Sub_Cursor);
                end if;
            end Update_Cluster_Index_To_Cluster_And_Argument_Cluster_Link_Type_To_Count_Map_Map;

            Cursor   : Cluster_Index_To_Cluster_And_Argument_Cluster_Link_Type_To_Count_Map_Maps.Cursor
              := Cluster_Index_To_Cluster_And_Argument_Cluster_Link_Type_To_Count_Map_Map.Find(Dependent_Cluster_Index);
        begin
            if not Cluster_Index_To_Cluster_And_Argument_Cluster_Link_Type_To_Count_Map_Maps.Has_Element(Cursor) then
                Insert
                  (Map      => Cluster_Index_To_Cluster_And_Argument_Cluster_Link_Type_To_Count_Map_Map,
                   Key      => Dependent_Cluster_Index,
                   Position => Cursor);
            end if;

            Cluster_Index_To_Cluster_And_Argument_Cluster_Link_Type_To_Count_Map_Map.Update_Element(Cursor, Update_Cluster_Index_To_Cluster_And_Argument_Cluster_Link_Type_To_Count_Map_Map'Access);
        end;

        -- Update: Arg_Num

        declare
            New_Num_Arg : constant Num_Arg_Type
              := Num_Arg_Type(Governor_Part.Argument_Cluster_Index_To_Dependent_Indexes_Map.Element(Argument_Cluster_Index).Length);

            Cursor   : Num_Arg_To_Count_Maps.Cursor;
        begin
            -- New Num Arg

            Cursor := Argument_Cluster.Num_Arg_Count_Map.Find(New_Num_Arg);
            if not Num_Arg_To_Count_Maps.Has_Element(Cursor) then
                Argument_Cluster.Num_Arg_Count_Map.Insert(New_Num_Arg, 1);
            else
                Increment(Argument_Cluster.Num_Arg_Count_Map, Cursor);
            end if;

            -- Prev Num Arg
            if New_Num_Arg > 1 then
                Cursor := Argument_Cluster.Num_Arg_Count_Map.Find(New_Num_Arg - 1);
                if Num_Arg_To_Count_Maps.Element(Cursor) = 1 then
                    Argument_Cluster.Num_Arg_Count_Map.Delete(Cursor);
                else
                    Decrement(Argument_Cluster.Num_Arg_Count_Map, Cursor);
                end if;
            end if;
        end;

        Insert
          (Set      => Argument_Cluster.Root_Node_ID_Set,
           New_Item => Governor_Part.Root_Node.ID);

        -- Old
        if Old_Argument_Cluster_Index /= -1 then
            On_Part_Unset_Argument(Cluster, Governor_Part, Argument_Part, Old_Argument_Cluster_Index);
        end if;

    end On_Part_Set_Argument;

    --------------------------------
    procedure On_Part_Unset_Argument
    --------------------------------
      (Cluster                : in out Cluster_Type;
       Governor_Part          : in     Part_Type;
       Argument_Part          : in     Part_Type;
       Argument_Cluster_Index : in     Argument_Cluster_Index_Type) is

        Deprel_Index            : Deprel_Index_Type  renames Argument_Part.Deprel_Index;
        Dependent_Cluster_Index : Cluster_Index_Type renames Argument_Part.Cluster_Index;

        Argument_Cluster : constant Argument_Cluster_Access_Type
          := Cluster.Argument_Cluster_Map.Element(Argument_Cluster_Index);
    begin
        -- Update: Argument_Cluster.Deprel_Index_To_Count_Map
        declare
            Cursor : Deprel_Index_To_Count_Maps.Cursor
              := Argument_Cluster.Deprel_Index_To_Count_Map.Find(Deprel_Index);
        begin
            if Deprel_Index_To_Count_Maps.Element(Cursor) = 1 then
                Argument_Cluster.Deprel_Index_To_Count_Map.Delete(Cursor);
            else
                Decrement(Argument_Cluster.Deprel_Index_To_Count_Map, Cursor);
            end if;
        end;

        -- Update: Argument_Cluster.Dependent_Cluster_Index_To_Count_Map
        declare
            Cursor : Cluster_Index_To_Count_Maps.Cursor
              := Argument_Cluster.Dependent_Cluster_Index_To_Count_Map.Find(Dependent_Cluster_Index);
        begin
            if Cluster_Index_To_Count_Maps.Element(Cursor) = 1 then
                Argument_Cluster.Dependent_Cluster_Index_To_Count_Map.Delete(Cursor);
            else
                Decrement(Argument_Cluster.Dependent_Cluster_Index_To_Count_Map, Cursor);
            end if;
        end;

        Argument_Cluster.Dependents_Count := Argument_Cluster.Dependents_Count - 1;

        -- Update: Cluster_Index_To_Cluster_Link_Type_To_Count_Map_Map

        declare

            Cluster_Link_Type_To_Count_Map_Size : Integer := -1;

            procedure Update_Cluster_Index_To_Cluster_And_Argument_Cluster_Link_Type_To_Count_Map_Map(Cluster_Index : in Cluster_Index_Type; Cluster_And_Argument_Cluster_Link_Type_To_Count_Map : in out Cluster_And_Argument_Cluster_Link_Type_To_Count_Maps.Map) is
                pragma Unreferenced (Cluster_Index);

                Cluster_And_Argument_Cluster_Link : constant Cluster_And_Argument_Cluster_Link_Type
                  := (Cluster_Index          => Cluster.Index,
                      Argument_Cluster_Index => Argument_Cluster_Index);

                Sub_Cursor   : Cluster_And_Argument_Cluster_Link_Type_To_Count_Maps.Cursor
                  := Cluster_And_Argument_Cluster_Link_Type_To_Count_Map.Find(Cluster_And_Argument_Cluster_Link);
            begin
                if Cluster_And_Argument_Cluster_Link_Type_To_Count_Maps.Element(Sub_Cursor) = 1 then
                    Cluster_And_Argument_Cluster_Link_Type_To_Count_Map.Delete(Sub_Cursor);
                else
                    Decrement(Cluster_And_Argument_Cluster_Link_Type_To_Count_Map, Sub_Cursor);
                end if;

                Cluster_Link_Type_To_Count_Map_Size := Natural(Cluster_And_Argument_Cluster_Link_Type_To_Count_Map.Length);
            end Update_Cluster_Index_To_Cluster_And_Argument_Cluster_Link_Type_To_Count_Map_Map;

            Cursor   : Cluster_Index_To_Cluster_And_Argument_Cluster_Link_Type_To_Count_Map_Maps.Cursor
              := Cluster_Index_To_Cluster_And_Argument_Cluster_Link_Type_To_Count_Map_Map.Find(Dependent_Cluster_Index);
        begin
            Cluster_Index_To_Cluster_And_Argument_Cluster_Link_Type_To_Count_Map_Map.Update_Element(Cursor, Update_Cluster_Index_To_Cluster_And_Argument_Cluster_Link_Type_To_Count_Map_Map'Access);

            if Cluster_Link_Type_To_Count_Map_Size = 0 then
                Cluster_Index_To_Cluster_And_Argument_Cluster_Link_Type_To_Count_Map_Map.Delete(Cursor);
            end if;
        end;

        declare
            C_Del : Atom_ID_Sets.Cursor
              := Argument_Cluster.Root_Node_ID_Set.Find(Governor_Part.Root_Node.ID);
        begin
            if Atom_ID_Sets.Has_Element(C_Del) then
                Argument_Cluster.Root_Node_ID_Set.Delete(C_Del);
            end if;
        end;

        if Argument_Cluster.Dependents_Count = 0 then

            -- Remove_Argument_Cluster

            Cluster.Argument_Cluster_Map.Delete(Argument_Cluster_Index);

            declare
                Deprel_Index_To_Delete : Deprel_Index_Sets.Set;

                Argument_Cluster_Index_Set_Size : Integer := -1;

                procedure Update_Deprel_Index_To_Argument_Cluster_Index_Set_Map(Deprel_Index : in Deprel_Index_Type; Argument_Cluster_Index_Set : in out Argument_Cluster_Index_Sets.Set) is
                    pragma Unreferenced (Deprel_Index);
                    C_Del : Argument_Cluster_Index_Sets.Cursor
                      := Argument_Cluster_Index_Set.Find(Argument_Cluster_Index);
                begin
                    if Argument_Cluster_Index_Sets.Has_Element(C_Del) then
                        Argument_Cluster_Index_Set.Delete(C_Del);
                    end if;
                    Argument_Cluster_Index_Set_Size := Natural(Argument_Cluster_Index_Set.Length);
                end Update_Deprel_Index_To_Argument_Cluster_Index_Set_Map;

                Cursor                 : Deprel_Index_To_Argument_Cluster_Index_Set_Maps.Cursor
                  := Cluster.Deprel_Index_To_Argument_Cluster_Index_Set_Map.First;
            begin

                while Deprel_Index_To_Argument_Cluster_Index_Set_Maps.Has_Element(Cursor) loop
                    Argument_Cluster_Index_Set_Size := -1;
                    Cluster.Deprel_Index_To_Argument_Cluster_Index_Set_Map.Update_Element(Cursor, Update_Deprel_Index_To_Argument_Cluster_Index_Set_Map'Access);
                    if Argument_Cluster_Index_Set_Size = 0 then
                        Insert
                          (Set      => Deprel_Index_To_Delete,
                           New_Item => Deprel_Index_To_Argument_Cluster_Index_Set_Maps.Key(Cursor));
                    end if;

                    Deprel_Index_To_Argument_Cluster_Index_Set_Maps.Next(Cursor);
                end loop;

                for Deprel_Index of Deprel_Index_To_Delete loop
                    Cluster.Deprel_Index_To_Argument_Cluster_Index_Set_Map.Delete(Deprel_Index);
                end loop;
            end;

            if Governor_Part.Argument_Cluster_Index_To_Dependent_Indexes_Map.Contains(Argument_Cluster_Index) then
                raise MLSE_Error with "Governor_Part.Argument_Cluster_Index_To_Dependent_Indexes_Map.Contains(Argument_Cluster_Index)";
            end if;

        else
            declare
                Old_Num_Arg : Num_Arg_Type := 0;

                Old_Cursor      : constant Argument_Cluster_Index_To_Part_Indexes_Maps.Cursor
                  := Governor_Part.Argument_Cluster_Index_To_Dependent_Indexes_Map.Find(Argument_Cluster_Index);

                Cursor   : Num_Arg_To_Count_Maps.Cursor;
            begin

                if Argument_Cluster_Index_To_Part_Indexes_Maps.Has_Element(Old_Cursor) then
                    Old_Num_Arg := Num_Arg_Type(Argument_Cluster_Index_To_Part_Indexes_Maps.Element(Old_Cursor).Length);
                end if;

                if Old_Num_Arg > 0 then
                    Cursor := Argument_Cluster.Num_Arg_Count_Map.Find(Old_Num_Arg);

                    if not Num_Arg_To_Count_Maps.Has_Element(Cursor) then
                        Argument_Cluster.Num_Arg_Count_Map.Insert(Old_Num_Arg, 1);
                    else
                        Increment(Argument_Cluster.Num_Arg_Count_Map, Cursor);
                    end if;
                end if;

                Cursor := Argument_Cluster.Num_Arg_Count_Map.Find(Old_Num_Arg + 1);
                if Num_Arg_To_Count_Maps.Element(Cursor) = 1 then
                    Argument_Cluster.Num_Arg_Count_Map.Delete(Cursor);
                else
                    Decrement(Argument_Cluster.Num_Arg_Count_Map, Cursor);
                end if;
            end;
        end if;

    end On_Part_Unset_Argument;

    ---------------------
    procedure Initialize
    ---------------------
      (Articles : in Article_Sets.Set) is

        Sentence_Counter   : Integer := 0;
        Word_Counter       : Integer := 0;
    begin

        for Article of Articles loop
            Sentence_Counter := Sentence_Counter + Natural(Article.Sentences.Length);

            declare
                Sentence_Number : Sentence_Index_Type := -1; -- With -1 it will start with 0
            begin
                for Sentence of Article.Sentences loop
                    Sentence_Number := Sentence_Number + 1;

                    Word_Counter := Word_Counter + Natural(Sentence.Length) - 1;

                    if not Sentence.First_Element.Dependents.Is_Empty then

                        -- build atom, part, cluster for all words

                        for Word of Sentence loop
                            if Word.ID /= 0 -- Skip ROOT
                              and then Word.Head /= -1  -- MG 28Aug2015

                              and then Check_Word_Ancestor
                                (Sentence => Sentence,
                                 Word_ID  => Word.ID) then

                                declare
                                    Atom    : Atom_Access_Type;
                                    Part    : Part_Access_Type;
                                begin

                                    -- Atom
                                    Atom := new Atom_Type;
                                    Atom.ID    := (Article.ID, Sentence_Number, Word.ID);
                                    Atom.Form  := Word.Form;
                                    Atom.Lemma := Word.Lemma;
                                    Atom.POS   := Word.POS;

                                    Atom_ID_To_Atom_Map.Insert(Atom.ID, Atom);

                                    -- Part
                                    Part := new Part_Type;
                                    Part.Root_Node := Atom;
                                    Part.Rel_Type_Index := Get_Rel_Type_Index(Atom.all);
                                    Atom_ID_To_Part_Map.Insert(Atom.ID, Part);

                                    -- Cluster
                                    declare
                                        Cluster_Set   : constant Cluster_Index_Sets.Set := Get_Clusters_With_Rel_Type(Part.Rel_Type_Index);
                                        Cluster_Index : Cluster_Index_Type     := -1;
                                    begin
                                        if not Cluster_Set.Is_Empty then
                                            Cluster_Index := Cluster_Set.First_Element;
                                        else
                                            Cluster_Index := Create_Cluster(Rel_Type_Index => Part.Rel_Type_Index,
                                                                            Is_Content     => Is_Content(Atom.POS));
                                        end if;

                                        Link_Part_And_Cluster(Part.all, Cluster_Map.Element(Cluster_Index));
                                    end;
                                end;
                            end if;
                        end loop;

                        -- build args; link part

                        for Root_Word_Index of Sentence.First_Element.Dependents loop

                            declare
                                Part    : constant Part_Access_Type    := Atom_ID_To_Part_Map.Element((Article.ID, Sentence_Number, Root_Word_Index));
                                Cluster : Cluster_Access_Type renames Part.Cluster_Access;
                            begin

                                -- Increment Cluster Root counter

                                declare
                                    Cursor : constant Cluster_Index_Root_Count_Maps.Cursor
                                      := Cluster_Index_Root_Count_Map.Find(Cluster.Index);
                                begin
                                    if not Cluster_Index_Root_Count_Maps.Has_Element(Cursor) then
                                        Cluster_Index_Root_Count_Map.Insert
                                          (Key      => Cluster.Index,
                                           New_Item => 1);
                                    else
                                        Increment
                                          (Map      => Cluster_Index_Root_Count_Map,
                                           Position => Cursor);
                                    end if;
                                end;

                                Create_Args(Atom_ID => (Article.ID, Sentence_Number, Root_Word_Index), Sentence => Sentence);
                            end;

                        end loop;
                    end if;
                end loop;
            end;
        end loop;

        T_IO.Put_Line("<INFO> Sentence_Counter=" & Str(Sentence_Counter) & " Word_Counter=" & Str(Word_Counter));
    end Initialize;

    ----------------------------
    procedure Merge_Arguments is
    ----------------------------
        Cluster_Cursor : Cluster_Maps.Cursor := Cluster_Map.First;
    begin
        while Cluster_Maps.Has_Element(Cluster_Cursor) loop
            declare
                Cluster : constant Cluster_Access_Type
                  := Cluster_Maps.Element(Cluster_Cursor);

                New_Argument_Cluster_Map : Argument_Cluster_Maps.Map;

                Argument_Cluster_Index_And_Count_Type_Set : Ordered_Argument_Cluster_Index_And_Count_Type_Sets.Set;

                Argument_Cluster_Cursor : Argument_Cluster_Maps.Cursor
                  := Cluster.Argument_Cluster_Map.First;
            begin
                -- generate an ordering: in decreasing order of freq
                while Argument_Cluster_Maps.Has_Element(Argument_Cluster_Cursor) loop
                    Argument_Cluster_Index_And_Count_Type_Set.Insert
                      ((Argument_Cluster_Index => Argument_Cluster_Maps.Key(Argument_Cluster_Cursor),
                        Dependents_Count       => Argument_Cluster_Maps.Element(Argument_Cluster_Cursor).Dependents_Count));

                    Argument_Cluster_Maps.Next(Argument_Cluster_Cursor);
                end loop;

                -- greedily add arg & merge

                for Item of Argument_Cluster_Index_And_Count_Type_Set loop
                    declare
                        Argument_Cluster_Index : Argument_Cluster_Index_Type renames Item.Argument_Cluster_Index;

                        Argument_Cluster       : constant Argument_Cluster_Access_Type
                          := Cluster.Argument_Cluster_Map.Element(Argument_Cluster_Index);
                    begin
                        if New_Argument_Cluster_Map.Is_Empty then
                            New_Argument_Cluster_Map.Insert
                              (Argument_Cluster_Index, Argument_Cluster);

                        else
                            -- determine which to add

                            declare
                                Max_Score     : Score_Type                  := 0.0; -- no merge
                                Max_Map_Index : Argument_Cluster_Index_Type := -1;

                                New_Cursor    : Argument_Cluster_Maps.Cursor
                                  := New_Argument_Cluster_Map.First;
                            begin

                                while Argument_Cluster_Maps.Has_Element(New_Cursor) loop
                                    declare
                                        Cur_Argument_Cluster_Index : constant Argument_Cluster_Index_Type
                                          := Argument_Cluster_Maps.Key(New_Cursor);

                                        Cur_Score                  : constant Score_Type
                                          := Score_Merge_Arguments(Cluster.all, Cur_Argument_Cluster_Index, Argument_Cluster_Index);
                                    begin
                                        if Cur_Score > Max_Score then
                                            Max_Score     := Cur_Score;
                                            Max_Map_Index := Cur_Argument_Cluster_Index;
                                        end if;
                                    end;
                                    Argument_Cluster_Maps.Next(New_Cursor);
                                end loop;

                                -- Add

                                if Max_Map_Index /= -1 then
                                    -- Executor Merge_Argument
                                    -- merge Argument_Cluster_Index into Max_Map_Index in the clust
                                    declare
                                        Argument_Cluster : constant Argument_Cluster_Access_Type
                                          := Cluster.Argument_Cluster_Map.Element(Argument_Cluster_Index);

                                        -- TODO: faccio una copia poiche' l'originale verra' modificato man mano che mi addentro nel ciclo
                                        Root_Node_ID_Set_Copy : constant Atom_ID_Sets.Set := Argument_Cluster.Root_Node_ID_Set;
                                    begin
                                        -- part: argclust maps
                                        for Root_Node_ID of Root_Node_ID_Set_Copy loop
                                            declare
                                                Part : constant Part_Access_Type
                                                  := Atom_ID_To_Part_Map.Element(Root_Node_ID);

                                                Cursor : Part_Index_To_Argument_Cluster_Index_Maps.Cursor
                                                  := Part.Argument_Index_To_Argument_Cluster_Index_Map.First;
                                            begin
                                                while Part_Index_To_Argument_Cluster_Index_Maps.Has_Element(Cursor) loop
                                                    declare
                                                        Cur_Argument_Index         : constant Part_Index_Type := Part_Index_To_Argument_Cluster_Index_Maps.Key(Cursor);
                                                        Cur_Argument_Cluster_Index : constant Argument_Cluster_Index_Type := Part_Index_To_Argument_Cluster_Index_Maps.Element(Cursor);
                                                    begin
                                                        if Cur_Argument_Cluster_Index = Argument_Cluster_Index then
                                                            Set_Argument_Cluster
                                                              (Part                   => Part.all,
                                                               Argument_Index         => Cur_Argument_Index,
                                                               Argument_Cluster_Index => Max_Map_Index);
                                                        end if;
                                                    end;
                                                    Part_Index_To_Argument_Cluster_Index_Maps.Next(Cursor);
                                                end loop;
                                            end;
                                        end loop;
                                    end;

                                else
                                    New_Argument_Cluster_Map.Insert
                                      (Argument_Cluster_Index, Argument_Cluster);
                                end if;
                            end;
                        end if;
                    end;
                end loop;

                -- reset argClust
                Cluster.Argument_Cluster_Map := New_Argument_Cluster_Map;
            end;

            Cluster_Maps.Next(Cluster_Cursor);
        end loop;
    end Merge_Arguments;

    -- merge args in same clust
    ------------------------------
    function Score_Merge_Arguments
    ------------------------------
      (Cluster                  : in Cluster_Type;
       Argument_Cluster_Index_1 : in Argument_Cluster_Index_Type;
       Argument_Cluster_Index_2 : in Argument_Cluster_Index_Type) return Score_Type is

        Score : Score_Type := 0.0;

        Argument_Cluster_1 : constant Argument_Cluster_Access_Type
          := Cluster.Argument_Cluster_Map.Element(Argument_Cluster_Index_1);

        Argument_Cluster_2 : constant Argument_Cluster_Access_Type
          := Cluster.Argument_Cluster_Map.Element(Argument_Cluster_Index_2);

        Part_Count : Natural renames Cluster.Part_Count;

        Root_Nodes_Count_1 : constant Natural := Natural(Argument_Cluster_1.Root_Node_ID_Set.Length);
        Root_Nodes_Count_2 : constant Natural := Natural(Argument_Cluster_2.Root_Node_ID_Set.Length);

        Dependents_Count_1 : constant Natural := Natural(Argument_Cluster_1.Dependents_Count);
        Dependents_Count_2 : constant Natural := Natural(Argument_Cluster_2.Dependents_Count);

        New_Num_Arg_Count_Map : Num_Arg_To_Count_Maps.Map;

    begin

        -- HP
        Score := Score - Configurations.Prior_Merge;

        --  same clust -> no change to ttlCnt; tac=1+2; ttlPartCnt,argNum_cnt may not -> need to loop thru parts

        -- old 1,2: xlx(ttlCnt-ttlPartCnt)-xlx(ttlCnt)+SUM_i(xlx(argnum_cnt_i))
        --         +SUM_i(xlx(argtype_cnt_i))+SUM_i(xlx(chdcl_cnt_i))-xlx(tac)
        -- new: xlx(ttlcnt-new_ttlptcnt)-xlx(ttlcnt)+SUM_i(xlx(new_argnum_cnt_i))
        --    +SUM_i(xlx(new_argtype_cnt_i))+SUM_i(xlx(new_chdcl_cnt_i))-xlx(tac1/2)

        Score := Score - (XlogX(Part_Count - Root_Nodes_Count_1) + XlogX(Part_Count - Root_Nodes_Count_2));
        Score := Score + XlogX(Part_Count); -- previously subtract two, now just one
        Score := Score - (2.0*(XlogX(Dependents_Count_1 + Dependents_Count_2) - XlogX(Dependents_Count_1) - XlogX(Dependents_Count_2))); -- argtype & chdcl

        -- Num Arg Count

        declare
            Cursor : Num_Arg_To_Count_Maps.Cursor
              := Argument_Cluster_1.Num_Arg_Count_Map.First;
        begin
            while Num_Arg_To_Count_Maps.Has_Element(Cursor) loop
                declare
                    Num_Arg : constant Num_Arg_Type := Num_Arg_To_Count_Maps.Key(Cursor);
                    Count   : constant Natural      := Num_Arg_To_Count_Maps.Element(Cursor);

                    New_Cursor : constant Num_Arg_To_Count_Maps.Cursor
                      := New_Num_Arg_Count_Map.Find(Num_Arg);

                    procedure Num_Arg_To_Count_Add(N_Num_Arg : in Num_Arg_Type; N_Count : in out Integer) is begin
                        pragma Unreferenced (N_Num_Arg);
                        N_Count := N_Count + Count;
                    end Num_Arg_To_Count_Add;
                begin
                    Score := Score - XlogX(Count);

                    if not Num_Arg_To_Count_Maps.Has_Element(New_Cursor) then
                        New_Num_Arg_Count_Map.Insert(Num_Arg, Count);
                    else
                        New_Num_Arg_Count_Map.Update_Element(New_Cursor, Num_Arg_To_Count_Add'Access);
                    end if;
                end;
                Num_Arg_To_Count_Maps.Next(Cursor);
            end loop;
        end;

        declare -- TODO: questo declare contiene codice analogo al precedente (eccetto "Argument_Cluster_2")
            Cursor : Num_Arg_To_Count_Maps.Cursor
              := Argument_Cluster_2.Num_Arg_Count_Map.First;
        begin
            while Num_Arg_To_Count_Maps.Has_Element(Cursor) loop
                declare
                    Num_Arg : constant Num_Arg_Type := Num_Arg_To_Count_Maps.Key(Cursor);
                    Count   : constant Natural      := Num_Arg_To_Count_Maps.Element(Cursor);

                    New_Cursor : constant Num_Arg_To_Count_Maps.Cursor
                      := New_Num_Arg_Count_Map.Find(Num_Arg);

                    procedure Num_Arg_To_Count_Add(N_Num_Arg : in Num_Arg_Type; N_Count : in out Integer) is begin
                        pragma Unreferenced (N_Num_Arg);
                        N_Count := N_Count + Count;
                    end Num_Arg_To_Count_Add;
                begin
                    Score := Score - XlogX(Count);

                    if not Num_Arg_To_Count_Maps.Has_Element(New_Cursor) then
                        New_Num_Arg_Count_Map.Insert(Num_Arg, Count);
                    else
                        New_Num_Arg_Count_Map.Update_Element(New_Cursor, Num_Arg_To_Count_Add'Access);
                    end if;
                end;
                Num_Arg_To_Count_Maps.Next(Cursor);
            end loop;
        end;

        declare
            Root_Nodes_Count_Remaining : Natural := Root_Nodes_Count_1 + Root_Nodes_Count_2;

            Root_Node_ID_Cursor_1      : Atom_ID_Sets.Cursor := Argument_Cluster_1.Root_Node_ID_Set.First;
            Root_Node_ID_Cursor_2      : Atom_ID_Sets.Cursor := Argument_Cluster_2.Root_Node_ID_Set.First;

            Root_Node_ID_1             : Atom_ID_Type        := Atom_ID_Sets.Element(Root_Node_ID_Cursor_1);
            Root_Node_ID_2             : Atom_ID_Type        := Atom_ID_Sets.Element(Root_Node_ID_Cursor_2);
        begin
            Atom_ID_Sets.Next(Root_Node_ID_Cursor_1);
            Atom_ID_Sets.Next(Root_Node_ID_Cursor_2);

            while True loop
                if Root_Node_ID_1 = Root_Node_ID_2 then
                    -- new cnt
                    declare
                        Part_C1 : Part_Access_Type;
                        Part_C2 : Part_Access_Type;
                        C1      : Num_Arg_Type; -- := Num_Arg_Type(Atom_ID_To_Part_Map.Element(Root_Node_ID_1).Argument_Cluster_Index_To_Dependent_Indexes_Map.Element(Argument_Cluster_Index_1).Length);
                        C2      : Num_Arg_Type; -- := Num_Arg_Type(Atom_ID_To_Part_Map.Element(Root_Node_ID_2).Argument_Cluster_Index_To_Dependent_Indexes_Map.Element(Argument_Cluster_Index_2).Length);
                        C0      : Num_Arg_Type; -- := C1 + C2;

                        New_Cursor : Num_Arg_To_Count_Maps.Cursor;
                    begin
                        Part_C1 := Atom_ID_To_Part_Map.Element(Root_Node_ID_1);
                        Part_C2 := Atom_ID_To_Part_Map.Element(Root_Node_ID_2);

                        C1 := Num_Arg_Type(Part_C1.Argument_Cluster_Index_To_Dependent_Indexes_Map.Element(Argument_Cluster_Index_1).Length);
                        C2 := Num_Arg_Type(Part_C2.Argument_Cluster_Index_To_Dependent_Indexes_Map.Element(Argument_Cluster_Index_2).Length);
                        C0 := C1 + C2;--

                        Root_Nodes_Count_Remaining := Root_Nodes_Count_Remaining - 1;

                        -- C0
                        New_Cursor := New_Num_Arg_Count_Map.Find(C0);
                        if not Num_Arg_To_Count_Maps.Has_Element(New_Cursor) then
                            New_Num_Arg_Count_Map.Insert(C0, 1);
                        else
                            Increment(New_Num_Arg_Count_Map, New_Cursor);
                        end if;

                        -- C1
                        New_Cursor := New_Num_Arg_Count_Map.Find(C1);
                        if Num_Arg_To_Count_Maps.Element(New_Cursor) = 1 then
                            New_Num_Arg_Count_Map.Delete(New_Cursor);
                        else
                            Decrement(New_Num_Arg_Count_Map, New_Cursor);
                        end if;

                        -- C2
                        New_Cursor := New_Num_Arg_Count_Map.Find(C2);
                        if Num_Arg_To_Count_Maps.Element(New_Cursor) = 1 then
                            New_Num_Arg_Count_Map.Delete(New_Cursor);
                        else
                            Decrement(New_Num_Arg_Count_Map, New_Cursor);
                        end if;

                        -- Next
                        if Atom_ID_Sets.Has_Element(Root_Node_ID_Cursor_1) and then Atom_ID_Sets.Has_Element(Root_Node_ID_Cursor_2) then
                            Root_Node_ID_1 := Atom_ID_Sets.Element(Root_Node_ID_Cursor_1);
                            Root_Node_ID_2 := Atom_ID_Sets.Element(Root_Node_ID_Cursor_2);

                            Atom_ID_Sets.Next(Root_Node_ID_Cursor_1);
                            Atom_ID_Sets.Next(Root_Node_ID_Cursor_2);
                        else
                            exit;
                        end if;
                    end;

                elsif Root_Node_ID_1 < Root_Node_ID_2 then
                    while Atom_ID_Sets.Has_Element(Root_Node_ID_Cursor_1) loop
                        Root_Node_ID_1 := Atom_ID_Sets.Element(Root_Node_ID_Cursor_1);
                        Atom_ID_Sets.Next(Root_Node_ID_Cursor_1);
                        exit when Root_Node_ID_1 >= Root_Node_ID_2;
                    end loop;

                    exit when Root_Node_ID_1 < Root_Node_ID_2;

                else
                    while Atom_ID_Sets.Has_Element(Root_Node_ID_Cursor_2) loop
                        Root_Node_ID_2 := Atom_ID_Sets.Element(Root_Node_ID_Cursor_2);
                        Atom_ID_Sets.Next(Root_Node_ID_Cursor_2);
                        exit when Root_Node_ID_1 <= Root_Node_ID_2;
                    end loop;

                    exit when Root_Node_ID_1 > Root_Node_ID_2;
                end if;
            end loop;

            Score := Score + XlogX(Part_Count - Root_Nodes_Count_Remaining);
        end;

        declare
            New_Cursor : Num_Arg_To_Count_Maps.Cursor
              := New_Num_Arg_Count_Map.First;
        begin
            while Num_Arg_To_Count_Maps.Has_Element(New_Cursor) loop
                Score := Score + XlogX(Num_Arg_To_Count_Maps.Element(New_Cursor));
                Num_Arg_To_Count_Maps.Next(New_Cursor);
            end loop;
        end;

        Score := Score + (Score_Type(Natural(Argument_Cluster_1.Num_Arg_Count_Map.Length) + Natural(Argument_Cluster_2.Num_Arg_Count_Map.Length) - Natural(New_Num_Arg_Count_Map.Length)) * Configurations.Prior_Num_Param); -- argnum/absent params

        -- Type

        declare
            Deprel_Index_To_Count_1 : Deprel_Index_To_Count_Maps.Map renames Argument_Cluster_1.Deprel_Index_To_Count_Map;
            Deprel_Index_To_Count_2 : Deprel_Index_To_Count_Maps.Map renames Argument_Cluster_2.Deprel_Index_To_Count_Map;

            Cursor : Deprel_Index_To_Count_Maps.Cursor;
        begin
            if Natural(Deprel_Index_To_Count_1.Length) <= Natural(Deprel_Index_To_Count_2.Length) then
                Cursor := Deprel_Index_To_Count_1.First;

                while Deprel_Index_To_Count_Maps.Has_Element(Cursor) loop
                    declare
                        Deprel_Index_1 : constant Deprel_Index_Type := Deprel_Index_To_Count_Maps.Key(Cursor);
                        Cursor_2       : constant Deprel_Index_To_Count_Maps.Cursor := Deprel_Index_To_Count_2.Find(Deprel_Index_1);
                    begin
                        if Deprel_Index_To_Count_Maps.Has_Element(Cursor_2) then
                            declare
                                Count_1 : constant Natural := Deprel_Index_To_Count_Maps.Element(Cursor);
                                Count_2 : constant Natural := Deprel_Index_To_Count_Maps.Element(Cursor_2);
                            begin
                                Score := Score + XlogX(Count_1 + Count_2) - XlogX(Count_1) - XlogX(Count_2);
                                Score := Score + Configurations.Prior_Num_Param;
                            end;
                        end if;
                    end;
                    Deprel_Index_To_Count_Maps.Next(Cursor);
                end loop;
            else
                Cursor := Deprel_Index_To_Count_2.First;

                while Deprel_Index_To_Count_Maps.Has_Element(Cursor) loop
                    declare
                        Deprel_Index_2 : constant Deprel_Index_Type := Deprel_Index_To_Count_Maps.Key(Cursor);
                        Cursor_1       : constant Deprel_Index_To_Count_Maps.Cursor := Deprel_Index_To_Count_1.Find(Deprel_Index_2);
                    begin
                        if Deprel_Index_To_Count_Maps.Has_Element(Cursor_1) then
                            declare
                                Count_1 : constant Natural := Deprel_Index_To_Count_Maps.Element(Cursor_1);
                                Count_2 : constant Natural := Deprel_Index_To_Count_Maps.Element(Cursor);
                            begin
                                Score := Score + XlogX(Count_1 + Count_2) - XlogX(Count_1) - XlogX(Count_2);
                                Score := Score + Configurations.Prior_Num_Param;
                            end;
                        end if;
                    end;
                    Deprel_Index_To_Count_Maps.Next(Cursor);
                end loop;
            end if;
        end;

        -- clust

        declare
            Dependent_Cluster_Index_To_Count_1 : Cluster_Index_To_Count_Maps.Map renames Argument_Cluster_1.Dependent_Cluster_Index_To_Count_Map;
            Dependent_Cluster_Index_To_Count_2 : Cluster_Index_To_Count_Maps.Map renames Argument_Cluster_2.Dependent_Cluster_Index_To_Count_Map;

            Cursor : Cluster_Index_To_Count_Maps.Cursor;
        begin
            if Natural(Dependent_Cluster_Index_To_Count_1.Length) <= Natural(Dependent_Cluster_Index_To_Count_2.Length) then
                Cursor := Dependent_Cluster_Index_To_Count_1.First;

                while Cluster_Index_To_Count_Maps.Has_Element(Cursor) loop
                    declare
                        Dependent_Cluster_Index_1 : constant Cluster_Index_Type := Cluster_Index_To_Count_Maps.Key(Cursor);
                        Cursor_2                  : constant Cluster_Index_To_Count_Maps.Cursor := Dependent_Cluster_Index_To_Count_2.Find(Dependent_Cluster_Index_1);
                    begin
                        if Cluster_Index_To_Count_Maps.Has_Element(Cursor_2) then
                            declare
                                Count_1 : constant Natural := Cluster_Index_To_Count_Maps.Element(Cursor);
                                Count_2 : constant Natural := Cluster_Index_To_Count_Maps.Element(Cursor_2);
                            begin
                                Score := Score + XlogX(Count_1 + Count_2) - XlogX(Count_1) - XlogX(Count_2);
                                Score := Score + Configurations.Prior_Num_Param;
                            end;
                        end if;
                    end;
                    Cluster_Index_To_Count_Maps.Next(Cursor);
                end loop;
            else
                Cursor := Dependent_Cluster_Index_To_Count_2.First;

                while Cluster_Index_To_Count_Maps.Has_Element(Cursor) loop
                    declare
                        Dependent_Cluster_Index_2 : constant Cluster_Index_Type := Cluster_Index_To_Count_Maps.Key(Cursor);
                        Cursor_1                  : constant Cluster_Index_To_Count_Maps.Cursor := Dependent_Cluster_Index_To_Count_1.Find(Dependent_Cluster_Index_2);
                    begin
                        if Cluster_Index_To_Count_Maps.Has_Element(Cursor_1) then
                            declare
                                Count_1 : constant Natural := Cluster_Index_To_Count_Maps.Element(Cursor_1);
                                Count_2 : constant Natural := Cluster_Index_To_Count_Maps.Element(Cursor);
                            begin
                                Score := Score + XlogX(Count_1 + Count_2) - XlogX(Count_1) - XlogX(Count_2);
                                Score := Score + Configurations.Prior_Num_Param;
                            end;
                        end if;
                    end;
                    Cluster_Index_To_Count_Maps.Next(Cursor);
                end loop;
            end if;
        end;

        return Score;

    end Score_Merge_Arguments;

    ----------------------
    procedure Merge_Agenda
    ----------------------
      (To_Agenda   : in out Agenda_Type;
       From_Agenda : in out Agenda_Type) is
    begin

        -----
        -- Agenda.Agenda_To_Score_Set
        -----
        --To_Agenda.Agenda_To_Score_Set.Union(From_Agenda.Agenda_To_Score_Set); <- can't do this with Repr access inside
        declare
            From_Cursor : Search_Operation_Sets.Cursor
              := From_Agenda.Agenda_To_Score_Set.First;
        begin
            To_Agenda.Agenda_To_Score_Set.Reserve_Capacity(To_Agenda.Agenda_To_Score_Set.Length + From_Agenda.Agenda_To_Score_Set.Length);

            while Search_Operation_Sets.Has_Element(From_Cursor) loop
                declare
                    Search_Operation : Search_Operation_Type
                      := Search_Operation_Sets.Element(From_Cursor);
                begin
                    if To_Agenda.Agenda_To_Score_Set.Contains(Search_Operation) then
                        Free(Search_Operation.Repr);
                    else
                        To_Agenda.Agenda_To_Score_Set.Insert(Search_Operation);

                        -- Remove the search operation from To_Agenda.Compose_To_Count
                        declare
                            CTC_Cursor : Compose_To_Count_Maps.Cursor
                              := To_Agenda.Compose_To_Count_Map.Find(Search_Operation);
                        begin
                            if Compose_To_Count_Maps.Has_Element(CTC_Cursor) then
                                declare
                                    Old_SO : Search_Operation_Type
                                      := Compose_To_Count_Maps.Key(CTC_Cursor);
                                begin
                                    Free(Old_SO.Repr);
                                    To_Agenda.Compose_To_Count_Map.Delete(CTC_Cursor);
                                end;
                            end if;
                        end;

                        -- Remove the search operation from To_Agenda.Search_Operation_To_Neighs_Set_Map
                        declare
                            SOTNS_Cursor : Search_Operation_To_Neighs_Set_Maps.Cursor
                              := To_Agenda.Search_Operation_To_Neighs_Set_Map.Find(Search_Operation);
                        begin
                            if Search_Operation_To_Neighs_Set_Maps.Has_Element(SOTNS_Cursor) then
                                declare
                                    Old_SO : Search_Operation_Type
                                      := Search_Operation_To_Neighs_Set_Maps.Key(SOTNS_Cursor);
                                begin
                                    Free(Old_SO.Repr);
                                    To_Agenda.Search_Operation_To_Neighs_Set_Map.Delete(SOTNS_Cursor);
                                end;
                            end if;
                        end;

                    end if;
                end;
                Search_Operation_Sets.Next(From_Cursor);
            end loop;

            From_Agenda.Agenda_To_Score_Set.Clear;
            From_Agenda.Agenda_To_Score_Set.Reserve_Capacity(0);
            To_Agenda.Agenda_To_Score_Set.Reserve_Capacity(0);
        end;

        -----
        -- Agenda.Compose_To_Count_Map
        -----
        declare
            From_Cursor : Compose_To_Count_Maps.Cursor
              := From_Agenda.Compose_To_Count_Map.First;
        begin
            To_Agenda.Compose_To_Count_Map.Reserve_Capacity(To_Agenda.Compose_To_Count_Map.Length + From_Agenda.Compose_To_Count_Map.Length);

            -- Loop each (Search_Opetarion => Count)
            while Compose_To_Count_Maps.Has_Element(From_Cursor) loop
                declare
                    Search_Operation : Search_Operation_Type
                      := Compose_To_Count_Maps.Key(From_Cursor);
                begin
                    if To_Agenda.Agenda_To_Score_Set.Contains(Search_Operation) then
                        -- If S.Op. already is into To_Agenda.Agenda_To_Score_Set, that's OK
                        Free(Search_Operation.Repr);
                    else
                        declare
                            From_Count : constant Natural
                              := Compose_To_Count_Maps.Element(From_Cursor);
                            To_Cursor  : Compose_To_Count_Maps.Cursor
                              := To_Agenda.Compose_To_Count_Map.Find(Search_Operation);
                        begin
                            if not Compose_To_Count_Maps.Has_Element(To_Cursor) then
                                -- Just transport the association to To_Agenda.Compose_To_Count_Map
                                To_Agenda.Compose_To_Count_Map.Insert(Search_Operation, From_Count);

                            else
                                -- Merge counts
                                declare
                                    To_Count    : Natural renames Compose_To_Count_Maps.Element(To_Cursor);
                                    Total_Count : constant Natural := From_Count + To_Count;
                                begin
                                    if Long_Float(Total_Count) >= Agenda_Min_Abs_Count_Observed then
                                        -- Move to Agenda_To_Score
                                        declare
                                            Repr : Integer_Array_Access_Type := Compose_To_Count_Maps.Key(To_Cursor).Repr;
                                        begin
                                            Free(Repr);
                                        end;
                                        To_Agenda.Compose_To_Count_Map.Delete(To_Cursor);
                                        To_Agenda.Agenda_To_Score_Set.Insert(Search_Operation);
                                    else
                                        -- Simply store sum into the destination
                                        To_Agenda.Compose_To_Count_Map.Replace_Element(To_Cursor, Total_Count);
                                        Free(Search_Operation.Repr);
                                    end if;
                                end;
                            end if;
                        end;
                    end if;
                end;
                Compose_To_Count_Maps.Next(From_Cursor);
            end loop;

            From_Agenda.Compose_To_Count_Map.Clear;
            From_Agenda.Compose_To_Count_Map.Reserve_Capacity(0);
            To_Agenda.Compose_To_Count_Map.Reserve_Capacity(0);
        end;

        -----
        -- Agenda.Search_Operation_To_Neighs_Set_Map
        -----
        declare
            From_Cursor : Search_Operation_To_Neighs_Set_Maps.Cursor
              := From_Agenda.Search_Operation_To_Neighs_Set_Map.First;
        begin

            To_Agenda.Search_Operation_To_Neighs_Set_Map.Reserve_Capacity(To_Agenda.Search_Operation_To_Neighs_Set_Map.Length + From_Agenda.Search_Operation_To_Neighs_Set_Map.Length);

            -- Loop each (Search_Opetarion => Count)
            while Search_Operation_To_Neighs_Set_Maps.Has_Element(From_Cursor) loop
                declare
                    Search_Operation : Search_Operation_Type
                      := Search_Operation_To_Neighs_Set_Maps.Key(From_Cursor);
                begin
                    if To_Agenda.Agenda_To_Score_Set.Contains(Search_Operation) then
                        Free(Search_Operation.Repr);
                        -- If S.Op. already is into To_Agenda.Agenda_To_Score_Set, that's OK
                    else
                        declare
                            From_Neighs_Set : constant Neighs_Sets.Set
                              := Search_Operation_To_Neighs_Set_Maps.Element(From_Cursor);
                            To_Cursor       : Search_Operation_To_Neighs_Set_Maps.Cursor
                              := To_Agenda.Search_Operation_To_Neighs_Set_Map.Find(Search_Operation);
                        begin
                            if not Search_Operation_To_Neighs_Set_Maps.Has_Element(To_Cursor) then
                                -- Just transport the association to To_Agenda.Compose_To_Count_Map
                                To_Agenda.Search_Operation_To_Neighs_Set_Map.Insert(Search_Operation, From_Neighs_Set);

                            else
                                -- Merge sets
                                declare
                                    To_Neighs_Set    : Neighs_Sets.Set renames Search_Operation_To_Neighs_Set_Maps.Element(To_Cursor);
                                    Total_Neighs_Set : Neighs_Sets.Set := Neighs_Sets.Union(From_Neighs_Set, To_Neighs_Set);
                                begin
                                    if Natural(Total_Neighs_Set.Length) >= Configurations.Min_Merge_Cluster_Count then -- TODO: See BUG on "Create Agenda" slides
                                        -- Move to Agenda_To_Score
                                        declare
                                            Repr : Integer_Array_Access_Type := Search_Operation_To_Neighs_Set_Maps.Key(To_Cursor).Repr;
                                        begin
                                            Free(Repr);
                                        end;
                                        To_Agenda.Search_Operation_To_Neighs_Set_Map.Delete(To_Cursor);
                                        To_Agenda.Agenda_To_Score_Set.Insert(Search_Operation);
                                    else
                                        -- Simply store sum into the destination
                                        Total_Neighs_Set.Reserve_Capacity(0);
                                        To_Agenda.Search_Operation_To_Neighs_Set_Map.Replace_Element(To_Cursor, Total_Neighs_Set);
                                        Free(Search_Operation.Repr);
                                    end if;
                                end;
                            end if;
                        end;
                    end if;
                end;
                Search_Operation_To_Neighs_Set_Maps.Next(From_Cursor);
            end loop;

            From_Agenda.Search_Operation_To_Neighs_Set_Map.Clear;
            From_Agenda.Search_Operation_To_Neighs_Set_Map.Reserve_Capacity(0);
            To_Agenda.Search_Operation_To_Neighs_Set_Map.Reserve_Capacity(0);
        end;

    end Merge_Agenda;

    ---------------------
    procedure Free_Agenda
    ---------------------
      (Agenda : in out Agenda_Type) is
    begin

        declare
            C : Search_Operation_To_Neighs_Set_Maps.Cursor
              := Agenda.Search_Operation_To_Neighs_Set_Map.First;
        begin
            while Search_Operation_To_Neighs_Set_Maps.Has_Element(C) loop
                declare
                    Repr : Integer_Array_Access_Type
                      := Search_Operation_To_Neighs_Set_Maps.Key(C).Repr;
                begin
                    Free(Repr);
                end;
                Search_Operation_To_Neighs_Set_Maps.Next(C);
            end loop;
            Agenda.Search_Operation_To_Neighs_Set_Map.Clear;
            Agenda.Search_Operation_To_Neighs_Set_Map.Reserve_Capacity(0);
        end;

        declare
            C : Search_Operation_Sets.Cursor
              := Agenda.Agenda_To_Score_Set.First;
        begin
            while Search_Operation_Sets.Has_Element(C) loop
                declare
                    Repr : Integer_Array_Access_Type
                      := Search_Operation_Sets.Element(C).Repr;
                begin
                    Free(Repr);
                end;
                Search_Operation_Sets.Next(C);
            end loop;
            Agenda.Agenda_To_Score_Set.Clear;
            Agenda.Agenda_To_Score_Set.Reserve_Capacity(0);
        end;

        declare
            C : Compose_To_Count_Maps.Cursor
              := Agenda.Compose_To_Count_Map.First;
        begin
            while Compose_To_Count_Maps.Has_Element(C) loop
                declare
                    Repr : Integer_Array_Access_Type
                      := Compose_To_Count_Maps.Key(C).Repr;
                begin
                    Free(Repr);
                end;
                Compose_To_Count_Maps.Next(C);
            end loop;
            Agenda.Compose_To_Count_Map.Clear;
            Agenda.Compose_To_Count_Map.Reserve_Capacity(0);
        end;

        declare
            C : Search_Operation_To_Score_Maps.Cursor
              := Agenda.Inactive_Agenda_To_Score_Map.First;
        begin
            while Search_Operation_To_Score_Maps.Has_Element(C) loop
                declare
                    Repr : Integer_Array_Access_Type
                      := Search_Operation_To_Score_Maps.Key(C).Repr;
                begin
                    Free(Repr);
                end;
                Search_Operation_To_Score_Maps.Next(C);
            end loop;
            Agenda.Inactive_Agenda_To_Score_Map.Clear;
            Agenda.Inactive_Agenda_To_Score_Map.Reserve_Capacity(0);
        end;

        declare
            C : Search_Operation_To_Score_Maps.Cursor
              := Agenda.Active_Agenda_To_Score_Map.First;
        begin
            while Search_Operation_To_Score_Maps.Has_Element(C) loop
                declare
                    Repr : Integer_Array_Access_Type
                      := Search_Operation_To_Score_Maps.Key(C).Repr;
                begin
                    Free(Repr);
                end;
                Search_Operation_To_Score_Maps.Next(C);
            end loop;
            Agenda.Active_Agenda_To_Score_Map.Clear;
            Agenda.Active_Agenda_To_Score_Map.Reserve_Capacity(0);
        end;

        for Item of Agenda.Score_Active_Agenda loop
            declare
                Repr : Integer_Array_Access_Type
                  := Item.Search_Operation.Repr;
            begin
                Free(Repr);
            end;
        end loop;
        Agenda.Score_Active_Agenda.Clear;
        -- Can't Reserve Capacity

        for Set of Agenda.Cluster_Index_To_Search_Operation_Set_Map loop
            for Item of Set loop
                declare
                    Repr : Integer_Array_Access_Type
                      := Item.Repr;
                begin
                    Free(Repr);
                end;
            end loop;
        end loop;
        Agenda.Cluster_Index_To_Search_Operation_Set_Map.Clear;
        Agenda.Cluster_Index_To_Search_Operation_Set_Map.Reserve_Capacity(0);

    end Free_Agenda;

    -------------------------
    procedure Minimize_Agenda
    -------------------------
      (Agenda : in out Agenda_Type) is
    begin

        Agenda.Search_Operation_To_Neighs_Set_Map.Reserve_Capacity(Agenda.Search_Operation_To_Neighs_Set_Map.Length);
        -- TODO: necessario fare un reserve_capacity sui Neighs_Set?

        Agenda.Agenda_To_Score_Set.Reserve_Capacity(Agenda.Agenda_To_Score_Set.Length);
        Agenda.Compose_To_Count_Map.Reserve_Capacity(Agenda.Compose_To_Count_Map.Length);
        Agenda.Inactive_Agenda_To_Score_Map.Reserve_Capacity(Agenda.Inactive_Agenda_To_Score_Map.Length);
        Agenda.Active_Agenda_To_Score_Map.Reserve_Capacity(Agenda.Active_Agenda_To_Score_Map.Length);

        -- Can't Reserve Capacity for Agenda.Score_Active_Agenda

        Agenda.Cluster_Index_To_Search_Operation_Set_Map.Reserve_Capacity(Agenda.Cluster_Index_To_Search_Operation_Set_Map.Length);
        -- TODO: necessario fare un reserve_capacity sui Search_Operation_Set?
    end Minimize_Agenda;

    -----------------------
    procedure Create_Agenda
    -----------------------
      (Agenda              : in out Agenda_Type;
       Use_Consumer        : in     Boolean;
       Cluster_Index_Start : in     Cluster_Index_Type := -1;
       Cluster_Index_End   : in     Cluster_Index_Type := -1) is

        Cluster_Map_Copy         : Cluster_Maps.Map;
        Atom_ID_To_Part_Map_Copy : Atom_ID_To_Part_Maps.Map;

        Cluster_Index            : Cluster_Index_Type;
        Cluster                  : Cluster_Access_Type;

        Counter : Natural := 0;
    begin

        if Use_Consumer then

            if Cluster_Index_Start /= -1 or else Cluster_Index_End /= -1 then
                raise MLSE_Error with "Cluster Index Start and End must NOT be specified when Use_Consumer is True";
            end if;

            -- TODO: attenzione, faccio copie per poi evitare i mutex
            Cluster_Map_Copy         := Cluster_Map;
            Atom_ID_To_Part_Map_Copy := Atom_ID_To_Part_Map;

            while True loop
                Local_Agenda_Range_Consumer.Pop(Cluster_Index);
                exit when Cluster_Index = -1;

                Cluster := Cluster_Map_Copy.Element(Cluster_Index);
                -- filter
                if Cluster.Is_Content -- non-content word doesn't count
                  and then not Cluster.Is_Stop then
                    Add_Agenda_For_New_Cluster(Agenda, Cluster.Index, Atom_ID_To_Part_Map_Copy);
                end if;
            end loop;

        else

            if Cluster_Index_Start = -1 or else Cluster_Index_End = -1 then
                raise MLSE_Error with "Cluster Index Start and End MUST be specified when Use_Consumer is False";
            end if;

            T_IO.Put_Line("Create Agenda - No Consumer - (" & Str(Cluster_Index_Start) & " .. " & Str(Cluster_Index_End) & ")");

            for Cluster of Cluster_Map loop
                Counter := Counter + 1;

                -- filter
                if Cluster.Index >= Cluster_Index_Start and then Cluster.Index <= Cluster_Index_End
                  and then Cluster.Is_Content -- non-content word doesn't count
                  and then not Cluster.Is_Stop then
                    Add_Agenda_For_New_Cluster(Agenda, Cluster.Index, Atom_ID_To_Part_Map);
                end if;

                if Counter mod 100 = 0 then
                    T_IO.Put_Line("Processed clusters: " & Counter'Img);
                end if;
            end loop;
        end if;

    end Create_Agenda;


    -- add new agenda enabled by parts in the new clusts
    ------------------------------------
    procedure Add_Agenda_For_New_Cluster
    ------------------------------------
      (Agenda                    : in out Agenda_Type;
       New_Cluster_Index         : in     Cluster_Index_Type;
       Atom_ID_To_Part_Map_Local : in     Atom_ID_To_Part_Maps.Map) is

        Atom_ID_Set : Atom_ID_Sets.Set;

        Part1       : Part_Access_Type := null;
        Part2       : Part_Access_Type := null;
    begin

        Cluster_Index_To_Atom_ID_Set_Map_Mutex.Seize;
        Atom_ID_Set := Cluster_Index_To_Atom_ID_Set_Map.Element(New_Cluster_Index);
        Cluster_Index_To_Atom_ID_Set_Map_Mutex.Release;

        if Atom_ID_Set.Length > 1 then
            for ID1 of Atom_ID_Set loop
                Part1 := Atom_ID_To_Part_Map_Local.Element(ID1);

                for ID2 of Atom_ID_Set loop
                    exit when ID1 <= ID2;

                    Part2 := Atom_ID_To_Part_Map_Local.Element(ID2);

                    Add_Agenda_After_Merge_Clust(Agenda, Part1.all, Part2.all);
                end loop;
            end loop;
        end if;
    end Add_Agenda_For_New_Cluster;

    --------------------------------------
    procedure Add_Agenda_After_Merge_Clust
    --------------------------------------
      (Agenda : in out Agenda_Type;
       Part1  : in     Part_Type;
       Part2  : in     Part_Type) is

        Cluster_Index : Cluster_Index_Type renames Part1.Cluster_Index;
    begin
        if Part1.Cluster_Index /= Part2.Cluster_Index then
            raise MLSE_Error with "Part1.Cluster_Index /= Part2.Cluster_Index";
        end if;

        -- merge pars/absorb
        if Part1.Governor_Part /= null and then Part2.Governor_Part /= null then
            declare
                Governor_Cluster_Index1 : Cluster_Index_Type renames Part1.Governor_Part.Cluster_Index;
                Governor_Cluster_Index2 : Cluster_Index_Type renames Part2.Governor_Part.Cluster_Index;
            begin
                if Governor_Cluster_Index1 /= Governor_Cluster_Index2 then
                    Add_Agenda_MC(Agenda, Part1.Governor_Part.Cluster_Access.all, Part2.Governor_Part.Cluster_Access.all, (2 * Neigh_Type_Type(Cluster_Index) + 1));
                else
                    Add_Agenda_Abs(Agenda, Governor_Cluster_Index1, Cluster_Index);
                end if;
            end;
        end if;

        --  merge chd/absorb?
        declare
            Argument_Parts_1    : Part_Maps.Map;
            Argument_Parts_2    : Part_Maps.Map;

            Dependent_Cluster_1 : Cluster_Access_Type;
            Dependent_Cluster_2 : Cluster_Access_Type;
        begin

            -- TODO: attenzione, faccio delle copie, ma almeno non usero' piu' il mutex
            -- TODO: provare lo stesso fottio di mutex di Cluster_Index_Mutex
            Argument_Parts_1 := Part1.Argument_Part_Map;
            Argument_Parts_2 := Part2.Argument_Part_Map;

            for Dependent_1 of Argument_Parts_1 loop
                Dependent_Cluster_1 := Dependent_1.Cluster_Access;

                for Dependent_2 of Argument_Parts_2 loop
                    Dependent_Cluster_2 := Dependent_2.Cluster_Access;

                    if Dependent_Cluster_1.Index /= Dependent_Cluster_2.Index then
                        Add_Agenda_MC(Agenda, Dependent_Cluster_1.all, Dependent_Cluster_2.all, (2 * Neigh_Type_Type(Cluster_Index)));
                    else
                        Add_Agenda_Abs(Agenda, Cluster_Index, Dependent_Cluster_1.Index);
                    end if;

                end loop;
            end loop;
        end;

    end Add_Agenda_After_Merge_Clust;

    -----------------------
    procedure Add_Agenda_MC
    -----------------------
      (Agenda          : in out Agenda_Type;
       Cluster_1       : in     Cluster_Type;
       Cluster_2       : in     Cluster_Type;
       Neigh_Type      : in     Neigh_Type_Type) is

        Cluster_Index_1 : Cluster_Index_Type renames Cluster_1.Index;
        Cluster_Index_2 : Cluster_Index_Type renames Cluster_2.Index;

        Search_Operation : Search_Operation_Type;
    begin
        if Agenda_Skip_MC or else Cluster_Index_1 = Cluster_Index_2 then
            return;
        elsif Cluster_1.Is_Content /= Cluster_2.Is_Content then
            return; -- must match type to be merged (content vs func)
        elsif not Cluster_1.Is_Content then
            return; -- ignore non-content merge for now: subsume by arg alignment
        end if;

        Search_Operation.Operation := Op_Merge_Clust;

        if Cluster_Index_1 < Cluster_Index_2 then
            Search_Operation.Cluster_Index_1_Or_Governor  := Cluster_Index_1;
            Search_Operation.Cluster_Index_2_Or_Dependent := Cluster_Index_2;
        else
            Search_Operation.Cluster_Index_1_Or_Governor  := Cluster_Index_2;
            Search_Operation.Cluster_Index_2_Or_Dependent := Cluster_Index_1;
        end if;

        Generate_Search_Operation_String(Search_Operation);

        if Move_Agenda_To_Score(Agenda, Search_Operation) then
            return; -- already in agenda
        end if;

        declare

            Delete_Operation : Boolean := False;

            procedure Update_Search_Operation_To_Neighs_Set_Map(Local_Search_Operation : in Search_Operation_Type; Neighs_Set : in out Neighs_Sets.Set) is begin
                -- TODO: attenzione: codice modificato di propria iniziativa (vedi sotto)
                Insert(Neighs_Set, Neigh_Type);

                if Natural(Neighs_Set.Length) >= Configurations.Min_Merge_Cluster_Count then
                    Insert(Agenda.Agenda_To_Score_Set, Search_Operation, True);
                    Delete_Operation := True;
                    declare
                        Repr : Integer_Array_Access_Type := Local_Search_Operation.Repr;
                    begin
                        Free(Repr);
                    end;
                end if;

                -- TODO: attenzione, questo era il codice originale:
                --                  if Natural(Neighs_Set.Length) + 1 >= Configurations.Min_Merge_Cluster_Count then
                --                      Insert(Agenda.Agenda_To_Score_Set, Search_Operation);
                --                      Delete_Operation := True;
                --                  else
                --                      Insert(Neighs_Set, Neigh_Type);
                --                  end if;
            end Update_Search_Operation_To_Neighs_Set_Map;

            Cursor   : Search_Operation_To_Neighs_Set_Maps.Cursor
              := Agenda.Search_Operation_To_Neighs_Set_Map.Find(Search_Operation);
            New_Inserted : Boolean := False;
        begin
            if not Search_Operation_To_Neighs_Set_Maps.Has_Element(Cursor) then
                Insert
                  (Map      => Agenda.Search_Operation_To_Neighs_Set_Map,
                   Key      => Search_Operation,
                   Position => Cursor);
                New_Inserted := True;
            end if;

            Agenda.Search_Operation_To_Neighs_Set_Map.Update_Element(Cursor, Update_Search_Operation_To_Neighs_Set_Map'Access);

            if Delete_Operation then
                Agenda.Search_Operation_To_Neighs_Set_Map.Delete(Cursor);
            elsif not New_Inserted then
                Free(Search_Operation.Repr);
            end if;
        end;

    end Add_Agenda_MC;

    ------------------------
    procedure Add_Agenda_Abs
    ------------------------
      (Agenda                  : in out Agenda_Type;
       Parent_Cluster_Index    : in     Cluster_Index_Type;
       Dependent_Cluster_Index : in     Cluster_Index_Type) is

        Search_Operation : Search_Operation_Type;
    begin

        if Agenda_Skip_Compose then
            return;
        end if;

        Search_Operation.Operation := Op_Compose;
        Search_Operation.Cluster_Index_1_Or_Governor  := Parent_Cluster_Index;
        Search_Operation.Cluster_Index_2_Or_Dependent := Dependent_Cluster_Index;

        Generate_Search_Operation_String(Search_Operation);

        if Move_Agenda_To_Score(Agenda, Search_Operation) then
            return;
        end if;

        declare
            Cursor : Compose_To_Count_Maps.Cursor
              := Agenda.Compose_To_Count_Map.Find(Search_Operation);
        begin
            if not Compose_To_Count_Maps.Has_Element(Cursor) then
                Agenda.Compose_To_Count_Map.Insert(Search_Operation, 1);
            elsif Long_Float(Compose_To_Count_Maps.Element(Cursor)) + 1.0 >= Agenda_Min_Abs_Count_Observed then -- counted square
                declare
                    Repr : Integer_Array_Access_Type := Compose_To_Count_Maps.Key(Cursor).Repr;
                begin
                    Free(Repr);
                end;
                Agenda.Compose_To_Count_Map.Delete(Cursor);
                Insert(Agenda.Agenda_To_Score_Set, Search_Operation, True);
            else
                Increment(Agenda.Compose_To_Count_Map, Cursor);
                Free(Search_Operation.Repr);
            end if;
        end;
    end Add_Agenda_Abs;

    -----------------------------
    function Move_Agenda_To_Score
    -----------------------------
      (Agenda                  : in out Agenda_Type;
       Search_Operation        : in out Search_Operation_Type) return Boolean is

        Cursor : Search_Operation_To_Score_Maps.Cursor;
    begin

        if Agenda.Agenda_To_Score_Set.Contains(Search_Operation) then
            Free(Search_Operation.Repr);
            return True;
        end if;

        Cursor := Agenda.Active_Agenda_To_Score_Map.Find(Search_Operation);
        if Search_Operation_To_Score_Maps.Has_Element(Cursor) then

            Agenda.Score_Active_Agenda.Delete
              ((Score            => Search_Operation_To_Score_Maps.Element(Cursor),
                Search_Operation => Search_Operation));
            Agenda.Active_Agenda_To_Score_Map.Delete(Search_Operation); -- TODO: free repr
            Insert(Agenda.Agenda_To_Score_Set, Search_Operation, True);

            return True;
        end if;

        Cursor := Agenda.Inactive_Agenda_To_Score_Map.Find(Search_Operation);
        if Search_Operation_To_Score_Maps.Has_Element(Cursor) then
            Agenda.Inactive_Agenda_To_Score_Map.Delete(Cursor); -- TODO: free repr
            Insert(Agenda.Agenda_To_Score_Set, Search_Operation, True);

            return True;
        end if;

        return False;
    end Move_Agenda_To_Score;

    ------------------------
    procedure Process_Agenda
    ------------------------
      (Agenda : in out Agenda_Type) is

        Total_Agenda_Scored : Natural := 0;
        Total_Exec_MC       : Natural := 0;
        Total_Exec_Abs      : Natural := 0;
    begin
        T_IO.Put_Line("<ProcAgenda> ...");
        T_IO.Put_Line("<INFO> Initial agenda MC=" & Str(Agenda.Search_Operation_To_Neighs_Set_Map.Length) & " ABS=" & Str(Agenda.Compose_To_Count_Map.Length));
        T_IO.Put_Line("<INFO> Initial agenda to score=" &  Str(Agenda.Agenda_To_Score_Set.Length));

        -- util.Timer timer=new util.Timer();
        -- long time;
        -- long ttlScoreTime=0, ttlExecMCTime=0, ttlExecAbsTime=0;

        Main_Loop: while True loop
            -- timer.timerStart();

            for Search_Operation of Agenda.Agenda_To_Score_Set loop
                declare
                    Score : constant Score_Type := Score_Operation(Search_Operation);
                begin
                    Total_Agenda_Scored := Total_Agenda_Scored + 1;

                    -- TODO: TO-DO
                    if Score >= -200.0 then
                        Add_Agenda(Agenda, Search_Operation, Score);
                    end if;
                end;
            end loop;
            Agenda.Agenda_To_Score_Set.Clear;

            -- time=timer.getTimeElapsed();
            -- ttlScoreTime+=time;

            exit Main_Loop when Agenda.Score_Active_Agenda.Is_Empty;

            -- Execute

            -- timer.timerStart();
            declare
                Last_Link : constant Score_And_Search_Operation_Link_Type
                  := Agenda.Score_Active_Agenda.Last_Element;

                New_Cluster_Index : Cluster_Index_Type := -1;
            begin
                --T_IO.Put_Line("<EXEC> " & Str(Last_Link.Search_Operation.Str) & " " & Last_Link.Score'Img);
                T_IO.Put_Line("<EXEC> " & " " & Last_Link.Score'Img);

                New_Cluster_Index := Execute_Operation(Last_Link.Search_Operation);

                if New_Cluster_Index = -1 then
                   T_IO.Put_Line("<FORCE EXIT DUE TO -1 NEW_CLUSTER_INDEX> ");
            end if;

               exit Main_Loop when New_Cluster_Index = -1; -- TODO: enorme limitazione, perde un sacco di cose

                Update_Agenda_After_Exec(Agenda, Last_Link.Search_Operation, New_Cluster_Index);

                -- time=timer.getTimeElapsed();

                case Last_Link.Search_Operation.Operation is
                when Op_Compose =>
                    Total_Exec_Abs := Total_Exec_Abs + 1;
                    -- ttlExecAbsTime+=time;

                when Op_Merge_Clust =>
                    Total_Exec_MC :=  Total_Exec_MC+ 1;
                    -- ttlExecMCTime+=time;

                when others =>
                    raise MLSE_Invalid_Search_Operation_Error with Last_Link.Search_Operation.Operation'Img;
               end case;
            end;
        end loop Main_Loop;

        T_IO.Put_Line("<INFO> Agenda scored=" & Str(Total_Agenda_Scored)); -- +" ("+((ttlScoreTime/1000))+"s)"
        T_IO.Put_Line("<INFO> Agenda MC executed=" & Str(Total_Exec_MC)); -- +" ("+((ttlExecMCTime/1000))+"s)");
        T_IO.Put_Line("<INFO> Agenda Compose executed=" & Str(Total_Exec_Abs)); -- +" ("+((ttlExecAbsTime/1000))+"s)");
    end Process_Agenda;

    --------------------
    procedure Add_Agenda
    --------------------
      (Agenda           : in out Agenda_Type;
       Search_Operation : in     Search_Operation_Type;
       Score            : in     Score_Type) is

        Cluster_Index_1 : Cluster_Index_Type renames Search_Operation.Cluster_Index_1_Or_Governor;
        Cluster_Index_2 : Cluster_Index_Type renames Search_Operation.Cluster_Index_2_Or_Dependent;
    begin

        declare
            Cursor   : Cluster_Index_To_Search_Operation_Set_Maps.Cursor;
        begin

            -- Cluster_Index_1
            Cursor := Agenda.Cluster_Index_To_Search_Operation_Set_Map.Find(Cluster_Index_1);

            if not Cluster_Index_To_Search_Operation_Set_Maps.Has_Element(Cursor) then
                Insert
                  (Map      => Agenda.Cluster_Index_To_Search_Operation_Set_Map,
                   Key      => Cluster_Index_1,
                   Position => Cursor);
            end if;

            Add_To_Set
              (Map          => Agenda.Cluster_Index_To_Search_Operation_Set_Map,
               Map_Cursor   => Cursor,
               Set_New_Item => Search_Operation);

            -- Cluster_Index_2
            Cursor := Agenda.Cluster_Index_To_Search_Operation_Set_Map.Find(Cluster_Index_2);

            if not Cluster_Index_To_Search_Operation_Set_Maps.Has_Element(Cursor) then
                Insert
                  (Map      => Agenda.Cluster_Index_To_Search_Operation_Set_Map,
                   Key      => Cluster_Index_2,
                   Position => Cursor);
            end if;

            Add_To_Set
              (Map          => Agenda.Cluster_Index_To_Search_Operation_Set_Map,
               Map_Cursor   => Cursor,
               Set_New_Item => Search_Operation);
        end;

        if Score < Configurations.Prior_Cutoff then
            if Agenda.Inactive_Agenda_To_Score_Map.Contains(Search_Operation) then -- FIXME: aggiunta dato lo spaccaggio totale!
                Agenda.Inactive_Agenda_To_Score_Map.Replace(Search_Operation, Score);
            else
                Agenda.Inactive_Agenda_To_Score_Map.Insert(Search_Operation, Score);
            end if;
        else

            if Agenda.Active_Agenda_To_Score_Map.Contains(Search_Operation) then -- FIXME: aggiunta dato lo spaccaggio totale!
                Agenda.Active_Agenda_To_Score_Map.Replace(Search_Operation, Score);
            else
                Agenda.Active_Agenda_To_Score_Map.Insert(Search_Operation, Score);
            end if;

            if not Agenda.Score_Active_Agenda.Contains((Score => Score, Search_Operation => Search_Operation)) then -- FIXME: aggiunta dato lo spaccaggio totale!
                Agenda.Score_Active_Agenda.Insert((Score => Score, Search_Operation => Search_Operation));
            end if;
        end if;

    end Add_Agenda;

    ---------------------------
    function Execute_Operation
    ---------------------------
      (Search_Operation : in Search_Operation_Type) return Cluster_Index_Type is
   begin

        if not (Cluster_Map.Contains(Search_Operation.Cluster_Index_1_Or_Governor)
        and then Cluster_Map.Contains(Search_Operation.Cluster_Index_2_Or_Dependent)) then
          return -1;
        end if;

        case Search_Operation.Operation is
            when Op_Merge_Clust =>
                return Execute_Operation_MC(Search_Operation.Cluster_Index_1_Or_Governor, Search_Operation.Cluster_Index_2_Or_Dependent);

            when Op_Compose =>
                return Execute_Operation_Compose(Search_Operation.Cluster_Index_1_Or_Governor, Search_Operation.Cluster_Index_2_Or_Dependent);

            when others =>
                raise MLSE_Invalid_Search_Operation_Error with Search_Operation.Operation'Img;
        end case;
    end Execute_Operation;

    -----------------------------
    function Execute_Operation_MC
    -----------------------------
      (Cluster_Index_1, Cluster_Index_2 : in Cluster_Index_Type) return Cluster_Index_Type is

        New_Cluster_Index : Cluster_Index_Type := Cluster_Index_1;

        -- TODO: potrebbe essere ottimizzato passando alla funzione direttamente gli accessi ai cluster
        Cluster_1 : Cluster_Access_Type := Cluster_Map.Element(Cluster_Index_1);
        Cluster_2 : Cluster_Access_Type := Cluster_Map.Element(Cluster_Index_2);

        Argument_Cluster_Index_2_To_Argument_Cluster_Index_1_Map : Argument_Cluster_Index_To_Argument_Cluster_Index_Maps.Map;
    begin

        -- fix parts, clust
        -- update agenda, filtered op scores
        -- remove from agenda

        if Cluster_1.Argument_Cluster_Map.Length < Cluster_2.Argument_Cluster_Map.Length then
            declare
                Cluster_Tmp : constant Cluster_Access_Type := Cluster_2;
            begin
                Cluster_2 := Cluster_1;
                Cluster_1 := Cluster_Tmp;
            end;
            New_Cluster_Index := Cluster_1.Index;
        end if;

        T_IO.Put_Line(ASCII.HT & "Merge: " & Str(To_String(Cluster_1.all)) & " " & Str(To_String(Cluster_2.all)));

        -- find optimal arg mapping
        Score_MC_For_Align(Cluster_1.all, Cluster_2.all, Argument_Cluster_Index_2_To_Argument_Cluster_Index_1_Map);

        --  create new argclusts
        declare
            AC2_Cursor : Argument_Cluster_Maps.Cursor
              := Cluster_2.Argument_Cluster_Map.First;
        begin
            while Argument_Cluster_Maps.Has_Element(AC2_Cursor) loop
                declare
                    Argument_Cluster_Index_2 : constant Argument_Cluster_Index_Type
                      := Argument_Cluster_Maps.Key(AC2_Cursor);
                begin

                    if not Argument_Cluster_Index_2_To_Argument_Cluster_Index_1_Map.Contains(Argument_Cluster_Index_2) then
                        declare
                            Argument_Cluster_2 : constant Argument_Cluster_Access_Type
                              := Argument_Cluster_Maps.Element(AC2_Cursor);

                            Dep_Cursor         : constant Deprel_Index_To_Count_Maps.Cursor
                              := Argument_Cluster_2.Deprel_Index_To_Count_Map.First;
                        begin
                            while Deprel_Index_To_Count_Maps.Has_Element(Dep_Cursor) loop
                                declare
                                    Deprel_Index : constant Deprel_Index_Type
                                      := Deprel_Index_To_Count_Maps.Key(Dep_Cursor);

                                    Argument_Cluster_Index_1 : constant Argument_Cluster_Index_Type
                                      := Create_Argument_Cluster(Cluster_1.all, Deprel_Index);
                                begin
                                    Argument_Cluster_Index_2_To_Argument_Cluster_Index_1_Map.Insert
                                      (Argument_Cluster_Index_2, Argument_Cluster_Index_1);

                                    exit; -- TODO: ma in pratica considero solo il primo? tanto vale non fare il ciclo?
                                end;
                            end loop;
                        end;
                    end if;
                end;
                Argument_Cluster_Maps.Next(AC2_Cursor);
            end loop;
        end;

        declare
            Atom_ID_Set : constant Atom_ID_Sets.Set
              := Cluster_Index_To_Atom_ID_Set_Map.Element(Cluster_2.Index); -- TODO: faccio una copia poiche' dopo faccio degli unsetparent?
        begin

            -- exec for each part; log affected parties
            for Atom_ID of Atom_ID_Set loop
                declare
                    Part   : constant Part_Access_Type := Atom_ID_To_Part_Map.Element(Atom_ID);
                    Cursor : Part_Maps.Cursor;
                begin
                    Cursor := Part.Argument_Part_Map.First;
                    while Part_Maps.Has_Element(Cursor) loop
                        declare
                            Argument_Part : constant Part_Access_Type := Part_Maps.Element(Cursor);
                        begin
                            Unset_Governor_Part(Argument_Part.all);
                        end;
                        Part_Maps.Next(Cursor);
                    end loop;

                    Change_Cluster(Part.all, Cluster_1, Argument_Cluster_Index_2_To_Argument_Cluster_Index_1_Map);

                    Cursor := Part.Argument_Part_Map.First;
                    while Part_Maps.Has_Element(Cursor) loop
                        declare
                            Argument_Index : constant Part_Index_Type := Part_Maps.Key(Cursor);
                            Argument_Part  : constant Part_Access_Type := Part_Maps.Element(Cursor);
                        begin
                            Set_Governor_Part(Argument_Part.all, Part, Argument_Index);
                        end;
                        Part_Maps.Next(Cursor);
                    end loop;
                end;
            end loop;
        end;

        Cluster_Map.Delete(Cluster_2.Index); -- TODO: Free Cluster?

        return New_Cluster_Index;

    end Execute_Operation_MC;

    ----------------------------------
    function Execute_Operation_Compose
    ----------------------------------
      (Governor_Cluster_Index, Dependent_Cluster_Index : in Cluster_Index_Type) return Cluster_Index_Type is

        New_Cluster       : Cluster_Access_Type := null;
        New_Cluster_Index : Cluster_Index_Type  := -1;

        -- TODO: si puo' evitare la richiesta a cluster_map?
        Governor_Cluster  : constant Cluster_Access_Type := Cluster_Map.Element(Governor_Cluster_Index);
        Dependent_Cluster : constant Cluster_Access_Type := Cluster_Map.Element(Dependent_Cluster_Index);

        Cluster_Link : constant Cluster_Link_Type := (Governor_Cluster_Index => Governor_Cluster_Index, Dependent_Cluster_Index => Dependent_Cluster_Index);

        Atom_Link_Set : constant Atom_Link_Sets.Set
          := Cluster_Link_To_Atom_Link_Set_Map.Element(Cluster_Link); -- TODO: copia perche' poi modifica?
    begin
        --  update parts/idx: assuming part.changeclust etc only update idx for par/self

        for Atom_Link of Atom_Link_Set loop

            if Atom_ID_To_Part_Map.Contains(Atom_Link.Governor_Atom_ID) and then Atom_ID_To_Part_Map.Contains(Atom_Link.Dependent_Atom_ID) then

                --  gen new reltype, get new clust
                declare
                    Governor_Part  : constant Part_Access_Type
                      := Atom_ID_To_Part_Map.Element(Atom_Link.Governor_Atom_ID);

                    Dependent_Part : constant Part_Access_Type
                      := Atom_ID_To_Part_Map.Element(Atom_Link.Dependent_Atom_ID);

                    Dependent_Argument_Index_In_Governor_Part: constant Part_Index_Type
                      := Dependent_Part.Argument_Index_In_Governor_Part;

                    Deprel         : constant Unbounded_String
                      := Governor_Part.Argument_Part_Map.Element(Dependent_Argument_Index_In_Governor_Part).Deprel;

                    Governor_Root_Node : constant Atom_Access_Type
                      := Governor_Part.Root_Node;

                    Governor_Rel_Type_Index : Rel_Type_Index_Type := -1;
                begin
                    Atom_Add_Dependent(Governor_Root_Node.all, Deprel, Dependent_Part.Root_Node);
                    Governor_Rel_Type_Index := Get_Rel_Type_Index(Governor_Root_Node.all); -- NOTA: importante chiederlo DOPO aver aggiunto il dependent

                    if New_Cluster = null then
                        declare
                            Cluster_Index_Set : constant Cluster_Index_Sets.Set
                              := Get_Clusters_With_Rel_Type(Governor_Rel_Type_Index);
                        begin

                            if Cluster_Index_Set.Is_Empty then
                                New_Cluster := Cluster_Map.Element(Create_Cluster(Governor_Rel_Type_Index, Is_Content(Governor_Root_Node.POS)));
                            elsif Cluster_Index_Set.Length = 1 then
                                New_Cluster := Cluster_Map.Element(Get_Clusters_With_Rel_Type(Governor_Rel_Type_Index).First_Element);
                            else
                                raise MLSE_Error with "ERR: multiple clusts for same reltype " & Str(To_String(Governor_Cluster.all)) & "-" & Str(To_String(Dependent_Cluster.all));
                            end if;

                            New_Cluster_Index := New_Cluster.Index;

                            T_IO.Put_Line("new clust=" & Str(New_Cluster_Index) & ":" & Str(To_String(New_Cluster.all)));
                        end;
                    end if;

                    --  first unsetArg, then changeclust; need old clust for remove/unset old args

                    Remove_Argument_From_Part(Governor_Part.all, Dependent_Argument_Index_In_Governor_Part);

                    if Governor_Part.Cluster_Index /= New_Cluster_Index then

                        declare
                            Cursor : Part_Maps.Cursor := Governor_Part.Argument_Part_Map.First;
                        begin
                            while Part_Maps.Has_Element(Cursor) loop
                                declare
                                    Argument_Index : constant Part_Index_Type := Part_Maps.Key(Cursor);
                                begin
                                    Unset_Argument_Cluster(Governor_Part.all, Argument_Index);
                                    Unset_Governor_Part(Governor_Part.Argument_Part_Map.Element(Argument_Index).all);
                                end;
                                Part_Maps.Next(Cursor);
                            end loop;
                        end;

                        Change_Cluster(Governor_Part.all, New_Cluster, Governor_Rel_Type_Index);

                        -- set new args
                        declare
                            Cursor : Part_Maps.Cursor := Governor_Part.Argument_Part_Map.First;
                        begin
                            while Part_Maps.Has_Element(Cursor) loop
                                declare
                                    Argument_Index : constant Part_Index_Type  := Part_Maps.Key(Cursor);
                                    Argument_Part  : constant Part_Access_Type := Part_Maps.Element(Cursor);

                                    Cur_Argument_Cluster_Index : Argument_Cluster_Index_Type := -1;

                                    Sub_Cursor                 : constant Deprel_Index_To_Argument_Cluster_Index_Set_Maps.Cursor
                                      := New_Cluster.Deprel_Index_To_Argument_Cluster_Index_Set_Map.Find(Argument_Part.Deprel_Index);
                                begin

                                    if not Deprel_Index_To_Argument_Cluster_Index_Set_Maps.Has_Element(Sub_Cursor)
                                      or else Deprel_Index_To_Argument_Cluster_Index_Set_Maps.Element(Sub_Cursor).Is_Empty then
                                        Cur_Argument_Cluster_Index := Create_Argument_Cluster(New_Cluster.all, Argument_Part.Deprel_Index);
                                    else
                                        Cur_Argument_Cluster_Index
                                          := Argument_Cluster_Index_Sets.Element
                                            (Deprel_Index_To_Argument_Cluster_Index_Set_Maps.Element(Sub_Cursor).First);
                                    end if;

                                    Set_Governor_Part(Argument_Part.all, Governor_Part, Argument_Index);
                                    Set_Argument_Cluster(Governor_Part.all, Argument_Index, Cur_Argument_Cluster_Index);
                                end;
                                Part_Maps.Next(Cursor);
                            end loop;
                        end;

                    else
                        Unset_Rel_Type(Governor_Part.all);
                        Set_Rel_Type(Governor_Part.all, Governor_Rel_Type_Index);
                    end if;

                    Set_Rel_Type(Governor_Part.all, Governor_Rel_Type_Index);

                    declare
                        Cursor : Part_Maps.Cursor := Dependent_Part.Argument_Part_Map.First;
                    begin
                        while Part_Maps.Has_Element(Cursor) loop
                            declare
                                Argument_Index : constant Part_Index_Type  := Part_Maps.Key(Cursor);
                                Argument_Part  : constant Part_Access_Type := Part_Maps.Element(Cursor);

                                Cur_Argument_Cluster_Index : Argument_Cluster_Index_Type := -1;

                                Sub_Cursor                 : constant Deprel_Index_To_Argument_Cluster_Index_Set_Maps.Cursor
                                  := New_Cluster.Deprel_Index_To_Argument_Cluster_Index_Set_Map.Find(Argument_Part.Deprel_Index);
                            begin

                                if not Deprel_Index_To_Argument_Cluster_Index_Set_Maps.Has_Element(Sub_Cursor)
                                  or else Deprel_Index_To_Argument_Cluster_Index_Set_Maps.Element(Sub_Cursor).Is_Empty then
                                    Cur_Argument_Cluster_Index := Create_Argument_Cluster(New_Cluster.all, Argument_Part.Deprel_Index);
                                else
                                    Cur_Argument_Cluster_Index
                                      := Argument_Cluster_Index_Sets.Element
                                        (Deprel_Index_To_Argument_Cluster_Index_Set_Maps.Element(Sub_Cursor).First);
                                end if;

                                -- can't call clust.unset; assume that part.unset is done
                                Unset_Argument_Cluster(Dependent_Part.all, Argument_Index);

                                declare
                                    New_Argument_Index : constant Part_Index_Type
                                      := Add_Argument(Governor_Part.all, Argument_Part, Null_Unbounded_String, -1);
                                begin
                                    Set_Argument_Cluster(Governor_Part.all, New_Argument_Index, Cur_Argument_Cluster_Index);
                                    Set_Governor_Part(Argument_Part.all, Governor_Part, New_Argument_Index);
                                end;
                            end;
                            Part_Maps.Next(Cursor);
                        end loop;
                    end;

                    Destroy_Part(Dependent_Part);
                end;
            end if;
        end loop;

        Delete_From_Set
          (Map          => Cluster_Index_To_Cluster_Link_Set_Map,
           Map_Position => Cluster_Index_To_Cluster_Link_Set_Map.Find(Governor_Cluster_Index),
           Set_Item     => Cluster_Link);

        Delete_From_Set
          (Map          => Cluster_Index_To_Cluster_Link_Set_Map,
           Map_Position => Cluster_Index_To_Cluster_Link_Set_Map.Find(Dependent_Cluster_Index),
           Set_Item     => Cluster_Link);

        Cluster_Link_To_Atom_Link_Set_Map.Delete(Cluster_Link);

        return New_Cluster_Index;

    end Execute_Operation_Compose;

    ----------------------------
    procedure Atom_Add_Dependent
    ----------------------------
      (Governor_Atom  : in out Atom_Type;
       Deprel         : in     Unbounded_String;
       Dependent_Atom : in     Atom_Access_Type) is

        Cursor   : Deprel_To_Atom_Set_Maps.Cursor
          := Governor_Atom.Dependents.Find(Deprel);
    begin

        if not Deprel_To_Atom_Set_Maps.Has_Element(Cursor) then
            Insert
              (Map      => Governor_Atom.Dependents,
               Key      => Deprel,
               Position => Cursor);
        end if;

        Add_To_Set
          (Map          => Governor_Atom.Dependents,
           Map_Cursor   => Cursor,
           Set_New_Item => Dependent_Atom);

    end Atom_Add_Dependent;

    ----------------------------------
    procedure Update_Agenda_After_Exec
    ----------------------------------
      (Agenda            : in out Agenda_Type;
       Search_Operation  : in Search_Operation_Type;
       New_Cluster_Index : in Cluster_Index_Type) is
    begin
        -- remove op from agenda
        Remove_Agenda(Agenda, Search_Operation);

        if New_Cluster_Index >= 0 then

            --  update affected agenda
            case Search_Operation.Operation is
            when Op_Merge_Clust =>
                Update_Agenda_After_Exec_MC(Agenda, Search_Operation, New_Cluster_Index);

            when Op_Compose =>
                Update_Agenda_After_Exec_Abs(Agenda, Search_Operation, New_Cluster_Index);

            when others =>
                raise MLSE_Invalid_Search_Operation_Error with Search_Operation.Operation'Img;
            end case;
        end if;
    end Update_Agenda_After_Exec;

    -------------------------------------
    procedure Update_Agenda_After_Exec_MC
    -------------------------------------
      (Agenda            : in out Agenda_Type;
       Search_Operation  : in     Search_Operation_Type;
       New_Cluster_Index : in     Cluster_Index_Type) is

        Old_Cluster_Index : Cluster_Index_Type;
    begin

        -- update affected agenda

        Old_Cluster_Index := Search_Operation.Cluster_Index_2_Or_Dependent;
        if Old_Cluster_Index = New_Cluster_Index then
            Old_Cluster_Index := Search_Operation.Cluster_Index_1_Or_Governor;
        end if;

        -- TODO: Ciclo forse inefficiente, capire se e come migliorarlo
        while not Agenda.Cluster_Index_To_Search_Operation_Set_Map.Element(Old_Cluster_Index).Is_Empty loop
            declare
                Old_Search_Operation : constant Search_Operation_Type
                  := Search_Operation_Sets.Element
                    (Agenda.Cluster_Index_To_Search_Operation_Set_Map.Element(Old_Cluster_Index).First);
            begin

                Remove_Agenda(Agenda, Old_Search_Operation);

                case Old_Search_Operation.Operation is
                when Op_Merge_Clust =>
                    declare
                        Cluster_Index_1 : Cluster_Index_Type := Old_Search_Operation.Cluster_Index_1_Or_Governor;
                        Cluster_Index_2 : Cluster_Index_Type := Old_Search_Operation.Cluster_Index_2_Or_Dependent;
                    begin
                        if Cluster_Index_1 = Old_Cluster_Index then
                            Cluster_Index_1 := New_Cluster_Index;
                        end if;

                        if Cluster_Index_2 = Old_Cluster_Index then
                            Cluster_Index_2 := New_Cluster_Index;
                        end if;

                        if Cluster_Index_1 /= Cluster_Index_2 then
                            declare
                                New_Search_Operation : Search_Operation_Type;
                            begin
                                New_Search_Operation.Operation := Op_Merge_Clust;

                                if Cluster_Index_1 < Cluster_Index_2 then
                                    New_Search_Operation.Cluster_Index_1_Or_Governor  := Cluster_Index_1;
                                    New_Search_Operation.Cluster_Index_2_Or_Dependent := Cluster_Index_2;
                                else
                                    New_Search_Operation.Cluster_Index_1_Or_Governor  := Cluster_Index_2;
                                    New_Search_Operation.Cluster_Index_2_Or_Dependent := Cluster_Index_1;
                                end if;

                                Generate_Search_Operation_String(New_Search_Operation);

                                Insert(Agenda.Agenda_To_Score_Set, New_Search_Operation, True);
                            end;
                        end if;
                    end;

                when Op_Compose =>
                    declare
                        Governor_Cluster_Index  : Cluster_Index_Type := Old_Search_Operation.Cluster_Index_1_Or_Governor;
                        Dependent_Cluster_Index : Cluster_Index_Type := Old_Search_Operation.Cluster_Index_2_Or_Dependent;
                    begin
                        if Governor_Cluster_Index = Old_Cluster_Index then
                            Governor_Cluster_Index := New_Cluster_Index;
                        end if;

                        if Dependent_Cluster_Index = Old_Cluster_Index then
                            Dependent_Cluster_Index := New_Cluster_Index;
                        end if;

                        declare
                            New_Search_Operation : Search_Operation_Type;
                        begin
                            New_Search_Operation.Operation := Op_Compose;

                            New_Search_Operation.Cluster_Index_1_Or_Governor  := Governor_Cluster_Index;
                            New_Search_Operation.Cluster_Index_2_Or_Dependent := Dependent_Cluster_Index;

                            Generate_Search_Operation_String(New_Search_Operation);

                            Insert(Agenda.Agenda_To_Score_Set, New_Search_Operation, True);
                        end;
                    end;

                when others =>
                    raise MLSE_Invalid_Search_Operation_Error with Old_Search_Operation.Operation'Img;
                end case;
            end;
        end loop;

        Agenda.Cluster_Index_To_Search_Operation_Set_Map.Delete(Old_Cluster_Index);

        --  add new agenda enabled by merge
        --  -> already did new-new, old-old at createAgenda; so only did new/old

        for Atom_ID_1 of Cluster_Index_To_Atom_ID_Set_Map.Element(New_Cluster_Index) loop
            declare
                Part_1 : constant Part_Access_Type := Atom_ID_To_Part_Map.Element(Atom_ID_1);
            begin
                for Atom_ID_2 of Cluster_Index_To_Atom_ID_Set_Map.Element(Old_Cluster_Index) loop
                    declare
                        Part_2 : constant Part_Access_Type := Atom_ID_To_Part_Map.Element(Atom_ID_2);
                    begin
                        Add_Agenda_After_Merge_Clust(Agenda, Part_1.all, Part_2.all);
                    end;
                end loop;
            end;
        end loop;

    end Update_Agenda_After_Exec_MC;

    --------------------------------------
    procedure Update_Agenda_After_Exec_Abs
    --------------------------------------
      (Agenda            : in out Agenda_Type;
       Search_Operation  : in     Search_Operation_Type;
       New_Cluster_Index : in     Cluster_Index_Type) is

        Governor_Cluster_Index  : Cluster_Index_Type renames Search_Operation.Cluster_Index_1_Or_Governor;
        Dependent_Cluster_Index : Cluster_Index_Type renames Search_Operation.Cluster_Index_2_Or_Dependent;

    begin

        --  handle affected agenda: any op involving par/chd should be re-evaluated

        Delete_From_Set
          (Map          => Agenda.Cluster_Index_To_Search_Operation_Set_Map,
           Map_Position => Agenda.Cluster_Index_To_Search_Operation_Set_Map.Find(Governor_Cluster_Index),
           Set_Item     => Search_Operation);

        Delete_From_Set
          (Map          => Agenda.Cluster_Index_To_Search_Operation_Set_Map,
           Map_Position => Agenda.Cluster_Index_To_Search_Operation_Set_Map.Find(Dependent_Cluster_Index),
           Set_Item     => Search_Operation);

        -- TODO: Ciclo forse inefficiente, capire se e come migliorarlo
        while not Agenda.Cluster_Index_To_Search_Operation_Set_Map.Element(Governor_Cluster_Index).Is_Empty loop
            declare
                Old_Search_Operation : Search_Operation_Type
                  := Search_Operation_Sets.Element
                    (Agenda.Cluster_Index_To_Search_Operation_Set_Map.Element(Governor_Cluster_Index).First);
            begin

                -- TODO: Free repr

                Remove_Agenda(Agenda, Old_Search_Operation);
                Generate_Search_Operation_String(Old_Search_Operation); -- TODO: davvero necessario?

                Insert(Agenda.Agenda_To_Score_Set, Old_Search_Operation, False); -- TODO: Free repr
            end;
        end loop;

        -- TODO: Ciclo forse inefficiente, capire se e come migliorarlo
        while not Agenda.Cluster_Index_To_Search_Operation_Set_Map.Element(Dependent_Cluster_Index).Is_Empty loop
            declare
                Old_Search_Operation : Search_Operation_Type
                  := Search_Operation_Sets.Element
                    (Agenda.Cluster_Index_To_Search_Operation_Set_Map.Element(Dependent_Cluster_Index).First);
            begin

                -- TODO: Free repr

                Remove_Agenda(Agenda, Old_Search_Operation);
                Generate_Search_Operation_String(Old_Search_Operation); -- TODO: davvero necessario?

                Insert(Agenda.Agenda_To_Score_Set, Old_Search_Operation, False); -- TODO: Free repr
            end;
        end loop;

        --  add new agenda enabled by new clust
        Add_Agenda_For_New_Cluster(Agenda, New_Cluster_Index, Atom_ID_To_Part_Map);

    end Update_Agenda_After_Exec_Abs;

    -----------------------
    procedure Remove_Agenda
    -----------------------
      (Agenda            : in out Agenda_Type;
       Search_Operation  : in     Search_Operation_Type) is

        Cursor : Search_Operation_To_Score_Maps.Cursor;
    begin

        Cursor := Agenda.Active_Agenda_To_Score_Map.Find(Search_Operation);
        if Search_Operation_To_Score_Maps.Has_Element(Cursor) then
            declare
                Score : constant Score_Type := Search_Operation_To_Score_Maps.Element(Cursor);

                Score_And_Search_Operation_Link : constant Score_And_Search_Operation_Link_Type
                  := (Score => Score, Search_Operation => Search_Operation);
            begin
                Agenda.Score_Active_Agenda.Delete(Score_And_Search_Operation_Link);
                Agenda.Active_Agenda_To_Score_Map.Delete(Search_Operation);
            end;
        end if;

        Cursor := Agenda.Inactive_Agenda_To_Score_Map.Find(Search_Operation);
        if Search_Operation_To_Score_Maps.Has_Element(Cursor) then
            Agenda.Inactive_Agenda_To_Score_Map.Delete(Cursor);
        end if;

      if Agenda.Cluster_Index_To_Search_Operation_Set_Map.Contains(Search_Operation.Cluster_Index_1_Or_Governor) then
        Delete_From_Set
          (Map          => Agenda.Cluster_Index_To_Search_Operation_Set_Map,
           Map_Position => Agenda.Cluster_Index_To_Search_Operation_Set_Map.Find(Search_Operation.Cluster_Index_1_Or_Governor),
           Set_Item     => Search_Operation);
      end if;

      if Agenda.Cluster_Index_To_Search_Operation_Set_Map.Contains(Search_Operation.Cluster_Index_2_Or_Dependent) then
        Delete_From_Set
          (Map          => Agenda.Cluster_Index_To_Search_Operation_Set_Map,
           Map_Position => Agenda.Cluster_Index_To_Search_Operation_Set_Map.Find(Search_Operation.Cluster_Index_2_Or_Dependent),
           Set_Item     => Search_Operation);
      end if;
    end Remove_Agenda;

    -- change clust only; leave args intact -> used in ABSORB
    ------------------------
    procedure Change_Cluster
    ------------------------
      (Part               : in out Part_Type;
       New_Cluster_Access : in     Cluster_Access_Type;
       New_Rel_Type_index : in     Rel_Type_Index_Type) is

        New_Cluster_Index : Cluster_Index_Type renames New_Cluster_Access.Index;
        Old_Cluster_Index : constant Cluster_Index_Type  := Part.Cluster_Index;
        Old_Cluster       : constant Cluster_Access_Type := Part.Cluster_Access;
    begin

        -- unset old clust
        Part.Cluster_Index  := New_Cluster_Index;
        Part.Cluster_Access := New_Cluster_Access;

        Delete_From_Set
          (Map          => Cluster_Index_To_Atom_ID_Set_Map,
           Map_Position => Cluster_Index_To_Atom_ID_Set_Map.Find(Old_Cluster_Index),
           Set_Item     => Part.Root_Node.ID);

        -- On Part Unset Cluster
        Old_Cluster.Part_Count := Old_Cluster.Part_Count - 1;
        Decrement
          (Map      => Old_Cluster.Rel_Type_Index_To_Count_Map,
           Position => Old_Cluster.Rel_Type_Index_To_Count_Map.Find(Part.Rel_Type_Index));

        -- set new clust
        Part.Rel_Type_Index := New_Rel_Type_index;
        Link_Part_And_Cluster(Part, New_Cluster_Access);

        -- root: not done this in clust.setclust because that might be called when parPart not known yet
        if Part.Governor_Part = null then
            declare
                Cursor : constant Cluster_Index_Root_Count_Maps.Cursor
                  := Cluster_Index_Root_Count_Map.Find(New_Cluster_Index);
            begin
                if not Cluster_Index_Root_Count_Maps.Has_Element(Cursor) then
                    Cluster_Index_Root_Count_Map.Insert(New_Cluster_Index, 1);
                else
                    Increment(Cluster_Index_Root_Count_Map, Cursor);
                end if;

                Decrement
                  (Map      => Cluster_Index_Root_Count_Map,
                   Position => Cluster_Index_Root_Count_Map.Find(Old_Cluster_Index));
            end;
        else
            --  fix parent part arg

            declare
                Parent_Argument_Cluster_Index : constant Argument_Cluster_Index_Type
                  := Part.Governor_Part.Argument_Index_To_Argument_Cluster_Index_Map.Element(Part.Argument_Index_In_Governor_Part);

                Parent_Cluster                : Cluster_Access_Type renames Part.Governor_Part.Cluster_Access;

                Parent_Argument_Cluster       : constant Argument_Cluster_Access_Type
                  := Parent_Cluster.Argument_Cluster_Map.Element(Parent_Argument_Cluster_Index);
            begin

                Decrement
                  (Map      => Parent_Argument_Cluster.Dependent_Cluster_Index_To_Count_Map,
                   Position => Parent_Argument_Cluster.Dependent_Cluster_Index_To_Count_Map.Find(Old_Cluster_Index));

                declare
                    Cursor : constant Cluster_Index_To_Count_Maps.Cursor
                      := Parent_Argument_Cluster.Dependent_Cluster_Index_To_Count_Map.Find(New_Cluster_Index);
                begin
                    if not Cluster_Index_To_Count_Maps.Has_Element(Cursor) then
                        Parent_Argument_Cluster.Dependent_Cluster_Index_To_Count_Map.Insert(New_Cluster_Index, 1);
                    else
                        Increment(Parent_Argument_Cluster.Dependent_Cluster_Index_To_Count_Map, Cursor);
                    end if;
                end;

                -- parArgs

                declare
                    Cluster_And_Argument_Cluster_Link : constant Cluster_And_Argument_Cluster_Link_Type
                      := (Cluster_Index          => Part.Governor_Part.Cluster_Index,
                          Argument_Cluster_Index => Parent_Argument_Cluster_Index);

                    procedure Decrement_Link_Count(Cluster_Index : in Cluster_Index_Type; Cluster_And_Argument_Cluster_Link_Type_To_Count_Map : in out Cluster_And_Argument_Cluster_Link_Type_To_Count_Maps.Map) is
                        pragma Unreferenced (Cluster_Index);
                    begin
                        Decrement
                          (Map      => Cluster_And_Argument_Cluster_Link_Type_To_Count_Map,
                           Position => Cluster_And_Argument_Cluster_Link_Type_To_Count_Map.Find(Cluster_And_Argument_Cluster_Link));
                    end Decrement_Link_Count;

                    procedure Update_Link_Count(Cluster_Index : in Cluster_Index_Type; Cluster_And_Argument_Cluster_Link_Type_To_Count_Map : in out Cluster_And_Argument_Cluster_Link_Type_To_Count_Maps.Map) is
                        pragma Unreferenced (Cluster_Index);
                        Cursor : constant Cluster_And_Argument_Cluster_Link_Type_To_Count_Maps.Cursor
                          := Cluster_And_Argument_Cluster_Link_Type_To_Count_Map.Find(Cluster_And_Argument_Cluster_Link);
                    begin
                        if not Cluster_And_Argument_Cluster_Link_Type_To_Count_Maps.Has_Element(Cursor) then
                            Cluster_And_Argument_Cluster_Link_Type_To_Count_Map.Insert(Cluster_And_Argument_Cluster_Link, 1);
                        else
                            Increment(Cluster_And_Argument_Cluster_Link_Type_To_Count_Map, Cursor);
                        end if;
                    end Update_Link_Count;

                    Cursor   : Cluster_Index_To_Cluster_And_Argument_Cluster_Link_Type_To_Count_Map_Maps.Cursor;
                begin

                    -- Old_Cluster_Index
                    Cursor := Cluster_Index_To_Cluster_And_Argument_Cluster_Link_Type_To_Count_Map_Map.Find(Old_Cluster_Index);
                    Cluster_Index_To_Cluster_And_Argument_Cluster_Link_Type_To_Count_Map_Map.Update_Element
                      (Cursor, Decrement_Link_Count'Access);

                    -- New_Cluster_Index
                    Cursor := Cluster_Index_To_Cluster_And_Argument_Cluster_Link_Type_To_Count_Map_Map.Find(New_Cluster_Index);
                    if not Cluster_Index_To_Cluster_And_Argument_Cluster_Link_Type_To_Count_Map_Maps.Has_Element(Cursor) then
                        Insert
                          (Map      => Cluster_Index_To_Cluster_And_Argument_Cluster_Link_Type_To_Count_Map_Map,
                           Key      => New_Cluster_Index,
                           Position => Cursor);
                    end if;

                    Cluster_Index_To_Cluster_And_Argument_Cluster_Link_Type_To_Count_Map_Map.Update_Element
                      (Cursor, Update_Link_Count'Access);
                end;

                -- pairclust->part

                declare
                    Old_Cluster_Link : constant Cluster_Link_Type
                      := (Governor_Cluster_Index  => Part.Governor_Part.Cluster_Index,
                          Dependent_Cluster_Index => Old_Cluster_Index);

                    New_Cluster_Link : constant Cluster_Link_Type
                      := (Governor_Cluster_Index  => Part.Governor_Part.Cluster_Index,
                          Dependent_Cluster_Index => New_Cluster_Index);

                    Atom_Link        : constant Atom_Link_Type
                      := (Governor_Atom_ID  => Part.Governor_Part.Root_Node.ID,
                          Dependent_Atom_ID => Part.Root_Node.ID);

                    Old_Atom_Link_Set_Size : Integer := -1;

                    procedure Delete_Old_Atom_Link(Cluster_Link : in Cluster_Link_Type; Atom_Link_Set : in out Atom_Link_Sets.Set) is begin
                        pragma Unreferenced (Cluster_Link);
                        Atom_Link_Set.Delete(Atom_Link);
                        Old_Atom_Link_Set_Size := Natural(Atom_Link_Set.Length);
                    end Delete_Old_Atom_Link;



                    procedure Delete_Old_Cluster_Link(Cluster_Index : in Cluster_Index_Type; Cluster_Link_Set : in out Cluster_Link_Sets.Set) is
                        pragma Unreferenced (Cluster_Index);
                        Position : Cluster_Link_Sets.Cursor
                          := Cluster_Link_Set.Find(Old_Cluster_Link);
                    begin
                        if Cluster_Link_Sets.Has_Element(Position) then
                            Cluster_Link_Set.Delete(Position);
                        end if;
                    end Delete_Old_Cluster_Link;

                    Cursor   : Cluster_Link_To_Atom_Link_Set_Maps.Cursor;
                begin

                    -- Old_Cluster_Link

                    Cursor := Cluster_Link_To_Atom_Link_Set_Map.Find(Old_Cluster_Link);

                    Cluster_Link_To_Atom_Link_Set_Map.Update_Element(Cursor, Delete_Old_Atom_Link'Access);

                    if Old_Atom_Link_Set_Size = 0 then
                        Cluster_Index_To_Cluster_Link_Set_Map.Update_Element
                          (Cluster_Index_To_Cluster_Link_Set_Map.Find(Old_Cluster_Index), Delete_Old_Cluster_Link'Access);

                        Cluster_Index_To_Cluster_Link_Set_Map.Update_Element
                          (Cluster_Index_To_Cluster_Link_Set_Map.Find(Part.Governor_Part.Cluster_Index), Delete_Old_Cluster_Link'Access);
                    end if;

                    -- New_Cluster_Link

                    Cursor := Cluster_Link_To_Atom_Link_Set_Map.Find(New_Cluster_Link);

                    if not Cluster_Link_To_Atom_Link_Set_Maps.Has_Element(Cursor) then
                        Insert
                          (Map      => Cluster_Link_To_Atom_Link_Set_Map,
                           Key      => New_Cluster_Link,
                           Position => Cursor);
                    end if;

                    Add_To_Set(Cluster_Link_To_Atom_Link_Set_Map, Cursor, Atom_Link);

                    declare
                        CCL_Cursor   : Cluster_Index_To_Cluster_Link_Set_Maps.Cursor;
                    begin

                        CCL_Cursor := Cluster_Index_To_Cluster_Link_Set_Map.Find(Part.Governor_Part.Cluster_Index);
                        Add_To_Set(Cluster_Index_To_Cluster_Link_Set_Map, CCL_Cursor, New_Cluster_Link);

                        CCL_Cursor := Cluster_Index_To_Cluster_Link_Set_Map.Find(New_Cluster_Index);
                        if not Cluster_Index_To_Cluster_Link_Set_Maps.Has_Element(CCL_Cursor) then
                            Insert
                              (Map      => Cluster_Index_To_Cluster_Link_Set_Map,
                               Key      => New_Cluster_Index,
                               Position => CCL_Cursor);
                        end if;

                        Add_To_Set(Cluster_Index_To_Cluster_Link_Set_Map, CCL_Cursor, New_Cluster_Link);
                    end;
                end;
            end;
        end if;

    end Change_Cluster;

    ------------------------
    procedure Change_Cluster
    ------------------------
      (Part                                                     : in out Part_Type;
       New_Cluster_Access                                       : in     Cluster_Access_Type;
       Argument_Cluster_Index_To_New_Argument_Cluster_Index_Map : in     Argument_Cluster_Index_To_Argument_Cluster_Index_Maps.Map) is

        Old_Cluster       : constant Cluster_Access_Type := Part.Cluster_Access;

        Part_Index_To_New_Argument_Cluster_Index_Map : Part_Index_To_Argument_Cluster_Index_Maps.Map;

        Argument_Cursor : Part_Maps.Cursor;
    begin

        -- handle clust change: no change in relTypeIdx_
        Change_Cluster(Part, New_Cluster_Access, Part.Rel_Type_Index);

        -- rermove old acs

        Argument_Cursor := Part.Argument_Part_Map.First;
        while Part_Maps.Has_Element(Argument_Cursor) loop
            declare
                Argument_Index : constant Part_Index_Type  := Part_Maps.Key(Argument_Cursor);
                Argument_Part  : constant Part_Access_Type := Part_Maps.Element(Argument_Cursor);

                Old_Argument_Cluster_Index : constant Argument_Cluster_Index_Type
                  := Part.Argument_Index_To_Argument_Cluster_Index_Map.Element(Argument_Index);

                Part_Index_Set_Is_Empty : Boolean := False;
                procedure Delete_Argument_Index(Argument_Cluster_Index : in Argument_Cluster_Index_Type; Part_Index_Set : in out Part_Index_Sets.Set) is begin
                    pragma Unreferenced (Argument_Cluster_Index);
                    Part_Index_Set.Delete(Argument_Index);
                    Part_Index_Set_Is_Empty := Part_Index_Set.Is_Empty;
                end Delete_Argument_Index;

                Cursor : Argument_Cluster_Index_To_Part_Indexes_Maps.Cursor;
            begin

                Part.Argument_Index_To_Argument_Cluster_Index_Map.Delete(Argument_Index);

                Cursor:= Part.Argument_Cluster_Index_To_Dependent_Indexes_Map.Find(Old_Argument_Cluster_Index);
                Part.Argument_Cluster_Index_To_Dependent_Indexes_Map.Update_Element
                  (Cursor, Delete_Argument_Index'Access);
                if Part_Index_Set_Is_Empty then
                    Part.Argument_Cluster_Index_To_Dependent_Indexes_Map.Delete(Cursor);
                end if;

                Part_Index_To_New_Argument_Cluster_Index_Map.Insert
                  (Argument_Index, Argument_Cluster_Index_To_New_Argument_Cluster_Index_Map.Element(Old_Argument_Cluster_Index));

                On_Part_Unset_Argument(Old_Cluster.all, Part, Argument_Part.all, Old_Argument_Cluster_Index);
            end;
            Part_Maps.Next(Argument_Cursor);
        end loop;

        --  set new acs
        Argument_Cursor := Part.Argument_Part_Map.First;
        while Part_Maps.Has_Element(Argument_Cursor) loop
            declare
                Argument_Index : constant Part_Index_Type  := Part_Maps.Key(Argument_Cursor);

                Argument_Cluster_Index : constant Argument_Cluster_Index_Type
                  := Part_Index_To_New_Argument_Cluster_Index_Map.Element(Argument_Index);
            begin
                Set_Argument_Cluster(Part, Argument_Index, Argument_Cluster_Index);
            end;
            Part_Maps.Next(Argument_Cursor);
        end loop;

    end Change_Cluster;

    ----------------------
    procedure Set_Rel_Type
    ----------------------
      (Part               : in out Part_Type;
       New_Rel_Type_Index : in     Rel_Type_Index_Type) is

        Cluster : Cluster_Access_Type renames Part.Cluster_Access;
        Cursor  : Rel_Type_Index_To_Count_Maps.Cursor;
    begin

        Part.Rel_Type_Index := New_Rel_Type_Index;

        -- On Part Set Rel Type Index
        Cursor := Cluster.Rel_Type_Index_To_Count_Map.Find(New_Rel_Type_Index);
        if not Rel_Type_Index_To_Count_Maps.Has_Element(Cursor) then
            Cluster.Rel_Type_Index_To_Count_Map.Insert(New_Rel_Type_Index, 1);
        else
            Increment(Cluster.Rel_Type_Index_To_Count_Map, Cursor);
        end if;
    end Set_Rel_Type;

    ------------------------
    procedure Unset_Rel_Type
    ------------------------
      (Part : in out Part_Type) is

        Old_Rel_Type_Index : constant Rel_Type_Index_Type := Part.Rel_Type_Index;
        Cluster            : Cluster_Access_Type renames Part.Cluster_Access;
    begin

        -- TODO: perche' viene usato Old_Rel_Type_Index se poi tanto Part.Rel_Type_Index non viene messo a -1?

        -- On Part Unset Rel Type Index
        Decrement
          (Map      => Cluster.Rel_Type_Index_To_Count_Map,
           Position => Cluster.Rel_Type_Index_To_Count_Map.Find(Old_Rel_Type_Index));
        -- TODO: perche' non lo elimino se dopo aver decrementato il valore rimane zero?

    end Unset_Rel_Type;

    ----------------------
    procedure Destroy_Part
    ----------------------
      (Part_Access : in Part_Access_Type) is

        Atom_ID_Set_Size : Integer := -1;

        procedure Delete_Root_Node(Cluster_Index : in Cluster_Index_Type; Atom_ID_Set : in out Atom_ID_Sets.Set) is begin
            pragma Unreferenced (Cluster_Index);
            Atom_ID_Set.Delete(Part_Access.Root_Node.ID);
            Atom_ID_Set_Size := Natural(Atom_ID_Set.Length);
        end Delete_Root_Node;

        Cursor : Cluster_Index_To_Atom_ID_Set_Maps.Cursor
          := Cluster_Index_To_Atom_ID_Set_Map.Find(Part_Access.Cluster_Index);
    begin

        Cluster_Index_To_Atom_ID_Set_Map.Update_Element(Cursor, Delete_Root_Node'Access);

        if Atom_ID_Set_Size = 0 then
            Cluster_Index_To_Atom_ID_Set_Map.Delete(Cursor);
        end if;

        Atom_ID_To_Part_Map.Delete(Part_Access.Root_Node.ID);

        -- TODO: Uncecked Deallocation?
    end Destroy_Part;

    ----------------------------
    procedure Score_MC_For_Align
    ----------------------------
      (Cluster_1                                                : in     Cluster_Type;
       Cluster_2                                                : in     Cluster_Type;
       Argument_Cluster_Index_2_To_Argument_Cluster_Index_1_Map : in out Argument_Cluster_Index_To_Argument_Cluster_Index_Maps.Map) is

        Score : Score_Type; pragma Unreferenced (Score);
    begin
        Score := Score_MC_For_Align(Cluster_1, Cluster_2, Argument_Cluster_Index_2_To_Argument_Cluster_Index_1_Map);
    end Score_MC_For_Align;

    -- greedy align
    ----------------------------
    function Score_MC_For_Align
    ----------------------------
      (Cluster_1                                                : in     Cluster_Type;
       Cluster_2                                                : in     Cluster_Type;
       Argument_Cluster_Index_2_To_Argument_Cluster_Index_1_Map : in out Argument_Cluster_Index_To_Argument_Cluster_Index_Maps.Map) return Score_Type is

        -- greedy
        Final_Score : Score_Type := 0.0;

        Delta_No_Merge_Argument_Cluster : Score_Type := 0.0;

        Argument_Cluster_Cursor_2 : Argument_Cluster_Maps.Cursor;

        Denominator_1_2 : constant Score_Type := XlogX(Cluster_1.Part_Count + Cluster_2.Part_Count);
    begin

        -- subtract new (merged cl, isolated argcl), add old (isolated cl)
        declare
            -- denominator: type/clust
            Denominator_1 : constant Score_Type := XlogX(Cluster_1.Part_Count);
            Denominator_2 : constant Score_Type := XlogX(Cluster_2.Part_Count);

            Argument_Cluster_Cursor : Argument_Cluster_Maps.Cursor;
        begin
            Argument_Cluster_Cursor := Cluster_1.Argument_Cluster_Map.First;
            while Argument_Cluster_Maps.Has_Element(Argument_Cluster_Cursor) loop
                declare
                    Root_Node_Count_1 : constant Natural := Natural(Argument_Cluster_Maps.Element(Argument_Cluster_Cursor).Root_Node_ID_Set.Length);
                begin
                    Delta_No_Merge_Argument_Cluster := Delta_No_Merge_Argument_Cluster + XlogX(Cluster_1.Part_Count + Cluster_2.Part_Count - Root_Node_Count_1) - Denominator_1_2 - XlogX(Cluster_1.Part_Count - Root_Node_Count_1) + Denominator_1;
                end;
                Argument_Cluster_Maps.Next(Argument_Cluster_Cursor);
            end loop;

            Argument_Cluster_Cursor := Cluster_2.Argument_Cluster_Map.First;
            while Argument_Cluster_Maps.Has_Element(Argument_Cluster_Cursor) loop
                declare
                    Root_Node_Count_2 : constant Natural := Natural(Argument_Cluster_Maps.Element(Argument_Cluster_Cursor).Root_Node_ID_Set.Length);
                begin
                    Delta_No_Merge_Argument_Cluster := Delta_No_Merge_Argument_Cluster + XlogX(Cluster_1.Part_Count + Cluster_2.Part_Count - Root_Node_Count_2) - Denominator_1_2 - XlogX(Cluster_2.Part_Count - Root_Node_Count_2) + Denominator_2;
                end;
                Argument_Cluster_Maps.Next(Argument_Cluster_Cursor);
            end loop;
        end;

        -- independently find 1-1 map for each arg2
        Argument_Cluster_Cursor_2 := Cluster_2.Argument_Cluster_Map.First;
        while Argument_Cluster_Maps.Has_Element(Argument_Cluster_Cursor_2) loop

            -- old arg1: xlogx(tc1-pc1)+SUM_num(xlogx(cnt_num))-denom1 + <SUM_i(xlogx(pci1))-denomTTL1>_(argtype / chdclust)
            -- old arg2: xlogx(tc2-pc2)+SUM_num(xlogx(cnt_num))-denom2 + <SUM_i(xlogx(pci2))-denomTTL2>_(argtype / chdclust)
            -- -- actually no need to consider old
            --
            -- new arg1 (no merge): xlogx(tc1+tc2-pc1)+SUM_num(xlogx(cnt_num))-denom + <SUM_i(xlogx(pci1))-denomTTL1>_(argtype / chdclust)
            -- new arg2 (no merge): xlogx(tc1+tc2-pc2)+SUM_num(xlogx(cnt_num))-denom + <SUM_i(xlogx(pci2))-denomTTL2>_(argtype / chdclust)
            -- new arg1,arg2 (merge): xlogx(tc1+tc2-pc1-pc2)+SUM_num(xlogx(new_cnt_num))-denom + <SUM_i(xlogx(pci_1/2))-denomTTL_1/2>_(argtype / chdclust)
            -- 	- one fewer absentee param; also in cnt/argtype/chdclust

            -- NO-MAP: reference state (subsequent scores are delta relative to this one)
            -- new prb for arg2
            -- ignore absentee changes in arg1s, subtracted in the following loop if it's changed due to merge
            -- common component in new arg1/2 - new arg1 - new arg2

            declare
                Argument_Cluster_Index_2 : constant Argument_Cluster_Index_Type  := Argument_Cluster_Maps.Key(Argument_Cluster_Cursor_2);
                Argument_Cluster_2       : constant Argument_Cluster_Access_Type := Argument_Cluster_Maps.Element(Argument_Cluster_Cursor_2);

                Root_Node_Count_2        : constant Natural := Natural(Argument_Cluster_2.Root_Node_ID_Set.Length);

                -- reference base to compare against
                New_Base_Score           : constant Score_Type
                  := (XlogX(Cluster_1.Part_Count + Cluster_2.Part_Count - Root_Node_Count_2) - Denominator_1_2) - (2.0 * XlogX(Argument_Cluster_2.Dependents_Count));

                Max_Score                : Score_Type
                  := New_Base_Score;

                Argument_Cluster_Cursor_1 : Argument_Cluster_Maps.Cursor
                  := Cluster_1.Argument_Cluster_Map.First;
            begin
                while Argument_Cluster_Maps.Has_Element(Argument_Cluster_Cursor_1) loop
                    declare
                        Argument_Cluster_Index_1 : constant Argument_Cluster_Index_Type  := Argument_Cluster_Maps.Key(Argument_Cluster_Cursor_1);
                        Argument_Cluster_1       : constant Argument_Cluster_Access_Type := Argument_Cluster_Maps.Element(Argument_Cluster_Cursor_1);

                        Root_Node_Count_1        : constant Natural := Natural(Argument_Cluster_1.Root_Node_ID_Set.Length);

                        Cur_Score                : Score_Type := 0.0;

                        Num_Arg_To_New_Count_Map : Num_Arg_To_Count_Maps.Map;
                    begin
                        -- TO-DO: why empty
                        if Argument_Cluster_1.Root_Node_ID_Set.Is_Empty then
                            -- Continue
                            --raise MLSE_Error with "TO-DO: why empty? (original code: continue)";
                            goto Continue_As_Original_USP;
                        end if;

                        if Argument_Cluster_2.Root_Node_ID_Set.Is_Empty then
                            Argument_Cluster_Index_2_To_Argument_Cluster_Index_1_Map.Insert(Argument_Cluster_Index_2, Argument_Cluster_Index_1);
                            Max_Score := 0.0;
                            exit;
                        end if;

                        -- HP
                        Cur_Score := Cur_Score - Configurations.Prior_Merge;

                        -- delta arg2 + delta of arg1: compared to no-map
                        Cur_Score := Cur_Score + XlogX(Cluster_1.Part_Count + Cluster_2.Part_Count - Root_Node_Count_1 - Root_Node_Count_2) - XlogX(Cluster_1.Part_Count + Cluster_2.Part_Count - Root_Node_Count_1) + (2.0 * XlogX(Argument_Cluster_1.Dependents_Count)) - (2.0 * XlogX(Argument_Cluster_1.Dependents_Count + Argument_Cluster_2.Dependents_Count));

                        -- loop thru all parts: determine chg in cnt
                        declare
                            Num_Arg_To_Count_Cursor : Num_Arg_To_Count_Maps.Cursor;
                        begin
                            Num_Arg_To_Count_Cursor := Argument_Cluster_1.Num_Arg_Count_Map.First;
                            while Num_Arg_To_Count_Maps.Has_Element(Num_Arg_To_Count_Cursor) loop
                                declare
                                    Num_Arg       : constant Num_Arg_Type := Num_Arg_To_Count_Maps.Key(Num_Arg_To_Count_Cursor);
                                    Num_Arg_Count : constant Natural      := Num_Arg_To_Count_Maps.Element(Num_Arg_To_Count_Cursor);

                                    procedure Add_Num_Arg_Count(Num_Arg : in Num_Arg_Type; Count : in out Integer) is begin
                                        pragma Unreferenced (Num_Arg);
                                        Count := Count + Num_Arg_Count;
                                    end Add_Num_Arg_Count;

                                    New_Cursor    : constant Num_Arg_To_Count_Maps.Cursor
                                      := Num_Arg_To_New_Count_Map.Find(Num_Arg);
                                begin
                                    if not Num_Arg_To_Count_Maps.Has_Element(New_Cursor) then
                                        Num_Arg_To_New_Count_Map.Insert(Num_Arg, Num_Arg_Count);
                                    else
                                        Num_Arg_To_New_Count_Map.Update_Element(New_Cursor, Add_Num_Arg_Count'Access);
                                    end if;
                                end;
                                Num_Arg_To_Count_Maps.Next(Num_Arg_To_Count_Cursor);
                            end loop;

                            Num_Arg_To_Count_Cursor := Argument_Cluster_2.Num_Arg_Count_Map.First;
                            while Num_Arg_To_Count_Maps.Has_Element(Num_Arg_To_Count_Cursor) loop
                                declare
                                    Num_Arg       : constant Num_Arg_Type := Num_Arg_To_Count_Maps.Key(Num_Arg_To_Count_Cursor);
                                    Num_Arg_Count : constant Natural      := Num_Arg_To_Count_Maps.Element(Num_Arg_To_Count_Cursor);

                                    procedure Add_Num_Arg_Count(Num_Arg : in Num_Arg_Type; Count : in out Integer) is begin
                                        pragma Unreferenced (Num_Arg);
                                        Count := Count + Num_Arg_Count;
                                    end Add_Num_Arg_Count;

                                    New_Cursor    : constant Num_Arg_To_Count_Maps.Cursor
                                      := Num_Arg_To_New_Count_Map.Find(Num_Arg);
                                begin
                                    if not Num_Arg_To_Count_Maps.Has_Element(New_Cursor) then
                                        Num_Arg_To_New_Count_Map.Insert(Num_Arg, Num_Arg_Count);
                                    else
                                        Num_Arg_To_New_Count_Map.Update_Element(New_Cursor, Add_Num_Arg_Count'Access);
                                    end if;
                                end;
                                Num_Arg_To_Count_Maps.Next(Num_Arg_To_Count_Cursor);
                            end loop;
                        end;

                        declare
                            Num_Arg_To_Count_Cursor : Num_Arg_To_Count_Maps.Cursor;
                        begin
                            Num_Arg_To_Count_Cursor := Num_Arg_To_New_Count_Map.First;
                            while Num_Arg_To_Count_Maps.Has_Element(Num_Arg_To_Count_Cursor) loop
                                declare
                                    Num_Arg_Count : constant Natural := Num_Arg_To_Count_Maps.Element(Num_Arg_To_Count_Cursor);
                                begin
                                    if Num_Arg_Count > 0 then
                                        Cur_Score := Cur_Score + XlogX(Num_Arg_Count) - Configurations.Prior_Num_Param;
                                    end if;
                                end;
                                Num_Arg_To_Count_Maps.Next(Num_Arg_To_Count_Cursor);
                            end loop;

                            Num_Arg_To_Count_Cursor := Argument_Cluster_1.Num_Arg_Count_Map.First;
                            while Num_Arg_To_Count_Maps.Has_Element(Num_Arg_To_Count_Cursor) loop
                                declare
                                    Num_Arg_Count : constant Natural := Num_Arg_To_Count_Maps.Element(Num_Arg_To_Count_Cursor);
                                begin
                                    if Num_Arg_Count > 0 then
                                        Cur_Score := Cur_Score - XlogX(Num_Arg_Count) + Configurations.Prior_Num_Param;
                                    end if;
                                end;
                                Num_Arg_To_Count_Maps.Next(Num_Arg_To_Count_Cursor);
                            end loop;

                            Num_Arg_To_Count_Cursor := Argument_Cluster_2.Num_Arg_Count_Map.First;
                            while Num_Arg_To_Count_Maps.Has_Element(Num_Arg_To_Count_Cursor) loop
                                declare
                                    Num_Arg_Count : constant Natural := Num_Arg_To_Count_Maps.Element(Num_Arg_To_Count_Cursor);
                                begin
                                    if Num_Arg_Count > 0 then
                                        Cur_Score := Cur_Score - XlogX(Num_Arg_Count) + Configurations.Prior_Num_Param;
                                    end if;
                                end;
                                Num_Arg_To_Count_Maps.Next(Num_Arg_To_Count_Cursor);
                            end loop;
                        end;

                        -- Type

                        declare
                            Deprel_Index_To_Count_Cursor_1 : Deprel_Index_To_Count_Maps.Cursor;
                            Deprel_Index_To_Count_Cursor_2 : Deprel_Index_To_Count_Maps.Cursor;
                        begin

                            if Argument_Cluster_1.Deprel_Index_To_Count_Map.Length <= Argument_Cluster_2.Deprel_Index_To_Count_Map.Length then

                                Deprel_Index_To_Count_Cursor_1 := Argument_Cluster_1.Deprel_Index_To_Count_Map.First;
                                while Deprel_Index_To_Count_Maps.Has_Element(Deprel_Index_To_Count_Cursor_1) loop
                                    declare
                                        Deprel_Index_1 : constant Deprel_Index_Type := Deprel_Index_To_Count_Maps.Key(Deprel_Index_To_Count_Cursor_1);
                                    begin

                                        Deprel_Index_To_Count_Cursor_2 := Argument_Cluster_2.Deprel_Index_To_Count_Map.Find(Deprel_Index_1);

                                        if Deprel_Index_To_Count_Maps.Has_Element(Deprel_Index_To_Count_Cursor_2) then
                                            declare
                                                Count_1 : constant Natural := Deprel_Index_To_Count_Maps.Element(Deprel_Index_To_Count_Cursor_1);
                                                Count_2 : constant Natural := Deprel_Index_To_Count_Maps.Element(Deprel_Index_To_Count_Cursor_2);
                                            begin
                                                Cur_Score := Cur_Score + XlogX(Count_1 + Count_2) - XlogX(Count_1) - XlogX(Count_2) + Configurations.Prior_Num_Param;
                                            end;
                                        end if;
                                    end;
                                    Deprel_Index_To_Count_Maps.Next(Deprel_Index_To_Count_Cursor_1);
                                end loop;

                            else

                                Deprel_Index_To_Count_Cursor_2 := Argument_Cluster_2.Deprel_Index_To_Count_Map.First;
                                while Deprel_Index_To_Count_Maps.Has_Element(Deprel_Index_To_Count_Cursor_2) loop
                                    declare
                                        Deprel_Index_2 : constant Deprel_Index_Type := Deprel_Index_To_Count_Maps.Key(Deprel_Index_To_Count_Cursor_2);
                                    begin

                                        Deprel_Index_To_Count_Cursor_1 := Argument_Cluster_1.Deprel_Index_To_Count_Map.Find(Deprel_Index_2);

                                        if Deprel_Index_To_Count_Maps.Has_Element(Deprel_Index_To_Count_Cursor_1) then
                                            declare
                                                Count_1 : constant Natural := Deprel_Index_To_Count_Maps.Element(Deprel_Index_To_Count_Cursor_1);
                                                Count_2 : constant Natural := Deprel_Index_To_Count_Maps.Element(Deprel_Index_To_Count_Cursor_2);
                                            begin
                                                Cur_Score := Cur_Score + XlogX(Count_1 + Count_2) - XlogX(Count_1) - XlogX(Count_2) + Configurations.Prior_Num_Param;
                                            end;
                                        end if;
                                    end;
                                    Deprel_Index_To_Count_Maps.Next(Deprel_Index_To_Count_Cursor_2);
                                end loop;
                            end if;
                        end;

                        -- Clust

                        declare
                            Dependent_Cluster_Index_To_Count_Cursor_1 : Cluster_Index_To_Count_Maps.Cursor;
                            Dependent_Cluster_Index_To_Count_Cursor_2 : Cluster_Index_To_Count_Maps.Cursor;
                        begin

                            if Argument_Cluster_1.Dependent_Cluster_Index_To_Count_Map.Length <= Argument_Cluster_2.Dependent_Cluster_Index_To_Count_Map.Length then

                                Dependent_Cluster_Index_To_Count_Cursor_1 := Argument_Cluster_1.Dependent_Cluster_Index_To_Count_Map.First;
                                while Cluster_Index_To_Count_Maps.Has_Element(Dependent_Cluster_Index_To_Count_Cursor_1) loop
                                    declare
                                        Dependent_Cluster_Index_1 : constant Cluster_Index_Type := Cluster_Index_To_Count_Maps.Key(Dependent_Cluster_Index_To_Count_Cursor_1);
                                    begin

                                        Dependent_Cluster_Index_To_Count_Cursor_2 := Argument_Cluster_2.Dependent_Cluster_Index_To_Count_Map.Find(Dependent_Cluster_Index_1);

                                        if Cluster_Index_To_Count_Maps.Has_Element(Dependent_Cluster_Index_To_Count_Cursor_2) then
                                            declare
                                                Count_1 : constant Natural := Cluster_Index_To_Count_Maps.Element(Dependent_Cluster_Index_To_Count_Cursor_1);
                                                Count_2 : constant Natural := Cluster_Index_To_Count_Maps.Element(Dependent_Cluster_Index_To_Count_Cursor_2);
                                            begin
                                                Cur_Score := Cur_Score + XlogX(Count_1 + Count_2) - XlogX(Count_1) - XlogX(Count_2) + Configurations.Prior_Num_Param;
                                            end;
                                        end if;
                                    end;
                                    Cluster_Index_To_Count_Maps.Next(Dependent_Cluster_Index_To_Count_Cursor_1);
                                end loop;

                            else

                                Dependent_Cluster_Index_To_Count_Cursor_2 := Argument_Cluster_2.Dependent_Cluster_Index_To_Count_Map.First;
                                while Cluster_Index_To_Count_Maps.Has_Element(Dependent_Cluster_Index_To_Count_Cursor_2) loop
                                    declare
                                        Dependent_Cluster_Index_2 : constant Cluster_Index_Type := Cluster_Index_To_Count_Maps.Key(Dependent_Cluster_Index_To_Count_Cursor_2);
                                    begin

                                        Dependent_Cluster_Index_To_Count_Cursor_1 := Argument_Cluster_1.Dependent_Cluster_Index_To_Count_Map.Find(Dependent_Cluster_Index_2);

                                        if Cluster_Index_To_Count_Maps.Has_Element(Dependent_Cluster_Index_To_Count_Cursor_1) then
                                            declare
                                                Count_1 : constant Natural := Cluster_Index_To_Count_Maps.Element(Dependent_Cluster_Index_To_Count_Cursor_1);
                                                Count_2 : constant Natural := Cluster_Index_To_Count_Maps.Element(Dependent_Cluster_Index_To_Count_Cursor_2);
                                            begin
                                                Cur_Score := Cur_Score + XlogX(Count_1 + Count_2) - XlogX(Count_1) - XlogX(Count_2) + Configurations.Prior_Num_Param;
                                            end;
                                        end if;
                                    end;
                                    Cluster_Index_To_Count_Maps.Next(Dependent_Cluster_Index_To_Count_Cursor_2);
                                end loop;
                            end if;
                        end;

                        if Cur_Score > Max_Score then
                            Max_Score := Cur_Score;
                            declare
                                Position : constant Argument_Cluster_Index_To_Argument_Cluster_Index_Maps.Cursor
                                  := Argument_Cluster_Index_2_To_Argument_Cluster_Index_1_Map.Find(Argument_Cluster_Index_2);
                            begin
                                if Argument_Cluster_Index_To_Argument_Cluster_Index_Maps.Has_Element(Position) then
                                    Argument_Cluster_Index_2_To_Argument_Cluster_Index_1_Map.Replace_Element(Position, Argument_Cluster_Index_1);
                                else
                                    Argument_Cluster_Index_2_To_Argument_Cluster_Index_1_Map.Insert
                                      (Key      => Argument_Cluster_Index_2,
                                       New_Item => Argument_Cluster_Index_1);
                                end if;
                            end;
                        end if;
                    end;


               <<Continue_As_Original_USP>>
               Argument_Cluster_Maps.Next(Argument_Cluster_Cursor_1);
                end loop;

                -- assume independent merges; additive score increment
                Final_Score := Final_Score + Max_Score - New_Base_Score;
            end;
            Argument_Cluster_Maps.Next(Argument_Cluster_Cursor_2);
        end loop;

        -- greedy search = heuristic to find alignment
        -- comp final score based on map
        Final_Score := Final_Score + Delta_No_Merge_Argument_Cluster;

        return Final_Score;
    end Score_MC_For_Align;

    ------------------------
    function Score_Operation
    ------------------------
      (Search_Operation : in Search_Operation_Type) return Score_Type is
    begin
        case Search_Operation.Operation is
            when Op_Merge_Clust =>
                return Score_Operation_MC(Search_Operation.Cluster_Index_1_Or_Governor, Search_Operation.Cluster_Index_2_Or_Dependent);

            when Op_Compose =>
                return Score_Operation_Compose(Search_Operation.Cluster_Index_1_Or_Governor, Search_Operation.Cluster_Index_2_Or_Dependent);

            when others =>
                raise MLSE_Invalid_Search_Operation_Error with Search_Operation.Operation'Img;
        end case;
    end Score_Operation;

    ---------------------------
    function Score_Operation_MC
    ---------------------------
      (Cluster_Index_1, Cluster_Index_2 : in Cluster_Index_Type) return Score_Type is

        Score : Score_Type := 0.0;

        -- TODO: si puo' evitare di chiedere l'elemento a cluster_map?
        Cluster_1 : constant Cluster_Access_Type := Cluster_Map.Element(Cluster_Index_1);
        Cluster_2 : constant Cluster_Access_Type := Cluster_Map.Element(Cluster_Index_2);
    begin

        if Cluster_Index_1 >= Cluster_Index_2 then
            raise MLSE_Error with "Cluster_Index_1 >= Cluster_Index_2";
        end if;

        -- HP
        Score := Score - Configurations.Prior_Merge;

        -- conj cnt
        declare
            Cursor : constant Cluster_Index_Pair_To_Conj_Count_Maps.Cursor
              := Cluster_Index_Pair_To_Conj_Count_Map.Find((Min_Cluster_Index => Cluster_Index_1, Max_Cluster_Index => Cluster_Index_2));
        begin
            if Cluster_Index_Pair_To_Conj_Count_Maps.Has_Element(Cursor) then
                Score := Score - (Configurations.Prior_Num_Conj * Score_Type(Cluster_Index_Pair_To_Conj_Count_Maps.Element(Cursor)));
            end if;
        end;

        -- relType:
        Score := Score - (XlogX(Cluster_1.Part_Count + Cluster_2.Part_Count) - XlogX(Cluster_1.Part_Count) - XlogX(Cluster_2.Part_Count));

        declare
            Cursor_1 : Rel_Type_Index_To_Count_Maps.Cursor
              := Cluster_1.Rel_Type_Index_To_Count_Map.First;
        begin
            while Rel_Type_Index_To_Count_Maps.Has_Element(Cursor_1) loop
                declare
                    Rel_Type_Index : constant Rel_Type_Index_Type := Rel_Type_Index_To_Count_Maps.Key(Cursor_1);

                    Cursor_2 : constant Rel_Type_Index_To_Count_Maps.Cursor
                      := Cluster_2.Rel_Type_Index_To_Count_Map.Find(Rel_Type_Index);
                begin
                    if Rel_Type_Index_To_Count_Maps.Has_Element(Cursor_2) then
                        declare
                            Count_1 : constant Natural := Rel_Type_Index_To_Count_Maps.Element(Cursor_1);
                            Count_2 : constant Natural := Rel_Type_Index_To_Count_Maps.Element(Cursor_2);
                        begin
                            Score := Score + XlogX(Count_1 + Count_2) - XlogX(Count_1) - XlogX(Count_2);
                            Score := Score + Configurations.Prior_Num_Param; -- one less param
                        end;
                    end if;
                end;
                Rel_Type_Index_To_Count_Maps.Next(Cursor_1);
            end loop;
        end;

        -- Impact for root clust likelihood:
        declare
            Cursor_1 : constant Cluster_Index_Root_Count_Maps.Cursor := Cluster_Index_Root_Count_Map.Find(Cluster_1.Index);
            Cursor_2 : constant Cluster_Index_Root_Count_Maps.Cursor := Cluster_Index_Root_Count_Map.Find(Cluster_2.Index);
        begin

            if Cluster_Index_Root_Count_Maps.Has_Element(Cursor_1) and then Cluster_Index_Root_Count_Maps.Has_Element(Cursor_2) then
                declare
                    Root_Count_1 : constant Natural := Cluster_Index_Root_Count_Maps.Element(Cursor_1);
                    Root_Count_2 : constant Natural := Cluster_Index_Root_Count_Maps.Element(Cursor_2);
                begin
                    Score := Score + XlogX(Root_Count_1 + Root_Count_2) - XlogX(Root_Count_1) - XlogX(Root_Count_2);
                    Score := Score + Configurations.Prior_Num_Param; -- one less param
                end;
            end if;
        end;

        -- Impact for parents:
        Score := Score + Score_MC_For_Parent(Cluster_Index_1, Cluster_Index_2);

        -- find one slot for each arg2: want the smaller one to save comp
        declare
            -- TODO: questa mappa e' in realta' inutile, trovare il modo per non passarla o, meglio ancora, per non farla usare a Score_MC_For_Align
            Argument_Cluster_Index_2_To_Argument_Cluster_Index_1_Map : Argument_Cluster_Index_To_Argument_Cluster_Index_Maps.Map;
        begin
            if Cluster_2.Argument_Cluster_Map.Length <= Cluster_1.Argument_Cluster_Map.Length then
                Score := Score + Score_MC_For_Align(Cluster_1.all, Cluster_2.all, Argument_Cluster_Index_2_To_Argument_Cluster_Index_1_Map);
            else null;
                Score := Score + Score_MC_For_Align(Cluster_2.all, Cluster_1.all, Argument_Cluster_Index_2_To_Argument_Cluster_Index_1_Map);
            end if;
        end;

        return Score;
    end Score_Operation_MC;

    ----------------------------
    function Score_MC_For_Parent
    ----------------------------
      (Cluster_Index_1, Cluster_Index_2 : in Cluster_Index_Type) return Score_Type is

        Score : Score_Type := 0.0;

        Cluster_Index_To_Link_To_Count_Map_Cursor_1 : constant Cluster_Index_To_Cluster_And_Argument_Cluster_Link_Type_To_Count_Map_Maps.Cursor
          := Cluster_Index_To_Cluster_And_Argument_Cluster_Link_Type_To_Count_Map_Map.Find(Cluster_Index_1);

        Cluster_Index_To_Link_To_Count_Map_Cursor_2 : constant Cluster_Index_To_Cluster_And_Argument_Cluster_Link_Type_To_Count_Map_Maps.Cursor
          := Cluster_Index_To_Cluster_And_Argument_Cluster_Link_Type_To_Count_Map_Map.Find(Cluster_Index_2);
    begin

        if Cluster_Index_To_Cluster_And_Argument_Cluster_Link_Type_To_Count_Map_Maps.Has_Element(Cluster_Index_To_Link_To_Count_Map_Cursor_1)
          and then Cluster_Index_To_Cluster_And_Argument_Cluster_Link_Type_To_Count_Map_Maps.Has_Element(Cluster_Index_To_Link_To_Count_Map_Cursor_2) then

            declare
                -- TODO: non usando gli access, queste due prossime assegnazioni sono forse molto inefficienti

                Cluster_And_Argument_Cluster_Link_Type_To_Count_Map_1 : constant Cluster_And_Argument_Cluster_Link_Type_To_Count_Maps.Map
                  := Cluster_Index_To_Cluster_And_Argument_Cluster_Link_Type_To_Count_Map_Maps.Element(Cluster_Index_To_Link_To_Count_Map_Cursor_1);

                Cluster_And_Argument_Cluster_Link_Type_To_Count_Map_2 : constant Cluster_And_Argument_Cluster_Link_Type_To_Count_Maps.Map
                  := Cluster_Index_To_Cluster_And_Argument_Cluster_Link_Type_To_Count_Map_Maps.Element(Cluster_Index_To_Link_To_Count_Map_Cursor_2);

                Link_To_Count_Cursor_1 : Cluster_And_Argument_Cluster_Link_Type_To_Count_Maps.Cursor;
                Link_To_Count_Cursor_2 : Cluster_And_Argument_Cluster_Link_Type_To_Count_Maps.Cursor;
            begin

                Link_To_Count_Cursor_1 := Cluster_And_Argument_Cluster_Link_Type_To_Count_Map_1.First;
                while Cluster_And_Argument_Cluster_Link_Type_To_Count_Maps.Has_Element(Link_To_Count_Cursor_1) loop
                    declare
                        Cluster_And_Argument_Cluster_Link : constant Cluster_And_Argument_Cluster_Link_Type
                          := Cluster_And_Argument_Cluster_Link_Type_To_Count_Maps.Key(Link_To_Count_Cursor_1);
                    begin

                        Link_To_Count_Cursor_2:= Cluster_And_Argument_Cluster_Link_Type_To_Count_Map_2.Find(Cluster_And_Argument_Cluster_Link);
                        if Cluster_And_Argument_Cluster_Link_Type_To_Count_Maps.Has_Element(Link_To_Count_Cursor_2) then
                            declare
                                Cluster_Cursor : constant Cluster_Maps.Cursor := Cluster_Map.Find(Cluster_And_Argument_Cluster_Link.Cluster_Index);
                            begin
                                if not Cluster_Maps.Has_Element(Cluster_Cursor) then
                                    T_IO.Put_Line("ERR: ScoreMC parClust==null " & Str(Cluster_Index_1) & " " & Str(Cluster_Index_2));
                                else
                                    if not Cluster_Maps.Element(Cluster_Cursor).Argument_Cluster_Map.Contains(Cluster_And_Argument_Cluster_Link.Argument_Cluster_Index) then
                                        T_IO.Put_Line("ERRORE ANOMALO: ScoreMC parClust==null " & Str(Cluster_Index_1) & " " & Str(Cluster_Index_2));
                                    else
                                        declare
                                            -- new mdl
                                            Cluster          : constant Cluster_Access_Type          := Cluster_Maps.Element(Cluster_Cursor);
                                            Argument_Cluster : constant Argument_Cluster_Access_Type := Cluster.Argument_Cluster_Map.Element(Cluster_And_Argument_Cluster_Link.Argument_Cluster_Index);

                                            Count_1          : constant Natural := Argument_Cluster.Dependent_Cluster_Index_To_Count_Map.Element(Cluster_Index_1);
                                            Count_2          : constant Natural := Argument_Cluster.Dependent_Cluster_Index_To_Count_Map.Element(Cluster_Index_2);
                                        begin
                                            -- only
                                            Score := Score + Configurations.Prior_Num_Param; -- one less param
                                            Score := Score + XlogX(Count_1 + Count_2) - XlogX(Count_1) - XlogX(Count_2);
                                        end;
                                    end if;
                                end if;
                            end;
                        end if;
                    end;
                    Cluster_And_Argument_Cluster_Link_Type_To_Count_Maps.Next(Link_To_Count_Cursor_1);
                end loop;
            end;
        end if;

        return Score;

    end Score_MC_For_Parent;

    --------------------------------
    function Score_Operation_Compose
    --------------------------------
      (Governor_Cluster_Index, Dependent_Cluster_Index : in Cluster_Index_Type) return Score_Type is

        Score : Score_Type := 0.0;

        -- --- COMP old A,B vs new A,B,A-B --- --  -- HP: better than check the difference
        -- parent: only change is mix of A -> A & A-B
        Cluster_And_Argument_Cluster_Link_Type_To_Count : Cluster_And_Argument_Cluster_Link_Type_To_Count_Maps.Map;

        -- delta for A,B,A-B
        Governor_Rel_Type_Index_To_New_Count  : Rel_Type_Index_To_Count_Maps.Map;
        Dependent_Rel_Type_Index_To_New_Count : Rel_Type_Index_To_Count_Maps.Map;
        Rel_Type_Index_Link_To_New_Count      : Rel_Type_Index_Link_To_Count_Maps.Map;

        -- those that are affected in specific values
        Governor_Argument_Cluster_Index_To_Num_Arg_To_Count      : Argument_Cluster_Index_To_Num_Arg_To_Count_Maps.Map;
        Dependent_Argument_Cluster_Index_To_Num_Arg_To_Count     : Argument_Cluster_Index_To_Num_Arg_To_Count_Maps.Map;
        Governor_New_Argument_Cluster_Index_To_Num_Arg_To_Count  : Argument_Cluster_Index_To_Num_Arg_To_Count_Maps.Map;
        Dependent_New_Argument_Cluster_Index_To_Num_Arg_To_Count : Argument_Cluster_Index_To_Num_Arg_To_Count_Maps.Map;

        Governor_Argument_Cluster_Index_To_Deprel_Index_To_Count      : Argument_Cluster_Index_To_Deprel_Index_To_Count_Maps.Map;
        Dependent_Argument_Cluster_Index_To_Deprel_Index_To_Count     : Argument_Cluster_Index_To_Deprel_Index_To_Count_Maps.Map;
        Governor_New_Argument_Cluster_Index_To_Deprel_Index_To_Count  : Argument_Cluster_Index_To_Deprel_Index_To_Count_Maps.Map;
        Dependent_New_Argument_Cluster_Index_To_Deprel_Index_To_Count : Argument_Cluster_Index_To_Deprel_Index_To_Count_Maps.Map;

        Governor_Argument_Cluster_Index_To_Dependent_Cluster_Index_To_Count      : Argument_Cluster_Index_To_Cluster_Index_To_Count_Maps.Map;
        Dependent_Argument_Cluster_Index_To_Dependent_Cluster_Index_To_Count     : Argument_Cluster_Index_To_Cluster_Index_To_Count_Maps.Map;
        Governor_New_Argument_Cluster_Index_To_Dependent_Cluster_Index_To_Count  : Argument_Cluster_Index_To_Cluster_Index_To_Count_Maps.Map;
        Dependent_New_Argument_Cluster_Index_To_Dependent_Cluster_Index_To_Count : Argument_Cluster_Index_To_Cluster_Index_To_Count_Maps.Map;

        Governor_Argument_Cluster_Index_To_Root_Node_Count      : Argument_Cluster_Index_To_Root_Node_Count_Maps.Map;
        Dependent_Argument_Cluster_Index_To_Root_Node_Count     : Argument_Cluster_Index_To_Root_Node_Count_Maps.Map;
        Governor_New_Argument_Cluster_Index_To_Root_Node_Count  : Argument_Cluster_Index_To_Root_Node_Count_Maps.Map;
        Dependent_New_Argument_Cluster_Index_To_Root_Node_Count : Argument_Cluster_Index_To_Root_Node_Count_Maps.Map;

        Governor_Argument_Cluster_Index_To_Dependents_Count      : Argument_Cluster_Index_To_Dependents_Count_Maps.Map;
        Dependent_Argument_Cluster_Index_To_Dependents_Count     : Argument_Cluster_Index_To_Dependents_Count_Maps.Map;
        Governor_New_Argument_Cluster_Index_To_Dependents_Count  : Argument_Cluster_Index_To_Dependents_Count_Maps.Map;
        Dependent_New_Argument_Cluster_Index_To_Dependents_Count : Argument_Cluster_Index_To_Dependents_Count_Maps.Map;

        -- Clusters

        -- TODO: si puo' evitare di richiedere l'elemento a Cluster_Map?
        Governor_Cluster  : constant Cluster_Access_Type := Cluster_Map.Element(Governor_Cluster_Index);
        Dependent_Cluster : constant Cluster_Access_Type := Cluster_Map.Element(Dependent_Cluster_Index);

        -- Misc

        New_Governor_Part_Count  : Integer := Governor_Cluster.Part_Count; -- TODO Natural
        New_Dependent_Part_Count : Integer := Dependent_Cluster.Part_Count; -- TODO Natural
        New_Compose_Part_Count   : Integer := 0; -- TODO Natural
        Compose_Root_Count       : Integer := 0; -- TODO Natural

        Atom_Link_Set    : Atom_Link_Sets.Set;
        Atom_Link_Cursor : Cluster_Link_To_Atom_Link_Set_Maps.Cursor;

        Argument_Cluster_Index_To_Dependent_Indexes_Cursor : Argument_Cluster_Index_To_Part_Indexes_Maps.Cursor; -- TODO: vedi l'assegnazione sotto, fare meglio magari con un cursore

    begin

        Atom_Link_Cursor := Cluster_Link_To_Atom_Link_Set_Map.Find
          ((Governor_Cluster_Index  => Governor_Cluster_Index,
            Dependent_Cluster_Index => Dependent_Cluster_Index));

        if not Cluster_Link_To_Atom_Link_Set_Maps.Has_Element(Atom_Link_Cursor) then
            return -10000.0;
        end if;

        -- TODO: copia, probabilmente inefficiente
        Atom_Link_Set := Cluster_Link_To_Atom_Link_Set_Maps.Element(Atom_Link_Cursor);

        -- <OLD>
        -- A: SUM_i(xlx(reltype_i))-xlx(tc)
        -- foreach A.par: xlx(A) in argclust
        -- A.chd: SUM_i(xlx(argtype_i))+SUM_i(xlx(chdcl_i))-2*xlx(tac)
        -- B: SUM_i(xlx(reltype_i))-xlx(tc)
        -- B.chd: SUM_i(xlx(argtype_i))+SUM_i(xlx(chdcl_i))-2*xlx(tac)
        --
        -- <NEW>
        -- A/B: 0 // no variatioon in reltype
        -- A: SUM_i(xlx(rt_i-A/B))-xlx(ttl-A/B)
        -- A.par: xlx(A-A/B)+xlx(A/B)
        -- A.chd: SUM_i(xlx(argtype_i-A/B))+SUM_i(xlx(chdcl_i-A/B))-2*xlx(tac-A/B)
        -- 			+SUM_i(xlx(argtype_i of A/B))+SUM_i(xlx(chdcl_i of A/B))-2*xlx(tac of A/B)	// except B
        -- B.chd: SUM_i(xlx(argtype_i-A/B))+SUM_i(xlx(chdcl_i-A/B))-2*xlx(tac-A/B)
        -- 			+SUM_i(xlx(argtype_i of A/B))+SUM_i(xlx(chdcl_i of A/B))-2*xlx(tac of A/B)

        for Atom_Link of Atom_Link_Set loop

            if Atom_ID_To_Part_Map.Contains(Atom_Link.Dependent_Atom_ID) and then Atom_ID_To_Part_Map.Contains(Atom_Link.Governor_Atom_ID) then
                declare
                    Governor_Part  : constant Part_Access_Type := Atom_ID_To_Part_Map.Element(Atom_Link.Governor_Atom_ID);
                    Dependent_Part : constant Part_Access_Type := Atom_ID_To_Part_Map.Element(Atom_Link.Dependent_Atom_ID);

                    Compose_Argument_Cluster_Index : constant Argument_Cluster_Index_Type := Governor_Part.Argument_Index_To_Argument_Cluster_Index_Map.Element(Dependent_Part.Argument_Index_In_Governor_Part);
                begin

                    New_Governor_Part_Count  := New_Governor_Part_Count  - 1;
                    New_Dependent_Part_Count := New_Dependent_Part_Count - 1;
                    New_Compose_Part_Count   := New_Compose_Part_Count   + 1;

                    declare
                        Cursor : Rel_Type_Index_To_Count_Maps.Cursor;
                    begin

                        -- Governor_Rel_Type_Index_To_New_Count

                        Cursor := Governor_Rel_Type_Index_To_New_Count.Find(Governor_Part.Rel_Type_Index);
                        if not Rel_Type_Index_To_Count_Maps.Has_Element(Cursor) then
                            Governor_Rel_Type_Index_To_New_Count.Insert
                              (Governor_Part.Rel_Type_Index,
                               Governor_Cluster.Rel_Type_Index_To_Count_Map.Element(Governor_Part.Rel_Type_Index) - 1);
                        else
                            Decrement(Governor_Rel_Type_Index_To_New_Count, Cursor);
                        end if;

                        -- Dependent_Rel_Type_Index_To_New_Count

                        Cursor := Dependent_Rel_Type_Index_To_New_Count.Find(Dependent_Part.Rel_Type_Index);
                        if not Rel_Type_Index_To_Count_Maps.Has_Element(Cursor) then
                            Dependent_Rel_Type_Index_To_New_Count.Insert
                              (Dependent_Part.Rel_Type_Index,
                               Dependent_Cluster.Rel_Type_Index_To_Count_Map.Element(Dependent_Part.Rel_Type_Index) - 1);
                        else
                            Decrement(Dependent_Rel_Type_Index_To_New_Count, Cursor);
                        end if;

                    end;

                    -- Rel_Type_Index_Link_To_New_Count

                    declare
                        Rel_Type_Index_Link : constant Rel_Type_Index_Link_Type
                          := (Governor_Rel_Type_Index => Governor_Part.Rel_Type_Index, Dependent_Rel_Type_Index => Dependent_Part.Rel_Type_Index);

                        Cursor              : constant Rel_Type_Index_Link_To_Count_Maps.Cursor
                          := Rel_Type_Index_Link_To_New_Count.Find(Rel_Type_Index_Link);
                    begin
                        if not Rel_Type_Index_Link_To_Count_Maps.Has_Element(Cursor) then
                            Rel_Type_Index_Link_To_New_Count.Insert(Rel_Type_Index_Link, 1);
                        else
                            Increment(Rel_Type_Index_Link_To_New_Count, Cursor);
                        end if;
                    end;

                    -- par of par-chd

                    if Governor_Part.Governor_Part /= null then
                        declare
                            Cluster_And_Argument_Cluster_Link : constant Cluster_And_Argument_Cluster_Link_Type
                              := (Cluster_Index          => Governor_Part.Governor_Part.Cluster_Index,
                                  Argument_Cluster_Index => Governor_Part.Governor_Part.Argument_Index_To_Argument_Cluster_Index_Map.Element(Governor_Part.Argument_Index_In_Governor_Part));

                            Cursor                            : constant Cluster_And_Argument_Cluster_Link_Type_To_Count_Maps.Cursor
                              := Cluster_And_Argument_Cluster_Link_Type_To_Count.Find(Cluster_And_Argument_Cluster_Link);
                        begin

                            if not Cluster_And_Argument_Cluster_Link_Type_To_Count_Maps.Has_Element(Cursor) then
                                Cluster_And_Argument_Cluster_Link_Type_To_Count.Insert(Cluster_And_Argument_Cluster_Link, 1);
                            else
                                Increment(Cluster_And_Argument_Cluster_Link_Type_To_Count, Cursor);
                            end if;
                        end;
                    else
                        Compose_Root_Count := Compose_Root_Count + 1;
                    end if;

                    -- Num Arg (governor)

                    Argument_Cluster_Index_To_Dependent_Indexes_Cursor := Governor_Part.Argument_Cluster_Index_To_Dependent_Indexes_Map.First;
                    while Argument_Cluster_Index_To_Part_Indexes_Maps.Has_Element(Argument_Cluster_Index_To_Dependent_Indexes_Cursor) loop
                        declare
                            Argument_Cluster_Index : constant Argument_Cluster_Index_Type
                              := Argument_Cluster_Index_To_Part_Indexes_Maps.Key(Argument_Cluster_Index_To_Dependent_Indexes_Cursor);

                            Num_Arg                : constant Num_Arg_Type
                              := Num_Arg_Type(Argument_Cluster_Index_To_Part_Indexes_Maps.Element(Argument_Cluster_Index_To_Dependent_Indexes_Cursor).Length);

                            Argument_Cluster       : constant Argument_Cluster_Access_Type := Governor_Cluster.Argument_Cluster_Map.Element(Argument_Cluster_Index);
                        begin

                            -- Governor_Argument_Cluster_Index_To_Root_Node_Count

                            declare
                                Cursor : constant Argument_Cluster_Index_To_Root_Node_Count_Maps.Cursor
                                  := Governor_Argument_Cluster_Index_To_Root_Node_Count.Find(Argument_Cluster_Index);
                            begin
                                if not Argument_Cluster_Index_To_Root_Node_Count_Maps.Has_Element(Cursor) then
                                    Governor_Argument_Cluster_Index_To_Root_Node_Count.Insert(Argument_Cluster_Index, Natural(Argument_Cluster.Root_Node_ID_Set.Length) - 1);
                                else
                                    Decrement(Governor_Argument_Cluster_Index_To_Root_Node_Count, Cursor);
                                end if;
                            end;

                            -- Governor_Argument_Cluster_Index_To_Num_Arg_To_Count

                            declare
                                procedure Update(Argument_Cluster_Index : in Argument_Cluster_Index_Type; Num_Arg_To_Count_Map : in out Num_Arg_To_Count_Maps.Map) is
                                    pragma Unreferenced (Argument_Cluster_Index);
                                    Cursor : constant Num_Arg_To_Count_Maps.Cursor := Num_Arg_To_Count_Map.Find(Num_Arg);
                                begin
                                    if not Num_Arg_To_Count_Maps.Has_Element(Cursor) then
                                        Num_Arg_To_Count_Map.Insert(Num_Arg, Argument_Cluster.Num_Arg_Count_Map.Element(Num_Arg) - 1);
                                    else
                                        Decrement(Num_Arg_To_Count_Map, Cursor);
                                    end if;
                                end Update;

                                Cursor   : Argument_Cluster_Index_To_Num_Arg_To_Count_Maps.Cursor
                                  := Governor_Argument_Cluster_Index_To_Num_Arg_To_Count.Find(Argument_Cluster_Index);
                            begin
                                if not Argument_Cluster_Index_To_Num_Arg_To_Count_Maps.Has_Element(Cursor) then
                                    Insert
                                      (Map      => Governor_Argument_Cluster_Index_To_Num_Arg_To_Count,
                                       Key      => Argument_Cluster_Index,
                                       Position => Cursor);
                                end if;

                                Governor_Argument_Cluster_Index_To_Num_Arg_To_Count.Update_Element(Cursor, Update'Access);
                            end;

                            -- argNum for A-B

                            declare
                                New_Num_Arg : Num_Arg_Type := Num_Arg;
                            begin
                                if Argument_Cluster_Index = Compose_Argument_Cluster_Index then
                                    New_Num_Arg := New_Num_Arg - 1;
                                end if;

                                if New_Num_Arg /= 0 then

                                    -- Governor_New_Argument_Cluster_Index_To_Num_Arg_To_Count

                                    declare
                                        procedure Update(Argument_Cluster_Index : in Argument_Cluster_Index_Type; Num_Arg_To_Count_Map : in out Num_Arg_To_Count_Maps.Map) is
                                            pragma Unreferenced (Argument_Cluster_Index);
                                            Cursor : constant Num_Arg_To_Count_Maps.Cursor := Num_Arg_To_Count_Map.Find(New_Num_Arg);
                                        begin
                                            if not Num_Arg_To_Count_Maps.Has_Element(Cursor) then
                                                Num_Arg_To_Count_Map.Insert(New_Num_Arg, 1);
                                            else
                                                Increment(Num_Arg_To_Count_Map, Cursor);
                                            end if;
                                        end Update;

                                        Cursor   : Argument_Cluster_Index_To_Num_Arg_To_Count_Maps.Cursor
                                          := Governor_New_Argument_Cluster_Index_To_Num_Arg_To_Count.Find(Argument_Cluster_Index);
                                    begin
                                        if not Argument_Cluster_Index_To_Num_Arg_To_Count_Maps.Has_Element(Cursor) then
                                            Insert
                                              (Map      => Governor_New_Argument_Cluster_Index_To_Num_Arg_To_Count,
                                               Key      => Argument_Cluster_Index,
                                               Position => Cursor);
                                        end if;

                                        Governor_New_Argument_Cluster_Index_To_Num_Arg_To_Count.Update_Element(Cursor, Update'Access);
                                    end;

                                    -- Governor_New_Argument_Cluster_Index_To_Root_Node_Count

                                    declare
                                        Cursor : constant Argument_Cluster_Index_To_Root_Node_Count_Maps.Cursor
                                          := Governor_New_Argument_Cluster_Index_To_Root_Node_Count.Find(Argument_Cluster_Index);
                                    begin
                                        if not Argument_Cluster_Index_To_Root_Node_Count_Maps.Has_Element(Cursor) then
                                            Governor_New_Argument_Cluster_Index_To_Root_Node_Count.Insert(Argument_Cluster_Index, 1);
                                        else
                                            Increment(Governor_New_Argument_Cluster_Index_To_Root_Node_Count, Cursor);
                                        end if;
                                    end;
                                end if;
                            end;
                        end;
                        Argument_Cluster_Index_To_Part_Indexes_Maps.Next(Argument_Cluster_Index_To_Dependent_Indexes_Cursor);
                    end loop;

                    -- Num Arg (dependent)

                    Argument_Cluster_Index_To_Dependent_Indexes_Cursor := Dependent_Part.Argument_Cluster_Index_To_Dependent_Indexes_Map.First;
                    while Argument_Cluster_Index_To_Part_Indexes_Maps.Has_Element(Argument_Cluster_Index_To_Dependent_Indexes_Cursor) loop
                        declare
                            Argument_Cluster_Index : constant Argument_Cluster_Index_Type
                              := Argument_Cluster_Index_To_Part_Indexes_Maps.Key(Argument_Cluster_Index_To_Dependent_Indexes_Cursor);

                            Num_Arg                : constant Num_Arg_Type
                              := Num_Arg_Type(Argument_Cluster_Index_To_Part_Indexes_Maps.Element(Argument_Cluster_Index_To_Dependent_Indexes_Cursor).Length);

                            Argument_Cluster       : constant Argument_Cluster_Access_Type := Dependent_Cluster.Argument_Cluster_Map.Element(Argument_Cluster_Index);
                        begin

                            -- Dependent_Argument_Cluster_Index_To_Root_Node_Count

                            declare
                                Cursor : constant Argument_Cluster_Index_To_Root_Node_Count_Maps.Cursor
                                  := Dependent_Argument_Cluster_Index_To_Root_Node_Count.Find(Argument_Cluster_Index);
                            begin
                                if not Argument_Cluster_Index_To_Root_Node_Count_Maps.Has_Element(Cursor) then
                                    Dependent_Argument_Cluster_Index_To_Root_Node_Count.Insert(Argument_Cluster_Index, Natural(Argument_Cluster.Root_Node_ID_Set.Length) - 1);
                                else
                                    Decrement(Dependent_Argument_Cluster_Index_To_Root_Node_Count, Cursor);
                                end if;
                            end;

                            -- Dependent_Argument_Cluster_Index_To_Num_Arg_To_Count

                            declare
                                procedure Update(Argument_Cluster_Index : in Argument_Cluster_Index_Type; Num_Arg_To_Count_Map : in out Num_Arg_To_Count_Maps.Map) is
                                    pragma Unreferenced (Argument_Cluster_Index);
                                    Cursor : constant Num_Arg_To_Count_Maps.Cursor := Num_Arg_To_Count_Map.Find(Num_Arg);
                                begin
                                    if not Num_Arg_To_Count_Maps.Has_Element(Cursor) then
                                        Num_Arg_To_Count_Map.Insert(Num_Arg, Argument_Cluster.Num_Arg_Count_Map.Element(Num_Arg) - 1);
                                    else
                                        Decrement(Num_Arg_To_Count_Map, Cursor);
                                    end if;
                                end Update;

                                Cursor   : Argument_Cluster_Index_To_Num_Arg_To_Count_Maps.Cursor
                                  := Dependent_Argument_Cluster_Index_To_Num_Arg_To_Count.Find(Argument_Cluster_Index);
                            begin
                                if not Argument_Cluster_Index_To_Num_Arg_To_Count_Maps.Has_Element(Cursor) then
                                    Insert
                                      (Map      => Dependent_Argument_Cluster_Index_To_Num_Arg_To_Count,
                                       Key      => Argument_Cluster_Index,
                                       Position => Cursor);
                                end if;

                                Dependent_Argument_Cluster_Index_To_Num_Arg_To_Count.Update_Element(Cursor, Update'Access);
                            end;

                            -- argNum for A-B

                            -- Dependent_New_Argument_Cluster_Index_To_Num_Arg_To_Count

                            declare
                                procedure Update(Argument_Cluster_Index : in Argument_Cluster_Index_Type; Num_Arg_To_Count_Map : in out Num_Arg_To_Count_Maps.Map) is
                                    pragma Unreferenced (Argument_Cluster_Index);
                                    Cursor : constant Num_Arg_To_Count_Maps.Cursor := Num_Arg_To_Count_Map.Find(Num_Arg);
                                begin
                                    if not Num_Arg_To_Count_Maps.Has_Element(Cursor) then
                                        Num_Arg_To_Count_Map.Insert(Num_Arg, 1);
                                    else
                                        Increment(Num_Arg_To_Count_Map, Cursor);
                                    end if;
                                end Update;

                                Cursor   : Argument_Cluster_Index_To_Num_Arg_To_Count_Maps.Cursor
                                  := Dependent_New_Argument_Cluster_Index_To_Num_Arg_To_Count.Find(Argument_Cluster_Index);
                            begin
                                if not Argument_Cluster_Index_To_Num_Arg_To_Count_Maps.Has_Element(Cursor) then
                                    Insert
                                      (Map      => Dependent_New_Argument_Cluster_Index_To_Num_Arg_To_Count,
                                       Key      => Argument_Cluster_Index,
                                       Position => Cursor);
                                end if;

                                Dependent_New_Argument_Cluster_Index_To_Num_Arg_To_Count.Update_Element(Cursor, Update'Access);
                            end;

                            -- Dependent_New_Argument_Cluster_Index_To_Root_Node_Count

                            declare
                                Cursor : constant Argument_Cluster_Index_To_Root_Node_Count_Maps.Cursor
                                  := Dependent_New_Argument_Cluster_Index_To_Root_Node_Count.Find(Argument_Cluster_Index);
                            begin
                                if not Argument_Cluster_Index_To_Root_Node_Count_Maps.Has_Element(Cursor) then
                                    Dependent_New_Argument_Cluster_Index_To_Root_Node_Count.Insert(Argument_Cluster_Index, 1);
                                else
                                    Increment(Dependent_New_Argument_Cluster_Index_To_Root_Node_Count, Cursor);
                                end if;
                            end;
                        end;
                        Argument_Cluster_Index_To_Part_Indexes_Maps.Next(Argument_Cluster_Index_To_Dependent_Indexes_Cursor);
                    end loop;

                    -- --- A.chd

                    -- Governor Part

                    declare
                        Argument_Part_Cursor : Part_Maps.Cursor := Governor_Part.Argument_Part_Map.First;
                    begin
                        while Part_Maps.Has_Element(Argument_Part_Cursor) loop
                            declare
                                Argument_Part_Index : constant Part_Index_Type := Part_Maps.Key(Argument_Part_Cursor);
                                Argument_Part       : constant Part_Access_Type := Part_Maps.Element(Argument_Part_Cursor);

                                Argument_Cluster_Index : constant Argument_Cluster_Index_Type  := Governor_Part.Argument_Index_To_Argument_Cluster_Index_Map.Element(Argument_Part_Index);
                                Argument_Cluster       : constant Argument_Cluster_Access_Type := Governor_Cluster.Argument_Cluster_Map.Element(Argument_Cluster_Index);
                            begin

                                -- old

                                -- Governor_Argument_Cluster_Index_To_Dependents_Count

                                declare
                                    Cursor : constant Argument_Cluster_Index_To_Dependents_Count_Maps.Cursor
                                      := Governor_Argument_Cluster_Index_To_Dependents_Count.Find(Argument_Cluster_Index);
                                begin
                                    if not Argument_Cluster_Index_To_Dependents_Count_Maps.Has_Element(Cursor) then
                                        Governor_Argument_Cluster_Index_To_Dependents_Count.Insert(Argument_Cluster_Index, Argument_Cluster.Dependents_Count - 1);
                                    else
                                        Decrement(Governor_Argument_Cluster_Index_To_Dependents_Count, Cursor);
                                    end if;
                                end;

                                -- Governor_Argument_Cluster_Index_To_Deprel_Index_To_Count

                                declare
                                    procedure Update(Argument_Cluster_Index : in Argument_Cluster_Index_Type; Deprel_Index_To_Count_Map : in out Deprel_Index_To_Count_Maps.Map) is
                                        pragma Unreferenced (Argument_Cluster_Index);
                                        Cursor : constant Deprel_Index_To_Count_Maps.Cursor := Deprel_Index_To_Count_Map.Find(Argument_Part.Deprel_Index);
                                    begin
                                        if not Deprel_Index_To_Count_Maps.Has_Element(Cursor) then
                                            Deprel_Index_To_Count_Map.Insert(Argument_Part.Deprel_Index, Argument_Cluster.Deprel_Index_To_Count_Map.Element(Argument_Part.Deprel_Index) - 1);
                                        else
                                            Decrement(Deprel_Index_To_Count_Map, Cursor); -- FIXME: come mai va a meno di zero?
                                        end if;
                                    end Update;

                                    Cursor   : Argument_Cluster_Index_To_Deprel_Index_To_Count_Maps.Cursor
                                      := Governor_Argument_Cluster_Index_To_Deprel_Index_To_Count.Find(Argument_Cluster_Index);
                                begin
                                    if not Argument_Cluster_Index_To_Deprel_Index_To_Count_Maps.Has_Element(Cursor) then
                                        Insert
                                          (Map      => Governor_Argument_Cluster_Index_To_Deprel_Index_To_Count,
                                           Key      => Argument_Cluster_Index,
                                           Position => Cursor);
                                    end if;

                                    Governor_Argument_Cluster_Index_To_Deprel_Index_To_Count.Update_Element(Cursor, Update'Access);
                                end;

                                -- Governor_Argument_Cluster_Index_To_Dependent_Cluster_Index_To_Count

                                declare
                                    procedure Update(Argument_Cluster_Index : in Argument_Cluster_Index_Type; Cluster_Index_To_Count_Map : in out Cluster_Index_To_Count_Maps.Map) is
                                        pragma Unreferenced (Argument_Cluster_Index);
                                        Cursor : constant Cluster_Index_To_Count_Maps.Cursor := Cluster_Index_To_Count_Map.Find(Argument_Part.Cluster_Index);
                                    begin
                                        if not Cluster_Index_To_Count_Maps.Has_Element(Cursor) then
                                            Cluster_Index_To_Count_Map.Insert(Argument_Part.Cluster_Index, Argument_Cluster.Dependent_Cluster_Index_To_Count_Map.Element(Argument_Part.Cluster_Index) - 1);
                                        else
                                            Decrement(Cluster_Index_To_Count_Map, Cursor); -- FIXME: come mai va a meno di zero?
                                        end if;
                                    end Update;

                                    Cursor   : Argument_Cluster_Index_To_Cluster_Index_To_Count_Maps.Cursor
                                      := Governor_Argument_Cluster_Index_To_Dependent_Cluster_Index_To_Count.Find(Argument_Cluster_Index);
                                begin
                                    if not Argument_Cluster_Index_To_Cluster_Index_To_Count_Maps.Has_Element(Cursor) then
                                        Insert
                                          (Map      => Governor_Argument_Cluster_Index_To_Dependent_Cluster_Index_To_Count,
                                           Key      => Argument_Cluster_Index,
                                           Position => Cursor);
                                    end if;

                                    Governor_Argument_Cluster_Index_To_Dependent_Cluster_Index_To_Count.Update_Element(Cursor, Update'Access);
                                end;

                                -- New Arg

                                if Argument_Part.Root_Node.ID /= Dependent_Part.Root_Node.ID then

                                    -- Governor_New_Argument_Cluster_Index_To_Dependents_Count

                                    declare
                                        Cursor : constant Argument_Cluster_Index_To_Dependents_Count_Maps.Cursor
                                          := Governor_New_Argument_Cluster_Index_To_Dependents_Count.Find(Argument_Cluster_Index);
                                    begin
                                        if not Argument_Cluster_Index_To_Dependents_Count_Maps.Has_Element(Cursor) then
                                            Governor_New_Argument_Cluster_Index_To_Dependents_Count.Insert(Argument_Cluster_Index, 1);
                                        else
                                            Increment(Governor_New_Argument_Cluster_Index_To_Dependents_Count, Cursor);
                                        end if;
                                    end;

                                    -- Governor_New_Argument_Cluster_Index_To_Deprel_Index_To_Count

                                    declare
                                        procedure Update(Argument_Cluster_Index : in Argument_Cluster_Index_Type; Deprel_Index_To_Count_Map : in out Deprel_Index_To_Count_Maps.Map) is
                                            pragma Unreferenced (Argument_Cluster_Index);
                                            Cursor : constant Deprel_Index_To_Count_Maps.Cursor := Deprel_Index_To_Count_Map.Find(Argument_Part.Deprel_Index);
                                        begin
                                            if not Deprel_Index_To_Count_Maps.Has_Element(Cursor) then
                                                Deprel_Index_To_Count_Map.Insert(Argument_Part.Deprel_Index, 1);
                                            else
                                                Increment(Deprel_Index_To_Count_Map, Cursor);
                                            end if;
                                        end Update;

                                        Cursor   : Argument_Cluster_Index_To_Deprel_Index_To_Count_Maps.Cursor
                                          := Governor_New_Argument_Cluster_Index_To_Deprel_Index_To_Count.Find(Argument_Cluster_Index);
                                    begin
                                        if not Argument_Cluster_Index_To_Deprel_Index_To_Count_Maps.Has_Element(Cursor) then
                                            Insert
                                              (Map      => Governor_New_Argument_Cluster_Index_To_Deprel_Index_To_Count,
                                               Key      => Argument_Cluster_Index,
                                               Position => Cursor);
                                        end if;

                                        Governor_New_Argument_Cluster_Index_To_Deprel_Index_To_Count.Update_Element(Cursor, Update'Access);
                                    end;

                                    -- Governor_New_Argument_Cluster_Index_To_Dependent_Cluster_Index_To_Count

                                    declare
                                        procedure Update(Argument_Cluster_Index : in Argument_Cluster_Index_Type; Cluster_Index_To_Count_Map : in out Cluster_Index_To_Count_Maps.Map) is
                                            pragma Unreferenced (Argument_Cluster_Index);
                                            Cursor : constant Cluster_Index_To_Count_Maps.Cursor := Cluster_Index_To_Count_Map.Find(Argument_Part.Cluster_Index);
                                        begin
                                            if not Cluster_Index_To_Count_Maps.Has_Element(Cursor) then
                                                Cluster_Index_To_Count_Map.Insert(Argument_Part.Cluster_Index, 1);
                                            else
                                                Increment(Cluster_Index_To_Count_Map, Cursor);
                                            end if;
                                        end Update;

                                        Cursor   : Argument_Cluster_Index_To_Cluster_Index_To_Count_Maps.Cursor
                                          := Governor_New_Argument_Cluster_Index_To_Dependent_Cluster_Index_To_Count.Find(Argument_Cluster_Index);
                                    begin
                                        if not Argument_Cluster_Index_To_Cluster_Index_To_Count_Maps.Has_Element(Cursor) then
                                            Insert
                                              (Map      => Governor_New_Argument_Cluster_Index_To_Dependent_Cluster_Index_To_Count,
                                               Key      => Argument_Cluster_Index,
                                               Position => Cursor);
                                        end if;

                                        Governor_New_Argument_Cluster_Index_To_Dependent_Cluster_Index_To_Count.Update_Element(Cursor, Update'Access);
                                    end;
                                end if;
                            end;
                            Part_Maps.Next(Argument_Part_Cursor);
                        end loop;
                    end;

                    -- Dependent Part

                    declare
                        Argument_Part_Cursor : Part_Maps.Cursor := Dependent_Part.Argument_Part_Map.First;
                    begin
                        while Part_Maps.Has_Element(Argument_Part_Cursor) loop
                            declare
                                Argument_Part_Index : constant Part_Index_Type  := Part_Maps.Key(Argument_Part_Cursor);
                                Argument_Part       : constant Part_Access_Type := Part_Maps.Element(Argument_Part_Cursor);

                                Argument_Cluster_Index : constant Argument_Cluster_Index_Type  := Dependent_Part.Argument_Index_To_Argument_Cluster_Index_Map.Element(Argument_Part_Index);
                                Argument_Cluster       : constant Argument_Cluster_Access_Type := Dependent_Cluster.Argument_Cluster_Map.Element(Argument_Cluster_Index);
                            begin

                                -- Dependent_Argument_Cluster_Index_To_Dependents_Count

                                declare
                                    Cursor : constant Argument_Cluster_Index_To_Dependents_Count_Maps.Cursor
                                      := Dependent_Argument_Cluster_Index_To_Dependents_Count.Find(Argument_Cluster_Index);
                                begin
                                    if not Argument_Cluster_Index_To_Dependents_Count_Maps.Has_Element(Cursor) then
                                        Dependent_Argument_Cluster_Index_To_Dependents_Count.Insert(Argument_Cluster_Index, Argument_Cluster.Dependents_Count - 1);
                                    else
                                        Decrement(Dependent_Argument_Cluster_Index_To_Dependents_Count, Cursor);
                                    end if;
                                end;

                                -- Dependent_Argument_Cluster_Index_To_Deprel_Index_To_Count

                                declare
                                    procedure Update(Argument_Cluster_Index : in Argument_Cluster_Index_Type; Deprel_Index_To_Count_Map : in out Deprel_Index_To_Count_Maps.Map) is
                                        pragma Unreferenced (Argument_Cluster_Index);
                                        Cursor : constant Deprel_Index_To_Count_Maps.Cursor := Deprel_Index_To_Count_Map.Find(Argument_Part.Deprel_Index);
                                    begin
                                        if not Deprel_Index_To_Count_Maps.Has_Element(Cursor) then
                                            Deprel_Index_To_Count_Map.Insert(Argument_Part.Deprel_Index, Argument_Cluster.Deprel_Index_To_Count_Map.Element(Argument_Part.Deprel_Index) - 1);
                                        else
                                            Decrement(Deprel_Index_To_Count_Map, Cursor);
                                        end if;
                                    end Update;

                                    Cursor   : Argument_Cluster_Index_To_Deprel_Index_To_Count_Maps.Cursor
                                      := Dependent_Argument_Cluster_Index_To_Deprel_Index_To_Count.Find(Argument_Cluster_Index);
                                begin
                                    if not Argument_Cluster_Index_To_Deprel_Index_To_Count_Maps.Has_Element(Cursor) then
                                        Insert
                                          (Map      => Dependent_Argument_Cluster_Index_To_Deprel_Index_To_Count,
                                           Key      => Argument_Cluster_Index,
                                           Position => Cursor);
                                    end if;

                                    Dependent_Argument_Cluster_Index_To_Deprel_Index_To_Count.Update_Element(Cursor, Update'Access);
                                end;

                                -- Dependent_Argument_Cluster_Index_To_Dependent_Cluster_Index_To_Count

                                declare
                                    procedure Update(Argument_Cluster_Index : in Argument_Cluster_Index_Type; Cluster_Index_To_Count_Map : in out Cluster_Index_To_Count_Maps.Map) is
                                        pragma Unreferenced (Argument_Cluster_Index);
                                        Cursor : constant Cluster_Index_To_Count_Maps.Cursor := Cluster_Index_To_Count_Map.Find(Argument_Part.Cluster_Index);
                                    begin
                                        if not Cluster_Index_To_Count_Maps.Has_Element(Cursor) then
                                            Cluster_Index_To_Count_Map.Insert(Argument_Part.Cluster_Index, Argument_Cluster.Dependent_Cluster_Index_To_Count_Map.Element(Argument_Part.Cluster_Index) - 1);
                                        else
                                            Decrement(Cluster_Index_To_Count_Map, Cursor);
                                        end if;
                                    end Update;

                                    Cursor   : Argument_Cluster_Index_To_Cluster_Index_To_Count_Maps.Cursor
                                      := Dependent_Argument_Cluster_Index_To_Dependent_Cluster_Index_To_Count.Find(Argument_Cluster_Index);
                                begin
                                    if not Argument_Cluster_Index_To_Cluster_Index_To_Count_Maps.Has_Element(Cursor) then
                                        Insert
                                          (Map      => Dependent_Argument_Cluster_Index_To_Dependent_Cluster_Index_To_Count,
                                           Key      => Argument_Cluster_Index,
                                           Position => Cursor);
                                    end if;

                                    Dependent_Argument_Cluster_Index_To_Dependent_Cluster_Index_To_Count.Update_Element(Cursor, Update'Access);
                                end;

                                -- New Arg

                                -- Dependent_New_Argument_Cluster_Index_To_Dependents_Count

                                declare
                                    Cursor : constant Argument_Cluster_Index_To_Dependents_Count_Maps.Cursor
                                      := Dependent_New_Argument_Cluster_Index_To_Dependents_Count.Find(Argument_Cluster_Index);
                                begin
                                    if not Argument_Cluster_Index_To_Dependents_Count_Maps.Has_Element(Cursor) then
                                        Dependent_New_Argument_Cluster_Index_To_Dependents_Count.Insert(Argument_Cluster_Index, 1);
                                    else
                                        Increment(Dependent_New_Argument_Cluster_Index_To_Dependents_Count, Cursor);
                                    end if;
                                end;

                                -- Dependent_New_Argument_Cluster_Index_To_Deprel_Index_To_Count

                                declare
                                    procedure Update(Argument_Cluster_Index : in Argument_Cluster_Index_Type; Deprel_Index_To_Count_Map : in out Deprel_Index_To_Count_Maps.Map) is
                                        pragma Unreferenced (Argument_Cluster_Index);
                                        Cursor : constant Deprel_Index_To_Count_Maps.Cursor := Deprel_Index_To_Count_Map.Find(Argument_Part.Deprel_Index);
                                    begin
                                        if not Deprel_Index_To_Count_Maps.Has_Element(Cursor) then
                                            Deprel_Index_To_Count_Map.Insert(Argument_Part.Deprel_Index, 1);
                                        else
                                            Increment(Deprel_Index_To_Count_Map, Cursor);
                                        end if;
                                    end Update;

                                    Cursor   : Argument_Cluster_Index_To_Deprel_Index_To_Count_Maps.Cursor
                                      := Dependent_New_Argument_Cluster_Index_To_Deprel_Index_To_Count.Find(Argument_Cluster_Index);
                                begin
                                    if not Argument_Cluster_Index_To_Deprel_Index_To_Count_Maps.Has_Element(Cursor) then
                                        Insert
                                          (Map      => Dependent_New_Argument_Cluster_Index_To_Deprel_Index_To_Count,
                                           Key      => Argument_Cluster_Index,
                                           Position => Cursor);
                                    end if;

                                    Dependent_New_Argument_Cluster_Index_To_Deprel_Index_To_Count.Update_Element(Cursor, Update'Access);
                                end;

                                -- Dependent_New_Argument_Cluster_Index_To_Dependent_Cluster_Index_To_Count

                                declare
                                    procedure Update(Argument_Cluster_Index : in Argument_Cluster_Index_Type; Cluster_Index_To_Count_Map : in out Cluster_Index_To_Count_Maps.Map) is
                                        pragma Unreferenced (Argument_Cluster_Index);
                                        Cursor : constant Cluster_Index_To_Count_Maps.Cursor := Cluster_Index_To_Count_Map.Find(Argument_Part.Cluster_Index);
                                    begin
                                        if not Cluster_Index_To_Count_Maps.Has_Element(Cursor) then
                                            Cluster_Index_To_Count_Map.Insert(Argument_Part.Cluster_Index, 1);
                                        else
                                            Increment(Cluster_Index_To_Count_Map, Cursor);
                                        end if;
                                    end Update;

                                    Cursor   : Argument_Cluster_Index_To_Cluster_Index_To_Count_Maps.Cursor
                                      := Dependent_New_Argument_Cluster_Index_To_Dependent_Cluster_Index_To_Count.Find(Argument_Cluster_Index);
                                begin
                                    if not Argument_Cluster_Index_To_Cluster_Index_To_Count_Maps.Has_Element(Cursor) then
                                        Insert
                                          (Map      => Dependent_New_Argument_Cluster_Index_To_Dependent_Cluster_Index_To_Count,
                                           Key      => Argument_Cluster_Index,
                                           Position => Cursor);
                                    end if;

                                    Dependent_New_Argument_Cluster_Index_To_Dependent_Cluster_Index_To_Count.Update_Element(Cursor, Update'Access);
                                end;
                            end;
                            Part_Maps.Next(Argument_Part_Cursor);
                        end loop;
                    end;

                end;
            end if; -- TODO: è proprio corretto saltare tutto questo??
        end loop;

        -- --- render score

        declare
            Old_Governor_Denom  : constant Score_Type := XlogX(Governor_Cluster.Part_Count);
            New_Governor_Denom  : constant Score_Type := XlogX(New_Governor_Part_Count);
            Old_Dependent_Denom : constant Score_Type := XlogX(Dependent_Cluster.Part_Count);
            New_Dependent_Denom : constant Score_Type := XlogX(New_Dependent_Part_Count);
            Compose_Denom       : constant Score_Type := XlogX(New_Compose_Part_Count);
        begin

            -- root

            if Compose_Root_Count > 0 then
                declare
                    Orig_Root_Count : constant Natural := Cluster_Index_Root_Count_Map.Element(Governor_Cluster_Index);
                begin
                    if Orig_Root_Count > Compose_Root_Count then
                        Score := Score + XlogX(Compose_Root_Count) + XlogX(Orig_Root_Count - Compose_Root_Count) - XlogX(Orig_Root_Count);
                        Score := Score - Configurations.Prior_Num_Param;
                    end if;
                end;
            end if;

            -- reltype (governor)

            declare
                Cursor : Rel_Type_Index_To_Count_Maps.Cursor
                  := Governor_Rel_Type_Index_To_New_Count.First;
            begin
                while Rel_Type_Index_To_Count_Maps.Has_Element(Cursor) loop
                    declare
                        Rel_Type_Index : constant Rel_Type_Index_Type := Rel_Type_Index_To_Count_Maps.Key(Cursor);
                        Count          : constant Integer             := Rel_Type_Index_To_Count_Maps.Element(Cursor); --TODO: Natural
                        Orig_Count     : constant Integer             := Governor_Cluster.Rel_Type_Index_To_Count_Map.Element(Rel_Type_Index); -- TODO: Natural
                    begin
                        if Orig_Count < Count then
                            --raise MLSE_Error with "Orig_Count < Count";
                            goto Continue_Not_Good;
                        end if;

                        Score := Score - XlogX(Orig_Count);

                        if Count > 0 then
                            Score := Score + XlogX(Count);
                        else
                            Score := Score + Configurations.Prior_Num_Param; -- old params eliminated
                        end if;
                    end;

                    <<Continue_Not_Good>>
                    Rel_Type_Index_To_Count_Maps.Next(Cursor);
                end loop;
            end;

            Score := Score + Old_Governor_Denom;
            Score := Score - New_Governor_Denom;

            -- reltype (dependent)

            declare
                Cursor : Rel_Type_Index_To_Count_Maps.Cursor
                  := Dependent_Rel_Type_Index_To_New_Count.First;
            begin
                while Rel_Type_Index_To_Count_Maps.Has_Element(Cursor) loop
                    declare
                        Rel_Type_Index : constant Rel_Type_Index_Type := Rel_Type_Index_To_Count_Maps.Key(Cursor);
                        Count          : constant Integer             := Rel_Type_Index_To_Count_Maps.Element(Cursor); --TODO: Natural
                        Orig_Count     : constant Integer             := Dependent_Cluster.Rel_Type_Index_To_Count_Map.Element(Rel_Type_Index); --TODO: Natural
                    begin
                        if Orig_Count < Count then
                            goto Continue_Not_Good_2;
                            --raise MLSE_Error with "Orig_Count < Count";
                        end if;

                        Score := Score - XlogX(Orig_Count);

                        if Count > 0 then
                            Score := Score + XlogX(Count);
                        else
                            Score := Score + Configurations.Prior_Num_Param; -- old params eliminated
                        end if;
                    end;
                    <<Continue_Not_Good_2>>
                    Rel_Type_Index_To_Count_Maps.Next(Cursor);
                end loop;
            end;

            Score := Score + Old_Dependent_Denom;
            Score := Score - New_Dependent_Denom;

            declare
                Cursor : Rel_Type_Index_Link_To_Count_Maps.Cursor
                  := Rel_Type_Index_Link_To_New_Count.First;
            begin
                while Rel_Type_Index_Link_To_Count_Maps.Has_Element(Cursor) loop
                    declare
                        Count               : constant Natural                  := Rel_Type_Index_Link_To_Count_Maps.Element(Cursor);
                    begin
                        Score := Score - Configurations.Prior_Num_Param; -- new param
                        Score := Score + XlogX(Count);
                    end;
                    Rel_Type_Index_Link_To_Count_Maps.Next(Cursor);
                end loop;
            end;

            Score := Score - Compose_Denom;

            -- par: no change in denom

            declare
                Cursor : Cluster_And_Argument_Cluster_Link_Type_To_Count_Maps.Cursor
                  := Cluster_And_Argument_Cluster_Link_Type_To_Count.First;
            begin
                while Cluster_And_Argument_Cluster_Link_Type_To_Count_Maps.Has_Element(Cursor) loop
                    declare
                        Cluster_And_Argument_Cluster_Link : constant Cluster_And_Argument_Cluster_Link_Type := Cluster_And_Argument_Cluster_Link_Type_To_Count_Maps.Key(Cursor);
                        Count                             : constant Natural := Cluster_And_Argument_Cluster_Link_Type_To_Count_Maps.Element(Cursor); -- A -> A-B

                        Cluster                           : constant Cluster_Access_Type := Cluster_Map.Element(Cluster_And_Argument_Cluster_Link.Cluster_Index);
                  Argument_Cluster                  : constant Argument_Cluster_Access_Type
                          := (if Cluster.Argument_Cluster_Map.Contains (Cluster_And_Argument_Cluster_Link.Argument_Cluster_Index)
                              then  Cluster.Argument_Cluster_Map.Element (Cluster_And_Argument_Cluster_Link.Argument_Cluster_Index)
                              else null);

                        Orig_Count                        : constant Natural
                    := (if Argument_Cluster /= null and then Argument_Cluster.Dependent_Cluster_Index_To_Count_Map.Contains(Governor_Cluster_Index)
                        then Argument_Cluster.Dependent_Cluster_Index_To_Count_Map.Element(Governor_Cluster_Index)
                        else 0);
                    begin
                        if Count /= Orig_Count then
                            Score := Score - Configurations.Prior_Num_Param; -- extra param for A-B
                            Score := Score + XlogX(Count) + XlogX(Orig_Count - Count) - XlogX(Orig_Count);
                        end if;
                    end;
                    Cluster_And_Argument_Cluster_Link_Type_To_Count_Maps.Next(Cursor);
                end loop;
            end;

            -- orig vs new A

            declare
                Argument_Cluster_Cursor : Argument_Cluster_Maps.Cursor
                  := Governor_Cluster.Argument_Cluster_Map.First;
            begin
                while Argument_Cluster_Maps.Has_Element(Argument_Cluster_Cursor) loop
                    declare
                        Argument_Cluster_Index : constant Argument_Cluster_Index_Type  := Argument_Cluster_Maps.Key(Argument_Cluster_Cursor);
                        Argument_Cluster       : constant Argument_Cluster_Access_Type := Argument_Cluster_Maps.Element(Argument_Cluster_Cursor);

                        Orig_Part_Count        : constant Natural := Natural(Argument_Cluster.Root_Node_ID_Set.Length);

                        Governor_Argument_Cluster_Index_To_Root_Node_Count_Cursor : constant Argument_Cluster_Index_To_Root_Node_Count_Maps.Cursor
                          := Governor_Argument_Cluster_Index_To_Root_Node_Count.Find(Argument_Cluster_Index);

                    begin

                        -- absent cnt
                        Score := Score - (XlogX(Governor_Cluster.Part_Count - Orig_Part_Count) - Old_Governor_Denom);

                        -- not affect this arg, except for absent

                        if not Argument_Cluster_Index_To_Root_Node_Count_Maps.Has_Element(Governor_Argument_Cluster_Index_To_Root_Node_Count_Cursor) then
                            Score := Score + XlogX(New_Governor_Part_Count - Orig_Part_Count) - New_Governor_Denom;

                        else
                            -- handle delta
                            -- ==null ~ no chg; ==0 ~ change a lot!
                            declare
                                New_Part_Count : constant Integer := Argument_Cluster_Index_To_Root_Node_Count_Maps.Element(Governor_Argument_Cluster_Index_To_Root_Node_Count_Cursor);
                            begin
                                if New_Part_Count > 0 then
                                    -- argnum
                                    Score := Score + XlogX(New_Governor_Part_Count - New_Part_Count) - New_Governor_Denom;
                                end if;
                            end;

                            declare
                                -- TODO: copia, forse inefficiente?
                                Num_Arg_To_Count_Map : constant Num_Arg_To_Count_Maps.Map
                                  := Governor_Argument_Cluster_Index_To_Num_Arg_To_Count.Element(Argument_Cluster_Index);

                                Cursor               : Num_Arg_To_Count_Maps.Cursor
                                  := Num_Arg_To_Count_Map.First;
                            begin
                                while Num_Arg_To_Count_Maps.Has_Element(Cursor) loop
                                    declare
                                        Num_Arg   : constant Num_Arg_Type := Num_Arg_To_Count_Maps.Key(Cursor);
                                        Count     : constant Integer      := Num_Arg_To_Count_Maps.Element(Cursor); -- FIXME: come mai < 0? vedi sopra...
                              Old_Count : constant Integer
                                := Argument_Cluster.Num_Arg_Count_Map.Element(Num_Arg);
                                    begin
                                        Score := Score - XlogX(Old_Count);

                                        if Count > 0 then
                                            Score := Score + XlogX(Count);
                                        else
                                            Score := Score + Configurations.Prior_Num_Param;
                                        end if;
                                    end;
                                    Num_Arg_To_Count_Maps.Next(Cursor);
                                end loop;
                            end;

                            -- argtype/chdclust
                            Score := Score - (2.0 * (XlogX(Governor_Argument_Cluster_Index_To_Dependents_Count.Element(Argument_Cluster_Index)) - XlogX(Argument_Cluster.Dependents_Count)));

                            declare
                                -- TODO: copia, forse inefficiente?
                                Deprel_Index_To_Count_Map : constant Deprel_Index_To_Count_Maps.Map
                                  := Governor_Argument_Cluster_Index_To_Deprel_Index_To_Count.Element(Argument_Cluster_Index);

                                Cursor               : Deprel_Index_To_Count_Maps.Cursor
                                  := Deprel_Index_To_Count_Map.First;
                            begin
                                while Deprel_Index_To_Count_Maps.Has_Element(Cursor) loop
                                    declare
                                        Deprel_Index : constant Deprel_Index_Type := Deprel_Index_To_Count_Maps.Key(Cursor);
                                        Count        : constant Integer           := Deprel_Index_To_Count_Maps.Element(Cursor); -- FIXME: Integer: come mai a meno di zero? vedi sopra...
                              Old_Count    : constant Integer
                                := Argument_Cluster.Deprel_Index_To_Count_Map.Element(Deprel_Index);
                                    begin
                                        Score := Score - XlogX(Old_Count);

                                        if Count > 0 then
                                            Score := Score + XlogX(Count);
                                        else
                                            Score := Score + Configurations.Prior_Num_Param;
                                        end if;
                                    end;
                                    Deprel_Index_To_Count_Maps.Next(Cursor);
                                end loop;
                            end;

                            declare
                                -- TODO: copia, forse inefficiente?
                                Cluster_Index_To_Count_Map : constant Cluster_Index_To_Count_Maps.Map
                                  := Governor_Argument_Cluster_Index_To_Dependent_Cluster_Index_To_Count.Element(Argument_Cluster_Index);

                                Cursor               : Cluster_Index_To_Count_Maps.Cursor
                                  := Cluster_Index_To_Count_Map.First;
                            begin
                                while Cluster_Index_To_Count_Maps.Has_Element(Cursor) loop
                                    declare
                                        Cluster_Index : constant Cluster_Index_Type := Cluster_Index_To_Count_Maps.Key(Cursor);
                                        Count         : constant Integer            := Cluster_Index_To_Count_Maps.Element(Cursor); -- FIXME: Integer: come mai a meno di zero? vedi sopra...
                              Old_Count     : constant Integer
                                := Argument_Cluster.Dependent_Cluster_Index_To_Count_Map.Element(Cluster_Index);
                                    begin
                                        Score := Score - XlogX(Old_Count);

                                        if Count > 0 then
                                            Score := Score + XlogX(Count);
                                        else
                                            Score := Score + Configurations.Prior_Num_Param;
                                        end if;
                                    end;
                                    Cluster_Index_To_Count_Maps.Next(Cursor);
                                end loop;
                            end;
                        end if;

                    end;
                    Argument_Cluster_Maps.Next(Argument_Cluster_Cursor);
                end loop;
            end; -- orig vs new A

            -- orig vs new B

            declare
                Argument_Cluster_Cursor : Argument_Cluster_Maps.Cursor
                  := Dependent_Cluster.Argument_Cluster_Map.First;
            begin
                while Argument_Cluster_Maps.Has_Element(Argument_Cluster_Cursor) loop
                    declare
                        Argument_Cluster_Index : constant Argument_Cluster_Index_Type  := Argument_Cluster_Maps.Key(Argument_Cluster_Cursor);
                        Argument_Cluster       : constant Argument_Cluster_Access_Type := Argument_Cluster_Maps.Element(Argument_Cluster_Cursor);

                        Orig_Part_Count        : constant Natural := Natural(Argument_Cluster.Root_Node_ID_Set.Length);

                        Dependent_Argument_Cluster_Index_To_Root_Node_Count_Cursor : constant Argument_Cluster_Index_To_Root_Node_Count_Maps.Cursor
                          := Dependent_Argument_Cluster_Index_To_Root_Node_Count.Find(Argument_Cluster_Index);
                    begin
                        -- absent cnt
                        Score := Score - (XlogX(Dependent_Cluster.Part_Count - Orig_Part_Count) - Old_Dependent_Denom);

                        -- not affect this arg, except for absent

                        if not Argument_Cluster_Index_To_Root_Node_Count_Maps.Has_Element(Dependent_Argument_Cluster_Index_To_Root_Node_Count_Cursor) then
                            Score := Score + XlogX(New_Dependent_Part_Count - Orig_Part_Count) - New_Dependent_Denom;

                        else
                            -- handle delta
                            -- if newpart=0, arg eliminated
                            declare
                                New_Part_Count : constant Integer := Argument_Cluster_Index_To_Root_Node_Count_Maps.Element(Dependent_Argument_Cluster_Index_To_Root_Node_Count_Cursor);
                            begin
                                if New_Part_Count > 0 then
                                    -- argnum
                                    Score := Score + XlogX(New_Dependent_Part_Count - New_Part_Count) - New_Dependent_Denom;
                                end if;
                            end;

                            declare
                                -- TODO: copia, forse inefficiente?
                                Num_Arg_To_Count_Map : constant Num_Arg_To_Count_Maps.Map
                                  := Dependent_Argument_Cluster_Index_To_Num_Arg_To_Count.Element(Argument_Cluster_Index);

                                Cursor               : Num_Arg_To_Count_Maps.Cursor
                                  := Num_Arg_To_Count_Map.First;
                            begin
                                while Num_Arg_To_Count_Maps.Has_Element(Cursor) loop
                                    declare
                                        Num_Arg   : constant Num_Arg_Type := Num_Arg_To_Count_Maps.Key(Cursor);
                                        Count     : constant Integer      := Num_Arg_To_Count_Maps.Element(Cursor);
                                        Old_Count : constant Integer      := Argument_Cluster.Num_Arg_Count_Map.Element(Num_Arg);
                                    begin
                                        Score := Score - XlogX(Old_Count);

                                        if Count > 0 then
                                            Score := Score + XlogX(Count);
                                        else
                                            Score := Score + Configurations.Prior_Num_Param;
                                        end if;
                                    end;
                                    Num_Arg_To_Count_Maps.Next(Cursor);
                                end loop;
                            end;

                            -- argtype/chdclust
                            Score := Score - (2.0 * (XlogX(Dependent_Argument_Cluster_Index_To_Dependents_Count.Element(Argument_Cluster_Index)) - XlogX(Argument_Cluster.Dependents_Count)));

                            declare
                                -- TODO: copia, forse inefficiente?
                                Deprel_Index_To_Count_Map : constant Deprel_Index_To_Count_Maps.Map
                                  := Dependent_Argument_Cluster_Index_To_Deprel_Index_To_Count.Element(Argument_Cluster_Index);

                                Cursor               : Deprel_Index_To_Count_Maps.Cursor
                                  := Deprel_Index_To_Count_Map.First;
                            begin
                                while Deprel_Index_To_Count_Maps.Has_Element(Cursor) loop
                                    declare
                                        Deprel_Index : constant Deprel_Index_Type := Deprel_Index_To_Count_Maps.Key(Cursor);
                                        Count        : constant Integer           := Deprel_Index_To_Count_Maps.Element(Cursor);
                                        Old_Count    : constant Integer           := Argument_Cluster.Deprel_Index_To_Count_Map.Element(Deprel_Index);
                                    begin
                                        Score := Score - XlogX(Old_Count);

                                        if Count > 0 then
                                            Score := Score + XlogX(Count);
                                        else
                                            Score := Score + Configurations.Prior_Num_Param;
                                        end if;
                                    end;
                                    Deprel_Index_To_Count_Maps.Next(Cursor);
                                end loop;
                            end;

                            declare
                                -- TODO: copia, forse inefficiente?
                                Cluster_Index_To_Count_Map : constant Cluster_Index_To_Count_Maps.Map
                                  := Dependent_Argument_Cluster_Index_To_Dependent_Cluster_Index_To_Count.Element(Argument_Cluster_Index);

                                Cursor               : Cluster_Index_To_Count_Maps.Cursor
                                  := Cluster_Index_To_Count_Map.First;
                            begin
                                while Cluster_Index_To_Count_Maps.Has_Element(Cursor) loop
                                    declare
                                        Cluster_Index : constant Cluster_Index_Type := Cluster_Index_To_Count_Maps.Key(Cursor);
                                        Count         : constant Integer            := Cluster_Index_To_Count_Maps.Element(Cursor);
                                        Old_Count     : constant Integer            := Argument_Cluster.Dependent_Cluster_Index_To_Count_Map.Element(Cluster_Index);
                                    begin
                                        Score := Score - XlogX(Old_Count);

                                        if Count > 0 then
                                            Score := Score + XlogX(Count);
                                        else
                                            Score := Score + Configurations.Prior_Num_Param;
                                        end if;
                                    end;
                                    Cluster_Index_To_Count_Maps.Next(Cursor);
                                end loop;
                            end;
                        end if;
                    end;
                    Argument_Cluster_Maps.Next(Argument_Cluster_Cursor);
                end loop;
            end; -- orig vs new B

            -- A-B: new A; B

            declare
                Cursor : Argument_Cluster_Index_To_Root_Node_Count_Maps.Cursor
                  := Governor_New_Argument_Cluster_Index_To_Root_Node_Count.First;
            begin
                while Argument_Cluster_Index_To_Root_Node_Count_Maps.Has_Element(Cursor) loop
                    declare
                        Argument_Cluster_Index : constant Argument_Cluster_Index_Type := Argument_Cluster_Index_To_Root_Node_Count_Maps.Key(Cursor);
                        Root_Node_Count        : constant Natural := Argument_Cluster_Index_To_Root_Node_Count_Maps.Element(Cursor);

                        -- TODO: copia, forse inefficiente?
                        Num_Arg_To_Count_Map   : constant Num_Arg_To_Count_Maps.Map := Governor_New_Argument_Cluster_Index_To_Num_Arg_To_Count.Element(Argument_Cluster_Index);
                        Sub_Cursor             : Num_Arg_To_Count_Maps.Cursor := Num_Arg_To_Count_Map.First;
                    begin

                        Score := Score + XlogX(New_Compose_Part_Count - Root_Node_Count) - Compose_Denom;

                        while Num_Arg_To_Count_Maps.Has_Element(Sub_Cursor) loop
                            declare
                                Num_Arg_Count : constant Natural := Num_Arg_To_Count_Maps.Element(Sub_Cursor);
                            begin
                                Score := Score + XlogX(Num_Arg_Count) - Configurations.Prior_Num_Param;
                            end;
                            Num_Arg_To_Count_Maps.Next(Sub_Cursor);
                        end loop;
                    end;
                    Argument_Cluster_Index_To_Root_Node_Count_Maps.Next(Cursor);
                end loop;
            end;

            declare
                Cursor : Argument_Cluster_Index_To_Root_Node_Count_Maps.Cursor
                  := Dependent_New_Argument_Cluster_Index_To_Root_Node_Count.First;
            begin
                while Argument_Cluster_Index_To_Root_Node_Count_Maps.Has_Element(Cursor) loop
                    declare
                        Argument_Cluster_Index : constant Argument_Cluster_Index_Type := Argument_Cluster_Index_To_Root_Node_Count_Maps.Key(Cursor);
                        Root_Node_Count        : constant Natural := Argument_Cluster_Index_To_Root_Node_Count_Maps.Element(Cursor);

                        -- TODO: copia, forse inefficiente?
                        Num_Arg_To_Count_Map   : constant Num_Arg_To_Count_Maps.Map := Dependent_New_Argument_Cluster_Index_To_Num_Arg_To_Count.Element(Argument_Cluster_Index);
                        Sub_Cursor             : Num_Arg_To_Count_Maps.Cursor := Num_Arg_To_Count_Map.First;
                    begin
                        Score := Score + XlogX(New_Compose_Part_Count - Root_Node_Count) - Compose_Denom;

                        while Num_Arg_To_Count_Maps.Has_Element(Sub_Cursor) loop
                            declare
                                Num_Arg_Count : constant Natural := Num_Arg_To_Count_Maps.Element(Sub_Cursor);
                            begin
                                Score := Score + XlogX(Num_Arg_Count) - Configurations.Prior_Num_Param;
                            end;
                            Num_Arg_To_Count_Maps.Next(Sub_Cursor);
                        end loop;
                    end;
                    Argument_Cluster_Index_To_Root_Node_Count_Maps.Next(Cursor);
                end loop;
            end;
        end;

        declare
            Cursor : Argument_Cluster_Index_To_Dependents_Count_Maps.Cursor
              := Governor_New_Argument_Cluster_Index_To_Dependents_Count.First;
        begin

            while Argument_Cluster_Index_To_Dependents_Count_Maps.Has_Element(Cursor) loop
                declare
                    Argument_Cluster_Index : constant Argument_Cluster_Index_Type := Argument_Cluster_Index_To_Dependents_Count_Maps.Key(Cursor);
                    Dependents_Count       : constant Natural                     := Argument_Cluster_Index_To_Dependents_Count_Maps.Element(Cursor);

                    -- TODO: copia, forse inefficiente?
                    Deprel_Index_To_Count_Map : constant Deprel_Index_To_Count_Maps.Map := Governor_New_Argument_Cluster_Index_To_Deprel_Index_To_Count.Element(Argument_Cluster_Index);
                    Deprel_Cursor             : Deprel_Index_To_Count_Maps.Cursor       := Deprel_Index_To_Count_Map.First;

                    -- TODO: copia, forse inefficiente?
                    Dependent_Cluster_Index_To_Count_Map : constant Cluster_Index_To_Count_Maps.Map := Governor_New_Argument_Cluster_Index_To_Dependent_Cluster_Index_To_Count.Element(Argument_Cluster_Index);
                    Dependent_Cluster_Cursor             : Cluster_Index_To_Count_Maps.Cursor       := Dependent_Cluster_Index_To_Count_Map.First;
                begin

                    Score := Score - (2.0 * XlogX(Dependents_Count));

                    while Deprel_Index_To_Count_Maps.Has_Element(Deprel_Cursor) loop
                        declare
                            Deprel_Count : constant Natural := Deprel_Index_To_Count_Maps.Element(Deprel_Cursor);
                        begin
                            Score := Score + XlogX(Deprel_Count) - Configurations.Prior_Num_Param;
                        end;
                        Deprel_Index_To_Count_Maps.Next(Deprel_Cursor);
                    end loop;

                    while Cluster_Index_To_Count_Maps.Has_Element(Dependent_Cluster_Cursor) loop
                        declare
                            Dependent_Cluster_Count : constant Natural := Cluster_Index_To_Count_Maps.Element(Dependent_Cluster_Cursor);
                        begin
                            Score := Score + XlogX(Dependent_Cluster_Count) - Configurations.Prior_Num_Param;
                        end;
                        Cluster_Index_To_Count_Maps.Next(Dependent_Cluster_Cursor);
                    end loop;
                end;
                Argument_Cluster_Index_To_Dependents_Count_Maps.Next(Cursor);
            end loop;
        end;

        declare
            Cursor : Argument_Cluster_Index_To_Dependents_Count_Maps.Cursor
              := Dependent_New_Argument_Cluster_Index_To_Dependents_Count.First;
        begin

            while Argument_Cluster_Index_To_Dependents_Count_Maps.Has_Element(Cursor) loop
                declare
                    Argument_Cluster_Index : constant Argument_Cluster_Index_Type := Argument_Cluster_Index_To_Dependents_Count_Maps.Key(Cursor);
                    Dependents_Count       : constant Natural                     := Argument_Cluster_Index_To_Dependents_Count_Maps.Element(Cursor);

                    -- TODO: copia, forse inefficiente?
                    Deprel_Index_To_Count_Map : constant Deprel_Index_To_Count_Maps.Map := Dependent_New_Argument_Cluster_Index_To_Deprel_Index_To_Count.Element(Argument_Cluster_Index);
                    Deprel_Cursor             : Deprel_Index_To_Count_Maps.Cursor       := Deprel_Index_To_Count_Map.First;

                    -- TODO: copia, forse inefficiente?
                    Dependent_Cluster_Index_To_Count_Map : constant Cluster_Index_To_Count_Maps.Map := Dependent_New_Argument_Cluster_Index_To_Dependent_Cluster_Index_To_Count.Element(Argument_Cluster_Index);
                    Dependent_Cluster_Cursor             : Cluster_Index_To_Count_Maps.Cursor       := Dependent_Cluster_Index_To_Count_Map.First;
                begin

                    Score := Score - (2.0 * XlogX(Dependents_Count));

                    while Deprel_Index_To_Count_Maps.Has_Element(Deprel_Cursor) loop
                        declare
                            Deprel_Count : constant Natural := Deprel_Index_To_Count_Maps.Element(Deprel_Cursor);
                        begin
                            Score := Score + XlogX(Deprel_Count) - Configurations.Prior_Num_Param;
                        end;
                        Deprel_Index_To_Count_Maps.Next(Deprel_Cursor);
                    end loop;

                    while Cluster_Index_To_Count_Maps.Has_Element(Dependent_Cluster_Cursor) loop
                        declare
                            Dependent_Cluster_Count : constant Natural := Cluster_Index_To_Count_Maps.Element(Dependent_Cluster_Cursor);
                        begin
                            Score := Score + XlogX(Dependent_Cluster_Count) - Configurations.Prior_Num_Param;
                        end;
                        Cluster_Index_To_Count_Maps.Next(Dependent_Cluster_Cursor);
                    end loop;
                end;
                Argument_Cluster_Index_To_Dependents_Count_Maps.Next(Cursor);
            end loop;
        end;

        return Score;
    end Score_Operation_Compose;

    ---------------------
    procedure Print_Model
    ---------------------
      (Result_Directory : in String) is

        -- MLN: formula/weights implicitly captured in Clust/Part probabilities
    begin

        -----
        -- Clustering
        -----

        declare
            Clustering_Filename : constant String := Path_Join(Result_Directory, "model.clustering");
            Clustering_File     : T_IO.File_Type;

            Cluster_Cursor      : Cluster_Maps.Cursor := Cluster_Map.First;
        begin
            T_IO.Create(Clustering_File, T_IO.Out_File, Clustering_Filename);

            T_IO.New_Line(Clustering_File);
            T_IO.New_Line(Clustering_File);
            T_IO.Put_Line(Clustering_File, "===== CLUSTERING =====");

            while Cluster_Maps.Has_Element(Cluster_Cursor) loop
                declare
                    Cluster : constant Cluster_Access_Type := Cluster_Maps.Element(Cluster_Cursor);
                begin
                    if Cluster.Rel_Type_Index_To_Count_Map.Length > 1 then
                        T_IO.Put_Line(Clustering_File, Str(To_String(Cluster.all)));
                    end if;
                end;
                Cluster_Maps.Next(Cluster_Cursor);
            end loop;

            T_IO.Close(Clustering_File);
        end;

        -----
        -- MLN
        -----

        declare
            MLN_Filename : constant String := Path_Join(Result_Directory, "model.mln");
            MLN_File     : T_IO.File_Type;

            Cluster_Cursor : Cluster_Maps.Cursor := Cluster_Map.First;
        begin
            T_IO.Create(MLN_File, T_IO.Out_File, MLN_Filename);

            while Cluster_Maps.Has_Element(Cluster_Cursor) loop
                declare
                    Cluster : constant Cluster_Access_Type := Cluster_Maps.Element(Cluster_Cursor);

                    Argument_Cluster_Cursor : Argument_Cluster_Maps.Cursor := Cluster.Argument_Cluster_Map.First;
                begin

                    T_IO.Put_Line(MLN_File, Str(Cluster.Index) & ASCII.HT & Str(To_String(Cluster.all)));

                    while Argument_Cluster_Maps.Has_Element(Argument_Cluster_Cursor) loop
                        declare
                            Argument_Cluster_Index : constant Argument_Cluster_Index_Type  := Argument_Cluster_Maps.Key(Argument_Cluster_Cursor);
                            Argument_Cluster       : constant Argument_Cluster_Access_Type := Argument_Cluster_Maps.Element(Argument_Cluster_Cursor);

                            Num_Arg_Cursor         : Num_Arg_To_Count_Maps.Cursor       := Argument_Cluster.Num_Arg_Count_Map.First;
                            Deprel_Cursor          : Deprel_Index_To_Count_Maps.Cursor  := Argument_Cluster.Deprel_Index_To_Count_Map.First;
                            Dependent_Cursor       : Cluster_Index_To_Count_Maps.Cursor := Argument_Cluster.Dependent_Cluster_Index_To_Count_Map.First;
                        begin

                            T_IO.Put(MLN_File, ASCII.HT & Str(Argument_Cluster_Index));

                            while Num_Arg_To_Count_Maps.Has_Element(Num_Arg_Cursor) loop
                                declare
                                    Num_Arg : constant Num_Arg_Type := Num_Arg_To_Count_Maps.Key(Num_Arg_Cursor);
                                    Count   : constant Natural      := Num_Arg_To_Count_Maps.Element(Num_Arg_Cursor);
                                begin
                                    T_IO.Put(MLN_File, ASCII.HT & Str(Num_Arg) & ':' & Str(Count));
                                end;
                                Num_Arg_To_Count_Maps.Next(Num_Arg_Cursor);
                            end loop;

                            T_IO.New_Line(MLN_File);
                            T_IO.Put(MLN_File, ASCII.HT);

                            while Deprel_Index_To_Count_Maps.Has_Element(Deprel_Cursor) loop
                                declare
                                    Deprel_Index : constant Deprel_Index_Type := Deprel_Index_To_Count_Maps.Key(Deprel_Cursor);
                                    Count        : constant Natural           := Deprel_Index_To_Count_Maps.Element(Deprel_Cursor);
                                    Deprel       : constant Unbounded_String  := Deprel_Vector.Element(Deprel_Index);
                                begin
                                    T_IO.Put(MLN_File, ASCII.HT & Str(Deprel_Index) & ':' & Str(Deprel) & ':' & Str(Count));
                                end;
                                Deprel_Index_To_Count_Maps.Next(Deprel_Cursor);
                            end loop;

                            T_IO.New_Line(MLN_File);
                            T_IO.Put(MLN_File, ASCII.HT);

                            while Cluster_Index_To_Count_Maps.Has_Element(Dependent_Cursor) loop
                                declare
                                    Dependent_Cluster_Index : constant Cluster_Index_Type  := Cluster_Index_To_Count_Maps.Key(Dependent_Cursor);
                                    Count                   : constant Natural             := Cluster_Index_To_Count_Maps.Element(Dependent_Cursor);

                                    Dependent_Cursor        : constant Cluster_Maps.Cursor := Cluster_Map.Find(Dependent_Cluster_Index);
                                begin
                                    if Cluster_Maps.Has_Element(Dependent_Cursor) then
                                        T_IO.Put(MLN_File, ASCII.HT & Str(Dependent_Cluster_Index) & ':' & Str(To_String(Cluster_Maps.Element(Dependent_Cursor).all)) & ':' & Str(Count));
                                    else
                                        T_IO.Put(MLN_File, ASCII.HT & Str(Dependent_Cluster_Index) & ":null:" & Str(Count));
                                    end if;
                                end;
                                Cluster_Index_To_Count_Maps.Next(Dependent_Cursor);
                            end loop;
                            T_IO.New_Line(MLN_File);
                        end;
                        Argument_Cluster_Maps.Next(Argument_Cluster_Cursor);
                    end loop;
                end;
                Cluster_Maps.Next(Cluster_Cursor);
            end loop;

            T_IO.Close(MLN_File);
        end;

        -----
        -- Parse
        -----

        declare
            Parse_Filename : constant String := Path_Join(Result_Directory, "model.parse");
            Parse_File     : T_IO.File_Type;

            Atom_ID_To_Part_Cursor : Atom_ID_To_Part_Maps.Cursor := Atom_ID_To_Part_Map.First;
        begin
            T_IO.Create(Parse_File, T_IO.Out_File, Parse_Filename);

            while Atom_ID_To_Part_Maps.Has_Element(Atom_ID_To_Part_Cursor) loop
                declare
                    Atom_ID : constant Atom_ID_Type        := Atom_ID_To_Part_Maps.Key(Atom_ID_To_Part_Cursor);
                    Part    : constant Part_Access_Type    := Atom_ID_To_Part_Maps.Element(Atom_ID_To_Part_Cursor);
                    Cluster : Cluster_Access_Type renames Part.Cluster_Access;
                begin

                    T_IO.Put_Line(Parse_File, To_String(Atom_ID) & ASCII.HT & Str(Get_Tree_Str(Part.Root_Node.all)));
                    T_IO.Put_Line(Parse_File, ASCII.HT & Str(Part.Cluster_Index) & ASCII.HT & Str(To_String(Cluster.all)));

                    if Part.Governor_Part = null then
                        T_IO.Put_Line(Parse_File, ASCII.HT & "");
                        T_IO.Put_Line(Parse_File, ASCII.HT & "");
                    else
                        declare
                            Argument_Part          : constant Part_Access_Type            := Part.Governor_Part.Argument_Part_Map.Element(Part.Argument_Index_In_Governor_Part);
                            Governor_Cluster       : constant Cluster_Access_Type         := Cluster_Map.Element(Part.Governor_Part.Cluster_Index);
                            Argument_Cluster_Index : constant Argument_Cluster_Index_Type := Part.Governor_Part.Argument_Index_To_Argument_Cluster_Index_Map.Element(Part.Argument_Index_In_Governor_Part);
                            Deprel                 : constant Unbounded_String            := Deprel_Vector.Element(Argument_Part.Deprel_Index);
                        begin
                            -- parpart; clust
                            T_IO.Put_Line(Parse_File, ASCII.HT & To_String(Part.Governor_Part.Root_Node.ID) & ASCII.HT & Str(Part.Governor_Part.Cluster_Index) & ASCII.HT & Str(To_String(Governor_Cluster.all)));
                            -- arg
                            T_IO.Put_Line(Parse_File, ASCII.HT & Str(Argument_Cluster_Index) & ASCII.HT & Str(Argument_Part.Deprel_Index) & ASCII.HT & Str(Deprel));
                        end;
                    end if;
                end;
                Atom_ID_To_Part_Maps.Next(Atom_ID_To_Part_Cursor);
            end loop;

            T_IO.Close(Parse_File);
        end;
    end Print_Model;

    ------------------
    function To_String
    ------------------
      (Argument_Cluster : in Argument_Cluster_Type) return Unbounded_String is

        Ret : Unbounded_String;

        Cursor : Deprel_Index_To_Count_Maps.Cursor
          := Argument_Cluster.Deprel_Index_To_Count_Map.First;
    begin
        while Deprel_Index_To_Count_Maps.Has_Element(Cursor) loop
            declare
                Deprel_Index : constant Deprel_Index_Type := Deprel_Index_To_Count_Maps.Key(Cursor);
                Count        : constant Natural           := Deprel_Index_To_Count_Maps.Element(Cursor);
            begin
                if Length(Ret) > 0 then
                    Append(Ret, " ");
                end if;

                Append(Ret, Deprel_Vector.Element(Deprel_Index));
                Append(Ret, ":");
                Append(Ret, Str(Count));
            end;
            Deprel_Index_To_Count_Maps.Next(Cursor);
        end loop;

        return Ret;
    end To_String;

    ------------------
    function To_String
    ------------------
      (Cluster : in Cluster_Type) return Unbounded_String is

        Cursor : Rel_Type_Index_To_Count_Maps.Cursor;
    begin
        return Ret : Unbounded_String do

            Cursor := Cluster.Rel_Type_Index_To_Count_Map.First;

            Append(Ret, "[");

            while Rel_Type_Index_To_Count_Maps.Has_Element(Cursor) loop

                declare
                    Count          : Integer             renames Rel_Type_Index_To_Count_Maps.Element(Cursor); -- TODO: Natural
                    Rel_Type_Index : Rel_Type_Index_Type renames Rel_Type_Index_To_Count_Maps.Key(Cursor);
                    Rel_Type_Str   : Unbounded_String    renames Rel_Type_Str_Vector.Element(Rel_Type_Index);
                begin

                    if Length(Ret) > 1 then -- not "[" only
                        Append(Ret, ",");
                        Append(Ret, ASCII.HT);
                    end if;

                    Append(Ret, Rel_Type_Str);
                    Append(Ret, ":");
                    Append(Ret, Str(Count));
                end;

                --                  declare
                --                      Count          : Natural             renames Rel_Type_Index_To_Count_Maps.Element(Cursor);
                --                      Rel_Type_Index : Rel_Type_Index_Type renames Rel_Type_Index_To_Count_Maps.Key(Cursor);
                --                  begin
                --
                --                      if Length(Ret) > 1 then -- not "[" only
                --                          Append(Ret, ",");
                --                      end if;
                --
                --                      Append(Ret, Str(Natural(Rel_Type_Index)));
                --                      Append(Ret, ":");
                --                      Append(Ret, Str(Count));
                --                  end;

                Rel_Type_Index_To_Count_Maps.Next(Cursor);
            end loop;

            Append(Ret, "]");
        end return;
    end To_String;

    ------------------------------------------
    procedure Generate_Search_Operation_String
    ------------------------------------------
      (Search_Operation : in out Search_Operation_Type) is

        Cluster_Index_A  : Cluster_Index_Type renames Search_Operation.Cluster_Index_1_Or_Governor;
        Cluster_Index_B  : Cluster_Index_Type renames Search_Operation.Cluster_Index_2_Or_Dependent;

        Cluster_A        : Cluster_Access_Type := null;
        Cluster_B        : Cluster_Access_Type := null;

        Repr_Length      : Natural := 0;
        Repr_Index       : Natural := 0;

        Cursor           : Rel_Type_Index_To_Count_Maps.Cursor;
        use Ada.Containers;
    begin

        -----
        -- Repr Length
        -----

        Repr_Length := 2; -- Operation command type & separator

        -- Cluster A
        if Cluster_Index_Mutex /= null then
            Cluster_Index_Mutex(Cluster_Index_A).Seize;
        end if;
        Cluster_A := Cluster_Map.Element(Cluster_Index_A);
        Repr_Length := Repr_Length + Natural(Cluster_A.Rel_Type_Index_To_Count_Map.Length) * 2;
        if Cluster_Index_Mutex /= null then
            Cluster_Index_Mutex(Cluster_Index_A).Release;
        end if;

        -- Cluster B
        if Cluster_Index_Mutex /= null then
            Cluster_Index_Mutex(Cluster_Index_B).Seize;
        end if;
        Cluster_B := Cluster_Map.Element(Cluster_Index_B);
        Repr_Length := Repr_Length + Natural(Cluster_B.Rel_Type_Index_To_Count_Map.Length) * 2;
        if Cluster_Index_Mutex /= null then
            Cluster_Index_Mutex(Cluster_Index_B).Release;
        end if;

        Free(Search_Operation.Repr);
        Search_Operation.Repr := new Integer_Array_Type(1 .. Repr_Length);

        -----
        -- Hash seed and first Repr element
        -----
        case Search_Operation.Operation is
            when Op_Merge_Clust =>
                Search_Operation.Repr(1)    := 1;
                Search_Operation.Hash_Value := 1;

            when Op_Compose =>
                Search_Operation.Repr(1)    := 3;
                Search_Operation.Hash_Value := 3;

            when others => raise MLSE_Invalid_Search_Operation_Error with Search_Operation.Operation'Img;
        end case;

        Repr_Index := 2;

        -----
        -- Cluster_A
        -----

        if Cluster_Index_Mutex /= null then
            Cluster_Index_Mutex(Cluster_Index_A).Seize;
        end if;
        Cursor := Cluster_A.Rel_Type_Index_To_Count_Map.First;
        while Rel_Type_Index_To_Count_Maps.Has_Element(Cursor) loop
            declare
                Key     : constant Integer := Integer(Rel_Type_Index_To_Count_Maps.Key(Cursor));
                Element : constant Integer := Integer(Rel_Type_Index_To_Count_Maps.Element(Cursor));
            begin
                Search_Operation.Repr(Repr_Index) := Key;
                Repr_Index := Repr_Index + 1;

                Search_Operation.Repr(Repr_Index) := Element;
                Repr_Index := Repr_Index + 1;

                Search_Operation.Hash_Value := Search_Operation.Hash_Value * 101 + Hash_Type(abs Key);
                Search_Operation.Hash_Value := Search_Operation.Hash_Value * 101 + Hash_Type(abs Element);

                Rel_Type_Index_To_Count_Maps.Next(Cursor);
            end;
        end loop;
        if Cluster_Index_Mutex /= null then
            Cluster_Index_Mutex(Cluster_Index_A).Release;
        end if;

        -----
        -- Separator
        -----

        Search_Operation.Repr(Repr_Index) := Integer'First;
        Repr_Index := Repr_Index + 1;

        Search_Operation.Hash_Value := Search_Operation.Hash_Value * 101 + Hash_Type(abs Integer'First);

        -----
        -- Cluster_B
        -----

        if Cluster_Index_Mutex /= null then
            Cluster_Index_Mutex(Cluster_Index_B).Seize;
        end if;
        Cursor := Cluster_B.Rel_Type_Index_To_Count_Map.First;
        while Rel_Type_Index_To_Count_Maps.Has_Element(Cursor) loop
            declare
                Key     : constant Integer := Integer(Rel_Type_Index_To_Count_Maps.Key(Cursor));
                Element : constant Integer := Integer(Rel_Type_Index_To_Count_Maps.Element(Cursor));
            begin
                Search_Operation.Repr(Repr_Index) := Key;
                Repr_Index := Repr_Index + 1;

                Search_Operation.Repr(Repr_Index) := Element;
                Repr_Index := Repr_Index + 1;

                Search_Operation.Hash_Value := Search_Operation.Hash_Value * 101 + Hash_Type(abs Key);
                Search_Operation.Hash_Value := Search_Operation.Hash_Value * 101 + Hash_Type(abs Element);

                Rel_Type_Index_To_Count_Maps.Next(Cursor);
            end;
        end loop;

        if Cluster_Index_Mutex /= null then
            Cluster_Index_Mutex(Cluster_Index_B).Release;
        end if;

    end Generate_Search_Operation_String;

    -----------------------------------------
    function Get_Total_Clusters_Count return Natural is
    -----------------------------------------
    begin
        return Natural(Cluster_Map.Length);
    end Get_Total_Clusters_Count;

    -----------------------------------------------------------------------------------------------------
    function Clusters_Atom_IDs_Count(Cluster_Index_Start, Cluster_Index_End : Cluster_Index_Type) return Natural is
    -----------------------------------------------------------------------------------------------------
    begin
        return Count : Natural := 0 do
            for Cluster of Cluster_Map loop
                if Cluster.Index >= Cluster_Index_Start and then Cluster.Index < Cluster_Index_End then
                    Count := Count + Natural(Cluster_Index_To_Atom_ID_Set_Map.Element(Cluster.Index).Length);
                end if;
            end loop;
        end return;
    end Clusters_Atom_IDs_Count;

    --------------------------------------------------------------------------------
    function Atom_IDs_Count(Cluster_Index : in Cluster_Index_Type) return Natural is
    --------------------------------------------------------------------------------
    begin
        return Natural(Cluster_Index_To_Atom_ID_Set_Map.Element(Cluster_Index).Length);
    end Atom_IDs_Count;

    --------------------------------------------
    procedure Serialize(Filename : in String) is
    --------------------------------------------
        use Ada.Containers;

        -- Serialized_String : aliased Unbounded_String;
        -- USS_Access  : Unbounded_String_Stream.USS_Access_Type := Create(Serialized_String'Unchecked_Access);
        -- Stream : constant Unbounded_String_Stream.Stream_Access := Get_Stream(USS_Access);

        File   : Ada.Streams.Stream_IO.File_Type;
        Stream : Ada.Streams.Stream_IO.Stream_Access;
    begin

        Ada.Streams.Stream_IO.Create(File, Ada.Streams.Stream_IO.Out_File, Filename);
        Stream := Ada.Streams.Stream_IO.Stream(File);

        -- Cluster_Map
        Cluster_Maps.Map'Output
          (Stream, Cluster_Map);

        -- Cluster_Index_To_Atom_ID_Set_Map
        Cluster_Index_To_Atom_ID_Set_Maps.Map'Output
          (Stream, Cluster_Index_To_Atom_ID_Set_Map);

        -- Rel_Type_Str_Vector
        Rel_Type_Str_Vectors.Vector'Output
          (Stream, Rel_Type_Str_Vector);

        -- Atom_ID_To_Part_Map
        Count_Type'Output(Stream, Atom_ID_To_Part_Map.Length);
        declare
            Cursor : Atom_ID_To_Part_Maps.Cursor := Atom_ID_To_Part_Map.First;
        begin
            while Atom_ID_To_Part_Maps.Has_Element(Cursor) loop
                declare
                    Key   : constant Atom_ID_Type     := Atom_ID_To_Part_Maps.Key(Cursor);
                    Value : constant Part_Access_Type := Atom_ID_To_Part_Maps.Element(Cursor);
                begin

                    Atom_ID_Type'Output(Stream, Key);

                    -- Root_Node (Atom_[Access_]Type)
                    Atom_ID_Type'Output(Stream, Value.Root_Node.ID);
                    Unbounded_String'Output(Stream, Value.Root_Node.Form);
                    Unbounded_String'Output(Stream, Value.Root_Node.Lemma);
                    Unbounded_String'Output(Stream, Value.Root_Node.POS);
                    -- Root_Node->Dependents (Atom_[Access_]Type)
                    Count_Type'Output(Stream, Value.Root_Node.Dependents.Length);
                    declare
                        C : Deprel_To_Atom_Set_Maps.Cursor := Value.Root_Node.Dependents.First;
                    begin
                        while Deprel_To_Atom_Set_Maps.Has_Element(C) loop
                            declare
                                Deprel   : constant Unbounded_String := Deprel_To_Atom_Set_Maps.Key(C);
                                Atom_Set : constant Atom_Sets.Set    := Deprel_To_Atom_Set_Maps.Element(C);
                            begin
                                Unbounded_String'Output(Stream, Deprel);
                                Count_Type'Output(Stream, Atom_Set.Length);
                                for Atom of Atom_Set loop
                                    Atom_ID_Type'Output(Stream, Atom.ID);
                                end loop;
                            end;
                            Deprel_To_Atom_Set_Maps.Next(C);
                        end loop;
                    end;

                    Rel_Type_Index_Type'Output(Stream, Value.Rel_Type_Index);
                    Cluster_Index_Type'Output(Stream, Value.Cluster_Index); -- Skip Cluster_Access
                    if Value.Governor_Part = null then
                        Boolean'Output(Stream, False);
                    else
                        Boolean'Output(Stream, True);
                        Atom_ID_Type'Output(Stream, Value.Governor_Part.Root_Node.ID); -- Governor_Part is access
                    end if;
                    Unbounded_String'Output(Stream, Value.Deprel);
                    Deprel_Index_Type'Output(Stream, Value.Deprel_Index);
                    Part_Index_Type'Output(Stream, Value.Argument_Index_In_Governor_Part);

                    -- Argument_Part_Map : Part_Maps.Map;
                    Count_Type'Output(Stream, Value.Argument_Part_Map.Length);
                    declare
                        C : Part_Maps.Cursor := Value.Argument_Part_Map.First;
                    begin
                        while Part_Maps.Has_Element(C) loop
                            declare
                                Part_Index : constant Part_Index_Type  := Part_Maps.Key(C);
                                Part       : constant Part_Access_Type := Part_Maps.Element(C);
                            begin
                                Part_Index_Type'Output(Stream, Part_Index);
                                Atom_ID_Type'Output(Stream, Part.Root_Node.ID);
                            end;
                            Part_Maps.Next(C);
                        end loop;
                    end;

                    Part_Index_Type'Output(Stream, Value.Next_Argument_Index);
                    Part_Index_To_Argument_Cluster_Index_Maps.Map'Output(Stream, Value.Argument_Index_To_Argument_Cluster_Index_Map);
                    Argument_Cluster_Index_To_Part_Indexes_Maps.Map'Output(Stream, Value.Argument_Cluster_Index_To_Dependent_Indexes_Map);
                end;
                Atom_ID_To_Part_Maps.Next(Cursor);
            end loop;
        end;

        Ada.Streams.Stream_IO.Close(File);
    end Serialize;

    ----------------------------------------------
    procedure Deserialize(Filename : in String) is
    ----------------------------------------------
        use Ada.Containers;

        -- Copy_Serialized_String : aliased Unbounded_String := Serialized_String;
        -- USS_Access             : Unbounded_String_Stream.USS_Access_Type := Create(Copy_Serialized_String'Unchecked_Access);
        -- Stream : constant Unbounded_String_Stream.Stream_Access := Get_Stream(USS_Access);

        File   : Ada.Streams.Stream_IO.File_Type;
        Stream : Ada.Streams.Stream_IO.Stream_Access;
    begin

        Ada.Streams.Stream_IO.Open(File, Ada.Streams.Stream_IO.In_File, Filename);
        Stream := Ada.Streams.Stream_IO.Stream(File);

        --------------------------------------------------------------

        -- Cluster_Map
        Cluster_Map := Cluster_Maps.Map'Input(Stream);

        -- Cluster_Index_To_Atom_ID_Set_Map
        Cluster_Index_To_Atom_ID_Set_Map := Cluster_Index_To_Atom_ID_Set_Maps.Map'Input(Stream);

        -- Rel_Type_Str_Vector
        Rel_Type_Str_Vector := Rel_Type_Str_Vectors.Vector'Input(Stream);

        -- Atom_ID_To_Part_Map
        declare

            function Create_Or_Get(Atom_ID : in Atom_ID_Type) return Part_Access_Type is
                C : constant Atom_ID_To_Part_Maps.Cursor
                  := Atom_ID_To_Part_Map.Find(Atom_ID);
            begin
                if Atom_ID_To_Part_Maps.Has_Element(C) then
                    return Atom_ID_To_Part_Maps.Element(C);
                else
                    declare
                        New_Part : constant Part_Access_Type
                          := new Part_Type;
                    begin
                        New_Part.Root_Node := new Atom_Type;
                        New_Part.Root_Node.ID := Atom_ID;

                        Atom_ID_To_Part_Map.Insert(Atom_ID, New_Part);
                        return New_Part;
                    end;
                end if;
            end Create_Or_Get;

            Atom_ID_To_Part_Map_Length : constant Count_Type := Count_Type'Input(Stream);
        begin
            for I in 1 .. Atom_ID_To_Part_Map_Length loop
                declare
                    Key   : Atom_ID_Type;
                    Value : Part_Access_Type;
                begin


                    Key   := Atom_ID_Type'Input(Stream);
                    Value := Create_Or_Get(Key);

                    -- Root_Node (Atom_[Access_]Type)
                    if Atom_ID_Type'Input(Stream) /= Value.Root_Node.ID then
                        raise MLSE_Error with "Wrong Value.Root_Node.ID";
                    end if;


                    Value.Root_Node.Form  := Unbounded_String'Input(Stream);
                    Value.Root_Node.Lemma := Unbounded_String'Input(Stream);
                    Value.Root_Node.POS   := Unbounded_String'Input(Stream);

                    -- Root_Node->Dependents (Atom_[Access_]Type)
                    declare
                        Dependents_Length : constant Count_Type
                          := Count_Type'Input(Stream);
                    begin
                        for J in 1 .. Dependents_Length loop
                            declare
                                Deprel          : constant Unbounded_String
                                  := Unbounded_String'Input(Stream);

                                Atom_Set_Length : constant Count_Type
                                  := Count_Type'Input(Stream);

                                Atom_Set : Atom_Sets.Set;
                            begin
                                for K in 1 .. Atom_Set_Length loop
                                    declare
                                        Atom_ID : constant Atom_ID_Type
                                          := Atom_ID_Type'Input(Stream);
                                    begin
                                        Atom_Set.Insert(Create_Or_Get(Atom_ID).Root_Node);
                                    end;
                                end loop;

                                Value.Root_Node.Dependents.Insert(Deprel, Atom_Set);
                            end;
                        end loop;
                    end;

                    Value.Rel_Type_Index := Rel_Type_Index_Type'Input(Stream);
                    Value.Cluster_Index  := Cluster_Index_Type'Input(Stream);
                    Value.Cluster_Access := Cluster_Map.Element(Value.Cluster_Index);

                    if Boolean'Input(Stream) then
                        Value.Governor_Part := Create_Or_Get(Atom_ID_Type'Input(Stream));
                    end if;

                    Value.Deprel := Unbounded_String'Input(Stream);
                    Value.Deprel_Index := Deprel_Index_Type'Input(Stream);
                    Value.Argument_Index_In_Governor_Part := Part_Index_Type'Input(Stream);

                    -- Argument_Part_Map : Part_Maps.Map;
                    declare
                        Argument_Part_Map_Length : constant Count_Type
                          := Count_Type'Input(Stream);
                    begin
                        for J in 1 .. Argument_Part_Map_Length loop
                            declare
                                Part_Index : constant Part_Index_Type
                                  := Part_Index_Type'Input(Stream);

                                Part_Atom_ID : constant Atom_ID_Type
                                  := Atom_ID_Type'Input(Stream);
                            begin
                                Value.Argument_Part_Map.Insert
                                  (Part_Index,
                                   Create_Or_Get(Part_Atom_ID));
                            end;
                        end loop;
                    end;

                    Value.Next_Argument_Index := Part_Index_Type'Input(Stream);
                    Value.Argument_Index_To_Argument_Cluster_Index_Map := Part_Index_To_Argument_Cluster_Index_Maps.Map'Input(Stream);
                    Value.Argument_Cluster_Index_To_Dependent_Indexes_Map := Argument_Cluster_Index_To_Part_Indexes_Maps.Map'Input(Stream);
                end;
            end loop;
        end;

        --Destroy(USS_Access);
        Ada.Streams.Stream_IO.Close(File);
    end Deserialize;

    ----------------------------------------------------------------------------
    procedure Serialize_Agenda(Agenda : in Agenda_Type; Filename : in String) is
    ----------------------------------------------------------------------------

        File   : Ada.Streams.Stream_IO.File_Type;
        Stream : Ada.Streams.Stream_IO.Stream_Access;
    begin

        Ada.Streams.Stream_IO.Create(File, Ada.Streams.Stream_IO.Out_File, Filename);
        Stream := Ada.Streams.Stream_IO.Stream(File);

        Agenda_Type'Output(Stream, Agenda);

        Ada.Streams.Stream_IO.Close(File);
    end Serialize_Agenda;

    -------------------------------------------------------------------------------
    procedure Deserialize_Agenda(Filename : in String; Agenda : out Agenda_Type) is
    -------------------------------------------------------------------------------

        File   : Ada.Streams.Stream_IO.File_Type;
        Stream : Ada.Streams.Stream_IO.Stream_Access;
    begin
        Ada.Streams.Stream_IO.Open(File, Ada.Streams.Stream_IO.In_File, Filename);
        Stream := Ada.Streams.Stream_IO.Stream(File);

        Agenda := Agenda_Type'Input(Stream);

        Ada.Streams.Stream_IO.Close(File);
    end Deserialize_Agenda;

    --------------------------------------
    procedure Free_Deserialized_Objects is
    --------------------------------------
    begin

        -- Cluster Map
        declare
            C : Cluster_Maps.Cursor
              := Cluster_Map.First;
        begin
            while Cluster_Maps.Has_Element(C) loop
                declare
                    Cluster_Access : Cluster_Access_Type
                      := Cluster_Maps.Element(C);

                    C2             : Argument_Cluster_Maps.Cursor
                      := Cluster_Access.Argument_Cluster_Map.First;
                begin
                    while Argument_Cluster_Maps.Has_Element(C2) loop
                        declare
                            Argument_Cluster_Access : Argument_Cluster_Access_Type
                              := Argument_Cluster_Maps.Element(C2);
                        begin
                            Free(Argument_Cluster_Access);
                        end;
                        Argument_Cluster_Maps.Next(C2);
                    end loop;

                    Free(Cluster_Access);
                end;
                Cluster_Maps.Next(C);
            end loop;

            Cluster_Map.Clear;
        end;

        -- Cluster_Index_To_Atom_ID_Set_Map
        Cluster_Index_To_Atom_ID_Set_Map.Clear;

        -- Rel_Type_Str_Vector
        Rel_Type_Str_Vector.Clear;

        -- Atom_ID_To_Part_Map
        declare
            Cursor : Atom_ID_To_Part_Maps.Cursor := Atom_ID_To_Part_Map.First;
        begin
            while Atom_ID_To_Part_Maps.Has_Element(Cursor) loop
                declare
                    Part_Access : Part_Access_Type := Atom_ID_To_Part_Maps.Element(Cursor);
                begin
                    Free(Part_Access.Root_Node);
                    Free(Part_Access);
                end;
                Atom_ID_To_Part_Maps.Next(Cursor);
            end loop;

            Atom_ID_To_Part_Map.Clear;
        end;
    end Free_Deserialized_Objects;

    --------------------------------
    procedure Debug_Agenda_For_Merge
    --------------------------------
      (Agenda : in Agenda_Type) is
    begin

        if not Agenda.Inactive_Agenda_To_Score_Map.Is_Empty then
            raise MLSE_Error with "Inactive_Agenda_To_Score_Map not empty!";
        elsif not Agenda.Active_Agenda_To_Score_Map.Is_Empty then
            raise MLSE_Error with "Active_Agenda_To_Score_Map not empty!";
        elsif not Agenda.Score_Active_Agenda.Is_Empty then
            raise MLSE_Error with "Score_Active_Agenda not empty!";
        elsif not Agenda.Cluster_Index_To_Search_Operation_Set_Map.Is_Empty then
            raise MLSE_Error with "Cluster_Index_To_Search_Operation_Set_Map not empty!";
        end if;

        T_IO.Put_Line("----- <Debug_Agenda_For_Merge> -----");

        --Agenda.Agenda_To_Score_Set
        for Search_Operation of Agenda.Agenda_To_Score_Set loop
            T_IO.Put("ATS: ");
            for I of Search_Operation.Repr.all loop
                T_IO.Put(Str(I) & ", ");
            end loop;
            T_IO.New_Line;
            --T_IO.Put_Line("ATS {" & Str(Search_Operation.Str) & "}");
        end loop;

        --Agenda.Compose_To_Count_Map
        declare
            C : Compose_To_Count_Maps.Cursor
              := Agenda.Compose_To_Count_Map.First;
        begin
            while Compose_To_Count_Maps.Has_Element(C) loop
                declare
                    Search_Operation : constant Search_Operation_Type := Compose_To_Count_Maps.Key(C);
                    Count            : constant Integer               := Compose_To_Count_Maps.Element(C);
                begin
                    T_IO.Put("CTCM: ");
                    for I of Search_Operation.Repr.all loop
                        T_IO.Put(Str(I) & ", ");
                    end loop;
                    T_IO.Put_Line("=>" & Count'Img);
                    --T_IO.Put_Line("CTCM {" & Str(Search_Operation.Str) & "} =>" & Count'Img);
                end;
                Compose_To_Count_Maps.Next(C);
            end loop;
        end;

        -- Agenda.Search_Operation_To_Neighs_Set_Map
        declare
            C : Search_Operation_To_Neighs_Set_Maps.Cursor
              := Agenda.Search_Operation_To_Neighs_Set_Map.First;
        begin
            while Search_Operation_To_Neighs_Set_Maps.Has_Element(C) loop
                declare
                    Search_Operation : constant Search_Operation_Type := Search_Operation_To_Neighs_Set_Maps.Key(C);
                    Neighs_Set       : constant Neighs_Sets.Set       := Search_Operation_To_Neighs_Set_Maps.Element(C);
                begin
                    for Item of Neighs_Set loop
                        T_IO.Put("SOTNSM: ");
                        for I of Search_Operation.Repr.all loop
                            T_IO.Put(Str(I) & ", ");
                        end loop;
                        T_IO.Put_Line("=>" & Item'Img);
                        --T_IO.Put_Line("SOTNSM {" & Str(Search_Operation.Str) & "} =>" & Item'Img);
                    end loop;
                end;

                Search_Operation_To_Neighs_Set_Maps.Next(C);
            end loop;
        end;

        T_IO.Put_Line("----- </Debug_Agenda_For_Merge> -----");

    end Debug_Agenda_For_Merge;

end MLSE;
