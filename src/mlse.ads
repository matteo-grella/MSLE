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

with Commons; use Commons;

private with Configurations;
with Utils; use Utils;

package MLSE is

    -------------
    -- Procedures
    -------------

    procedure Initialize
      (Articles : in Article_Sets.Set);

    function Get_Rel_Type_Index
      (Atom : Atom_Type) return Rel_Type_Index_Type;
    pragma Inline(Get_Rel_Type_Index);

    function Get_Clusters_With_Rel_Type
      (Rel_Type_Index : Rel_Type_Index_Type) return Cluster_Index_Sets.Set;
    pragma Inline(Get_Clusters_With_Rel_Type);

    function Create_Cluster
      (Rel_Type_Index : Rel_Type_Index_Type;
       Is_Content     : Boolean := False) return Cluster_Index_Type;

    procedure Link_Part_And_Cluster
      (Part           : in out Part_Type;
       Cluster_Access : in     Cluster_Access_Type);

    function Add_Argument
      (Governor_Part         : in out Part_Type;
       Dependent_Part_Access : in     Part_Access_Type;
       Deprel                : in     Unbounded_String;
       Deprel_Index          : in     Deprel_Index_Type) return Part_Index_Type;
    pragma Inline(Add_Argument);

    procedure Remove_Argument_From_Part
      (Part                                      : in out Part_Type;
       Dependent_Argument_Index_In_Governor_Part : in     Part_Index_Type);
    -- Needed when ExecAbs.

    procedure Unset_Governor_Part
      (Part : in out Part_Type);

    procedure Set_Governor_Part
      (Dependent_Part        : in out Part_Type;
       Governor_Part_Access  : in     Part_Access_Type;
       Argument_Index        : in     Part_Index_Type);

    function Create_Argument_Cluster
      (Cluster        : in out Cluster_Type;
       Deprel_Index   : in     Deprel_Index_Type) return Argument_Cluster_Index_Type;

    procedure Set_Argument_Cluster
      (Part                   : in out Part_Type;
       Argument_Index         : in     Part_Index_Type;
       Argument_Cluster_Index : in     Argument_Cluster_Index_Type);

    procedure Unset_Argument_Cluster
      (Part                   : in out Part_Type;
       Argument_Index         : in     Part_Index_Type);

    procedure On_Part_Set_Argument
      (Cluster                    : in out Cluster_Type;
       Governor_Part              : in     Part_Type;
       Argument_Part              : in     Part_Type;
       Argument_Cluster_Index     : in     Argument_Cluster_Index_Type;
       Old_Argument_Cluster_Index : in     Argument_Cluster_Index_Type);

    procedure On_Part_Unset_Argument
      (Cluster                : in out Cluster_Type;
       Governor_Part          : in     Part_Type;
       Argument_Part          : in     Part_Type;
       Argument_Cluster_Index : in     Argument_Cluster_Index_Type);

    procedure Merge_Arguments;

    function Score_Merge_Arguments
      (Cluster                  : in Cluster_Type;
       Argument_Cluster_Index_1 : in Argument_Cluster_Index_Type;
       Argument_Cluster_Index_2 : in Argument_Cluster_Index_Type) return Score_Type;

    procedure Create_Agenda
      (Agenda              : in out Agenda_Type;
       Use_Consumer        : in     Boolean;
       Cluster_Index_Start : in     Cluster_Index_Type := -1;
       Cluster_Index_End   : in     Cluster_Index_Type := -1);
    pragma Inline(Create_Agenda);

    procedure Merge_Agenda
      (To_Agenda   : in out Agenda_Type;
       From_Agenda : in out Agenda_Type);
    pragma Inline(Merge_Agenda);
    -- Merge from From_Agenda to To_Agenda.
    -- While merging, From_Agenda is proressively completely cleared.

    procedure Free_Agenda
      (Agenda : in out Agenda_Type);

    procedure Minimize_Agenda
      (Agenda : in out Agenda_Type);
    -- Minimize Reserve_Capacity in agenda elements

    procedure Add_Agenda_For_New_Cluster
      (Agenda                    : in out Agenda_Type;
       New_Cluster_Index         : in     Cluster_Index_Type;
       Atom_ID_To_Part_Map_Local : in     Atom_ID_To_Part_Maps.Map);
    pragma Inline(Add_Agenda_For_New_Cluster);

    procedure Process_Agenda
      (Agenda : in out Agenda_Type);

    procedure Add_Agenda_After_Merge_Clust
      (Agenda : in out Agenda_Type;
       Part1  : in     Part_Type;
       Part2  : in     Part_Type);

    procedure Add_Agenda_MC
      (Agenda          : in out Agenda_Type;
       Cluster_1       : in     Cluster_Type;
       Cluster_2       : in     Cluster_Type;
       Neigh_Type      : in     Neigh_Type_Type);

    procedure Add_Agenda_Abs
      (Agenda                  : in out Agenda_Type;
       Parent_Cluster_Index    : in     Cluster_Index_Type;
       Dependent_Cluster_Index : in     Cluster_Index_Type);

    function Move_Agenda_To_Score
      (Agenda                  : in out Agenda_Type;
       Search_Operation        : in out Search_Operation_Type) return Boolean;

    procedure Add_Agenda
      (Agenda           : in out Agenda_Type;
       Search_Operation : in     Search_Operation_Type;
       Score            : in     Score_Type);

    function Execute_Operation
      (Search_Operation : in Search_Operation_Type) return Cluster_Index_Type;
    pragma Inline(Execute_Operation);

    function Execute_Operation_MC
      (Cluster_Index_1, Cluster_Index_2 : in Cluster_Index_Type) return Cluster_Index_Type;

    function Execute_Operation_Compose
      (Governor_Cluster_Index, Dependent_Cluster_Index : in Cluster_Index_Type) return Cluster_Index_Type;

    procedure Atom_Add_Dependent
      (Governor_Atom  : in out Atom_Type;
       Deprel         : in     Unbounded_String;
       Dependent_Atom : in     Atom_Access_Type);
    pragma Inline(Atom_Add_Dependent);

    procedure Update_Agenda_After_Exec
      (Agenda            : in out Agenda_Type;
       Search_Operation  : in Search_Operation_Type;
       New_Cluster_Index : in Cluster_Index_Type);
    pragma Inline(Update_Agenda_After_Exec);

    procedure Update_Agenda_After_Exec_MC
      (Agenda            : in out Agenda_Type;
       Search_Operation  : in     Search_Operation_Type;
       New_Cluster_Index : in     Cluster_Index_Type);

    procedure Update_Agenda_After_Exec_Abs
      (Agenda            : in out Agenda_Type;
       Search_Operation  : in     Search_Operation_Type;
       New_Cluster_Index : in     Cluster_Index_Type);

    procedure Remove_Agenda
      (Agenda            : in out Agenda_Type;
       Search_Operation  : in     Search_Operation_Type);

    procedure Change_Cluster
      (Part               : in out Part_Type;
       New_Cluster_Access : in     Cluster_Access_Type;
       New_Rel_Type_index : in     Rel_Type_Index_Type);

    procedure Change_Cluster
      (Part                                                     : in out Part_Type;
       New_Cluster_Access                                       : in     Cluster_Access_Type;
       Argument_Cluster_Index_To_New_Argument_Cluster_Index_Map : in     Argument_Cluster_Index_To_Argument_Cluster_Index_Maps.Map);
    -- Change clust and remap argclust -> used in MC

    procedure Set_Rel_Type
      (Part               : in out Part_Type;
       New_Rel_Type_Index : in     Rel_Type_Index_Type);
    pragma Inline(Set_Rel_Type);

    procedure Unset_Rel_Type
      (Part : in out Part_Type);
    pragma Inline(Unset_Rel_Type);

    procedure Destroy_Part
      (Part_Access : in Part_Access_Type);
    pragma Inline(Destroy_Part);

    procedure Score_MC_For_Align
      (Cluster_1                                                : in     Cluster_Type;
       Cluster_2                                                : in     Cluster_Type;
       Argument_Cluster_Index_2_To_Argument_Cluster_Index_1_Map : in out Argument_Cluster_Index_To_Argument_Cluster_Index_Maps.Map);
    pragma Inline(Score_MC_For_Align);

    function Score_MC_For_Align
      (Cluster_1                                                : in     Cluster_Type;
       Cluster_2                                                : in     Cluster_Type;
       Argument_Cluster_Index_2_To_Argument_Cluster_Index_1_Map : in out Argument_Cluster_Index_To_Argument_Cluster_Index_Maps.Map) return Score_Type;

    function Score_Operation
      (Search_Operation : in Search_Operation_Type) return Score_Type;
    pragma Inline(Score_Operation);

    function Score_Operation_MC
      (Cluster_Index_1, Cluster_Index_2 : in Cluster_Index_Type) return Score_Type;

    function Score_MC_For_Parent
      (Cluster_Index_1, Cluster_Index_2 : in Cluster_Index_Type) return Score_Type;

    function Score_Operation_Compose
      (Governor_Cluster_Index, Dependent_Cluster_Index : in Cluster_Index_Type) return Score_Type;

    procedure Print_Model(Result_Directory : in String);

    function To_String(Argument_Cluster : in Argument_Cluster_Type) return Unbounded_String;
    pragma Inline(To_String);
    -- String representation of an Argument_Cluster.

    function To_String(Cluster : in Cluster_Type) return Unbounded_String;
    pragma Inline(To_String);
    -- String representation of a Cluster.

    procedure Generate_Search_Operation_String(Search_Operation : in out Search_Operation_Type);
    pragma Inline(Generate_Search_Operation_String);
    -- Creates the Str value in the Search_Operation.

    function Get_Total_Clusters_Count return Natural;
    pragma Inline(Get_Total_Clusters_Count);

    function Clusters_Atom_IDs_Count(Cluster_Index_Start, Cluster_Index_End : Cluster_Index_Type) return Natural;
    pragma Inline(Clusters_Atom_IDs_Count);

    function Atom_IDs_Count(Cluster_Index : in Cluster_Index_Type) return Natural;
    pragma Inline(Atom_IDs_Count);

    -----
    -- Master/Slave Serialization
    -----

    procedure Serialize(Filename : in String);
    procedure Deserialize(Filename : in String);
    procedure Serialize_Agenda(Agenda : in Agenda_Type; Filename : in String);
    procedure Deserialize_Agenda(Filename : in String; Agenda : out Agenda_Type);

    -----
    -- Free
    -----

    procedure Free_Deserialized_Objects;
    -- Free only the public objects  by Deserialize

    -----
    -- Debug
    -----

    procedure Debug_Agenda_For_Merge(Agenda : in Agenda_Type);


private

    ------------------
    -- Shared Memories
    ------------------

    Atom_ID_To_Atom_Map                     : Atom_ID_To_Atom_Maps.Map;
    Rel_Type_Str_Vector                     : Rel_Type_Str_Vectors.Vector;
    Rel_Type_Str_To_Index_Map               : Rel_Type_Str_To_Index_Maps.Map;
    Atom_ID_To_Part_Map                     : Atom_ID_To_Part_Maps.Map;
    Rel_Type_Index_To_Cluster_Index_Set_Map : Rel_Type_Index_To_Cluster_Index_Set_Maps.Map;
    Cluster_Map                             : Cluster_Maps.Map;
    Next_Cluster_Index                      : Cluster_Index_Type := 1;
    Cluster_Index_To_Atom_ID_Set_Map        : Cluster_Index_To_Atom_ID_Set_Maps.Map;
    Cluster_Index_Root_Count_Map            : Cluster_Index_Root_Count_Maps.Map;
    Deprel_Vector                           : Deprel_Vectors.Vector;
    Deprel_To_Index_Map                     : Deprel_To_Index_Maps.Map;
    Cluster_Index_To_Cluster_Link_Set_Map   : Cluster_Index_To_Cluster_Link_Set_Maps.Map;
    Cluster_Link_To_Atom_Link_Set_Map       : Cluster_Link_To_Atom_Link_Set_Maps.Map;
    Cluster_Index_Pair_To_Conj_Count_Map    : Cluster_Index_Pair_To_Conj_Count_Maps.Map;
    Cluster_Index_To_Cluster_And_Argument_Cluster_Link_Type_To_Count_Map_Map : Cluster_Index_To_Cluster_And_Argument_Cluster_Link_Type_To_Count_Map_Maps.Map;

    -----
    -- Mutex (for parallel Create_Agenda)
    -----

    Cluster_Map_Mutex : Mutex_Type;
    Cluster_Index_To_Atom_ID_Set_Map_Mutex : Mutex_Type;
    Atom_ID_To_Part_Map_Mutex : Mutex_Type;
    Rel_Type_Mutex : Mutex_Type;
    --Part_Argument_Part_Map_Mutex : Mutex_Type;

    ---

    Agenda_Skip_MC      : constant Boolean := False;
    Agenda_Skip_Compose : constant Boolean := False;
    Agenda_Min_Abs_Count_Observed : constant Long_Float
      := Long_Float(Configurations.Min_Abs_Count) * (Long_Float(Configurations.Min_Abs_Count) - 1.0) / 2.0; -- counts equate actual cnt chose 2 // !!!

end MLSE;
