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

with Commons; use Commons;
with Utils; use Utils;
with Ada.Unchecked_Deallocation;
with XML_Configurations;

package Create_Agenda_Manager is

    Public_Agenda        : Agenda_Type;
    Public_Agenda_Mutex  : Mutex_Type;

    type Cluster_Index_Mutex_Array_Type is array (Cluster_Index_Type range <>) of Mutex_Type;
    type Cluster_Index_Mutex_Array_Access_Type is access all Cluster_Index_Mutex_Array_Type;
    procedure Free is new Ada.Unchecked_Deallocation
      (Object => Cluster_Index_Mutex_Array_Type, Name => Cluster_Index_Mutex_Array_Access_Type);

    Cluster_Index_Mutex  : Cluster_Index_Mutex_Array_Access_Type := null;

    --type Aegnda_Access_Array_Type is array (Positive range <>) of Agenda_Access_Type;
    --type Aegnda_Access_Array_Access_Type is access all Aegnda_Access_Array_Type;
    --procedure Free is new Ada.Unchecked_Deallocation(Aegnda_Access_Array_Type, Aegnda_Access_Array_Access_Type);
    --Local_Agenda_Array : Aegnda_Access_Array_Access_Type := null;

    procedure Advanced_Create_Agenda
        (Cluster_Index_Start, Cluster_Index_End : Positive);

    protected type Agenda_Range_Consumer_Type is
        procedure Setup(First_Cluster_Index, Last_Cluster_Index : Cluster_Index_Type);
        procedure Pop (Cluster_Index : out Cluster_Index_Type);
    private
        First_Index : Cluster_Index_Type := -1;
        Last_Index  : Cluster_Index_Type := -1;
        Cur_Index   : Cluster_Index_Type := -1;
    end Agenda_Range_Consumer_Type;

    Local_Agenda_Range_Consumer : Agenda_Range_Consumer_Type;

private

    Remaining_Tasks : Integer := 0;

    -- Local

    task type Create_Agenda_Local_Task is
        entry Start(ID : Natural);
        entry Wait;
    end Create_Agenda_Local_Task;

    type Create_Agenda_Local_Task_Array is
      array (Positive range <>) of Create_Agenda_Local_Task;

    -- Remote

    task type Create_Agenda_Remote_Task is
        entry Start(ID : Natural; First_Cluster_Index, Last_Cluster_Index : Positive; Node : XML_Configurations.Create_Agenda_Remote_Node_Type);
        entry Wait;
    end Create_Agenda_Remote_Task;

    type Create_Agenda_Remote_Task_Array is
      array (Positive range <>) of Create_Agenda_Remote_Task;

end Create_Agenda_Manager;
