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
with Ada.Containers.Vectors;

package XML_Configurations is

    type Create_Agenda_Remote_Node_Type is record
        Address : Unbounded_String;
        Port    : Integer := -1;
        Tasks   : Integer := -1;
    end record;

    package Create_Agenda_Remote_Nodes_Vectors is new
      Ada.Containers.Vectors
        (Index_Type   => Positive,
         Element_Type => Create_Agenda_Remote_Node_Type);

    -----

    Create_Agenda_Local_Tasks  : Integer := -1;
    -- Number of Create_Agenda tasks to be performed locally

    Create_Agenda_Remote_Tasks : Integer := -1;
    -- Number of Create_Agenda tasks to be performed on remote nodes

    Create_Agenda_Remote_Nodes : Create_Agenda_Remote_Nodes_Vectors.Vector;
    -- List of all slave nodes

    procedure Load(XML_Filename : in String);

    XML_Configuration_Error : exception;

end XML_Configurations;
