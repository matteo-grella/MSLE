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

with Input_Sources.File;
with DOM.Readers;
with DOM.Core;
with DOM.Core.Documents;
with DOM.Core.Nodes;
with DOM.Core.Elements;
with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Commons; use Commons;

with Ada.Text_IO;

with AWS;
with AWS.Client;
with AWS.Response;

package body XML_Configurations is

    procedure Load
      (XML_Filename : in String) is
        Input  : Input_Sources.File.File_Input;
        Reader : DOM.Readers.Tree_Reader;
        Doc    : DOM.Core.Document;
    begin

        Input_Sources.File.Open(XML_Filename, Input);
        DOM.Readers.Parse(Reader, Input);
        Input_Sources.File.Close(Input);

        Doc := DOM.Readers.Get_Tree(Reader);

        declare
            use Ada.Strings;
            use Ada.Strings.Maps;
            use Ada.Strings.Fixed;
            use DOM.Core;

            Trim_Chars : constant Character_Set
              := Ada.Strings.Maps.To_Set(Character_Sequence'(1 => ' ', 2 => ASCII.HT, 3 => ASCII.CR, 4 => ASCII.LF));

            N_Parse_Master : constant Node
              := Nodes.Item(Documents.Get_Elements_By_Tag_Name(Doc, "parse_master"), 0);
        begin

            -- <create_agenda_local>
            declare
                N_Create_Agenda_Local : constant Node
                  := Nodes.Item(Elements.Get_Elements_By_Tag_Name(N_Parse_Master, "create_agenda_local"), 0);

                N_Enabled             : constant Node
                  := Nodes.Item(Elements.Get_Elements_By_Tag_Name(N_Create_Agenda_Local, "enabled"), 0);
                Enabled               : constant Boolean
                  := Boolean'Value(Trim(Nodes.Node_Value(Nodes.First_Child(N_Enabled)), Trim_Chars, Trim_Chars)); -- Text node

                N_Tasks               : constant Node
                  := Nodes.Item(Elements.Get_Elements_By_Tag_Name(N_Create_Agenda_Local, "tasks"), 0);
                Tasks                 : constant Integer
                  := Integer'Value(Trim( Nodes.Node_Value( Nodes.First_Child(N_Tasks) ), Trim_Chars, Trim_Chars)); -- Text node
            begin
                if not Enabled then
                    Create_Agenda_Local_Tasks := 0;

                else
                    if Tasks <= 0 then
                        raise XML_Configuration_Error with "Invalid <create_agenda_local>/<tasks> value.";
                    end if;
                    Create_Agenda_Local_Tasks := Tasks;
                end if;
            end;

            -- <create_agenda_remote_nodes>
            declare
                N_Create_Agenda_Remote_Nodes : constant Node
                  := Nodes.Item(Elements.Get_Elements_By_Tag_Name(N_Parse_Master, "create_agenda_remote_nodes"), 0);

                N_Nodes                      : constant Node_List
                  := Elements.Get_Elements_By_Tag_Name(N_Create_Agenda_Remote_Nodes, "node");
            begin

                for I in 1 .. Nodes.Length(N_Nodes) loop
                    declare
                        N_Node : constant Node
                          := Nodes.Item(N_Nodes, I - 1);

                        N_Address             : constant Node
                          := Nodes.Item(Elements.Get_Elements_By_Tag_Name(N_Node, "address"), 0);
                        Address               : constant String
                          := Trim(Nodes.Node_Value(Nodes.First_Child(N_Address)), Trim_Chars, Trim_Chars); -- Text node

                        N_Port                : constant Node
                          := Nodes.Item(Elements.Get_Elements_By_Tag_Name(N_Node, "port"), 0);
                        Port                  : constant Positive
                          := Positive'Value(Trim( Nodes.Node_Value( Nodes.First_Child(N_Port) ), Trim_Chars, Trim_Chars)); -- Text node
                    begin
                        Create_Agenda_Remote_Nodes.Append
                          ((Address => UStr(Address), Port => Port, Tasks => 0));
                    end;
                end loop;
            end;

        end;

        DOM.Readers.Free(Reader);

        Ada.Text_IO.Put_Line(Ada.Text_IO.Standard_Error, "Create Agenda Local Tasks: " & Str(Create_Agenda_Local_Tasks));

        -----
        -- Count remote tasks
        -----

        Create_Agenda_Remote_Tasks := 0;

        for Index in Create_Agenda_Remote_Nodes.First_Index .. Create_Agenda_Remote_Nodes.Last_Index loop
            declare
                Node : Create_Agenda_Remote_Node_Type
                  := Create_Agenda_Remote_Nodes.Element(Index);

                URL : constant String
                  := "http://" & Str(Node.Address) & ":" & Str(Node.Port);

                Response : constant AWS.Response.Data
                  := AWS.Client.Get(URL & "/tasks_count");
            begin

                --Node.Tasks

                Node.Tasks := Positive'Value(AWS.Response.Message_Body(Response));

                if Node.Tasks < 1 then
                    raise XML_Configuration_Error with "Node.Tasks < 1";
                end if;

                Create_Agenda_Remote_Tasks := Create_Agenda_Remote_Tasks + Node.Tasks;

                Ada.Text_IO.Put_Line(Ada.Text_IO.Standard_Error, "  (Remote Node: " & URL & " => " & Str(Node.Tasks) & ")");

                Create_Agenda_Remote_Nodes.Replace_Element(Index, Node);
            end;
        end loop;

        Ada.Text_IO.Put_Line(Ada.Text_IO.Standard_Error, "Create Agenda Remote Tasks: " & Str(Create_Agenda_Remote_Tasks));

        if Create_Agenda_Local_Tasks + Create_Agenda_Remote_Tasks < 1 then
            raise XML_Configuration_Error with "No local nor remote tasks";
        end if;

    end Load;

end XML_Configurations;
