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

with AWS; use AWS;
with AWS.Config.Set;
with AWS.Server;
with AWS.Status;
with AWS.Response;
with AWS.Messages;
with AWS.MIME;
with AWS.Attachments;
with AWS.Parameters;

with Ada.Text_IO;

with Commons; use Commons;
with MLSE; use MLSE;
with Utils;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Command_Line;
with XML_Configurations;
with Create_Agenda_Manager; use Create_Agenda_Manager;

procedure Parse_Slave is

    package T_IO renames Ada.Text_IO;

    function Process_Agenda_Server_Callback
      (Request : Status.Data)
       return Response.Data is

        Chrono : Utils.Chronometer_Type;

        Attachments : AWS.Attachments.List;
        Attachment  : AWS.Attachments.Element;

        Request_URI : constant String := AWS.Status.URI(Request);

        Params      : constant AWS.Parameters.List
          := AWS.Status.Parameters(Request);

        First_Cluster_Index : Integer := -1;
        Last_Cluster_Index  : Integer := -1;
    begin

        T_IO.Put_Line("[" & Request_URI & "]");

        if Request_URI = "/tasks_count" then
            T_IO.Put_Line("Request: Tasks_Count");

            return Response.Build("text/plain", Str(XML_Configurations.Create_Agenda_Local_Tasks + XML_Configurations.Create_Agenda_Remote_Tasks));

        elsif Request_URI = "/create_agenda" then
            T_IO.Put_Line("Request: Create_Agenda");

            AWS.Server.Get_Message_Body; -- Force file upload

            First_Cluster_Index := Integer'Value(Params.Get("first_cluster_index"));
            Last_Cluster_Index  := Integer'Value(Params.Get("last_cluster_index"));

            T_IO.Put_Line("    First_Cluster_Index: " & Str(First_Cluster_Index));
            T_IO.Put_Line("    Last_Cluster_Index:  " & Str(Last_Cluster_Index));

            -----
            -- Check and get Attachments
            -----

            Attachments := Status.Attachments(Request);
            if AWS.Attachments.Count(Attachments) /= 1 then

                T_IO.Put_Line("    Bad attachments. Error 404. - " & AWS.Attachments.Count(Attachments)'Img);

                return Response.Acknowledge
                  (Status_Code  => Messages.S404,
                   Message_Body => "Bad attachments.",
                   Content_Type => MIME.Text_Plain);
            end if;

            Attachment := AWS.Attachments.Get(Attachments, 1);

            -----
            -- Deserialize clusters etc...
            -----

            T_IO.Put_Line("    Deserialize...");
            Chrono.Chronometer_Start;

            Deserialize(AWS.Attachments.Local_Filename(Attachment)); -- Uploaded file

            Chrono.Chronometer_Stop_And_Print;

            declare
                --Agenda : Agenda_Type;
            begin

                -----
                -- Create Agenda
                -----

                T_IO.New_Line;
                T_IO.Put_Line("    Create Agenda...");
                Chrono.Chronometer_Start;

                Advanced_Create_Agenda
                  (Cluster_Index_Start => First_Cluster_Index,
                   Cluster_Index_End   => Last_Cluster_Index);

                Chrono.Chronometer_Stop_And_Print;

                --Debug_Agenda_For_Merge(Public_Agenda);

                -----
                -- Free public objects
                -----

                T_IO.Put_Line("(Deserialize public objects...)");
                Free_Deserialized_Objects;

                -----
                -- Serialize Sgenda
                -----

                T_IO.Put_Line("    Serialize Agenda...");
                Chrono.Chronometer_Start;

                Serialize_Agenda(Public_Agenda, "_serialized_agenda_slave.ser");

                Chrono.Chronometer_Stop_And_Print;

                --Debug_Agenda_For_Merge(Agenda); -- TODO: Agenda Debug
            end;

            -----

            T_IO.Put_Line("    Response...");

            return Response.File
              (Content_Type  => MIME.Application_Octet_Stream,
               Filename      => "_serialized_agenda_slave.ser");

        else
            return Response.Build
              (Content_Type  => MIME.Text_Plain,
               Message_Body  => "Invalid URI.",
               Status_Code   => Messages.S404);
        end if;

    end Process_Agenda_Server_Callback;

    Web_Server : Server.HTTP;
    Web_Config : Config.Object;

    XML_Configuration_Filename: Unbounded_String;

begin

    -----
    -- Arguments
    -----

    if Ada.Command_Line.Argument_Count /= 1 then
        T_IO.Put_Line(T_IO.Standard_Error, "  Usage: ./parse_slave <XML_Configuration>");
        return;
        --XML_Configuration_Filename := UStr("./parse_slave_config.xml");
    end if;

    XML_Configuration_Filename := UStr(Ada.Command_Line.Argument(1));

    -----
    -- Load XML Configurations
    -----

    XML_Configurations.Load(Str(XML_Configuration_Filename));

    -----
    -- Server
    -----

    Config.Set.Server_Name(Web_Config, "USP Parse Slave");
    Config.Set.Server_Host(Web_Config, "0.0.0.0");
    Config.Set.Server_Port(Web_Config, 8080);
    Config.Set.Reuse_Address(Web_Config, True);
    Config.Set.Max_Connection(Web_Config, 1);
    Config.Set.Upload_Directory(Web_Config, "./");
    Config.Set.Log_File_Directory(Web_Config, "./");
    Config.Set.Log_Filename_Prefix(Web_Config, "USP_Parse_Slave_Log_");
    Config.Set.Log_Split_Mode(Web_Config, "Each_Run");
    Config.Set.Error_Log_Filename_Prefix(Web_Config, "USP_Parse_Slave_Error_");
    Config.Set.Error_Log_Split_Mode(Web_Config, "Each_Run");
    Config.Set.Max_Concurrent_Download(1);

    Ada.Text_IO.Put_Line("""" & Config.Server_Name(Web_Config) & """ server started.");

    Server.Start
      (Web_Server => Web_Server,
       Callback   => Process_Agenda_Server_Callback'Unrestricted_Access,
       Config     => Web_Config);

    Server.Wait(Server.Forever); -- TODO: wait for single task response, not forever

    T_IO.Put_Line("Server shutdown...");
    Server.Shutdown(Web_Server);

end Parse_Slave;

