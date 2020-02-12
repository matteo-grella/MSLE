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

with MLSE; use MLSE;

with Ada.Text_IO;

with AWS;
with AWS.Client;
with AWS.Response;
with AWS.MIME;
with AWS.Resources;
with Ada.Sequential_IO;
with Ada.Streams;

package body Create_Agenda_Manager is

    package T_IO renames Ada.Text_IO;

    --------------------------------
    procedure Advanced_Create_Agenda
    --------------------------------
      (Cluster_Index_Start, Cluster_Index_End : Positive) is

        Num_Of_Clusters   : constant Positive := Cluster_Index_End - Cluster_Index_Start + 1;
        Num_Of_Atom_IDs   : constant Positive := Clusters_Atom_IDs_Count(Cluster_Index_Type(Cluster_Index_Start), Cluster_Index_Type(Cluster_Index_End));

        Atom_IDs_Per_Task : Natural := 0;

        Num_Of_Tasks      : constant Positive
          := XML_Configurations.Create_Agenda_Local_Tasks + XML_Configurations.Create_Agenda_Remote_Tasks;
        -- Total number of local and remote tasks

    begin

        if XML_Configurations.Create_Agenda_Local_Tasks = -1 then
            raise MLSE_Error with "Create_Agenda_Local_Tasks = -1";
        elsif XML_Configurations.Create_Agenda_Remote_Tasks = -1 then
            raise MLSE_Error with "Create_Agenda_Remote_Tasks = -1";
        end if;

        if XML_Configurations.Create_Agenda_Remote_Tasks = 0 and then XML_Configurations.Create_Agenda_Local_Tasks = 1 then
            T_IO.Put_Line("<Simple_Create_Agenda>");
            Create_Agenda
              (Agenda              => Public_Agenda,
               Use_Consumer        => False,
               Cluster_Index_Start => Cluster_Index_Type(Cluster_Index_Start),
               Cluster_Index_End   => Cluster_Index_Type(Cluster_Index_End));
            T_IO.Put_Line("</Simple_Create_Agenda>");
            return;
        end if;

        T_IO.Put_Line("<Advanced_Create_Agenda>");

        -- When remote tasks exists, serialize clusters etc.
        if XML_Configurations.Create_Agenda_Remote_Tasks > 0 then
            T_IO.New_Line;
            T_IO.Put_Line("Serialization (for remote agenda)...");
            --Chrono.Chronometer_Start;

            Serialize("./_serialized_clusters_master.ser");

            T_IO.Put_Line("Done.");
            --Chrono.Chronometer_Stop_And_Print;
        end if;

        -- Create mutex for each cluster
        Cluster_Index_Mutex := new Cluster_Index_Mutex_Array_Type(1 .. Cluster_Index_Type(Get_Total_Clusters_Count));

        Atom_IDs_Per_Task := Positive(Float'Ceiling(Float(Num_Of_Atom_IDs) / Float(Num_Of_Tasks)));

        T_IO.Put_Line("  - Clusters_Count    = " & Str(Num_Of_Clusters) & " (" & Str(Cluster_Index_Start) & " .. " & Str(Cluster_Index_End) & ")");
        T_IO.Put_Line("  - Num_Of_Atom_IDs   = " & Str(Num_Of_Atom_IDs));
        T_IO.Put_Line("  - Num_Of_Tasks      = " & Str(Num_Of_Tasks));
        T_IO.Put_Line("  - Atom_IDs_Per_Task = " & Str(Atom_IDs_Per_Task));

        -----
        -- Create and start tasks
        -----

        declare
            Local_Tasks        : Create_Agenda_Local_Task_Array(1 .. XML_Configurations.Create_Agenda_Local_Tasks);
            Remote_Tasks       : Create_Agenda_Remote_Task_Array(1 .. Integer(XML_Configurations.Create_Agenda_Remote_Nodes.Length));

            Cur_Cluster_Index  : Positive := Cluster_Index_Start;

            Total_Local_First_Index : constant Integer := Cluster_Index_Start;
            Total_Local_Last_Index  : Integer := -1;
        begin

            -----
            -- Local
            -----

            for I in Local_Tasks'Range loop
                declare
                    First_Cluster_Index : constant Positive
                      := Cur_Cluster_Index;
                    Last_Cluster_Index  : Positive
                      := First_Cluster_Index;

                    Atom_IDs_In_Range_Count : Natural
                      := Atom_IDs_Count(Cluster_Index_Type(Last_Cluster_Index));
                begin

                    while Last_Cluster_Index < Cluster_Index_End
                      and then Atom_IDs_In_Range_Count < Atom_IDs_Per_Task loop

                        Last_Cluster_Index := Last_Cluster_Index + 1;
                        Atom_IDs_In_Range_Count := Atom_IDs_In_Range_Count +
                          Atom_IDs_Count(Cluster_Index_Type(Last_Cluster_Index));
                    end loop;

                    if I = Local_Tasks'Last and then XML_Configurations.Create_Agenda_Remote_Nodes.Is_Empty then
                        Last_Cluster_Index := Cluster_Index_End;
                    end if;

                    -- TODO: agenda access & mutex

                    T_IO.Put_Line(" Atom_IDs for local task " & Str(I) & ": " & Atom_IDs_In_Range_Count'Img);

                    Total_Local_Last_Index := Last_Cluster_Index;

                    Cur_Cluster_Index := Last_Cluster_Index + 1;
                    exit when Cur_Cluster_Index > Cluster_Index_End;
                end;
            end loop;

            Local_Agenda_Range_Consumer.Setup
              (First_Cluster_Index => Cluster_Index_Type(Total_Local_First_Index),
               Last_Cluster_Index  => Cluster_Index_Type(Total_Local_Last_Index));

            T_IO.Put_Line("Local Cluster Index Range (Consumer): " & Str(Total_Local_First_Index) & " .. " & Str(Total_Local_Last_Index));

            -----
            -- Remote
            -----

            for I in Remote_Tasks'Range loop
                declare
                    First_Cluster_Index : constant Positive
                      := Cur_Cluster_Index;
                    Last_Cluster_Index  : Positive
                      := First_Cluster_Index;

                    Atom_IDs_In_Range_Count : Natural
                      := Atom_IDs_Count(Cluster_Index_Type(Last_Cluster_Index));

                    Remote_Node : constant XML_Configurations.Create_Agenda_Remote_Node_Type
                      := XML_Configurations.Create_Agenda_Remote_Nodes.Element(I);

                begin

                    -- FIXME: non contemplo quanti task ha ciascun nodo remoto!

                    for Node_Task_Counter in 1 .. Remote_Node.Tasks loop

                        while Last_Cluster_Index < Cluster_Index_End
                          and then Atom_IDs_In_Range_Count < Atom_IDs_Per_Task loop

                            Last_Cluster_Index := Last_Cluster_Index + 1;
                            Atom_IDs_In_Range_Count := Atom_IDs_In_Range_Count +
                              Atom_IDs_Count(Cluster_Index_Type(Last_Cluster_Index));
                        end loop;

                        if I = Remote_Tasks'Last then
                            Last_Cluster_Index := Cluster_Index_End;
                            exit; -- break
                        end if;
                    end loop;

                    -- TODO: agenda access & mutex

                    T_IO.Put_Line(" Atom_IDs for remote task " & Str(I) & ": " & Atom_IDs_In_Range_Count'Img);

                    Remote_Tasks(I).Start(I, First_Cluster_Index, Last_Cluster_Index, Remote_Node);

                    Total_Local_Last_Index := Last_Cluster_Index;

                    Cur_Cluster_Index := Last_Cluster_Index + 1;
                    exit when Cur_Cluster_Index > Cluster_Index_End;
                end;
            end loop;

            for I in Local_Tasks'Range loop
                Local_Tasks(I).Start(I);
            end loop;

            -----
            -- Waiting for tasks end
            -----

            for I in Local_Tasks'Range loop
                Local_Tasks(I).Wait;
            end loop;

            for I in Remote_Tasks'Range loop
                Remote_Tasks(I).Wait;
            end loop;

            T_IO.Put_Line("All tasks done.");

            Free(Cluster_Index_Mutex);

        end;

        T_IO.Put_Line("</Advanced_Create_Agenda>");

    end Advanced_Create_Agenda;

    -------------------------------------
    task body Create_Agenda_Local_Task is
    -------------------------------------

        Task_ID      : Natural            := 0;
    begin

        -- TODO Start OR Wait

        accept Start(ID : Natural) do
            Task_ID             := ID;

            Remaining_Tasks := Remaining_Tasks + 1;
            T_IO.Put_Line("    [Local Task " & Str(Task_ID) & ": start]");
        end;

        declare
            Local_Agenda : Agenda_Type;
        begin

            -- Create Agenda

            Create_Agenda(Local_Agenda, True);

            -- Merge Agenda

            Public_Agenda_Mutex.Seize;
            T_IO.Put("    [Local Task " & Str(Task_ID) & ": merge ..."); T_IO.Flush;

            --Press_A_Key_To_Continue;

            Merge_Agenda(Public_Agenda, Local_Agenda);

            Minimize_Agenda(Local_Agenda);
            Minimize_Agenda(Public_Agenda);

            Remaining_Tasks := Remaining_Tasks - 1;

            T_IO.Put_Line(" wait] (Remaining:" & Remaining_Tasks'Img & ")");
            Public_Agenda_Mutex.Release;

        end;

        accept Wait do
            T_IO.Put_Line("    [Local Task " & Str(Task_ID) & ": end]");
        end;

    end Create_Agenda_Local_Task;

    --------------------------------------
    task body Create_Agenda_Remote_Task is
    --------------------------------------

        Task_ID      : Natural            := 0;
        First_Index, Last_Index : Integer := -1;
        Remote_Node  : XML_Configurations.Create_Agenda_Remote_Node_Type;
    begin

        -- TODO Start OR Wait

        accept Start(ID : Natural; First_Cluster_Index, Last_Cluster_Index : Positive; Node : XML_Configurations.Create_Agenda_Remote_Node_Type) do
            Task_ID     := ID;
            First_Index := First_Cluster_Index;
            Last_Index  := Last_Cluster_Index;
            Remote_Node := Node;

            Remaining_Tasks := Remaining_Tasks + 1;
            T_IO.Put_Line("    [Remote Task " & Str(Task_ID) & ": start] (" & Str(First_Index) & " .. " & Str(Last_Index) & ") | http://" & Str(Remote_Node.Address) & ":" & Str(Remote_Node.Port));
        end;

        declare
            Connection  : AWS.Client.HTTP_Connection;
            Result      : AWS.Response.Data;
            Attachments : AWS.Client.Attachment_List;
        begin

            AWS.Client.Set_Debug(False);

            Attachments.Add("./_serialized_clusters_master.ser", "_serialized_clusters_master");

            AWS.Client.Create
              (Connection  => Connection,
               Host        => "http://" & Str(Remote_Node.Address) & ":" & Str(Remote_Node.Port) & "/create_agenda?first_cluster_index=" & Str(First_Index) & "&last_cluster_index=" & Str(Last_Index),
               Retry       => 0,
               Persistent  => True,
               Timeouts    => AWS.Client.No_Timeout,
               Server_Push => False);

            AWS.Client.Post
              (Connection   => Connection,
               Result       => Result,
               Data         => "",
               Content_Type => AWS.MIME.Application_Octet_Stream,
               Attachments  => Attachments);

            -- Read message and write agenda to file
            declare
                Ret_File : AWS.Resources.File_Type;

                package Stream_IO is new Ada.Sequential_IO(Ada.Streams.Stream_Element);
                File     : Stream_IO.File_Type;

                Buffer   : Ada.Streams.Stream_Element_Array(1 .. 1_000_000); -- TODO: qualcosa di meglio?
                Last     : Ada.Streams.Stream_Element_Offset;
            begin

                AWS.Response.Message_Body(Result, Ret_File);

                Stream_IO.Create(File, Stream_IO.Out_File, "_task" & Str(Task_ID) & "_serialized_agenda_master.ser");

                while not AWS.Resources.End_Of_File(Ret_File) loop
                    AWS.Resources.Read(Ret_File, Buffer, Last);
                    for I in Buffer'First .. Last loop
                        Stream_IO.Write(File, Buffer(I));
                    end loop;
                end loop;

                Stream_IO.Close(File);
                AWS.Resources.Close(Ret_File);
            end;

            AWS.Client.Close(Connection);
        end;

        -----
        -- Deserialize and Merge Agenda
        -----

        declare
            Local_Agenda : Agenda_Type;
        begin
            -- Deserialize Agenda
            Deserialize_Agenda("_task" & Str(Task_ID) & "_serialized_agenda_master.ser", Local_Agenda);

            -- Merge Agenda

            Public_Agenda_Mutex.Seize;
            T_IO.Put("    [Remote Task " & Str(Task_ID) & ": merge ..."); T_IO.Flush;

            Merge_Agenda(Public_Agenda, Local_Agenda);

            Minimize_Agenda(Local_Agenda);
            Minimize_Agenda(Public_Agenda);

            Remaining_Tasks := Remaining_Tasks - 1;

            T_IO.Put_Line(" wait] (Remaining:" & Remaining_Tasks'Img & ")");
            Public_Agenda_Mutex.Release;

        end;

        accept Wait do
            T_IO.Put_Line("    [Remote Task " & Str(Task_ID) & ": end]");
        end;

    end Create_Agenda_Remote_Task;

    --------------------------------------------
    protected body Agenda_Range_Consumer_Type is
    --------------------------------------------
        procedure Setup(First_Cluster_Index, Last_Cluster_Index : Cluster_Index_Type) is
        begin
            First_Index := First_Cluster_Index;
            Last_Index  := Last_Cluster_Index;
            Cur_Index   := First_Index;
        end Setup;

        procedure Pop (Cluster_Index : out Cluster_Index_Type) is
        begin
            if Cur_Index <= Last_Index then
                Cluster_Index := Cur_Index;
                Cur_Index := Cur_Index + 1;
            else
                Cluster_Index := -1;
            end if;
        end Pop;
    end Agenda_Range_Consumer_Type;

end Create_Agenda_Manager;

--      -----
--      -- Serialization
--      -----
--
--      T_IO.New_Line;
--      T_IO.Put_Line("Serialization (for remote agenda)...");
--      Chrono.Chronometer_Start;
--
--      Serialize("/Users/marco/work/USP/ada_usp/bin_debug/serialized_clusters_master.ser");
--
--      Chrono.Chronometer_Stop_And_Print;
--
--      -----
--      -- Remote agenda
--      -----
--
--      T_IO.New_Line;
--      T_IO.Put_Line("Remote agenda...");
--      Chrono.Chronometer_Start;
--
--
--      declare
--          Connection  : AWS.Client.HTTP_Connection;
--          Result      : AWS.Response.Data;
--          Attachments : AWS.Client.Attachment_List;
--      begin
--          AWS.Client.Set_Debug(False);
--
--          Attachments.Add("/Users/marco/work/USP/ada_usp/bin_debug/serialized_clusters_master.ser", "serialized_clusters_master");
--
--          AWS.Client.Create
--            (Connection  => Connection,
--             Host        => "http://127.0.0.1:8080",
--             Retry       => 0,
--             Persistent  => True,
--             Timeouts    => AWS.Client.No_Timeout,
--             Server_Push => False);
--
--          T_IO.Put_Line("    Send POST message...");
--
--          AWS.Client.Post
--            (Connection   => Connection,
--             Result       => Result,
--             Data         => "",
--             Content_Type => AWS.MIME.Application_Octet_Stream,
--             Attachments  => Attachments);
--
--          T_IO.Put_Line("    Receiving data...");
--
--          -- Read message and write agenda to file
--          declare
--              Ret_File : AWS.Resources.File_Type;
--
--              package Stream_IO is new Ada.Sequential_IO(Ada.Streams.Stream_Element);
--              File     : Stream_IO.File_Type;
--
--              Buffer   : Ada.Streams.Stream_Element_Array(1 .. 1_000_000); -- TODO: qualcosa di meglio?
--              Last     : Ada.Streams.Stream_Element_Offset;
--
--          begin
--              AWS.Response.Message_Body(Result, Ret_File);
--
--              Stream_IO.Create(File, Stream_IO.Out_File, "serialized_agenda_master.ser");
--
--              while not AWS.Resources.End_Of_File(Ret_File) loop
--                  AWS.Resources.Read(Ret_File, Buffer, Last);
--                  for I in Buffer'First .. Last loop
--                      Stream_IO.Write(File, Buffer(I));
--                  end loop;
--              end loop;
--
--              Stream_IO.Close(File);
--
--              AWS.Resources.Close(Ret_File);
--          end;
--
--          T_IO.Put_Line(AWS.Response.Message_Body(Result));
--
--
--          AWS.Client.Close(Connection);
--      end;
--
--      Chrono.Chronometer_Stop_And_Print;
--
--      -----
--      -- Deserialize Agenda
--      -----
--
--      T_IO.New_Line;
--      T_IO.Put_Line("Deserialize Agenda...");
--      Chrono.Chronometer_Start;
--
--      Deserialize_Agenda("serialized_agenda_master.ser", Agenda);
--
--      Chrono.Chronometer_Stop_And_Print;
