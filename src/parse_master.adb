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
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
--  with Ada.Sequential_IO;
--  with AWS.Client;
--  with AWS.Response;
--  with AWS.MIME;
--  with AWS.Resources;

with Commons; use Commons;
with MLSE; use MLSE;
with Corpus; use Corpus;
with Utils; use Utils;
with XML_Configurations;
with Create_Agenda_Manager; use Create_Agenda_Manager;

procedure Parse_Master is

    package T_IO renames Ada.Text_IO;

    Articles : Article_Sets.Set;
    --Agenda   : aliased Agenda_Type;

    Chrono, All_Chrono   : Chronometer_Type;

    Articles_Directory,
    Result_Directory,
    XML_Configuration_Filename: Unbounded_String;

begin

    -----
    -- Arguments
    -----

    if Ada.Command_Line.Argument_Count /= 3 then
        T_IO.Put_Line(T_IO.Standard_Error, "  Usage: ./parse_master <XML_Configuration> <Articles_Directory> <Result_Directory>");
        return;
    end if;

    XML_Configuration_Filename := UStr(Ada.Command_Line.Argument(1));
    Articles_Directory         := UStr(Ada.Command_Line.Argument(2));
    Result_Directory           := UStr(Ada.Command_Line.Argument(3));

    -----
    -- Load XML Configurations
    -----

    XML_Configurations.Load(Str(XML_Configuration_Filename));

    All_Chrono.Chronometer_Start;

    --------
    -- Read articles
    --------

    T_IO.Put_Line("Read syntax...");
    Chrono.Chronometer_Start;

    Read_Articles(Directory_Name => Str(Articles_Directory),
                  Articles       => Articles,
                  Ignore_Deprel  => True);

    Chrono.Chronometer_Stop_And_Print;

    -- Print(Articles);

    -----
    -- Initialize
    -----

    T_IO.New_Line;
    T_IO.Put_Line("Initial partition/clust/agenda ...");
    Chrono.Chronometer_Start;

    Initialize(Articles);

    Chrono.Chronometer_Stop_And_Print;

    -----
    -- Merge args for bootstrap clusters
    -----

    T_IO.New_Line;
    T_IO.Put_Line("Merge args for bootstrap clusters...");
    Chrono.Chronometer_Start;

    Merge_Arguments;

    Chrono.Chronometer_Stop_And_Print;

    -----
    -- Advanced Agenda Creation
    -----

    T_IO.New_Line;
    T_IO.Put_Line("Advanced agenda creation...");
    Chrono.Chronometer_Start;

    Advanced_Create_Agenda
      (Cluster_Index_Start => 1,
       Cluster_Index_End   => Get_Total_Clusters_Count);

    --Create_Agenda(Public_Agenda, False);
    --Press_A_Key_To_Continue;

    Chrono.Chronometer_Stop_And_Print;

    --Debug_Agenda_For_Merge(Public_Agenda);

    --T_IO.Put_Line("Wait....");
    --delay 1000.0;

    --Free_Agenda(Public_Agenda);
    --Press_A_Key_To_Continue;

    -----
    -- Process Agenda
    -----

    T_IO.New_Line;
    T_IO.Put_Line("Process Agenda...");
    Chrono.Chronometer_Start;

    Process_Agenda(Public_Agenda);

    Chrono.Chronometer_Stop_And_Print;

    -----
    -- Print Model
    -----

    T_IO.New_Line;
    T_IO.Put_Line("Print Model...");
    Chrono.Chronometer_Start;

    Print_Model(Str(Result_Directory));

    Chrono.Chronometer_Stop_And_Print;

    --------------------------------------

    T_IO.Put_Line("Total execution time:");
    All_Chrono.Chronometer_Stop_And_Print;

    T_IO.Put_Line("The End. Bye!");

    --Press_A_Key_To_Continue;

end Parse_Master;

