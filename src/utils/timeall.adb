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

private with Ada.Text_IO;
private with Ada.Float_Text_IO;

package body TimeAll is

    procedure Start(Key : in String) is
    begin
        if not Subjects_Map.Contains(Key) then
            Subjects_Map.Insert
              (Key,
               Time_Record'(
                 Started => True,
                 Start   => Ada.Real_Time.Clock,
                 Sum     => 0.0));
        else
            declare
                TR : Time_Record := Subjects_Map.Element(Key);
                use type Ada.Real_Time.Time;
            begin
                if TR.Started or TR.Start /= Ada.Real_Time.Time_First then
                    raise Already_Started
                      with "Key """ & Key & """ already started.";
                end if;

                TR.Started := True;
                TR.Start   := Ada.Real_Time.Clock;

                Subjects_Map.Replace(Key, TR);
            end;
        end if;
    end Start;

    procedure Stop(Key : in String) is
        TR        : Time_Record;
        Stop_Time : Ada.Real_Time.Time;
        Diff      : Float := -1.0;
        use type Ada.Real_Time.Time;
    begin
        if not Subjects_Map.Contains(Key) then
            raise Unknown_Key with "Unkwnown key """ & Key & """.";
        end if;

        TR := Subjects_Map.Element(Key);

        if not TR.Started or TR.Start = Ada.Real_Time.Time_First then
            raise Never_Started
              with "Key """ & Key & """ never started.";
        end if;

        Stop_Time  := Ada.Real_Time.Clock;
        Diff       := Float(Ada.Real_Time.To_Duration(Stop_Time - TR.Start));
        TR.Sum     := TR.Sum + Diff;
        TR.Start   := Ada.Real_Time.Time_First;
        TR.Started := False;

        Subjects_Map.Replace(Key, TR);
    end Stop;

    function Get(Key : in String) return Float is
    begin
        if not Subjects_Map.Contains(Key) then
            --raise Unknown_Key with "Unkwnown key """ & Key & """.";
            return 0.0;
        else
            return Subjects_Map.Element(Key).Sum;
        end if;

        --return Subjects_Map.Element(Key).Sum;
    end Get;

    procedure Print(Key : in String) is
    begin
        Print(Key, Get(Key));
    end Print;

    procedure Print(Key : in String; F : in Float) is
    begin
        Ada.Text_IO.Put
          (Ada.Text_IO.Standard_Error,
           Key & ": ");

        Ada.Float_Text_IO.Put
          (Ada.Text_IO.Standard_Error,
           F, 1, 9, 0);

        Ada.Text_IO.Put_Line
          (Ada.Text_IO.Standard_Error,
           " s");
    end Print;

    procedure Print_All is
        SubjCursor : Subjects_Hashed_Maps.Cursor;
    begin

        if Subjects_Map.Is_Empty then
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error,
               "TimeAll: no keys-time recorded.");
            return;
        end if;

        SubjCursor := Subjects_Map.First;
        while Subjects_Hashed_Maps.Has_Element(SubjCursor) loop
            Print
              (Subjects_Hashed_Maps.Key(SubjCursor),
               Subjects_Hashed_Maps.Element(SubjCursor).Sum);
            SubjCursor := Subjects_Hashed_Maps.Next(SubjCursor);
        end loop;

    end Print_All;


    procedure Reset is
    begin

	Subjects_Map.Clear;

    end Reset;


    procedure Print_Ratio(Key1, Key2: in String) is
        F1, F2, Ratio: Float := -1.0;
    begin
        F1 := Get(Key1);
        F2 := Get(Key2);
        Ratio := F1 / F2 * 100.0;

        Ada.Text_IO.Put
          (Ada.Text_IO.Standard_Error,
           Key1 & "/" & Key2 & " = ");

        Ada.Float_Text_IO.Put
          (Ada.Text_IO.Standard_Error,
           Ratio, 1, 3, 0);

        Ada.Text_IO.Put
          (Ada.Text_IO.Standard_Error,
           "%  (");

        Ada.Float_Text_IO.Put
          (Ada.Text_IO.Standard_Error,
           F1, 1, 9, 0);

        Ada.Text_IO.Put
          (Ada.Text_IO.Standard_Error,
           "/");

        Ada.Float_Text_IO.Put
          (Ada.Text_IO.Standard_Error,
           F2, 1, 9, 0);

        Ada.Text_IO.Put_Line
          (Ada.Text_IO.Standard_Error,
           ")");
    end Print_Ratio;

end TimeALL;
