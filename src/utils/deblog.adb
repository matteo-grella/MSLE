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

with Ada.Task_Identification;

package body Deblog is

    procedure Open is
    begin
        if Is_Open then
            raise Deblog_Exception;
        end if;

        Ada.Text_IO.Create
          (File => File,
           Mode => Ada.Text_IO.Out_File,
           Name => File_Name);

        Is_Open := True;

        Put_Line("[Deblog.Open;]");
    end Open;

    procedure Close is
    begin
        if not Is_Open then
            raise Deblog_Exception;
        end if;

        Put_Line("[Deblog.Close;]");

        Ada.Text_IO.Close(File);

        Is_Open := False;
    end Close;

    procedure Put(S : in String) is
    begin
        if not Is_Open then
            Open;
        end if;
        Ada.Text_IO.Put(File, S);
        Ada.Text_IO.Flush(File);
    end Put;

    procedure Put_Line(S : in String) is
    begin
        if not Is_Open then
            Open;
        end if;
        Ada.Text_IO.Put_Line(File, S);
        Ada.Text_IO.Flush(File);
    end Put_Line;

    procedure Force_Exit is
    begin
        Put_Line("[Deblog.Force_Exit;]");
        Close;
        Ada.Task_Identification.Abort_Task(Ada.Task_Identification.Current_Task);
    end Force_Exit;

end Deblog;
