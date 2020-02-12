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

package Deblog is

    procedure Open;
    procedure Close;

    procedure Put(S : in String);
    procedure Put_Line(S : in String);

    procedure Force_Exit;

    Deblog_Exception : exception;

private
    File_Name : constant String       := "deblog_ada.log";
    Is_Open   : Boolean               := False;
    File      : Ada.Text_IO.File_Type;

end Deblog;
