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

private with Ada.Real_Time;
private with Ada.Strings.Hash;
private with Ada.Containers.Indefinite_Hashed_Maps;

package TimeAll is

    procedure Start(Key : in String);
    procedure Stop(Key : in String);
    procedure Reset;

    function Get(Key : in String) return Float;
    procedure Print(Key : in String);
    procedure Print_All;
    procedure Print_Ratio(Key1, Key2: in String);

    Already_Started,
    Unknown_Key,
    Never_Started: exception;

private

    type Time_Record is record
        Started : Boolean            := False;
        Start   : Ada.Real_Time.Time := Ada.Real_Time.Time_First;
        Sum     : Float              := 0.0;
    end record;

    package Subjects_Hashed_Maps is
        new Ada.Containers.Indefinite_Hashed_Maps
        (Key_Type        => String,
         Element_Type    => Time_Record,
         Hash            => Ada.Strings.Hash,
         Equivalent_Keys => "=");

    Subjects_Map : Subjects_Hashed_Maps.Map;

    procedure Print(Key : in String; F : in Float);

end TimeALL;
