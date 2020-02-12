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

with Ada.Streams; use Ada.Streams;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Unbounded_String_Stream is

    type Unbounded_String_Access is access all Unbounded_String;

    type Stream_Access is access all Root_Stream_Type'Class;
    type USS_Access_Type is limited private;

    function Create
      (USA : in  Unbounded_String_Access) return USS_Access_Type;
    pragma Inline(Create);

    procedure Destroy
      (S : in out USS_Access_Type);
    pragma Inline(Destroy);

    function Get_Stream
      (S : USS_Access_Type) return Stream_Access;
    pragma Inline(Get_Stream);

    procedure Test;

private

    type USS_Type is new Ada.Streams.Root_Stream_Type with record
        Acc        : Unbounded_String_Access := null;
        Next_First : Natural := 1;
    end record;

    type USS_Access_Type is access all USS_Type;

    procedure Read
      (Stream : in out USS_Type;
       Item   : out    Stream_Element_Array;
       Last   : out    Stream_Element_Offset);
    pragma Inline(Read);

    procedure Write
      (Stream : in out USS_Type;
       Item   : in     Stream_Element_Array);
    pragma Inline(Write);

end Unbounded_String_Stream;
