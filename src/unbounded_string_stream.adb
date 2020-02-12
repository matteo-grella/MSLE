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

with Ada.Unchecked_Deallocation;
with Ada.Strings; use Ada.Strings;
with Ada.Text_IO;

package body Unbounded_String_Stream is

    function Create
      (USA : in  Unbounded_String_Access) return USS_Access_Type is
        USS_Access : constant USS_Access_Type
          := new USS_Type;
    begin
        USS_Access.Acc := USA;
        return USS_Access;
    end Create;

    procedure Destroy
      (S : in out USS_Access_Type) is
        procedure Free is new Ada.Unchecked_Deallocation
          (Object => USS_Type,
           Name   => USS_Access_Type);
    begin
        Free(S);
    end Destroy;

    procedure Read
      (Stream : in out USS_Type;
       Item   : out    Stream_Element_Array;
       Last   : out    Stream_Element_Offset) is
        S_Last : Positive renames Length(Stream.Acc.all);
    begin
        Last := Item'First - 1;
        for I in Stream.Next_First .. S_Last loop
            if Last = Item'Last then
                Stream.Next_First := I;
                exit;
            end if;
            Last := Last + 1;
            Item(Last) := Stream_Element(Character'Pos(Element(Stream.Acc.all, I)));
        end loop;
    end Read;

    procedure Write
      (Stream : in out USS_Type;
       Item   : in     Stream_Element_Array) is
        S : String(Natural(Item'First) .. Natural(Item'Last));
    begin
        for I in Item'Range loop
            S(Natural(I)) := Character'Val(Item(I));
        end loop;
        Append(Stream.Acc.all, S);
    end Write;

    function Get_Stream
      (S : USS_Access_Type) return Stream_Access is
    begin
        return Stream_Access(S);
    end Get_Stream;

    procedure Test is
        S       : aliased Unbounded_String;
        Stream  : Unbounded_String_Stream.USS_Access_Type
          := Create(S'Unchecked_Access);

        SAccess : constant Unbounded_String_Stream.Stream_Access
          := Get_Stream(Stream);
    begin

        declare
            Number  : constant Integer := 42;
        begin
            Integer'Write(SAccess, Number);
            Ada.Text_IO.Put_Line("Input :" & Number'Img);
        end;

        declare
            Number : Integer := -1;
        begin
            Integer'Read(SAccess, Number);
            Ada.Text_IO.Put_Line("Output:" & Number'Img);
        end;

        Unbounded_String_Stream.Destroy(Stream);
    end Test;

end Unbounded_String_Stream;
