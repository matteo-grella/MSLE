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

with Ada.Strings.Unbounded;
with Ada.Strings.Maps;
with Ada.Strings.Hash;
with Ada.Containers.Indefinite_Hashed_Maps;


package Ini_Reader is

	Insertion_Error,
	Section_Not_Found,
	Name_Not_Found,
	Ini_File_Line_Error : exception;

	use Ada.Strings.Unbounded;

	type Ini_Reader_Object is private;

	procedure Load
	  (Obj       : out Ini_Reader_Object;
           File_Name : in  String);

	procedure Load
	  (Obj       : out Ini_Reader_Object;
           File_Name : in  Unbounded_String);

	procedure Show_Content
	  (Obj : in Ini_Reader_Object);

	-----
	-- Get: Unbounded_String
 	-----

	function Get
	  (Obj      : in Ini_Reader_Object;
	   Section  : in String;
	   Name     : in String) return Unbounded_String;

	function Get
	  (Obj      : in Ini_Reader_Object;
	   Section  : in String;
	   Name     : in String;
	   Default  : in Unbounded_String) return Unbounded_String;

	-----
	-- Get: String
 	-----

	function Get
	  (Obj      : in Ini_Reader_Object;
	   Section  : in String;
	   Name     : in String) return String;

	function Get
	  (Obj      : in Ini_Reader_Object;
	   Section  : in String;
	   Name     : in String;
	   Default  : in String) return String;

	-----
	-- Get: Integer
 	-----

	function Get
	  (Obj      : in Ini_Reader_Object;
	   Section  : in String;
	   Name     : in String) return Integer;

	function Get
	  (Obj      : in Ini_Reader_Object;
	   Section  : in String;
	   Name     : in String;
	   Default  : in Integer) return Integer;

	-----
	-- Get: Long_Float
 	-----

	function Get
	  (Obj      : in Ini_Reader_Object;
	   Section  : in String;
	   Name     : in String) return Long_Float;

	function Get
	  (Obj      : in Ini_Reader_Object;
	   Section  : in String;
	   Name     : in String;
	   Default  : in Long_Float) return Long_Float;


	-----
	-- Get: Float
 	-----

	function Get
	  (Obj      : in Ini_Reader_Object;
	   Section  : in String;
	   Name     : in String) return Float;

	function Get
	  (Obj      : in Ini_Reader_Object;
	   Section  : in String;
	   Name     : in String;
	   Default  : in Float) return Float;

	-----
	-- Get: Boolean
 	-----

	function Get
	  (Obj      : in Ini_Reader_Object;
	   Section  : in String;
	   Name     : in String) return Boolean;

	function Get
	  (Obj      : in Ini_Reader_Object;
	   Section  : in String;
	   Name     : in String;
	   Default  : in Boolean) return Boolean;

private

	use Ada.Strings.Maps;
	use Ada.Containers;

	Trim_Chars_Ranges : Character_Ranges
	  := ((' ', ' '), (ASCII.HT, ASCII.HT));

	Trim_Chars_Set : Character_Set
	  := To_Set(Trim_Chars_Ranges);

	function Ini_Trim(S : Unbounded_String) return Unbounded_String;
	function Ini_Trim(S : String) return String;

	package Values_Maps is new Indefinite_Hashed_Maps
	  (Key_Type        => String,
	   Element_Type    => Unbounded_String,
	   Hash            => Ada.Strings.Hash,
	   Equivalent_Keys => "=");

	Empty_Values_Map : Values_Maps.Map;

	package Sections_Maps is new Indefinite_Hashed_Maps
	  (Key_Type        => String,
	   Element_Type    => Values_Maps.Map,
	   Hash            => Ada.Strings.Hash,
	   Equivalent_Keys => "=",
	   "="             => Values_Maps."=");

	type Ini_Reader_Object is record
		File_Name : Unbounded_String;
		Sections  : Sections_Maps.Map;
	end record;

end Ini_Reader;
