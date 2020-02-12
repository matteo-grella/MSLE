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
private with Ada.Strings.Fixed;

package body Ini_Reader is

	use Ada.Text_IO;
	use Ada.Strings.Fixed;

	function Ini_Trim(S : Unbounded_String) return Unbounded_String is
	begin
		return Trim
		  (Source => S,
		   Left   => Trim_Chars_Set,
		   Right  => Trim_Chars_Set);
	end Ini_Trim;

	function Ini_Trim(S : String) return String is
	begin
		return Trim
		  (Source => S,
		   Left   => Trim_Chars_Set,
		   Right  => Trim_Chars_Set);
	end Ini_Trim;

	procedure Load
	  (Obj       : out Ini_Reader_Object;
           File_Name : in  String) is

		File                  : File_Type;
		Last_Section_Position : Sections_Maps.Cursor;
		Map_Inserted          : Boolean;
	begin

		-- First empty section
		Sections_Maps.Insert
		  (Container => Obj.Sections,
		   Key       => "",
		   New_Item  => Empty_Values_Map,
		   Position  => Last_Section_Position,
		   Inserted  => Map_Inserted);

		if not Map_Inserted then
			raise Insertion_Error
			  with "Cannot insert empty section.";
		end if;

		-- Object filename

		Obj.File_Name := To_Unbounded_String(File_Name);

		-- Read ini file line by line

		Open(File, In_File, File_Name);

		while not End_Of_File(File) loop
			declare
				Line : constant String
				  := Ini_Trim(Get_Line(File));

				Equal_Index  : Integer;
				Section_Name : Unbounded_String;
				Name, Value  : Unbounded_String;
			begin
				-- Skipping empty and comment lines
				if Line'Length > 0
				  and then Line(Line'First) /= ';'
				  and then Line(Line'First) /= '#' then

					-- Section line
					if Line(Line'First) = '[' and
					  Line(Line'Last) = ']' then

						Section_Name
						  := To_Unbounded_String
						    (Ini_Trim(Line(Line'First + 1 .. Line'Last - 1)));

						if Length(Section_Name) = 0 then
							raise Ini_File_Line_Error
							  with "Invalid section name: " & Line;
						end if;

						-- Insert new section

						Sections_Maps.Insert
						  (Container => Obj.Sections,
						   Key       => To_String(Section_Name),
						   New_Item  => Empty_Values_Map,
						   Position  => Last_Section_Position,
						   Inserted  => Map_Inserted);

						if not Map_Inserted then
							raise Insertion_Error
							  with "Cannot insert section: " & To_String(Section_Name);
						end if;

					else -- "name=value" line

						Equal_Index := Index(Line, "=");

						If Equal_Index < Line'First then
							raise Ini_File_Line_Error
							  with "Missing '=': " & Line;
						end if;

						Name := Ini_Trim
						  (To_Unbounded_String(Line(Line'First .. Equal_Index - 1)));

						if Length(Name) = 0 then
							raise Ini_File_Line_Error
							  with "Invalid name: " & Line;
						end if;

						Value := Ini_Trim
						  (To_Unbounded_String(Line(Equal_Index + 1 .. Line'Last)));

						if Length(Value) = 0 then
							raise Ini_File_Line_Error
							  with "Invalid value: " & Line;
						end if;

						-- Insert new name/value

						declare
							procedure Add_Name_Value
							  (Key     : String;
	                                                   Element : in out Values_Maps.Map) is
								pragma Unreferenced (Key);
							begin
								Values_Maps.Insert
								  (Container => Element,
								   Key       => To_String(Name),
								   New_Item  => Value);
							end Add_Name_Value;
						begin
							Sections_Maps.Update_Element
							  (Container => Obj.Sections,
							   Position  => Last_Section_Position,
							   Process   => Add_Name_Value'Access);
						end;

					end if;

				end if;
			end;
		end loop;

		Close(File);

	end Load;

	procedure Load
	  (Obj       : out Ini_Reader_Object;
           File_Name : in  Unbounded_String) is
	begin
		Load(Obj, To_String(File_Name));
	end Load;

	procedure Show_Content
	  (Obj : in Ini_Reader_Object) is

		Section_Position : Sections_Maps.Cursor;
		Value_Position   : Values_Maps.Cursor;
		Value_Element    : Values_Maps.Map;
	begin
		New_Line;
		Put_Line("INI File Name: " & To_String(Obj.File_Name));
		New_Line;

		Section_Position := Sections_Maps.First(Obj.Sections);
		while Sections_Maps.Has_Element(Section_Position) loop

			Put_Line("[" & Sections_Maps.Key(Section_Position) & "]");

			Value_Element  := Sections_Maps.Element(Section_Position);
			Value_Position := Value_Element.First;
			while Values_Maps.Has_Element(Value_Position) loop

				Put_Line
				  (Values_Maps.Key(Value_Position) & " = "
				   & To_String(Values_Maps.Element(Value_Position)));

				Value_Position := Values_Maps.Next(Value_Position);
			end loop;

			New_Line;

			Section_Position := Sections_Maps.Next(Section_Position);
		end loop;

	end Show_Content;

	-----
	-- Get: Unbounded_String
 	-----

	function Get
	  (Obj      : in Ini_Reader_Object;
	   Section  : in String;
	   Name     : in String) return Unbounded_String is

		Section_Position : Sections_Maps.Cursor;
		Value_Element    : Values_Maps.Map;
		Value_Position   : Values_Maps.Cursor;

		use Sections_Maps;
		use Values_Maps;
	begin
		Section_Position := Sections_Maps.Find(Obj.Sections, Section);

		if Section_Position = Sections_Maps.No_Element then
			raise Section_Not_Found with "Section: " & Section;
		end if;

		Value_Element  := Sections_Maps.Element(Section_Position);
		Value_Position := Values_Maps.Find(Value_Element, Name);

		if Value_Position = Values_Maps.No_Element then
			raise Name_Not_Found with "Name: " & Name;
		end if;

		return Values_Maps.Element(Value_Position);
	end Get;

	function Get
	  (Obj      : in Ini_Reader_Object;
	   Section  : in String;
	   Name     : in String;
	   Default  : in Unbounded_String) return Unbounded_String is
	begin
		return Get(Obj, Section, Name);
	exception
		when Section_Not_Found | Name_Not_Found =>
			return Default;
	end Get;

	-----
	-- Get: String
 	-----

	function Get
	  (Obj      : in Ini_Reader_Object;
	   Section  : in String;
	   Name     : in String) return String is
	begin
		return To_String(Get(Obj, Section, Name));
	end Get;

	function Get
	  (Obj      : in Ini_Reader_Object;
	   Section  : in String;
	   Name     : in String;
	   Default  : in String) return String is
	begin
		return To_String(Get(Obj, Section, Name));
	exception
		when Section_Not_Found | Name_Not_Found =>
			return Default;
	end Get;

	-----
	-- Get: Integer
 	-----

	function Get
	  (Obj      : in Ini_Reader_Object;
	   Section  : in String;
	   Name     : in String) return Integer is
	begin
		return Integer'Value(Get(Obj, Section, Name));
	end Get;

	function Get
	  (Obj      : in Ini_Reader_Object;
	   Section  : in String;
	   Name     : in String;
	   Default  : in Integer) return Integer is
	begin
		return Integer'Value(Get(Obj, Section, Name));
	exception
		when Section_Not_Found | Name_Not_Found =>
			return Default;
	end Get;

	-----
	-- Get: Long_Float
 	-----

	function Get
	  (Obj      : in Ini_Reader_Object;
	   Section  : in String;
	   Name     : in String) return Long_Float is
	begin
		return Long_Float'Value(Get(Obj, Section, Name));
	end Get;

	function Get
	  (Obj      : in Ini_Reader_Object;
	   Section  : in String;
	   Name     : in String;
	   Default  : in Long_Float) return Long_Float is
	begin
		return Long_Float'Value(Get(Obj, Section, Name));
	exception
		when Section_Not_Found | Name_Not_Found =>
			return Default;
	end Get;


	-----
	-- Get: Float
 	-----

	function Get
	  (Obj      : in Ini_Reader_Object;
	   Section  : in String;
	   Name     : in String) return Float is
	begin
		return Float'Value(Get(Obj, Section, Name));
	end Get;

	function Get
	  (Obj      : in Ini_Reader_Object;
	   Section  : in String;
	   Name     : in String;
	   Default  : in Float) return Float is
	begin
		return Float'Value(Get(Obj, Section, Name));
	exception
		when Section_Not_Found | Name_Not_Found =>
			return Default;
	end Get;

	-----
	-- Get: Boolean
 	-----

	function Get
	  (Obj      : in Ini_Reader_Object;
	   Section  : in String;
	   Name     : in String) return Boolean is
	begin
		return Boolean'Value(Get(Obj, Section, Name));
	end Get;

	function Get
	  (Obj      : in Ini_Reader_Object;
	   Section  : in String;
	   Name     : in String;
	   Default  : in Boolean) return Boolean is
	begin
		return Boolean'Value(Get(Obj, Section, Name));
	exception
		when Section_Not_Found | Name_Not_Found =>
			return Default;
	end Get;

end Ini_Reader;
