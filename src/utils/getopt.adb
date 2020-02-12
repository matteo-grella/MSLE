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

with Ada.Command_Line;
with Ada.Strings.Unbounded;

package body Getopt is
  package Command_Line renames Ada.Command_Line;
  package US renames Ada.Strings.Unbounded;

  -- current option
  Internal_Optcur : Natural;
  Internal_Optarg : US.Unbounded_String;
  Internal_Optind : Positive;
  Internal_Optopt : Character;

  -- reset Processing (called on elaboration)
  procedure Reset is
  begin
    Internal_Optcur := 2;
    Internal_Optarg := US.To_Unbounded_String ("");
    Internal_Optind := 1;
    Internal_Optopt := ' ';
  end Reset;

  -- accessors
  function optarg return String is
  begin
    return US.To_String (Internal_Optarg);
  end optarg;
  function optind return Positive is
  begin
    return Internal_Optind;
  end optind;
  function optopt return Character is
  begin
    return Internal_Optopt;
  end optopt;

  -- main Processing function
  function Process (Options : String) return Integer is

    -- handle Argument connected to option character '-ffile.txt'
    function Handle_Argument_No_Space (Option_Index : Positive) return Integer is
    begin
      Internal_Optarg :=
        US.To_Unbounded_String (Command_Line.Argument (Internal_Optind)
          (Internal_Optcur + 1 .. Command_Line.Argument (Internal_Optind)'Length));
      Internal_Optind := Internal_Optind + 1;
      return Character'Pos (Options (Option_Index));
    end Handle_Argument_No_Space;

    -- handle Argument not connected to option character '-f file.txt'
    function Handle_Argument_Separate (Option_Index : Positive) return Integer is
    begin
      Internal_Optcur := 2;
      Internal_Optind := Internal_Optind + 1;
      Internal_Optarg := US.To_Unbounded_String (Command_Line.Argument (Internal_Optind));
      Internal_Optind := Internal_Optind + 1;
      return Character'Pos (Options (Option_Index));
    end Handle_Argument_Separate;

    -- handle case of a matching option character with a missing string Argument
    procedure Handle_Missing_Argument is
    begin
      if (Internal_Optcur < Command_Line.Argument (Internal_Optind)'Length) then
        Internal_Optcur := Internal_Optcur + 1;
      else
        Internal_Optcur := 2;
        Internal_Optind := Internal_Optind + 1;
      end if;
    end Handle_Missing_Argument;

    -- Process args or not?
    function Want_Processing return Boolean is
    begin
      if Command_Line.Argument_Count = 0 then
        return False;
      end if;
      if Internal_Optind > Command_Line.Argument_Count then
        return False;
      end if;
      if Command_Line.Argument (Internal_Optind) (1) /= '-' then
        return False;
      end if;
      if Command_Line.Argument (Internal_Optind)'Length = 1 then
        return False;
      end if;
      return True;
    end Want_Processing;

  begin
    -- validity
    if not Want_Processing then
      return Option_EOF;
    end if;

    -- return -1 and Internal_Optind++ upon '--'
    if (Command_Line.Argument (Internal_Optind) (2) = '-') then
      Internal_Optind := Internal_Optind + 1;
      return Option_EOF;
    end if;

    -- apparently valid option found
    for Option_Index in Options'Range loop
      if (Options (Option_Index) = Command_Line.Argument (Internal_Optind) (Internal_Optcur)) then
        if (Option_Index < Options'Length) then
          if (Options (Option_Index + 1) = ':') then

            -- optarg doesn't follow a space?
            if (Command_Line.Argument (Internal_Optind)'Length - Internal_Optcur > 0) then
              return Handle_Argument_No_Space (Option_Index);
            end if;

            -- optarg follows a space?
            if (Internal_Optind < Command_Line.Argument_Count) then
              return Handle_Argument_Separate (Option_Index);
            else
              Internal_Optind := Internal_Optind + 1;
              Internal_Optopt := Options (Option_Index);

              -- arg missing
              raise Argument_Required with "option requires an Argument -- " & Options (Option_Index .. Option_Index);
            end if;

          else  -- current optchar matches and has no arg option
            Handle_Missing_Argument;
            return Character'Pos (Options (Option_Index));
          end if;
        else
          -- last char in options, can't have Argument
          Handle_Missing_Argument;
          return Character'Pos (Options (Option_Index));
        end if;
      end if;
    end loop;

    Internal_Optopt := Command_Line.Argument (Internal_Optind) (Internal_Optcur);
    Handle_Missing_Argument;

    -- unknown arg
    raise Argument_Illegal with "illegal option -- " & Character'Image (Internal_Optopt);
  end Process;

  -- remaining args
  function Argument_Count return Natural is
  begin
    return Command_Line.Argument_Count - (Getopt.optind - 1);
  end Argument_Count;

  -- remaining Argument
  function Argument (index : Positive) return String is
  begin
    return Command_Line.Argument (optind + (index - 1));
  end Argument;

begin
  Reset;
end Getopt;
