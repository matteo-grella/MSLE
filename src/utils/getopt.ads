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

package Getopt is
  pragma Elaborate_Body (Getopt);

  function Process (Options : String) return Integer;

  Argument_Required : exception;
  Argument_Illegal  : exception;

  function Argument (index : Positive) return String;

  function Argument_Count return Natural;
  pragma Inline (Argument_Count);

  procedure Reset;
  pragma Inline (Reset);

  function optarg return String;
  function Option_Argument return String renames optarg;
  pragma Inline (optarg);

  function optind return Positive;
  function Option_Index return Positive renames optind;
  pragma Inline (optind);

  function optopt return Character;
  function Option_Character return Character renames optopt;
  pragma Inline (optopt);

  Option_EOF : constant := -1;
  opteof     : constant := Option_EOF;

end Getopt;
