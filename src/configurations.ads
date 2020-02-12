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

package Configurations is

    Min_Merge_Cluster_Count : Natural := 5; -- (default: 10) 10
    Min_Abs_Count           : Natural := 5; -- (default: 50) 10
    Prior_Cutoff            : Long_Float   := 5.0; -- agenda cutoff ~ cluster change (default 10.0)
    Prior_Merge             : Long_Float   := 2.0;
    Prior_Num_Param         : Long_Float   := 5.0; -- (default 5.0)
    Prior_Num_Conj          : Long_Float   := 10.0; -- penalize merge score if the two appears in conjunction (default: 10.0)

end Configurations;
