------------------------------------------------------------------------------
--  GIANT - Graphical IML Analysis and Navigation Tool
--
--  Copyright (C) 2003 Philipp Haeuser, Steffen Keul, Oliver Kopp,
--  Steffen Pingel, Gerrit Schulz and Martin Schwienbacher.
--
--  This program is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program; if not, write to the Free Software
--  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
--
--  First Author: Steffen Keul
--
--  $RCSfile: giant_test-iterative_calculations.ads,v $, $Revision: 1.1 $
--  $Author: keulsn $
--  $Date: 2003/06/02 00:09:08 $
--
------------------------------------------------------------------------------


with Giant.Evolutions;

package Giant_Test.Iterative_Calculations is

   type Counter is new Giant.Evolutions.Iterative_Evolution with private;

   type Counter_Access is access all Counter;
   type Counter_Class_Access is access all Counter'Class;

   function Create
     (Calculation_Number : in Natural;
      Number_Of_Steps    : in Natural)
      return Counter_Access;

   procedure Step
     (Individual  : access Counter;
      Next_Action :    out Giant.Evolutions.Evolution_Action);

   procedure Synchronized_Step
     (Individual   : access Counter;
      Next_Action  :    out Giant.Evolutions.Evolution_Action);

   procedure Finish
     (Individual : access Counter;
      Canceled   : in     Boolean);

private

   type Counter is new Giant.Evolutions.Iterative_Evolution with
      record
         Number    : Natural;
      end record;

end Giant_Test.Iterative_Calculations;
