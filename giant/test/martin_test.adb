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
--  First Author: Martin Schwienbacher
--
--  $RCSfile: martin_test.adb,v $, $Revision: 1.13 $
--  $Author: schwiemn $
--  $Date: 2003/07/11 16:24:44 $
--  
with AUnit.Test_Suites; use AUnit.Test_Suites;
with AUnit.Test_Runner;

with Giant.Config.Vis_Styles.Test; 
with Giant.Projects.Test;
with Giant.XML_File_Access.Test;
with Giant.File_Management.Test;
with Giant.Node_Annotations.Test;
with Giant.GSL_Support.Test;
with Giant.Vis_Windows.Test;
with Giant.Config.Class_Sets.Test;

with Giant.Default_Logger;

procedure Martin_Test is

   function Suite return Access_Test_Suite is
      Result : Access_Test_Suite := new Test_Suite;
   begin
   
   
  --   Add_Test (Result, new Giant.Vis_Windows.Test.Test_Case); 
   
 --  Add_Test (Result, new Giant.Projects.Test.Test_Case);
       
       
    Add_Test (Result, new Giant.Config.Vis_Styles.Test.Test_Case);
 
  --  Add_Test (Result, new Giant.Config.Class_Sets.Test.Test_Case);
         
    --   Add_Test (Result, new Giant.XML_File_Access.Test.Test_Case);
  
    -- Add_Test (Result, new Giant.File_Management.Test.Test_Case);  
     
    --   Add_Test (Result, new Giant.Node_Annotations.Test.Test_Case);

-- Add_Test (Result, new Giant.GSL_Support.Test.Test_Case);
             
      return Result;
   end Suite;

   procedure Run is new AUnit.Test_Runner (Suite);

begin
   Giant.Default_Logger.Init ("a_martin_test.log");
   Giant.Default_Logger.Debug ("Starting Test...");

   Run;

   Giant.Default_Logger.Close;
end Martin_Test;
