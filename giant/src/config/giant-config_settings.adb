------------------------------------------------------------------------------
-- GIANT - Graphical IML Analysis and Navigation Tool
--
-- Copyright (C) 2003 Philipp Haeuser, Steffen Keul, Oliver Kopp,
-- Steffen Pingel, Gerrit Schulz and Martin Schwienbacher.
--
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
--
-- First Author: Martin Schwienbacher
--
-- $RCSfile: giant-config_settings.adb,v $, $Revision: 1.2 $
-- $Author: schwiemn $
-- $Date: 2003/06/20 17:12:49 $
--
with Ada.Unchecked_Deallocation;

with Hashed_Mappings;       -- from Bauhaus Reuse.src
pragma Elaborate_All (Hashed_Mappings);
with Unbounded_String_Hash; -- from Bauhaus Reuse.src

with Giant.Logger;  -- from GIANT

package body Giant.Config_Settings is
   
   -- logging functionality
   package Logger is new Giant.Logger ("Giant.Config_Settings");

   --------------------------------------------------------------------------
   --  Internal Datasstructure of the ADO
   --------------------------------------------------------------------------

   package Setting_Hashs is new Hashed_Mappings
     (Key_Type   => Ada.Strings.Unbounded.Unbounded_String,
      Equal      => Ada.Strings.Unbounded."=",
      Hash       => Unbounded_String_Hash,
      Value_Type => Ada.Strings.Unbounded.Unbounded_String);
      
   -- Read from "GIANT_Config_File"
   Giant_Settings : Setting_Hashs.Mapping;
   
   -- Read from "User_Config_File"
   User_settings  : Setting_Hashs.Mapping; 


   --------------------------------------------------------------------------
   -- 0.1 Validators
   --------------------------------------------------------------------------

   --------------------------------------------------------------------------
   function Validate_Integer (Val : in String) return Boolean is
   
   begin
   
      return True;
   end Validate_Integer;

    
   ---------------------------------------------------------------------------
   -- A
   -- Initialisation and finalisation
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   procedure Initialize_Config_Settings
     (GIANT_Config_File : in String;
      User_Config_File  : in String) is 
   begin
     null; 
   end Initialize_Config_Settings; 


   ---------------------------------------------------------------------------
   procedure Clear_Config_Data is
   
   begin
     null;
   end Clear_Config_Data;


   ---------------------------------------------------------------------------
   -- B
   -- Access to configuration data.
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   function Does_Setting_Exist (Name : in String)
     return Boolean is
   begin
   
      return True;
   end Does_Setting_Exist;

   ---------------------------------------------------------------------------
   function Return_Setting_As_String (Name : in String)
     return String is
   begin
     return "1";
   end Return_Setting_As_String;
     

   ---------------------------------------------------------------------------
   function Return_Setting_As_Integer (Name : in String)
     return Integer is
   begin
    return 1;
   end Return_Setting_As_Integer;
     
     
   ---------------------------------------------------------------------------
   procedure Set_Setting (Name : in String; Value : in String) is
   begin
      null;
   end Set_Setting;
   
   
   ---------------------------------------------------------------------------
   procedure Set_Setting (Name : in String; Value : in Integer) is
   begin
      null;
   end Set_Setting;
   
   
   ---------------------------------------------------------------------------
   procedure Store_User_Config_File (File_Name : in String) is
   begin
      null;
   end Store_User_Config_File;
       
end Giant.Config_Settings;
