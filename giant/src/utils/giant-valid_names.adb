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
-- $RCSfile: giant-valid_names.adb,v $, $Revision: 1.1 $
-- $Author: schwiemn $
-- $Date: 2003/05/27 09:14:55 $
--
with GNAT.OS_Lib;

package body Giant.Valid_Names is

   ---------------------------------------------------------------------------
   function Is_String_A_Correct_Standard_Name
     (String_Value : in String) return Boolean is

   begin

      if ((String_Value'Length < Min_Name_Length) or
          (String_Value'Length > Max_Name_Length)) then

        return False;
      end if;


      for I in String_Value'Range loop

         if (Character'Pos(String_Value(I))
                 -- '0'(ASCII 48) .. '9'(ASCII 57)
                 not in Character'Pos('0') .. Character'Pos('9'))
           and  (Character'Pos(String_Value(I))
                 -- 'A'(ASCII 65) .. 'Z'(ASCII 90)
                 not in Character'Pos('A') .. Character'Pos('Z'))
           and  (Character'Pos(String_Value(I))
                 -- 'a'(ASCII 97) .. 'z'(ASCII 122)
                 not in Character'Pos('a') .. Character'Pos('z'))
                 -- '_'(ASCII 95)
           and  (Character'Pos(String_Value(I)) /= Character'Pos('_'))
         then

            return False;
         end if;
      end loop;

      return True;
   end Is_String_A_Correct_Standard_Name;


   ---------------------------------------------------------------------------
   function To_Standard_Name
     (String_Value : in String) return Standard_Name is

   begin
      if (Is_String_A_Correct_Standard_Name(String_Value) = False) then
         raise No_Correct_Standard_Name_Exception;
      end if;

      return Standard_Name
        (Ada.Strings.Unbounded.To_Unbounded_String(String_Value));
   end To_Standard_Name;


   ---------------------------------------------------------------------------
   function To_String
     (Name : Standard_Name) return String is


   begin

      return Ada.Strings.Unbounded.To_String
        (Ada.Strings.Unbounded.Unbounded_String(Name));
   end To_String;
   
   ---------------------------------------------------------------------------	
   function Calculate_Name_For_File (File_Name : in String) 
     return Standard_Name is
	
	   Name : Ada.Strings.Unbounded.Unbounded_String;
	
	   Dir_Separator : Character := GNAT.OS_Lib.Directory_Separator; 
	
	   Cut_Dot_Index : Integer := 0;
	   Dot_Found : Boolean := False;
	  
       Cut_Dir_Sep_Index : Integer := 0;
	   Dir_Sep_Found : Boolean := False;
	
   begin 
	
      for I in reverse File_Name'Range loop
	  	  
	     -- search firs dot "." from behind
		 if (Dot_Found = False)
		   and then (File_Name(I) = '.') then
			 
		    Dot_Found := True;
		    Cut_Dot_Index := I;
  	     end if;
	  
	     -- search first directory separator from behind
	     if (Dir_Sep_Found = False)
		   and then (File_Name(I) = Dir_Separator) then
			 
		    Dir_Sep_Found := True;
		    Cut_Dir_Sep_Index := I;
			  
			-- nessary because the dot must be behind the first
			-- directory separator
			exit;
         end if;
		   		   
      end loop;
	   	   
	  -- calculate name
	  if (Dir_Sep_Found = True) and (Dot_Found = True)  then
	   
	     Name := Ada.Strings.Unbounded.To_Unbounded_String
		   (File_Name (Cut_Dir_Sep_Index + 1 .. Cut_Dot_Index - 1));
	  elsif (Dir_Sep_Found = True) and (Dot_Found = False) then
	  
	     Name := Ada.Strings.Unbounded.To_Unbounded_String
           (File_Name (Cut_Dir_Sep_Index + 1 .. File_Name'Last));
	  elsif (Dir_Sep_Found = False) and (Dot_Found = True) then
	  
	     Name := Ada.Strings.Unbounded.To_Unbounded_String
	       (File_Name (File_Name'First .. Cut_Dot_Index - 1));
	  else
         Name := Ada.Strings.Unbounded.To_Unbounded_String (File_Name);
	  end if;
	  
	  -- check for correctness 
	  if (Is_String_A_Correct_Standard_Name 
	       (Ada.Strings.Unbounded.To_String (Name)) = False) then
		   
         raise No_Correct_Standard_Name_Calculated_Exception;
	  end if;
	  
	  return To_Standard_Name (Ada.Strings.Unbounded.To_String (Name));
   end Calculate_Name_For_File;

end Giant.Valid_Names;
