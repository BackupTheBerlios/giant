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
-- $RCSfile: giant-file_management.adb,v $, $Revision: 1.3 $
-- $Author: schwiemn $
-- $Date: 2003/06/13 17:12:55 $
--
package body Giant.File_Management is
   ---------------------------------------------------------------------------
   function Get_Filtered_Files_From_Directory
     (Path_To_Dir   : in String;
      Filter        : in Boolean;
      Filter_String : in String)
     return String_Lists.List is

      GNAT_Directory : GNAT.Directory_Operations.Dir_Type;

      -- The Result - holds all file names found in the directory.
      File_Names_List : String_Lists.List;

      -- (Files and directory)
      Potential_File_Name_String : String(1 .. Max_File_Name_Length);

      Last_Character_Pos : Integer := 0;

      Complete_File_Name : Ada.Strings.Unbounded.Unbounded_String;

      GNAT_File_Name_String_Access : GNAT.OS_Lib.String_Access;

      ADA_Text_IO_File : ADA.Text_IO.File_Type;

   begin

      -- Check whether directory exists
      begin
         GNAT.Directory_Operations.Open
           (GNAT_Directory, Path_To_Dir);
      exception
         when GNAT.Directory_Operations.Directory_Error =>
            raise Invalid_Directory_Exception;
      end;

      -- Creates the empty List about the file names
      File_Names_List := String_Lists.Create;

      -- Search and process (filter etc.) all files in the directory
      loop

         -- read a potential files out of the directory
         GNAT.Directory_Operations.Read
           (GNAT_Directory, Potential_File_Name_String, Last_Character_Pos);

         -- No Files left in directory - leave loop
         exit when (Last_Character_Pos <= 0);

         -- only take real files (filter directories etc.)
         -- (GNAT_File_Access = null for everything else than files).
         GNAT_File_Name_String_Access := GNAT.OS_Lib.Locate_Regular_File
           (File_Name => Potential_File_Name_String(1 .. Last_Character_Pos),
            Path      => Path_To_Dir);

         -- Filter File_Names with wrong ending
         -- GNAT_File_Name_String_Acces.all must end with the
         -- character sequence of Filter_String
         if (Filter = True) and then
           GNAT.OS_Lib."/="(GNAT_File_Name_String_Access, null)
           and then
           ( (GNAT_File_Name_String_Access.all'Length < Filter_String'Length)
             or else
             (GNAT_File_Name_String_Access.all
              (GNAT_File_Name_String_Access.all'Last
               - Filter_String'Length + 1
               ..
               GNAT_File_Name_String_Access.all'Last)

              /= Filter_String(Filter_String'First .. Filter_String'Last)
              ) )

         then

            -- deallocates file names that are not needed according
            -- to the filter criterion
            GNAT.OS_Lib.Free(GNAT_File_Name_String_Access);

         end if;

         -- calculate full path and put result into list
         if GNAT.OS_Lib."/="(GNAT_File_Name_String_Access, null) then

            -- calculate "real" file name including path
            ADA.Text_IO.Open
              (File => ADA_Text_IO_File,
               Mode => ADA.Text_IO.In_File,
               Name => GNAT_File_Name_String_Access.all);

            -- supposed to calculate a absolute path including
            -- the name of the file (see Ada Reference Manual).
            -- No warantee!
            Complete_File_Name :=
              Ada.Strings.Unbounded.To_Unbounded_String
              (ADA.Text_IO.Name(ADA_Text_IO_File));

            ADA.Text_IO.Close(ADA_Text_IO_File);

            -- insert file name into the list:
            String_Lists.Attach
              (File_Names_List,
               Complete_File_Name);

            -- deallocates file name object
            GNAT.OS_Lib.Free(GNAT_File_Name_String_Access);

         end if;

      end loop;

      GNAT.Directory_Operations.Close (GNAT_Directory);
      return File_Names_List;
   end Get_Filtered_Files_From_Directory;

   ---------------------------------------------------------------------------
   function Return_Dir_Path_For_File_Path (File_Path : String)
                                          return String is

      Path : Ada.Strings.Unbounded.Unbounded_String;

      -- Position of last directory spearator in "File_Path"
      -- that separates the path from the file name;
      Cut_Index : Integer := 0;

      Dir_Separator : Character := GNAT.OS_Lib.Directory_Separator;

      Path_Found : Boolean := False;

   begin

      for I in reverse File_Path'Range loop

         if (File_Path(I) = Dir_Separator) then
            Path_Found := True;
            Cut_Index := I;

            exit;
         end if;
      end loop;

      -- Return current "working directory for the execution environment"
      -- if only a filename with no path (directory) was passed.
      if (Path_Found = False) then

         return GNAT.Directory_Operations.Get_Current_Dir;
      end if;

      if (GNAT.OS_Lib.Is_Directory
          (File_Path(File_Path'First .. Cut_Index)) = False) then

         raise Directory_Could_Not_Be_Calculated_Exception;
      end if;

      return File_Path(File_Path'First .. Cut_Index);
   end Return_Dir_Path_For_File_Path;

   ---------------------------------------------------------------------------
   function Get_Absolute_Path_To_File_From_Relative
     (Start_Dir : in String;
      Relative_Path_To_File : in String) return String is

      -- store "working directory for the execution environment"
      Old_Exec_Dir : String := GNAT.Directory_Operations.Get_Current_Dir;

      -- needed to calculate an absolute path
      ADA_Text_IO_File : ADA.Text_IO.File_Type;

      Abs_Path : Ada.Strings.Unbounded.Unbounded_String;

   begin

      if (GNAT.OS_Lib.Is_Directory (Start_Dir) = False) then

         -- if an invalid start directory is passed the file obviously could
         -- not be found
         raise File_Does_Not_Exist_Exception;
      end if;

      GNAT.Directory_Operations.Change_Dir (Start_Dir);

      ADA.Text_IO.Open
        (File => ADA_Text_IO_File,
         Mode => ADA.Text_IO.In_File,
         Name => Relative_Path_To_File);

      Abs_Path := Ada.Strings.Unbounded.To_Unbounded_String
        (ADA.Text_IO.Name(ADA_Text_IO_File));

      ADA.Text_IO.Close(ADA_Text_IO_File);
                  
      -- check for a regular file
      if (GNAT.OS_Lib.Is_Regular_File 
        (Ada.Strings.Unbounded.To_String (Abs_Path)) = False) then
        
         raise File_Does_Not_Exist_Exception;
      end if;
      
      -- switch back to old
      -- "working directory for the execution environment"
      GNAT.Directory_Operations.Change_Dir (Old_Exec_Dir);

      return Ada.Strings.Unbounded.To_String (Abs_Path);

   exception
      when others =>
         GNAT.Directory_Operations.Change_Dir (Old_Exec_Dir);
         raise File_Does_Not_Exist_Exception;
   end Get_Absolute_Path_To_File_From_Relative;
      
   ---------------------------------------------------------------------------
   function Get_Absolute_Path_To_Directory_From_Relative
     (Start_Dir    : in String;
      Rel_Dir_Path : in String)
     return String is

      Old_Exec_Dir : String := GNAT.Directory_Operations.Get_Current_Dir;
      Abs_Dir_Path : Ada.Strings.Unbounded.Unbounded_String;
   begin
  
      GNAT.Directory_Operations.Change_Dir (Start_Dir);
      GNAT.Directory_Operations.Change_Dir (Rel_Dir_Path); 
            
      -- should return an absolute path
      Abs_Dir_Path := Ada.Strings.Unbounded.To_Unbounded_String
        (GNAT.Directory_Operations.Get_Current_Dir);    
               
      GNAT.Directory_Operations.Change_Dir (Old_Exec_Dir);
            
      return Ada.Strings.Unbounded.To_String (Abs_Dir_Path);
   exception      
      when GNAT.Directory_Operations.Directory_Error =>
         raise Directory_Does_Not_Exist_Exception;
   end Get_Absolute_Path_To_Directory_From_Relative;

   ---------------------------------------------------------------------------
   procedure Set_Currunt_Working_Dir_To_Exec_Dir is

      Exec_File_Call_String : String := Ada.Command_Line.Command_Name;

   begin

      GNAT.Directory_Operations.Change_Dir
        (Return_Dir_Path_For_File_Path (Exec_File_Call_String));

   end Set_Currunt_Working_Dir_To_Exec_Dir;

end Giant.File_Management;
