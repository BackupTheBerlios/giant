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
--  First Author: Steffen Pingel
--
--  $RCSfile: giant-default_logger.adb,v $, $Revision: 1.8 $
--  $Author: squig $
--  $Date: 2003/06/18 18:40:37 $
--

with Ada.IO_Exceptions;
with Ada.Strings.Fixed;
with Ada.Text_IO;

package body Giant.Default_Logger is

   procedure Close
   is
   begin
      if (Ada.Text_IO.Is_Open (Out_File)) then
         Ada.Text_IO.Close (Out_File);
      end if;
   exception
      when others =>
         Err_Put_Line ("Could not close log file.");
   end Close;

   procedure Init
   is
   begin
      Ada.Text_IO.Create (Out_File, Ada.Text_IO.Out_File, "debug.log");
   exception
      when others =>
         Err_Put_Line ("Could not open log file.");
   end Init;

   ---------------------------------------------------------------------------
   --  Prints a single line to the log file if open.
   --
   procedure Put_Line
     (Level   : in Level_Type;
      Name    : in String;
      Message : in String)
   is
      use Ada.Strings.Fixed;
   begin
      if (Listener /= null) then
         Listener (Level, Name, Message);
      end if;

      if (Ada.Text_IO.Is_Open (Out_File)) then
         -- cut off Level_ and pad to 5 letters
         Ada.Text_IO.Put (Out_File, Get_Level_String (Level));
         Ada.Text_IO.Put (Out_File, " [");
         Ada.Text_IO.Put (Out_File, Head (Name, 15));
         Ada.Text_IO.Put (Out_File, "] ");
         Ada.Text_IO.Put (Out_File, Message);
         Ada.Text_IO.New_Line (Out_File);
         Ada.Text_IO.Flush (Out_File);
      end if;
--     exception
--       when others =>
--          null; -- just ignore it to not clutter stderr
   end Put_Line;

   ---------------------------------------------------------------------------
   --  Prints an exception to the log file if open.
   --
   procedure Put_Exception
     (Error : in Ada.Exceptions.Exception_Occurrence)
   is
      use Ada.Exceptions;
   begin
      if (Ada.Text_IO.Is_Open (Out_File)) then
         Ada.Text_IO.Put_Line(Out_File, " " & Exception_Name (Error));
         Ada.Text_IO.Put_Line(Out_File, " " & Exception_Message (Error));
      end if;
--     exception
--       when others =>
--          null; -- just ignore it to not clutter stderr
   end Put_Exception;

   procedure Debug (Message : in String;
                    Name    : in String := DEFAULT_NAME)
   is
   begin
      Put_Line (Level_Debug, Name, Message);
   end Debug;

   procedure Error
     (Message : in String;
      Name    : in String := DEFAULT_NAME)
   is
   begin
      Put_Line (Level_Error, Name, Message);
   end Error;

   procedure Error
     (Error : in Ada.Exceptions.Exception_Occurrence)
   is
   begin
      Put_Exception (Error);
   end Error;

   procedure Fatal (Message : in String;
                    Name    : in String := DEFAULT_NAME)
   is
   begin
      Put_Line (Level_Fatal, Name, Message);
   end Fatal;

   procedure Info (Message : in String;
                   Name    : in String := DEFAULT_NAME)
   is
   begin
      Put_Line (Level_Info, Name, Message);
   end Info;

   procedure Warn (Message : in String;
                   Name    : in String := DEFAULT_NAME)
   is
   begin
      Put_Line (Level_Warn, Name, Message);
   end Warn;

   function Get_Level_String
     (Level : in Level_Type)
     return String
   is
      use Ada.Strings.Fixed;
   begin
      return Head (Delete (Level_Type'Image (Level), 1, 6), 5);
   end;

   procedure Set_Listener
     (Listener : in Logger_Listener)
   is
   begin
      Default_Logger.Listener := Listener;
   end Set_Listener;

   procedure Err_Put_Line (Message : in String)
   is
   begin
      Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, Message);
   end Err_Put_Line;

end Giant.Default_Logger;

