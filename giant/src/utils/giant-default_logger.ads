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
--  $RCSfile: giant-default_logger.ads,v $, $Revision: 1.6 $
--  $Author: squig $
--  $Date: 2003/06/18 18:40:37 $
--
------------------------------------------------------------------------------
--
--  Contains the logging package.
--

with Ada.Exceptions;
with Ada.Text_IO;

package Giant.Default_Logger is

   DEFAULT_NAME : constant String := "giant";

   ---------------------------------------------------------------------------
   --  Represents the severity level of a log message. Higher levels include
   --  all lower levels (i.e. filtering for DEBUG catches all messages).
   type Level_Type is (Level_Off, Level_Fatal, Level_Error, Level_Warn,
                       Level_Info, Level_Debug);

   type Logger_Listener is access
     procedure (Level   : in Level_Type;
                Name    : in String;
                Message : in String);

   ---------------------------------------------------------------------------
   --  Opens the log file. Prints a message to stderr if open fails.
   --
   --  Call this method first.
   --
   procedure Init;

   ---------------------------------------------------------------------------
   --  Closes the log file. Prints a message to stderr if close fails.
   --
   procedure Close;

   ---------------------------------------------------------------------------
   --  Prints a message with DEBUG priority to the log file.
   --
   --  Parameters:
   --    Message - The message
   --    Name - The name of the logger
   procedure Debug
     (Message : in String;
      Name    : in String := DEFAULT_NAME);

   ---------------------------------------------------------------------------
   --  Prints a message with ERROR priority to the log file.
   --
   --  Parameters:
   --    Message - The message
   --    Name - The name of the logger
   procedure Error
     (Message : in String;
      Name    : in String := DEFAULT_NAME);

   ---------------------------------------------------------------------------
   --  Prints an exception to the log file. Use this method like this:
   --
   --    when Error : others =>
   --      Default_Logger.Debug ("an exception occured", "giant.package");
   --      Default_Logger.Error (Error);
   --
   --  Parameters:
   --    Message - The message
   --    Name - The name of the logger
   procedure Error
     (Error : in Ada.Exceptions.Exception_Occurrence);

   ---------------------------------------------------------------------------
   --  Prints a message with FATAL priority to the log file.
   --
   --  Parameters:
   --    Message - The message
   --    Name - The name of the logger
   procedure Fatal
     (Message : in String;
      Name    : in String := DEFAULT_NAME);

   ---------------------------------------------------------------------------
   --  Prints a message with INFO priority to the log file.
   --
   --  Parameters:
   --    Message - The message
   --    Name - The name of the logger
   procedure Info
     (Message : in String;
      Name    : in String := DEFAULT_NAME);

   ---------------------------------------------------------------------------
   --  Prints a message with WARN priority to the log file.
   --
   --  Parameters:
   --    Message - The message
   --    Name - The name of the logger
   procedure Warn
     (Message : in String;
      Name    : in String := DEFAULT_NAME);


   ---------------------------------------------------------------------------
   --  Returns a string representation of Level.
   --
   --  Parameters:
   --    Level - The level to convert
   function Get_Level_String
     (Level : in Level_Type)
     return String;

   ---------------------------------------------------------------------------
   --  Registers a listener for all log messages.
   --
   --  Parameters:
   --    Listener - A callback that is invoked when a message is received
   procedure Set_Listener
     (Listener : in Logger_Listener);

   ---------------------------------------------------------------------------
   --  Prints Message to stderr.
   --
   procedure Err_Put_Line
     (Message : in String);

private

   Out_File : Ada.Text_IO.File_Type;
   Listener : Logger_Listener := null;

end Giant.Default_Logger;

