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
--  $RCSfile: giant-default_logger.ads,v $, $Revision: 1.2 $
--  $Author: squig $
--  $Date: 2003/05/23 19:03:25 $
--
------------------------------------------------------------------------------
--
--  Contains the logging package.
--

package Giant.Default_Logger is

   DEFAULT_NAME : constant String := "giant";

   ---------------------------------------------------------------------------
   --  Represents the severity level of a log message. Higher levels include
   --  all lower levels (i.e. filtering for DEBUG catches all messages).
   type Level_Type is (Level_Off, Level_Fatal, Level_Error, Level_Warn,
                       Level_Info, Level_Debug);


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
   --  Prints Message to stderr.
   --
   procedure Err_Put_Line
     (Message : in String);

end Giant.Default_Logger;

