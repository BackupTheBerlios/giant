------------------------------------------------------------------------------
--
--  GIANT - Graphical IML Analysis and Navigation Tool
--
--  Copyright (C) 2003 Philipp Haeuser, Steffen Keul, Oliver Kopp,
--  Steffen Pingel, Gerrit Schulz and Martin Schwienbacher.
--
--  Redistribution and use in source and binary forms, with or without
--  modification, are permitted to the department of
--  Programmiersprachen und Übersetzerbau, University of Stuttgart for
--  academic purposes.
--
--  THIS SOFTWARE IS PROVIDED ``AS IS'' AND ANY EXPRESSED OR IMPLIED
--  WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
--  OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
--  DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS NAMED ABOVE OR
--  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
--  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
--  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
--  USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
--  AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
--  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
--  ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
--  POSSIBILITY OF SUCH DAMAGE.
--
--  First Author: Steffen Pingel
--
--  $RCSfile: giant-default_logger.adb,v $, $Revision: 1.15 $
--  $Author: squig $
--  $Date: 2003/12/04 11:46:55 $
--

with Ada.IO_Exceptions;
with Ada.Strings.Fixed;
with Ada.Text_IO;
with Calendar_Utilities;

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
     (Filename : in String)
   is
   begin
      Ada.Text_IO.Create (Out_File, Ada.Text_IO.Out_File, Filename);
   exception
      when others =>
         Err_Put_Line ("Could not open log file.");
   end Init;

   procedure Init
   is
   begin
      null;
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

      declare
         -- cut off Level_ and pad to 5 letters
         Composed_Message : constant String
           := Get_Level_String (Level) & " [" & Head (Name, 30) & "] "
           & Message;
      begin
         if (Ada.Text_IO.Is_Open (Out_File)) then
            --  put timestamp
            Ada.Text_IO.Put (Out_File, Calendar_Utilities.Clock_String & " ");

            Ada.Text_IO.Put (Out_File, Composed_Message);
            Ada.Text_IO.New_Line (Out_File);
            Ada.Text_IO.Flush (Out_File);

            if (Level >= Level_Warn) then
               Ada.Text_IO.Put (Ada.Text_Io.Standard_Error, Composed_Message);
               Ada.Text_IO.New_Line (Ada.Text_Io.Standard_Error);
               Ada.Text_IO.Flush (Ada.Text_Io.Standard_Error);
            end if;
         else
            Ada.Text_IO.Put (Ada.Text_Io.Standard_Error, Composed_Message);
            Ada.Text_IO.New_Line (Ada.Text_Io.Standard_Error);
            Ada.Text_IO.Flush (Ada.Text_Io.Standard_Error);
         end if;
      end;
   exception
     when others =>
        null; -- just ignore it to not clutter stderr
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
         Ada.Text_IO.Flush (Out_File);
      end if;
   exception
     when others =>
        null; -- just ignore it to not clutter stderr
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

