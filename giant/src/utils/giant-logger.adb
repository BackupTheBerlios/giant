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
--  $RCSfile: giant-logger.adb,v $, $Revision: 1.5 $
--  $Author: squig $
--  $Date: 2003/12/04 11:46:56 $
--

with Ada.IO_Exceptions;
with Ada.Strings.Fixed;
with Ada.Text_IO;

with Giant.Default_Logger;

package body Giant.Logger is

   procedure Debug (Message : in String)
   is
   begin
      Default_Logger.Debug (Message, Name);
   end Debug;

   procedure Error (Message : in String)
   is
   begin
      Default_Logger.Error (Message, Name);
   end Error;

   procedure Error
     (Error : in Ada.Exceptions.Exception_Occurrence)
   is
   begin
      Default_Logger.Error (Error);
   end;

   procedure Fatal (Message : in String)
   is
   begin
      Default_Logger.fatal (Message, Name);
   end Fatal;

   procedure Info (Message : in String)
   is
   begin
      Default_Logger.Info (Message, Name);
   end Info;

   procedure Warn (Message : in String)
   is
   begin
      Default_Logger.Warn (Message, Name);
   end Warn;

end Giant.Logger;

