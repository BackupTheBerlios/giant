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
--  $RCSfile: giant-logger.ads,v $, $Revision: 1.8 $
--  $Author: squig $
--  $Date: 2003/12/04 11:46:56 $
--
------------------------------------------------------------------------------
--
--  Contains the generic logging package.
--
--  To use this package, first create an instance of this package:
--
--    package My_Logger is new Logger("giant.mypackage");
--
--  Then call one of the methods depending on the severity:
--
--    My_Logger.Debug ("debug message");
--
--  See:
--    Giant.Default_Logger

with Ada.Exceptions;

generic

   ---------------------------------------------------------------------------
   --  The name of the logger. Use a hierarchial name like
   --  giant.mypackage. Log messages can be filtered by this name.
   Name : String;

package Giant.Logger is

   ---------------------------------------------------------------------------
   --  See:
   --    Default_Logger.Debug
   procedure Debug (Message : in String);

   ---------------------------------------------------------------------------
   --  See:
   --    Default_Logger.Error
   procedure Error (Message : in String);

   ---------------------------------------------------------------------------
   --  See:
   --    Default_Logger.Error
   procedure Error
     (Error : in Ada.Exceptions.Exception_Occurrence);

   ---------------------------------------------------------------------------
   --  See:
   --    Default_Logger.Fatal
   procedure Fatal (Message : in String);

   ---------------------------------------------------------------------------
   --  See:
   --    Default_Logger.Info
   procedure Info (Message : in String);

   ---------------------------------------------------------------------------
   --  See:
   --    Default_Logger.Warn
   procedure Warn (Message : in String);

end Giant.Logger;

