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
--  First Author: Steffen Keul
--
--  $RCSfile: giant-gsl_identifiers.adb,v $
--  $Author: squig $
--  $Date: 2003/12/04 11:46:53 $
--
------------------------------------------------------------------------------


with Ada.Strings.Unbounded;

with Hashed_Mappings;
pragma Elaborate_All (Hashed_Mappings);
with Unbounded_String_Hash;
pragma Elaborate (Unbounded_String_Hash);

package body Giant.Gsl_Identifiers is

   package Strings renames Ada.Strings.Unbounded;

   package Name_Mappings is new Hashed_Mappings
     (Key_Type   => Strings.Unbounded_String,
      Equal      => Strings."=",
      Hash       => Unbounded_String_Hash,
      Value_Type => Identifier_Type);

   package Identifier_Mappings is new Hashed_Mappings
     (Key_Type   => Identifier_Type,
      Equal      => "=",
      Hash       => Hash,
      Value_Type => Strings.Unbounded_String);


   ----------------------
   -- Global variables --
   ----------------------

   Next_Identifier : Identifier_Type;

   Name_Map        : Name_Mappings.Mapping;
   Identifier_Map  : Identifier_Mappings.Mapping;


   -----------------
   -- Subprograms --
   -----------------

   function Get_Identifier
     (Name : in     String)
     return Identifier_Type
   is
      Value : Identifier_Type;
      Key   : Strings.Unbounded_String := Strings.To_Unbounded_String (Name);
   begin
      begin
         Value := Name_Mappings.Fetch (Name_Map, Key);
      exception
         when Name_Mappings.Not_Bound =>
            Value := Next_Identifier;
            Name_Mappings.Bind (Name_Map, Key, Value);
            Identifier_Mappings.Bind (Identifier_Map, Value, Key);
            Next_Identifier := Next_Identifier + 1;
      end;
      return Value;
   end Get_Identifier;

   function Get_Name
     (Identifier : in     Identifier_Type)
     return String is
   begin
      return Strings.To_String
        (Identifier_Mappings.Fetch (Identifier_Map, Identifier));
   end Get_Name;

   function Hash
     (Identifier : in     Identifier_Type)
     return Integer is
   begin
      return Integer (Identifier);
   end Hash;


   --------------------
   -- Initialization --
   --------------------

begin
   Next_Identifier := Identifier_Type'First;
   Name_Map := Name_Mappings.Create;
   Identifier_Map := Identifier_Mappings.Create;
end Giant.Gsl_Identifiers;
