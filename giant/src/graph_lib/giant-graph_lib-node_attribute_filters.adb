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
--  First Author: Oliver Kopp
--
--  $RCSfile: giant-graph_lib-node_attribute_filters.adb,v $, $Revision: 1.13 $
--  $Author: squig $
--  $Date: 2003/12/04 11:46:53 $
--
------------------------------------------------------------------------------

with Ada.Strings.Unbounded;

package body Giant.Graph_Lib.Node_Attribute_Filters is

   ---------------------------------------------------------------------------
   function Create
     (Node_Class                : in     Node_Class_Id;
      Node_Attribute_Names_List : in     String_Lists.List)
      return Filter
   is
      Temp_Filter  : Filter;
      Res          : Filter;
      I            : Natural;
      Iter         : String_Lists.ListIter;
      Current_Name : Ada.Strings.Unbounded.Unbounded_String;
   begin
      Temp_Filter  := new Filter_Type
        (1..String_Lists.Length (Node_Attribute_Names_List));

      Iter := String_Lists.MakeListIter (Node_Attribute_Names_List);
      I    := 0;

      --  Fill Filter with Node_Attributes
      while String_Lists.More (Iter) loop
         String_Lists.Next (Iter, Current_Name);

         declare
            Current_Name_As_String : String :=
              Ada.Strings.Unbounded.To_String (Current_Name);
         begin
            if Graph_Lib.Does_Node_Attribute_Exist
              (Node_Class          => Node_Class,
               Node_Attribute_Name => Current_Name_As_String) then
               --  Attribute exists in given Class, add it to the filter
               I:=I+1;
               Temp_Filter (I) :=
                 Convert_Node_Attribute_Name_To_Id
                 (Node_Class, Current_Name_As_String);
            end if;
         end;
      end loop;

      --  Convert Temp_Filter to Res
      --     by adjusting length
      Res := new Filter_Type (1..I);

      if Res'Length /= 0 then
         --  "I" could also have been 0, if no given attribute existed in the
         --     given Node_Class
         Res (1..I) := Temp_Filter (1..I);
      end if;

      Free_Filter (Temp_Filter);

      return Res;
   end Create;

   ---------------------------------------------------------------------------
   procedure Destroy
     (Node_Attribute_Filter  : in out Filter)
   is

   begin
      Free_Filter (Node_Attribute_Filter);
   end Destroy;

   ---------------------------------------------------------------------------
   function Make_Filtered_Iter
     (Node_Attribute_Filter : in Filter)
     return Filtered_Iterator
   is
      Res : Filtered_Iterator;
   begin
      Res.Used_Filter      := Node_Attribute_Filter;
      Reset (Res);
      return Res;
   end Make_Filtered_Iter;

   ---------------------------------------------------------------------------
   function More
     (Iter   : in     Filtered_Iterator)
     return Boolean
   is
   begin
      return Iter.Current_Position <= Iter.Used_Filter'Last;
   end More;

   ---------------------------------------------------------------------------
   procedure Next
     (Iter   : in out Filtered_Iterator;
      Attrib :    out Node_Attribute_Id)
   is
   begin
      pragma Assert (More (Iter));
      Attrib := Iter.Used_Filter (Iter.Current_Position);
      Iter.Current_Position := Iter.Current_Position + 1;
   end Next;

   ---------------------------------------------------------------------------
   procedure Reset
     (Iter   : in out Filtered_Iterator)
   is
   begin
      Iter.Current_Position := Iter.Used_Filter'First;
   end Reset;

   ---------------------------------------------------------------------------
   function Size
     (Node_Attribute_Filter  : in Filter)
     return Natural
   is
   begin
      return Node_Attribute_Filter'Length;
   end Size;

end Giant.Graph_Lib.Node_Attribute_Filters;
