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
--  $RCSfile: giant-merging_iterators.adb,v $, $Revision: 1.3 $
--  $Author: squig $
--  $Date: 2003/12/04 11:46:56 $
--
------------------------------------------------------------------------------


with Ada.Unchecked_Deallocation;

package body Giant.Merging_Iterators is

   function Has_Higher_Priority
     (Left  : in Single_Iterator;
      Right : in Single_Iterator)
     return Boolean is
   begin
      return Left.Head < Right.Head;
   end Has_Higher_Priority;

   ------------------------------
   -- Access via access values --
   ------------------------------

   function Create
     (Iterators : in Iterator_Lists.List)
     return Merger_Access is

      Merger   : Merger_Access;
      Iterator : Iterator_Lists.ListIter := Iterator_Lists.MakeListIter
        (Iterators);
      Current  : Sets.Iterator;
   begin
      Merger := new Merger_Type (Iterator_Lists.Length (Iterators));
      while Iterator_Lists.More (Iterator) loop
         Iterator_Lists.Next (Iterator, Current);
         Add_Iterator (Merger.all, Current);
      end loop;
      Start_Iteration (Merger.all);
      return Merger;
   end Create;

   function Has_More
     (Merger    : access Merger_Type)
     return Boolean is
   begin
      return Has_More (Merger.all);
   end Has_More;

   function Get_Current
     (Merger    : access Merger_Type)
     return Item_Type is
   begin
      return Get_Current (Merger.all);
   end Get_Current;

   procedure Forward
     (Merger    : access Merger_Type) is
   begin
      Forward (Merger.all);
   end Forward;

   procedure Destroy
     (Merger    : in out Merger_Access) is

      procedure Free is new Ada.Unchecked_Deallocation
        (Object => Merger_Type,
         Name   => Merger_Access);

   begin
      if Merger /= null then
         Destroy (Merger.all);
         Free (Merger);
      end if;
   end Destroy;


   -------------------------------
   -- Local Merger_Type-objects --
   -------------------------------

   procedure Advance_Iterator
     (Merger : in out Merger_Type;
      Item   : in out Single_Iterator) is
   begin
      if Sets.More (Merger.Pool (Item.Pool_Index)) then
         Item.Head := Sets.Current (Merger.Pool (Item.Pool_Index));
         Sets.Next (Merger.Pool (Item.Pool_Index));
         Single_Iterator_Queues.Insert (Merger.Queue, Item);
      else
         Sets.Destroy (Merger.Pool (Item.Pool_Index));
      end if;
   end Advance_Iterator;

   procedure Update_Next
     (Merger : in out Merger_Type) is

      Head_Item : Single_Iterator;
   begin
      pragma Assert (Merger.Current_Is_Available);
      Merger.Next_Is_Available := False;
      while not (Merger.Next_Is_Available or else
                 Single_Iterator_Queues.Is_Empty (Merger.Queue)) loop

         Head_Item := Single_Iterator_Queues.Get_Head (Merger.Queue);
         Single_Iterator_Queues.Remove_Head (Merger.Queue);

         Merger.Next := Head_Item.Head;
         Merger.Next_Is_Available := Merger.Next /= Merger.Current;

         Advance_Iterator (Merger, Head_Item);
      end loop;
   end Update_Next;

   procedure Add_Iterator
     (Merger   : in out Merger_Type;
      Iterator : in     Sets.Iterator) is

      Item       : Single_Iterator;
   begin
      pragma Assert (not Merger.Current_Is_Available);
      Item.Pool_Index := Single_Iterator_Queues.Get_Size (Merger.Queue) + 1;
      --  raises Constraint_Error if too many Iterators are added.
      Merger.Pool (Item.Pool_Index) := Sets.Copy (Iterator);

      if Sets.More (Merger.Pool (Item.Pool_Index)) then
         Item.Head := Sets.Current (Merger.Pool (Item.Pool_Index));
         Sets.Next (Merger.Pool (Item.Pool_Index));
         Single_Iterator_Queues.Insert (Merger.Queue, Item);
      else
         Sets.Destroy (Merger.Pool (Item.Pool_Index));
      end if;
   end Add_Iterator;

   procedure Start_Iteration
     (Merger   : in out Merger_Type) is

      Head_Item : Single_Iterator;
   begin
      Merger.Current_Is_Available := not Single_Iterator_Queues.Is_Empty
        (Merger.Queue);
      if Merger.Current_Is_Available then
         Head_Item := Single_Iterator_Queues.Get_Head (Merger.Queue);
         Single_Iterator_Queues.Remove_Head (Merger.Queue);
         Merger.Current := Head_Item.Head;

         Advance_Iterator (Merger, Head_Item);
         Update_Next (Merger);
      end if;
   end Start_Iteration;

   function Has_More
     (Merger   : in     Merger_Type)
     return Boolean is
   begin
      return Merger.Current_Is_Available;
   end Has_More;

   function Get_Current
     (Merger   : in     Merger_Type)
     return Item_Type is
   begin
      pragma Assert (Has_More (Merger));
      return Merger.Current;
   end Get_Current;

   procedure Forward
     (Merger   : in out Merger_Type) is
   begin
      Merger.Current := Merger.Next;
      Merger.Current_Is_Available := Merger.Next_Is_Available;
      if Merger.Current_Is_Available then
         Update_Next (Merger);
      end if;
   end Forward;

   procedure Destroy
     (Merger   : in out Merger_Type) is

      Item : Single_Iterator;
   begin
      while not Single_Iterator_Queues.Is_Empty (Merger.Queue) loop
         Item := Single_Iterator_Queues.Get_Head (Merger.Queue);
         Single_Iterator_Queues.Remove_Head (Merger.Queue);
         Sets.Destroy (Merger.Pool (Item.Pool_Index));
      end loop;
      Merger.Current_Is_Available := False;
      Merger.Next_Is_Available := False;
   end Destroy;

end Giant.Merging_Iterators;
