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
--  $RCSfile: giant-simple_priority_queues.ads,v $, $Revision: 1.4 $
--  $Author: squig $
--  $Date: 2003/12/04 11:46:56 $
--
------------------------------------------------------------------------------
--
--  Generic heap implementation of a priority queue with a fixed maximum size.
--
--  The type of items is a generic parameter. A strict total order must
--  be provided on that type ("priority order").
--
--  It is not possible to determine if a given Item is contained in a
--  queue or not. The priority of any item contained in any queue except
--  the head item must not change. If this is needed, then the package
--  Giant.Fixed_Priority_Queues should be used.
--


generic

   ----------------------------------------------------------------------------
   --  Type for items
   type Item_Type is private;

   ----------------------------------------------------------------------------
   --  Priority order. This function must either always return the
   --  same value for the same arguments, or the data structure must
   --  be updated by calls 'Update_Head'
   --
   --  Returns:
   --    True if 'Left' has a higher priority than 'Right', False else
   with function Has_Higher_Priority
     (Left  : in Item_Type;
      Right : in Item_Type)
     return Boolean;

package Giant.Simple_Priority_Queues is

   ----------------------------------------------------------------------------
   --  Type for fixed size priority queues of size at most 'Max_Size' elements
   type Queue_Type (Max_Size : Natural) is private;

   ----------------------------------------------------------------------------
   --  Raised whenever the size of a Q : 'Queue_Type' exceeds
   --  'Get_Max_Size (Q)' items
   Too_Many_Items   : exception;

   ----------------------------------------------------------------------------
   --  Raised whenever a 'Queue_Type' is empty, but an item is needed
   Not_Enough_Items : exception;

   ----------------------------------------------------------------------------
   --  Inserts an item into a queue. Note that each item must be
   --  contained at most in one queue at any point of time.
   --
   --  Precondition:
   --    Get_Size (Queue) < Get_Max_Size (Queue) and
   --  Postcondition:
   --    not Is_Empty (Queue)
   --  Raises:
   --    * Too_Many_Items if Get_Size (Queue) >= Get_Max_Size (Queue)
   procedure Insert
     (Queue : in out Queue_Type;
      Item  : in     Item_Type);

   ----------------------------------------------------------------------------
   --  Must be called whenever the priority of the head item has changed.
   --
   --  Precondition:
   --    not Is_Empty (Queue)
   --  Raises:
   --    Not_Enough_Items if Precondition not satisfied
   procedure Update_Head
     (Queue : in out Queue_Type);

   ----------------------------------------------------------------------------
   --  Removes the head item from 'Queue'
   --
   --  Precondition:
   --    not Is_Empty (Queue)
   --  Raises:
   --    Not_Enough_Items if Precondition not satisfied
   procedure Remove_Head
     (Queue : in out Queue_Type);

   ----------------------------------------------------------------------------
   --  Postcondition:
   --    Is_Empty (Queue)
   procedure Clear
     (Queue : in out Queue_Type);

   ----------------------------------------------------------------------------
   --  Returns an item contained in 'Queue' that has the maximum priority.
   --  If there are more than one items in 'Queue' that have the maximum
   --  priority then it is unspecified which one will be returned. However
   --  two calls to 'Get_Head' always yield the same item unless 'Queue'
   --  was modified.
   --
   --  Returns:
   --    Head item in 'Queue':
   --    for all I : Item_Type and I is contained 'Queue':
   --      not Has_Higher_Priority (I, Head)
   --  Precondition:
   --    not Is_Empty (Queue)
   --  Raises:
   --    Not_Enough_Items if Precondition not satisfied
   function Get_Head
     (Queue : in     Queue_Type)
     return Item_Type;

   ----------------------------------------------------------------------------
   --  Returns:
   --    True if there is no Item in 'Queue', False else
   function Is_Empty
     (Queue : in     Queue_Type)
     return Boolean;

   ----------------------------------------------------------------------------
   --  Returns:
   --    Number of Items inserted in, but not removed from 'Queue'
   function Get_Size
     (Queue : in     Queue_Type)
     return Natural;

   ----------------------------------------------------------------------------
   --  Returns:
   --    The number of Items that can be inserted at most into 'Queue'
   function Get_Max_Size
     (Queue : in     Queue_Type)
     return Natural;

private

   subtype Array_Bounds is Positive;

   type Heap_Array is array (Array_Bounds range <>) of Item_Type;

   type Queue_Type (Max_Size : Natural) is
      record
         Size  : Natural := 0;
         Field : Heap_Array (1 .. Max_Size);
      end record;

end Giant.Simple_Priority_Queues;
