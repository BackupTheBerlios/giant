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
--  First Author: Steffen Keul
--
--  $RCSfile: giant-fixed_priority_queues.ads,v $, $Revision: 1.1 $
--  $Author: keulsn $
--  $Date: 2003/06/01 19:12:28 $
--
------------------------------------------------------------------------------
--
--  Generic heap implementation of a priority queue with a fixed maximum size.
--
--  The type of items is a generic parameter. A strict total order must
--  be provided on that type ("priority order").
--
--  Two subprograms must be provided as generic parameters that allow to
--  associate a Natural value with an item. Each item can be contained
--  in one queue at most.
--


generic
   ----------------------------------------------------------------------------
   --  Number of items a queue can contain
   Max_Size : Natural;

   ----------------------------------------------------------------------------
   --  Type for items
   type Item_Type is private;

   ----------------------------------------------------------------------------
   --  Priority order. This function must either always return the
   --  same value for the same arguments, or the data structure must
   --  be updated by calls to 'Update_Item' or 'Update_Head_Item'
   --
   --  Returns:
   --    True if 'Left' has a higher priority than 'Right', False else
   with function Has_Higher_Priority
     (Left  : in Item_Type;
      Right : in Item_Type)
     return Boolean;

   ----------------------------------------------------------------------------
   --  Associates the value of 'Position' with the item 'Item'. Should not
   --  be called from outside of this package.
   --
   --  Postcondition:
   --    Get_Position (Item) = Position
   with procedure Set_Position
     (Item     : in     Item_Type;
      Position : in     Natural);

   ----------------------------------------------------------------------------
   --  Returns the value previously associates with 'Item' by a call
   --  to 'Set_Position'.
   --
   --  Returns:
   --    P if preceeded by a call 'Set_Position (Item, P)',
   --    0 if 'Set_Position' has never been called on 'Item'
   with function Get_Position
     (Item : in     Item_Type)
     return Natural;

package Giant.Fixed_Priority_Queues is

   pragma Elaborate_Body;

   ----------------------------------------------------------------------------
   --  Type for fixed size priority queues
   type Queue_Type is private;

   ----------------------------------------------------------------------------
   --  Raised whenever the size of a 'Queue_Type' exceeds 'Max_Size' items
   Too_Many_Items   : exception;

   ----------------------------------------------------------------------------
   --  Raised whenever a 'Queue_Type' is empty, but an item is needed
   Not_Enough_Items : exception;

   ----------------------------------------------------------------------------
   --  Raised whenever a method expects one special item in a 'Queue_Type',
   --  but that item is not contained in that 'Queue_Type'
   Unknown_Item     : exception;

   ----------------------------------------------------------------------------
   --  Raised whenever an item is inserted into a queue but is already
   --  contained in one
   Invalid_Item     : exception;

   ----------------------------------------------------------------------------
   --  Inserts an item into a queue. Note that each item must at most be
   --  contained in one queue at any point of time.
   --
   --  Precondition:
   --    Get_Size (Queue) < Max_Size and
   --    Get_Position (Item) = 0
   --    for all Q : Queue_Type: not Contains (Q, Item)
   --  Postcondition:
   --    Contains (Queue, Item)
   --    and not Is_Empty (Queue)
   --    and Get_Position (Item) /= 0
   --  Raises:
   --    * Too_Many_Items if Get_Size (Queue) >= Max_Size
   --    * Invalid_Item if Item already contained in some queue
   --                   precisely: Get_Position (Item) /= 0
   --  Warning:
   --    Set_Position (Item) must never be called from outside this package
   --    until 'Item' is removed from 'Queue'
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
   procedure Update_Head_Item
     (Queue : in out Queue_Type);

   ----------------------------------------------------------------------------
   --  Must be called whenever the priority of 'Item' has changed.
   --
   --  Precondition:
   --    Contains (Queue, Item)
   --  Raises:
   --    Unknown_Item if Precondition not satisfied
   procedure Update_Item
     (Queue : in out Queue_Type;
      Item  : in     Item_Type);

   ----------------------------------------------------------------------------
   --  Removes the head item from 'Queue'
   --
   --  Precondition:
   --    not Is_Empty (Queue) and I := Get_Head (Queue)
   --  Postcondition:
   --    not Contains (Queue, I) and
   --    Get_Position (I) = 0
   --  Raises:
   --    Not_Enough_Items if Precondition not satisfied
   procedure Remove_Head
     (Queue : in out Queue_Type);

   ----------------------------------------------------------------------------
   --  Precondition:
   --    Contains (Queue, Item)
   --  Postcondition
   --    not Contains (Queue, Item) and
   --    Get_Position (Item) = 0
   --  Raises:
   --    Unknown_Item if Precondition not satisfied
   procedure Remove_Item
     (Queue : in out Queue_Type;
      Item  : in     Item_Type);

   ----------------------------------------------------------------------------
   --  Precondition:
   --    True and Former_Content := {I | Contains (Queue, I)}
   --  Postcondition:
   --    Is_Empty (Queue)
   --    for all I in Former_Content: Get_Position (I) = 0
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
   --    for all I : Item_Type:
   --      Contains (Queue, I) ==> not Has_Higher_Priority (I, Head)
   --  Precondition:
   --    not Is_Empty (Queue)
   --  Raises:
   --    Not_Enough_Items if Precondition not satisfied
   function Get_Head
     (Queue : in     Queue_Type)
     return Item_Type;

   ----------------------------------------------------------------------------
   --  Checks if 'Item' is contained in 'Queue'
   --
   --  Returns:
   --    True if the statement
   --    'while Get_Head (Queue) /= Item loop Remove_Head (Queue) end loop;'
   --    does not raise an exception, False otherwise
   function Contains
     (Queue : in     Queue_Type;
      Item  : in     Item_Type)
     return Boolean;

   ----------------------------------------------------------------------------
   --  Returns:
   --    for all I : Item_Type: not Contains (Queue, I)
   function Is_Empty
     (Queue : in     Queue_Type)
     return Boolean;

   ----------------------------------------------------------------------------
   --  Returns:
   --    Cardinality of the set {I | Contains (Queue, I)}
   function Get_Size
     (Queue : in     Queue_Type)
     return Natural;

private

   subtype Array_Bounds is Positive range 1 .. Max_Size;

   type Heap_Array is array (Array_Bounds) of Item_Type;

   type Queue_Type is
      record
         Size  : Natural := 0;
         Field : Heap_Array;
      end record;

end Giant.Fixed_Priority_Queues;