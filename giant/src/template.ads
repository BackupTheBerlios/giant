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
--  First Author: <unknown>
--
--  $RCSfile: template.ads,v $, $Revision: 1.3 $
--  $Author: squig $
--  $Date: 2003/12/04 11:46:53 $
--
------------------------------------------------------------------------------
--
--  Contains the GIANT source template.
--

with Giant.Controller;

package Giant.Template is

   ---------------------------------------------------------------------------
   --  Stores a foo bar.
   type Coordinate is private record;

   ---------------------------------------------------------------------------
   --  Raised on attempt show already visible window.
   Already_Visible_Exception : exception;

   ---------------------------------------------------------------------------
   --  Makes W visible on screen.
   --
   --  Parameters:
   --    W - The Window
   --  Returns:
   --    True, if successful; False, otherwise
   --  Raises:
   --    Already_Visible_Exception - raised if W is already visible
   function Show_Window
     (W : in Coordinate)
     return Booolean;

private

   type Coordinate is record
      --  X Coordinate
      X : Float;
      --  Y Coordinate
      Y : Float;
   end record;


end Giant.Template;
