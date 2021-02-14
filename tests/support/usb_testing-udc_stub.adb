------------------------------------------------------------------------------
--                                                                          --
--                        Copyright (C) 2018, AdaCore                       --
--                                                                          --
--  Redistribution and use in source and binary forms, with or without      --
--  modification, are permitted provided that the following conditions are  --
--  met:                                                                    --
--     1. Redistributions of source code must retain the above copyright    --
--        notice, this list of conditions and the following disclaimer.     --
--     2. Redistributions in binary form must reproduce the above copyright --
--        notice, this list of conditions and the following disclaimer in   --
--        the documentation and/or other materials provided with the        --
--        distribution.                                                     --
--     3. Neither the name of the copyright holder nor the names of its     --
--        contributors may be used to endorse or promote products derived   --
--        from this software without specific prior written permission.     --
--                                                                          --
--   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS    --
--   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT      --
--   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR  --
--   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT   --
--   HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, --
--   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT       --
--   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,  --
--   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY  --
--   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT    --
--   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE  --
--   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.   --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Text_IO;
with Hex_Dump;

package body USB_Testing.UDC_Stub is

   procedure Reset (This : in out Controller'Class);

   procedure Check_Event (This : Controller'Class;
                          Evt  : UDC_Event);
   --  Raise an exception if the UDC event is not valid given the current
   --  configuration of the controller (e.g. data on stalled EP)

   -----------
   -- Reset --
   -----------

   procedure Reset (This : in out Controller'Class) is
   begin
      This.Put_Line ("UDC Reset EPs");
      for EP_Couple of This.EPs loop
         for EP of EP_Couple loop
            EP := (others => <>);
         end loop;
      end loop;
   end Reset;

   -----------------
   -- Check_Event --
   -----------------

   procedure Check_Event (This : Controller'Class;
                          Evt  : UDC_Event)
   is
      Prefix : constant String := "UDC_Event error: ";
   begin
      case Evt.Kind is
         when Setup_Request =>
            if Evt.Req_EP not in This.EPs'Range then
               raise Program_Error with
                 Prefix & "Invalid EP id in Setup_Request";
            end if;

            if not This.EPs (Evt.Req_EP) (EP_Out).Setup then
               raise Program_Error with
                 Prefix & "EP not setup for Setup_Request";
            end if;

            if This.EPs (Evt.Req_EP) (EP_Out).Typ /= Control then
               raise Program_Error with
                 Prefix & "EP not in control mode for Setup_Request";
            end if;

            if This.EPs (Evt.Req_EP) (EP_Out).Stall then
               raise Program_Error with
                 Prefix & "EP stalled for Setup_Request";
            end if;

            if This.EPs (Evt.Req_EP) (EP_Out).Max_Size < 8 then
               raise Program_Error with
                 Prefix & "EP Max_Size too small for Setup_Request";
            end if;

         when Data_Ready =>
            if Evt.RX_EP not in This.EPs'Range then
               raise Program_Error with
                 Prefix & "Invalid EP id in Data_Ready";
            end if;

            if not This.EPs (Evt.RX_EP) (EP_Out).Setup then
               raise Program_Error with
                 Prefix & "EP not setup for Data_Ready";
            end if;

            if This.EPs (Evt.RX_EP) (EP_Out).Stall then
               raise Program_Error with
                 Prefix & "EP stalled for Data_Ready";
            end if;

            if UInt16 (Evt.RX_BCNT) > This.EPs (Evt.RX_EP) (EP_Out).Max_Size
            then
               raise Program_Error with
                 Prefix & "packet to big for EP in Data_Ready";
            end if;

         when Transfer_Complete =>
            if Evt.T_EP.Num not in This.EPs'Range then
               raise Program_Error with
                 Prefix & "Invalid EP id in Transfer_Complete";
            end if;

            if not This.EPs (Evt.T_EP.Num) (Evt.T_EP.Dir).Setup then
               raise Program_Error with
                 Prefix & "EP not setup for Transfer_Complete";
            end if;

            if This.EPs (Evt.T_EP.Num) (Evt.T_EP.Dir).Stall then
               raise Program_Error with
                 Prefix & "EP stalled for Transfer_Complete";
            end if;

         when None | Reset =>
            null; -- Nothing to check
      end case;
   end Check_Event;

   ---------------------
   -- End_Of_Scenario --
   ---------------------

   function End_Of_Scenario (This : Controller)
                             return Boolean
   is (This.Scenario_Index not in This.Scenario'Range);

   ----------------
   -- Initialize --
   ----------------

   overriding
   procedure Initialize (This : in out Controller)
   is
   begin
      This.Put_Line ("UDC Initialize");

      Reset (This);
   end Initialize;

   -----------
   -- Start --
   -----------

   overriding
   procedure Start (This : in out Controller)
   is
   begin
      This.Put_Line ("UDC Start");
   end Start;

   ----------
   -- Poll --
   ----------

   overriding
   function Poll (This : in out Controller)
                  return UDC_Event
   is
      Ret : constant UDC_Event :=
        (if This.Scenario_Index in This.Scenario'Range then
            This.Scenario (This.Scenario_Index).Evt
         else
            No_Event);
   begin
      if This.Scenario_Index in This.Scenario'Range then

         if This.Scenario (This.Scenario_Index).Verbose /= This.Verbose then
            This.Verbose := not This.Verbose;
            Ada.Text_IO.Put_Line ("UDC Verbose "  & (if This.Verbose then
                                     "on" else "off"));
         end if;

         This.Scenario_Index := This.Scenario_Index + 1;
      end if;

      This.Put_Line ("UDC Poll -> " & Img (Ret));

      Check_Event (This, Ret);

      if Ret.Kind = Reset then
         Reset (This);
      end if;

      if Ret.Kind = Data_Ready then
         This.EPs (Ret.RX_EP) (EP_Out).Bytes_Available := Ret.RX_BCNT;
      end if;

      return Ret;
   end Poll;

   ---------------------
   -- Set_EP_Callback --
   ---------------------

   overriding
   procedure Set_EP_Callback (This     : in out Controller;
                              EP       : EP_Addr;
                              Callback : EP_Callback)
   is
   begin
      null;
   end Set_EP_Callback;

   ------------------------
   -- Set_Setup_Callback --
   ------------------------

   overriding
   procedure Set_Setup_Callback (This     : in out Controller;
                                 EP       : EP_Id;
                                 Callback : Setup_Callback)
   is
   begin
      null;
   end Set_Setup_Callback;

   --------------------
   -- EP_Read_Packet --
   --------------------

   overriding
   procedure EP_Read_Packet (This : in out Controller;
                             Ep   : EP_Id;
                             Addr : System.Address;
                             Len  : UInt32)
   is
      Data : UInt8_Array (1 .. Natural (Len)) with Address => Addr;
   begin
      This.Put_Line ("UDC EP_Read_Packet " & Img (EP_Addr'(Ep, EP_Out)) &
                       " Len:" & Len'Img);

      if Ep not in This.EPs'Range then
         raise Program_Error with "UDC Error: invalid EP number in EP_Read_Packet";
      end if;

      if not This.EPs (Ep) (EP_In).Setup then
         raise Program_Error with "UDC Error: EP not setup in EP_Read_Packet";
      end if;

      if This.EPs (Ep) (EP_In).Stall then
         raise Program_Error with "UDC Error: EP stalled in EP_Read_Packet";
      end if;

      if Len > UInt32 (This.EPs (Ep) (EP_Out).Bytes_Available) then
         raise Program_Error with "UDC Error: Trying to read too much data in EP_Read_Packet";
      end if;

      This.EPs (Ep) (EP_In).Bytes_Available :=
        This.EPs (Ep) (EP_In).Bytes_Available - UInt11 (Len);

      for Elt of Data loop
         if This.RX_Index in This.RX_Data'Range then
            Elt := This.RX_Data (This.RX_Index);
            This.RX_Index := This.RX_Index + 1;
         else
            raise Program_Error with "UDC Error: Not enough data in RX_Data";
         end if;
      end loop;

      This.Hex_Dump (Data);
   end EP_Read_Packet;

   ---------------------
   -- EP_Write_Packet --
   ---------------------

   overriding
   procedure EP_Write_Packet (This : in out Controller;
                              Ep   : EP_Id;
                              Addr : System.Address;
                              Len  : UInt32)
   is
      Data : UInt8_Array (1 .. Natural (Len)) with Address => Addr;

   begin
      This.Put_Line ("UDC EP_Write_Packet " & Img (EP_Addr'(Ep, EP_In)) &
                    (if Len = 0 then " ZLP" else ""));

      if Ep not in This.EPs'Range then
         raise Program_Error with "UDC Error: invalid EP number in EP_Write";
      end if;

      if not This.EPs (Ep) (EP_In).Setup then
         raise Program_Error with "UDC Error: EP not setup in EP_Write_Packet";
      end if;

      if This.EPs (Ep) (EP_In).Stall then
         raise Program_Error with "UDC Error: EP stalled in EP_Write_Packet";
      end if;

      if Len > UInt32 (This.EPs (Ep) (EP_In).Max_Size) then
         raise Program_Error with "UDC Error: Packet too big in EP_Write_Packet";
      end if;

      This.Hex_Dump (Data);

   end EP_Write_Packet;

   --------------
   -- EP_Setup --
   --------------

   overriding
   procedure EP_Setup (This     : in out Controller;
                       EP       : EP_Addr;
                       Typ      : EP_Type;
                       Max_Size : UInt16;
                       Callback : EP_Callback)
   is
      pragma Unreferenced (Callback);
   begin
      This.Put_Line ("UDC EP_Setup " & Img (EP) &
                       " Type: " & Typ'Img &
                       " Max_Size:" & Max_Size'Img);

      if EP.Num not in This.EPs'Range then
         raise Program_Error with "UDC Error: invalid EP number in EP_Setup";
      end if;

      This.EPs (EP.Num) (EP.Dir).Setup := True;
      This.EPs (EP.Num) (EP.Dir).Typ := Typ;
      This.EPs (EP.Num) (EP.Dir).Max_Size := Max_Size;
   end EP_Setup;

   ----------------
   -- EP_Set_NAK --
   ----------------

   overriding
   procedure EP_Set_NAK (This : in out Controller;
                         EP   : EP_Addr;
                         NAK  : Boolean)
   is
   begin
      This.Put_Line ("UDC EP_Set_NAK " & Img (EP) & " " & NAK'Img);

      if EP.Dir /= EP_Out then
         raise Program_Error with "UDC Error: cannot NAK IN endpoints";
      end if;

      if EP.Num not in This.EPs'Range then
         raise Program_Error with "UDC Error: invalid EP number in EP_Set_NAK";
      end if;

      This.EPs (EP.Num) (EP.Dir).NAK := NAK;
   end EP_Set_NAK;

   ------------------
   -- EP_Set_Stall --
   ------------------

   overriding
   procedure EP_Set_Stall (This : in out Controller;
                           EP   : EP_Addr)
   is
   begin
      This.Put_Line ("UDC EP_Set_NAK " & Img (EP));

      if EP.Num not in This.EPs'Range then
         raise Program_Error with "UDC Error: invalid EP number in EP_Set_Stall";
      end if;

      This.EPs (EP.Num) (EP.Dir).Stall := True;
   end EP_Set_Stall;

   -----------------
   -- Set_Address --
   -----------------

   overriding
   procedure Set_Address (This : in out Controller;
                          Addr : UInt7)
   is
   begin
      This.Put_Line ("UDC Set_Address" & Addr'Img);
   end Set_Address;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (This : Controller;
                       Str  : String)
   is
   begin
      if This.Verbose then
         Ada.Text_IO.Put_Line (Str);
      end if;
   end Put_Line;

   ---------
   -- Put --
   ---------

   procedure Put (This : Controller;
                  Str  : String)
   is
   begin
      if This.Verbose then
         Ada.Text_IO.Put (Str);
      end if;
   end Put;

   --------------
   -- Hex_Dump --
   --------------

   procedure Hex_Dump (This : Controller;
                       Data : HAL.UInt8_Array)
   is
   begin
      if This.Verbose then
         Standard.Hex_Dump.Hex_Dump (Data, Ada.Text_IO.Put_Line'Access);
      end if;
   end Hex_Dump;

end USB_Testing.UDC_Stub;
