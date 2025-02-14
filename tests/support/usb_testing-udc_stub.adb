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

with System; use System;

package body USB_Testing.UDC_Stub is

   procedure Check_Event (This : Controller'Class;
                          Evt  : UDC_Event);
   --  Raise an exception if the UDC event is not valid given the current
   --  configuration of the controller (e.g. data on stalled EP)

   procedure Do_Out_Transfer (This : in out Controller'Class;
                              Ep   : EP_Id;
                              Len  : Packet_Size);
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

         when Transfer_Complete =>
            if Evt.EP.Num not in This.EPs'Range then
               raise Program_Error with
                 Prefix & "Invalid EP id in Transfer_Complete";
            end if;

            if not This.EPs (Evt.EP.Num) (Evt.EP.Dir).Setup then
               raise Program_Error with
                 Prefix & "EP not setup for Transfer_Complete";
            end if;

            if This.EPs (Evt.EP.Num) (Evt.EP.Dir).Stall then
               raise Program_Error with
                 Prefix & "EP stalled for Transfer_Complete";
            end if;

         when None | Reset =>
            null; -- Nothing to check
      end case;
   end Check_Event;

   ---------------------
   -- Do_Out_Transfer --
   ---------------------

   procedure Do_Out_Transfer (This : in out Controller'Class;
                              Ep   : EP_Id;
                              Len  : Packet_Size)
   is
   begin
      if Len = 0 then
         return;
      end if;

      This.Put_Line ("UDC OUT Transfer " & Img (EP_Addr'(Ep, EP_Out)) &
                     Len'Img & " bytes");

      if Ep not in This.EPs'Range then
         raise Program_Error with "UDC Error: invalid EP number in EP_Read_Packet";
      end if;

      if not This.EPs (Ep) (EP_In).Setup then
         raise Program_Error with "UDC Error: EP not setup in EP_Read_Packet";
      end if;

      if This.EPs (Ep) (EP_In).Stall then
         raise Program_Error with "UDC Error: EP stalled in EP_Read_Packet";
      end if;

      if This.EPs (Ep) (EP_In).Stall then
         raise Program_Error with "UDC Error: EP stalled in EP_Read_Packet";
      end if;

      if Len > This.EPs (Ep) (EP_Out).Transfer_Len then
         raise Program_Error with "UDC Error: Trying to write " & Len'Img &
           " byte(s) to a" & This.EPs (Ep) (EP_Out).Transfer_Len'Img &
           " byte(s) OUT transfer";
      end if;

      if This.EPs (Ep) (EP_Out).EP_Buf = System.Null_Address then
         raise Program_Error with "UDC Error: Trying to write to a null EP OUT buffer";
      end if;

      declare
         Data : UInt8_Array (1 .. Natural (Len))
           with Address => This.EPs (Ep) (EP_Out).EP_Buf;
      begin
         for Elt of Data loop
            if This.RX_Index in This.RX_Data'Range then
               Elt := This.RX_Data (This.RX_Index);
               This.RX_Index := This.RX_Index + 1;
            else
               raise Program_Error with "UDC Error: Not enough data in RX_Data";
            end if;
         end loop;
         This.Hex_Dump (Data);
      end;
   end Do_Out_Transfer;

   ---------------------
   -- End_Of_Scenario --
   ---------------------

   function End_Of_Scenario (This : Controller)
                             return Boolean
   is (This.Stack.Is_Empty);

   ----------------
   -- Initialize --
   ----------------

   overriding
   procedure Initialize (This : in out Controller)
   is
   begin
      for Elt of This.Scenario.all loop
         This.Stack.Prepend (Elt);
      end loop;

      This.Put_Line ("UDC Initialize");

      for EP_Couple of This.EPs loop
         for EP of EP_Couple loop
            EP.Setup := False;
            EP.NAK := False;
            EP.Stall := False;
            EP.Transfer_Len := 0;
         end loop;
      end loop;
   end Initialize;

   --------------------
   -- Request_Buffer --
   --------------------

   overriding
   function Request_Buffer (This          : in out Controller;
                            Ep            :        EP_Addr;
                            Len           :        Packet_Size)
                            return System.Address
   is
   begin
      This.Put_Line ("UDC Request_Buffer (" & Img (Ep) &
                       ", Len =>" & Len'Img & ")");

      This.EPs (Ep.Num) (Ep.Dir).EP_Buf :=
        Standard.USB.Utils.Allocate (This.Alloc,
                                     Alignment => 1,
                                     Len       => Len);

      if This.EPs (Ep.Num) (Ep.Dir).EP_Buf /= System.Null_Address then
         This.EPs (Ep.Num) (Ep.Dir).Max_Size := Len;
      else
         This.EPs (Ep.Num) (Ep.Dir).Max_Size := 0;
      end if;

      return This.EPs (Ep.Num) (Ep.Dir).EP_Buf;
   end Request_Buffer;

   -----------
   -- Start --
   -----------

   overriding
   procedure Start (This : in out Controller)
   is
   begin
      This.Put_Line ("UDC Start");
   end Start;

   -----------
   -- Reset --
   -----------

   overriding
   procedure Reset (This : in out Controller)
   is
   begin
      This.Put_Line ("UDC Reset");
   end Reset;

   ----------
   -- Poll --
   ----------

   overriding
   function Poll (This : in out Controller)
                  return UDC_Event
   is
      Ret : UDC_Event;
   begin

      loop
         if This.Stack.Is_Empty then
            Ret := No_Event;
            exit;
         end if;

         declare
            Step : Scenario_Event renames This.Pop;
         begin

            case Step.Kind is
            when Set_Verbose =>
               if Step.Verbose /= This.Verbose then
                  This.Verbose := not This.Verbose;
                  This.Output.Put_Line ("UDC Verbose "  & (if This.Verbose then
                                           "on" else "off"));
               end if;

            when Transfer_Out =>
               null;

            when Transfer_In =>
               This.EPs (Step.EP_In) (EP_In).Scenario_Waiting_For_Data := True;

            when UDC_Event_E =>
               Ret := Step.Evt;
               exit;
            end case;
         end;
      end loop;

      This.Put_Line ("UDC Poll -> " & Img (Ret));

      Check_Event (This, Ret);

      if Ret.Kind = Reset then
         Reset (This);
      end if;

      if Ret.Kind = Transfer_Complete and then Ret.EP.Dir = EP_Out then
         Do_Out_Transfer (This, Ret.EP.Num, Ret.BCNT);
      end if;

      return Ret;
   end Poll;

   --------------------
   -- EP_Send_Packet --
   --------------------

   overriding
   procedure EP_Send_Packet (This : in out Controller;
                             Ep   : EP_Id;
                             Len  : Packet_Size)
   is
      Data : UInt8_Array (1 .. Natural (Len))
        with Address => This.EPs (Ep)(EP_In).EP_Buf;

   begin
      This.Put_Line ("UDC EP_Write_Packet " & Img (EP_Addr'(Ep, EP_In)) &
                    (if Len = 0 then " ZLP" else Len'Img & " bytes"));

      if Ep not in This.EPs'Range then
         raise Program_Error with "UDC Error: invalid EP number in EP_Write";
      end if;

      if not This.EPs (Ep) (EP_In).Setup then
         raise Program_Error with "UDC Error: EP not setup in EP_Write_Packet";
      end if;

      if This.EPs (Ep) (EP_In).Stall then
         raise Program_Error with "UDC Error: EP stalled in EP_Write_Packet";
      end if;

      if This.EPs (Ep) (EP_In).EP_Buf = System.Null_Address then
         raise Program_Error with "UDC Error: Trying to read from a null EP IN buffer";
      end if;

      if Len > This.EPs (Ep) (EP_In).Max_Size then
         raise Program_Error with "UDC Error: Packet too big in EP_Write_Packet";
      end if;

      This.Hex_Dump (Data);

      This.Push ((Kind => UDC_Event_E,
                  Evt  => (Kind => Transfer_Complete,
                           EP   => (Ep, EP_In),
                           BCNT => Len)));

   end EP_Send_Packet;

   --------------
   -- EP_Setup --
   --------------

   overriding
   procedure EP_Setup (This : in out Controller;
                       EP   : EP_Addr;
                       Typ  : EP_Type)
   is
   begin
      This.Put_Line ("UDC EP_Setup " & Img (EP) &
                       " Type: " & Typ'Img);

      if EP.Num not in This.EPs'Range then
         raise Program_Error with "UDC Error: invalid EP number in EP_Setup";
      end if;

      This.EPs (EP.Num) (EP.Dir).Setup := True;
      This.EPs (EP.Num) (EP.Dir).Typ := Typ;
   end EP_Setup;

   -----------------------
   -- EP_Ready_For_Data --
   -----------------------

   overriding
   procedure EP_Ready_For_Data (This  : in out Controller;
                                EP    : EP_Id;
                                Size  : Packet_Size;
                                Ready : Boolean := True)
   is
   begin
      This.Put_Line ("UDC EP_Ready_For_Data " &
                       Img (EP_Addr'(EP, EP_Out)) & " " & Ready'Img);

      if EP not in This.EPs'Range then
         raise Program_Error
           with "UDC Error: invalid EP number in EP_Ready_For_Data";
      end if;

      This.EPs (EP) (EP_Out).NAK := not Ready;
      This.EPs (EP) (EP_Out).Transfer_Len := Size;
   end EP_Ready_For_Data;

   --------------
   -- EP_Stall --
   --------------

   overriding
   procedure EP_Stall (This : in out Controller;
                       EP   : EP_Addr;
                       Set  : Boolean)
   is
   begin
      This.Put_Line ("UDC EP_Stall " & Img (EP) & " " & Set'Img);

      if EP.Num not in This.EPs'Range then
         raise Program_Error with "UDC Error: invalid EP number in EP_Stall";
      end if;

      This.EPs (EP.Num) (EP.Dir).Stall := Set;
   end EP_Stall;

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

   ---------
   -- Pop --
   ---------

   function Pop (This : in out Controller) return Scenario_Event is
      Ret : constant Scenario_Event := This.Stack.Last_Element;
   begin
      This.Stack.Delete_Last;
      return Ret;
   end Pop;

   ----------
   -- Push --
   ----------

   procedure Push (This : in out Controller;
                   Evt  :        Scenario_Event)
   is
   begin
      This.Stack.Append (Evt);
   end Push;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (This : in out Controller;
                       Str  : String)
   is
   begin
      if This.Verbose then
         This.Output.Put_Line (Str);
      end if;
   end Put_Line;

   ---------
   -- Put --
   ---------

   procedure Put (This : in out Controller;
                  Str  : String)
   is
   begin
      if This.Verbose then
         This.Output.Put (Str);
      end if;
   end Put;

   --------------
   -- Hex_Dump --
   --------------

   procedure Hex_Dump (This : in out Controller;
                       Data : HAL.UInt8_Array)
   is
   begin
      if This.Verbose then
         This.Output.Hex_Dump (Data);
      end if;
   end Hex_Dump;

end USB_Testing.UDC_Stub;
