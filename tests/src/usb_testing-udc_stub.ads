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

--  USB Device Controller stub for testing

with Ada.Containers.Doubly_Linked_Lists;

with System;

with HAL; use HAL;
with USB; use USB;
with USB.HAL.Device; use USB.HAL.Device;
with USB.Utils;

with USB_Testing.Output; use USB_Testing.Output;

package USB_Testing.UDC_Stub is

   type Scenario_Event_Kind is (Set_Verbose,
                                UDC_Event_E,
                                Transfer_Out,
                                Transfer_In);

   type Scenario_Event (Kind : Scenario_Event_Kind := UDC_Event_E) is record
      case Kind is
         when Set_Verbose =>
            Verbose : Boolean;
         when UDC_Event_E =>
            Evt     : UDC_Event;
         when Transfer_Out =>
            EP_Out : EP_Id;
            Count_Out  : Natural;
         when Transfer_In =>
            EP_In : EP_Id;
      end case;
   end record;

   type Stub_Scenario is array (Natural range <>) of Scenario_Event;
   Empty_Scenario : constant Stub_Scenario (1 .. 0) := (others => <>);

   type Controller
     (Output            : not null Text_Output_Acc;
      Scenario          : not null access constant Stub_Scenario;
      RX_Data           : not null access constant UInt8_Array;
      Has_Early_Address : Boolean;
      Max_Packet_Size   : UInt32;
      EP_Buffers_Size   : Natural;
      Number_Of_EPs     : EP_Id;
      Init_Verbose      : Boolean)
   is new USB_Device_Controller
   with private;

   function End_Of_Scenario (This : Controller) return Boolean;

   overriding
   procedure Initialize (This : in out Controller);

   overriding
   function Request_Buffer (This          : in out Controller;
                            Ep            :        EP_Addr;
                            Len           :        Packet_Size)
                            return System.Address;

   overriding
   function Valid_EP_Id (This : in out Controller;
                         EP   :        EP_Id)
                         return Boolean
   is (EP <= This.Number_Of_EPs);

   overriding
   procedure Start (This : in out Controller);

   overriding
   procedure Reset (This : in out Controller);

   overriding
   function Poll (This : in out Controller) return UDC_Event;

   overriding
   procedure EP_Send_Packet (This : in out Controller;
                             Ep   : EP_Id;
                             Len  : Packet_Size);

   overriding
   procedure EP_Setup (This : in out Controller;
                       EP   : EP_Addr;
                       Typ  : EP_Type);

   overriding
   procedure EP_Ready_For_Data (This  : in out Controller;
                                EP    : EP_Id;
                                Size  : Packet_Size;
                                Ready : Boolean := True);

   overriding
   procedure EP_Stall (This : in out Controller;
                       EP   :        EP_Addr;
                       Set  :        Boolean);

   overriding
   procedure Set_Address (This : in out Controller;
                          Addr : UInt7);

   overriding
   function Early_Address (This : Controller) return Boolean
   is (This.Has_Early_Address);

private

   package Event_Stack
   is new Ada.Containers.Doubly_Linked_Lists (Scenario_Event);
   use Event_Stack;

   type EP_Stub is record
      Setup            : Boolean := False;
      NAK              : Boolean := False;
      Stall            : Boolean := False;
      Typ              : EP_Type := Control;
      EP_Buf           : System.Address := System.Null_Address;
      Max_Size         : Packet_Size := 0;

      Transfer_Len     : Packet_Size := 0;
      Scenario_Waiting_For_Data : Boolean := False;
   end record;

   type EP_Stub_Couple is array (EP_Dir) of EP_Stub;

   type EP_Stub_Array is array (EP_Id range <>) of EP_Stub_Couple;

   type Controller_State is (Nominal,
                             Device_To_Host_Transfer,
                             Host_To_Device_Transfer);

   type Controller
     (Output            : not null Text_Output_Acc;
      Scenario          : not null access constant Stub_Scenario;
      RX_Data           : not null access constant UInt8_Array;
      Has_Early_Address : Boolean;
      Max_Packet_Size   : UInt32;
      EP_Buffers_Size   : Natural;
      Number_Of_EPs     : EP_Id;
      Init_Verbose      : Boolean)
   is new USB_Device_Controller
   with record

      State : Controller_State := Nominal;
      Stack : Event_Stack.List;

      Scenario_Index : Natural := Scenario.all'First;
      RX_Index       : Natural := RX_Data.all'First;

      Verbose        : Boolean := Init_Verbose;

      EPs            : EP_Stub_Array (0 .. 5); -- Arbitrary number of EP

      Got_Ack        : Boolean := False;

      Alloc          : USB.Utils.Basic_RAM_Allocator (EP_Buffers_Size);
   end record;

   function Pop (This : in out Controller) return Scenario_Event
     with Pre => not This.Stack.Is_Empty;

   procedure Push (This : in out Controller;
                   Evt  :        Scenario_Event);

   procedure Put_Line (This : in out Controller;
                       Str  : String);
   --  Print on the console iff in verbose mode

   procedure Put (This : in out Controller;
                  Str  : String);
   --  Print on the console iff in verbose mode

   procedure Hex_Dump (This : in out Controller;
                       Data : HAL.UInt8_Array);
   --  Print on the console iff in verbose mode
end USB_Testing.UDC_Stub;
