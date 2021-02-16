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
with USB.Device; use USB.Device;
with USB.HAL.Device; use USB.HAL.Device;

package USB_Testing.UDC_Stub is

   LANG_EN_US : constant USB.USB_String := (ASCII.HT, ASCII.EOT); -- 0x0409

   Strings : aliased constant USB.String_Array :=
     (
      (0, new USB.String_Descriptor'(2 + 2,  3, LANG_EN_US)),
      (1, new USB.String_Descriptor'(2 + 12 * 2, 3,
       ('M', ASCII.NUL,
        'a', ASCII.NUL,
        'n', ASCII.NUL,
        'u', ASCII.NUL,
        'f', ASCII.NUL,
        'a', ASCII.NUL,
        'c', ASCII.NUL,
        't', ASCII.NUL,
        'u', ASCII.NUL,
        'r', ASCII.NUL,
        'e', ASCII.NUL,
        'r', ASCII.NUL))),
      (2, new USB.String_Descriptor'(2 + 7 * 2, 3,
       ('P', ASCII.NUL,
        'r', ASCII.NUL,
        'o', ASCII.NUL,
        'd', ASCII.NUL,
        'u', ASCII.NUL,
        'c', ASCII.NUL,
        't', ASCII.NUL))),
      (3, new USB.String_Descriptor'(2 + 6 * 2, 3,
       ('S', ASCII.NUL,
        'e', ASCII.NUL,
        'r', ASCII.NUL,
        'i', ASCII.NUL,
        'a', ASCII.NUL,
        'l', ASCII.NUL)))
     );
   --  String descriptor than can be used for testing

   type Scenario_Event_Kind is (Set_Verbose, UDC_Event_E, Transfer_All);

   type Scenario_Event (Kind : Scenario_Event_Kind := UDC_Event_E) is record
      case Kind is
         when Set_Verbose =>
            Verbose : Boolean;
         when UDC_Event_E =>
            Evt     : UDC_Event;
         when Transfer_All =>
            EP : EP_Addr;
      end case;
   end record;

   type Stub_Scenario is array (Natural range <>) of Scenario_Event;

   type Controller
     (Scenario          : not null access constant Stub_Scenario;
      RX_Data           : not null access constant UInt8_Array;
      Has_Early_Address : Boolean)
   is new USB_Device_Controller
   with private;

   function End_Of_Scenario (This : Controller) return Boolean;

   overriding
   procedure Initialize (This : in out Controller);

   overriding
   procedure Start (This : in out Controller);

   overriding
   function Poll (This : in out Controller) return UDC_Event;

   overriding
   procedure Set_EP_Callback (This     : in out Controller;
                              EP       : EP_Addr;
                              Callback : EP_Callback);

   overriding
   procedure Set_Setup_Callback (This     : in out Controller;
                                 EP       : EP_Id;
                                 Callback : Setup_Callback);

   overriding
   procedure EP_Read_Packet (This : in out Controller;
                             Ep   : EP_Id;
                             Addr : System.Address;
                             Len  : UInt32);

   overriding
   procedure EP_Write_Packet (This : in out Controller;
                              Ep   : EP_Id;
                              Addr : System.Address;
                              Len  : UInt32);

   overriding
   procedure EP_Setup (This     : in out Controller;
                       EP       : EP_Addr;
                       Typ      : EP_Type;
                       Max_Size : UInt16;
                       Callback : EP_Callback);

   overriding
   procedure EP_Set_NAK (This : in out Controller;
                         EP   : EP_Addr;
                         NAK  : Boolean);

   overriding
   procedure EP_Set_Stall (This : in out Controller;
                           EP   : EP_Addr);

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
      Setup           : Boolean := False;
      NAK             : Boolean := False;
      Stall           : Boolean := False;
      Typ             : EP_Type := Control;
      Max_Size        : UInt16  := 0;
      Bytes_Available : UInt11  := 0;
   end record;

   type EP_Stub_Couple is array (EP_Dir) of EP_Stub;

   type EP_Stub_Array is array (EP_Id range <>) of EP_Stub_Couple;

   type Controller_State is (Nominal,
                             Device_To_Host_Transfer,
                             Host_To_Device_Transfer);

   type Controller
     (Scenario          : not null access constant Stub_Scenario;
      RX_Data           : not null access constant UInt8_Array;
      Has_Early_Address : Boolean)
   is new USB_Device_Controller
   with record
      State : Controller_State := Nominal;
      Stack : Event_Stack.List;

      Scenario_Index : Natural := Scenario.all'First;
      RX_Index       : Natural := RX_Data.all'First;

      Verbose        : Boolean := True;

      EPs            : EP_Stub_Array (0 .. 5); -- Arbitrary number of EP

      Got_Ack        : Boolean := False;
   end record;

   function Pop (This : in out Controller) return Scenario_Event
     with Pre => not This.Stack.Is_Empty;

   procedure Push (This : in out Controller;
                   Evt  :        Scenario_Event);

   procedure Put_Line (This : Controller;
                       Str  : String);
   --  Print on the console iff in verbose mode

   procedure Put (This : Controller;
                  Str  : String);
   --  Print on the console iff in verbose mode

   procedure Hex_Dump (This : Controller;
                       Data : HAL.UInt8_Array);
   --  Print on the console iff in verbose mode
end USB_Testing.UDC_Stub;
