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

with HAL.USB;

package USB.Device.MIDI is

   type Default_MIDI_Class
   is new USB_Device_Class with private;

   overriding
   function Configure (This  : in out Default_MIDI_Class;
                       UDC   : in out USB_Device_Controller'Class;
                       Index : UInt16)
                       return Setup_Request_Answer;

   overriding
   function Setup_Read_Request (This  : in out Default_MIDI_Class;
                                Req   : HAL.USB.Setup_Data;
                                Buf   : out System.Address;
                                Len   : out Buffer_Len)
                                return Setup_Request_Answer;

   overriding
   function Setup_Write_Request (This  : in out Default_MIDI_Class;
                                 Req   : HAL.USB.Setup_Data;
                                 Data  : UInt8_Array)
                                 return Setup_Request_Answer;

   overriding
   procedure Transfer_Complete (This : in out Default_MIDI_Class;
                                UDC  : in out USB_Device_Controller'Class;
                                EP   : HAL.USB.EP_Addr);

   overriding
   procedure Data_Ready (This : in out Default_MIDI_Class;
                         UDC  : in out USB_Device_Controller'Class;
                         EP   : HAL.USB.EP_Id;
                         BCNT : UInt32);

   function Ready (This : in out Default_MIDI_Class) return Boolean;

   function Last (This : in out Default_MIDI_Class) return UInt8_Array
     with Pre => This.Ready;

private

   type Class_State is (Stop, Idle, Data_Ready);

   subtype Message is UInt8_Array (1 .. 4);

   type Message_Array is array (Natural range <>) of Message;

   FIFO_Size : constant := 8;

   type Default_MIDI_Class is new USB_Device_Class with record
      Last_In    : UInt8_Array (1 .. 64) := (others => 0);
      RX_BCNT    : UInt32 := 0;

      RX_FIFO      : Message_Array (0 .. FIFO_Size - 1);
      RX_FIFO_CNT  : Natural := 0;
      RX_In_Index  : Natural := 0;
      RX_Out_Index : Natural := 0;

      State      : Class_State := Stop;
      Idle_State : UInt8 := 0;
   end record;

end USB.Device.MIDI;
