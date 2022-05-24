------------------------------------------------------------------------------
--                                                                          --
--                        Copyright (C) 2021, AdaCore                       --
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
with HAL;
with USB.HAL.Device;

package USB.Logging.Device is

   procedure Log (Evt : USB.HAL.Device.UDC_Event);

   procedure Log_Serial_Init;
   procedure Log_Serial_Config;
   procedure Log_Serial_Send;
   procedure Log_Serial_Receive;
   procedure Log_Serial_Out_TC;
   procedure Log_Serial_In_TC;
   procedure Log_Serial_Setup_TX;
   procedure Log_Serial_Setup_RX;
   procedure Log_Serial_RX_Discarded;
   procedure Log_Serial_TX_Discarded;
   procedure Log_Serial_Write_Packet;

   procedure Log_MIDI_Init;
   procedure Log_MIDI_Config;
   procedure Log_MIDI_Send;
   procedure Log_MIDI_Receive;
   procedure Log_MIDI_Out_TC;
   procedure Log_MIDI_In_TC;
   procedure Log_MIDI_Setup_TX;
   procedure Log_MIDI_Setup_RX;
   procedure Log_MIDI_RX_Discarded;
   procedure Log_MIDI_TX_Discarded;
   procedure Log_MIDI_Write_Packet;

private

   type Log_Event_Kind is (None,
                           UDC_Evt,

                           -- Serial Class --
                           Serial_Init,
                           Serial_Config,
                           Serial_Send,
                           Serial_Receive,
                           Serial_Out_TC,
                           Serial_In_TC,
                           Serial_Setup_TX,
                           Serial_Setup_RX,
                           Serial_RX_Discarded,
                           Serial_TX_Discarded,
                           Serial_Write_Packet,

                           --  MIDI Class --
                           MIDI_Init,
                           MIDI_Config,
                           MIDI_Send,
                           MIDI_Receive,
                           MIDI_Out_TC,
                           MIDI_In_TC,
                           MIDI_Setup_TX,
                           MIDI_Setup_RX,
                           MIDI_RX_Discarded,
                           MIDI_TX_Discarded,
                           MIDI_Write_Packet
                          );

   type Log_Event_ID is new Standard.HAL.UInt16;

   type Log_Event (Kind : Log_Event_Kind := None) is record
      ID : Log_Event_ID;
      case Kind is
         when UDC_Evt =>
            UDC_Event : USB.HAL.Device.UDC_Event;
         when others =>
            null;
      end case;
   end record;

end USB.Logging.Device;
