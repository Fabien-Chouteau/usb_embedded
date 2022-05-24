------------------------------------------------------------------------------
--                                                                          --
--                        Copyright (C) 2022, AdaCore                       --
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

package body USB.Logging.Device is

   procedure Log (Evt : USB.HAL.Device.UDC_Event) is null;
   procedure Log_Serial_Init is null;
   procedure Log_Serial_Config is null;
   procedure Log_Serial_Send is null;
   procedure Log_Serial_Receive is null;
   procedure Log_Serial_Out_TC is null;
   procedure Log_Serial_In_TC is null;
   procedure Log_Serial_Setup_TX is null;
   procedure Log_Serial_Setup_RX is null;
   procedure Log_Serial_RX_Discarded is null;
   procedure Log_Serial_TX_Discarded is null;
   procedure Log_Serial_Write_Packet is null;
   procedure Log_MIDI_Init is null;
   procedure Log_MIDI_Config is null;
   procedure Log_MIDI_Send is null;
   procedure Log_MIDI_Receive is null;
   procedure Log_MIDI_Out_TC is null;
   procedure Log_MIDI_In_TC is null;
   procedure Log_MIDI_Setup_TX is null;
   procedure Log_MIDI_Setup_RX is null;
   procedure Log_MIDI_RX_Discarded is null;
   procedure Log_MIDI_TX_Discarded is null;
   procedure Log_MIDI_Write_Packet is null;

end USB.Logging.Device;
