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

package body USB.Logging.Device is

   Event_Buffer_Size : constant := Usb_Embedded_Config.Event_Log_Buffer_Size;

   type Event_Index is mod Event_Buffer_Size;

   Event_Buffer : array (Event_Index) of Log_Event :=
     (others => (Kind  => None, ID => 0));
   pragma Unreferenced (Event_Buffer);

   Index : Event_Index := Event_Index'First;
   ID    : Log_Event_ID;

   procedure Log (Evt : Log_Event);

   ---------
   -- Log --
   ---------

   procedure Log (Evt : Log_Event) is
   begin
      Event_Buffer (Index) := Evt;
      Index := Index + 1;
   end Log;

   ---------
   -- Log --
   ---------

   procedure Log (Evt : USB.HAL.Device.UDC_Event) is
      use USB.HAL.Device;
   begin
      if Evt.Kind /= None then
         ID := ID + 1;
         Log (Log_Event'(Kind      => UDC_Evt,
                         ID        => ID,
                         UDC_Event => Evt));
      end if;
   end Log;

   -------------------
   -- Log_Serial_Init --
   -------------------

   procedure Log_Serial_Init is
   begin
      ID := ID + 1;
      Log (Log_Event'(Kind      => Serial_Init,
                      ID        => ID));
   end Log_Serial_Init;

   ---------------------
   -- Log_Serial_Config --
   ---------------------

   procedure Log_Serial_Config is
   begin
      ID := ID + 1;
      Log (Log_Event'(Kind      => Serial_Config,
                      ID        => ID));
   end Log_Serial_Config;

   -------------------
   -- Log_Serial_Send --
   -------------------

   procedure Log_Serial_Send is
   begin
      ID := ID + 1;
      Log (Log_Event'(Kind      => Serial_Send,
                      ID        => ID));
   end Log_Serial_Send;

   ----------------------
   -- Log_Serial_Receive --
   ----------------------

   procedure Log_Serial_Receive is
   begin
      ID := ID + 1;
      Log (Log_Event'(Kind      => Serial_Receive,
                      ID        => ID));
   end Log_Serial_Receive;

   ---------------------
   -- Log_Serial_Out_TC --
   ---------------------

   procedure Log_Serial_Out_TC is
   begin
      ID := ID + 1;
      Log (Log_Event'(Kind      => Serial_Out_TC,
                      ID        => ID));
   end Log_Serial_Out_TC;

   --------------------
   -- Log_Serial_In_TC --
   --------------------

   procedure Log_Serial_In_TC is
   begin
      ID := ID + 1;
      Log (Log_Event'(Kind      => Serial_In_TC,
                      ID        => ID));
   end Log_Serial_In_TC;

   -----------------------
   -- Log_Serial_Setup_TX --
   -----------------------

   procedure Log_Serial_Setup_TX is
   begin
      ID := ID + 1;
      Log (Log_Event'(Kind      => Serial_Setup_TX,
                      ID        => ID));
   end Log_Serial_Setup_TX;

   -----------------------
   -- Log_Serial_Setup_RX --
   -----------------------

   procedure Log_Serial_Setup_RX is
   begin
      ID := ID + 1;
      Log (Log_Event'(Kind      => Serial_Setup_RX,
                      ID        => ID));
   end Log_Serial_Setup_RX;

   ---------------------------
   -- Log_Serial_RX_Discarded --
   ---------------------------

   procedure Log_Serial_RX_Discarded is
   begin
      ID := ID + 1;
      Log (Log_Event'(Kind      => Serial_RX_Discarded,
                      ID        => ID));
   end Log_Serial_RX_Discarded;

   ---------------------------
   -- Log_Serial_TX_Discarded --
   ---------------------------

   procedure Log_Serial_TX_Discarded is
   begin
      ID := ID + 1;
      Log (Log_Event'(Kind      => Serial_TX_Discarded,
                      ID        => ID));
   end Log_Serial_TX_Discarded;

   -----------------------------
   -- Log_Serial_Write_Packet --
   -----------------------------

   procedure Log_Serial_Write_Packet is
   begin
      ID := ID + 1;
      Log (Log_Event'(Kind      => Serial_Write_Packet,
                      ID        => ID));
   end Log_Serial_Write_Packet;

   -------------------
   -- Log_MIDI_Init --
   -------------------

   procedure Log_MIDI_Init is
   begin
      ID := ID + 1;
      Log (Log_Event'(Kind      => MIDI_Init,
                      ID        => ID));
   end Log_MIDI_Init;

   ---------------------
   -- Log_MIDI_Config --
   ---------------------

   procedure Log_MIDI_Config is
   begin
      ID := ID + 1;
      Log (Log_Event'(Kind      => MIDI_Config,
                      ID        => ID));
   end Log_MIDI_Config;

   -------------------
   -- Log_MIDI_Send --
   -------------------

   procedure Log_MIDI_Send is
   begin
      ID := ID + 1;
      Log (Log_Event'(Kind      => MIDI_Send,
                      ID        => ID));
   end Log_MIDI_Send;

   ----------------------
   -- Log_MIDI_Receive --
   ----------------------

   procedure Log_MIDI_Receive is
   begin
      ID := ID + 1;
      Log (Log_Event'(Kind      => MIDI_Receive,
                      ID        => ID));
   end Log_MIDI_Receive;

   ---------------------
   -- Log_MIDI_Out_TC --
   ---------------------

   procedure Log_MIDI_Out_TC is
   begin
      ID := ID + 1;
      Log (Log_Event'(Kind      => MIDI_Out_TC,
                      ID        => ID));
   end Log_MIDI_Out_TC;

   --------------------
   -- Log_MIDI_In_TC --
   --------------------

   procedure Log_MIDI_In_TC is
   begin
      ID := ID + 1;
      Log (Log_Event'(Kind      => MIDI_In_TC,
                      ID        => ID));
   end Log_MIDI_In_TC;

   -----------------------
   -- Log_MIDI_Setup_TX --
   -----------------------

   procedure Log_MIDI_Setup_TX is
   begin
      ID := ID + 1;
      Log (Log_Event'(Kind      => MIDI_Setup_TX,
                      ID        => ID));
   end Log_MIDI_Setup_TX;

   -----------------------
   -- Log_MIDI_Setup_RX --
   -----------------------

   procedure Log_MIDI_Setup_RX is
   begin
      ID := ID + 1;
      Log (Log_Event'(Kind      => MIDI_Setup_RX,
                      ID        => ID));
   end Log_MIDI_Setup_RX;

   ---------------------------
   -- Log_MIDI_RX_Discarded --
   ---------------------------

   procedure Log_MIDI_RX_Discarded is
   begin
      ID := ID + 1;
      Log (Log_Event'(Kind      => MIDI_RX_Discarded,
                      ID        => ID));
   end Log_MIDI_RX_Discarded;

   ---------------------------
   -- Log_MIDI_TX_Discarded --
   ---------------------------

   procedure Log_MIDI_TX_Discarded is
   begin
      ID := ID + 1;
      Log (Log_Event'(Kind      => MIDI_TX_Discarded,
                      ID        => ID));
   end Log_MIDI_TX_Discarded;

   ---------------------------
   -- Log_MIDI_Write_Packet --
   ---------------------------

   procedure Log_MIDI_Write_Packet is
   begin
      ID := ID + 1;
      Log (Log_Event'(Kind      => MIDI_Write_Packet,
                      ID        => ID));
   end Log_MIDI_Write_Packet;
end USB.Logging.Device;
