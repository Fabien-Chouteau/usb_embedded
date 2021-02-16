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

--  with Hex_Dump;
with Ada.Text_IO;

package body USB.Device.MIDI is

   overriding
   procedure Initialize (This            : in out Default_MIDI_Class;
                         Dev             : in out USB_Device;
                         Interface_Index :        Class_Index)
   is
   begin
      if not Dev.Request_Endpoint (This.EP) then
         raise Program_Error with "Cannot get EP for MIDI class";
      end if;
      This.Interface_Index := Interface_Index;
   end Initialize;

   ------------------------------
   -- Config_Descriptor_Length --
   ------------------------------

   overriding
   function Config_Descriptor_Length (This : in out Default_MIDI_Class)
                                      return Positive
   is
      pragma Unreferenced (This);
   begin
      return 91;
   end Config_Descriptor_Length;

   ----------------------------
   -- Fill_Config_Descriptor --
   ----------------------------

   overriding
   procedure Fill_Config_Descriptor (This : in out Default_MIDI_Class;
                                     Data :    out UInt8_Array)
   is
      F : constant Natural := Data'First;
      USB_DESC_TYPE_INTERFACE     : constant := 4;
      USB_DESC_TYPE_ENDPOINT      : constant := 5;

   begin

      pragma Style_Checks (Off);

      --  B.3.1 Standard AC Interface Descriptor
      --  The AudioControl interface has no dedicated endpoints associated with it. It uses the
      --  default pipe (endpoint 0) for all communication purposes. Class-specific AudioControl
      --  Requests are sent using the default pipe. There is no Status Interrupt endpoint provided.
      --  descriptor follows inline: */
      Data (F + 0 .. F + 90) :=
        (9, --  sizeof(usbDescrInterface): length of descriptor in bytes
         USB_DESC_TYPE_INTERFACE, --  descriptor type
         This.Interface_Index, --  index of this interface
         0, --  alternate setting for this interface
         0, --  endpoints excl 0: number of endpoint descriptors to follow
         1, --
         1, --
         0, --
         0, --  string index for interface */

         --  B.3.2 Class-specific AC Interface Descriptor
         --  The Class-specific AC interface descriptor is always headed by a Header descriptor
         --  that contains general information about the AudioControl interface. It contains all
         --  the pointers needed to describe the Audio Interface Collection, associated with the
         --  described audio function. Only the Header descriptor is present in this device
         --  because it does not contain any audio functionality as such.
         --  descriptor follows inline: */
         9, --  sizeof(usbDescrCDC_HeaderFn): length of descriptor in bytes */
         36, --  descriptor type */
         1, --  header functional descriptor */
         0, 0, --  bcdADC */
         9, 0, --  wTotalLength */
         1, --  */
         1, --  */


         --  B.4 MIDIStreaming Interface Descriptors

         --  B.4.1 Standard MS Interface Descriptor
         --  descriptor follows inline: */
         9, --  length of descriptor in bytes */
         USB_DESC_TYPE_INTERFACE, --  descriptor type */
         1, --  index of this interface */
         0, --  alternate setting for this interface */
         2, --  endpoints excl 0: number of endpoint descriptors to follow */
         1, --  AUDIO */
         3, --  MS */
         0, --  unused */
         0, --  string index for interface */

         --  B.4.2 Class-specific MS Interface Descriptor
         --  descriptor follows inline: */
         7, --  length of descriptor in bytes */
         36, --  descriptor type */
         1, --  header functional descriptor */
         0, 1, --  bcdADC */
         65, 0, --  wTotalLength */

         --  B.4.3 MIDI IN Jack Descriptor
         --  descriptor follows inline: */
         6, --  bLength */
         36, --  descriptor type */
         2, --  MIDI_IN_JACK desc subtype */
         1, --  EMBEDDED bJackType */
         1, --  bJackID */
         0, --  iJack */

         --  descriptor follows inline: */
         6, --  bLength */
         36, --  descriptor type */
         2, --  MIDI_IN_JACK desc subtype */
         2, --  EXTERNAL bJackType */
         2, --  bJackID */
         0, --  iJack */

         --  B.4.4 MIDI OUT Jack Descriptor
         --  descriptor follows inline: */
         9, --  length of descriptor in bytes */
         36, --  descriptor type */
         3, --  MIDI_OUT_JACK descriptor */
         1, --  EMBEDDED bJackType */
         3, --  bJackID */
         1, --  No of input pins */
         2, --  BaSourceID */
         1, --  BaSourcePin */
         0, --  iJack */

         --  descriptor follows inline: */
         9, --  bLength of descriptor in bytes */
         36, --  bDescriptorType */
         3, --  MIDI_OUT_JACK bDescriptorSubtype */
         2, --  EXTERNAL bJackType */
         4, --  bJackID */
         1, --  bNrInputPins */
         1, --  baSourceID (0) */
         1, --  baSourcePin (0) */
         0, --  iJack */

         --  B.5 Bulk OUT Endpoint Descriptors

         --  here 27 ---

         --  B.5.1 Standard Bulk OUT Endpoint Descriptor
         --  descriptor follows inline: */
         9, --  bLenght */
         USB_DESC_TYPE_ENDPOINT, --  bDescriptorType = endpoint */
         1, --  bEndpointAddress OUT endpoint number 1 */
         3, --  bmAttributes: 2:Bulk, 3:Interrupt endpoint */
         8, 0, --  wMaxPacketSize */
         10, --  bInterval in ms */
         0, --  bRefresh */
         0, --  bSyncAddress */

         --  B.5.2 Class-specific MS Bulk OUT Endpoint Descriptor
         --  descriptor follows inline: */
         5, --  bLength of descriptor in bytes */
         37, --  bDescriptorType */
         1, --  bDescriptorSubtype */
         1, --  bNumEmbMIDIJack  */
         1, --  baAssocJackID (0) */

         --  B.6 Bulk IN Endpoint Descriptors

         --  B.6.1 Standard Bulk IN Endpoint Descriptor
         --  descriptor follows inline: */
         9, --  bLenght */
         USB_DESC_TYPE_ENDPOINT, --  bDescriptorType = endpoint */
         16#81#, --  bEndpointAddress IN endpoint number 1 */
         3, --  bmAttributes: 2: Bulk, 3: Interrupt endpoint */
         8, 0, --  wMaxPacketSize */
         10, --  bInterval in ms */
         0, --  bRefresh */
         0, --  bSyncAddress */

         --  B.6.2 Class-specific MS Bulk IN Endpoint Descriptor
         --  descriptor follows inline: */
         5, --  bLength of descriptor in bytes */
         37, --  bDescriptorType */
         1, --  bDescriptorSubtype */
         1 --  bNumEmbMIDIJack (0) */
        );


   end Fill_Config_Descriptor;

   ---------------
   -- Configure --
   ---------------

   overriding function Configure
     (This  : in out Default_MIDI_Class;
      UDC   : in out USB_Device_Controller'Class;
      Index : UInt16)
      return Setup_Request_Answer
   is
   begin
      if Index = 1 then

         UDC.EP_Setup (EP       => (This.EP, EP_In),
                       Typ      => Bulk,
                       Max_Size => This.Last_In'Length,
                       Callback => null);
         UDC.EP_Setup (EP       => (This.EP, EP_Out),
                       Typ      => Bulk,
                       Max_Size => This.Last_In'Length,
                       Callback => null);

         This.State := Idle;
         return Handled;
      else
         return Not_Supported;
      end if;
   end Configure;

   -------------------
   -- Setup_Request --
   -------------------

   overriding
   function Setup_Read_Request (This  : in out Default_MIDI_Class;
                                Req   : Setup_Data;
                                Buf   : out System.Address;
                                Len   : out Buffer_Len)
                                return Setup_Request_Answer
   is
   begin
      Ada.Text_IO.Put_Line (Img (Req));

      Buf := System.Null_Address;
      Len := 0;

      if Req.RType.Typ = Class and then Req.RType.Recipient = Iface then
         case Req.Request is
         when 1 => -- GET_REPORT
            return Not_Supported;
         when 2 => -- GET_IDLE
            return Not_Supported;
         when 3 => -- GET_PROTOCOL
            return Not_Supported;
         when 9 => -- SET_REPORT
            return Not_Supported;
         when 10 => -- SET_IDLE
            This.Idle_State := UInt8 (Shift_Right (Req.Value, 8) and 16#FF#);
            return Handled;
         when 11 => -- SET_PROTOCOL
            return Not_Supported;
         when others =>
            raise Program_Error with "Unknown MIDI requset";
         end case;
      end if;

      if Req.RType.Typ = Stand
        and then
          Req.Request = 6 -- GET_DESCRIPTOR
      then
         declare
--              Index     : constant UInt8 := UInt8 (Req.Value and 16#FF#);
            Desc_Type : constant UInt8 :=
              UInt8 (Shift_Right (Req.Value, 8) and 16#FF#);

         begin
            case Desc_Type is
               when others =>
                  raise Program_Error with "Unknown desc in MIDI class";
            end case;
         end;
      end if;

      raise Program_Error with "You have to implement stuff here";
      return Not_Supported;
   end Setup_Read_Request;

   -------------------------
   -- Setup_Write_Request --
   -------------------------

   overriding
   function Setup_Write_Request (This  : in out Default_MIDI_Class;
                                 Req   : Setup_Data;
                                 Data  : UInt8_Array)
                                 return Setup_Request_Answer
   is (Not_Supported);

   -----------------------
   -- Transfer_Complete --
   -----------------------

   overriding
   procedure Transfer_Complete (This : in out Default_MIDI_Class;
                                UDC  : in out USB_Device_Controller'Class;
                                EP   : EP_Addr)
   is
      Index : Natural := This.Last_In'First;
   begin

      pragma Assert (EP.Num = This.EP);

      UDC.EP_Set_NAK (EP, False);

      --  Setup the endpoint for the next packet
      UDC.EP_Setup (EP       => (This.EP, EP_Out),
                    Typ      => Bulk,
                    Max_Size => This.Last_In'Length,
                    Callback => null);

      --  Hex_Dump.Hex_Dump (This.Last_In (1 .. Integer (This.RX_BCNT)),
      --                     Ada.Text_IO.Put_Line'Access);

      while This.RX_BCNT >= 4 and then This.RX_FIFO_CNT <= FIFO_Size loop
         This.RX_FIFO (This.RX_In_Index) :=
           This.Last_In (Index .. Index + 4 - 1);

         Index := Index + 4;
         This.RX_BCNT := This.RX_BCNT - 4;
         This.RX_FIFO_CNT := This.RX_FIFO_CNT + 1;
         This.RX_In_Index := (This.RX_In_Index + 1) mod FIFO_Size;
      end loop;
   end Transfer_Complete;

   ----------------
   -- Data_Ready --
   ----------------

   overriding
   procedure Data_Ready (This : in out Default_MIDI_Class;
                         UDC  : in out USB_Device_Controller'Class;
                         EP   : EP_Id;
                         BCNT : UInt32)
   is
   begin
      pragma Assert (EP = This.EP);

      if BCNT > This.Last_In'Length then
         raise Program_Error;
      end if;

      This.RX_BCNT := BCNT;

      UDC.EP_Read_Packet (Ep   => This.EP,
                          Addr => This.Last_In'Address,
                          Len  => This.Last_In'Length);
   end Data_Ready;

   -----------
   -- Ready --
   -----------

   function Ready (This : in out Default_MIDI_Class) return Boolean
   is (This.RX_FIFO_CNT /= 0);

   ----------
   -- Last --
   ----------

   function Last (This : in out Default_MIDI_Class) return UInt8_Array
   is
   begin
      if This.RX_FIFO_CNT = 0 then
         return UInt8_Array'(1 .. 0 => 0);
      end if;

      return E : Message do
         E := This.RX_FIFO (This.RX_Out_Index);
         This.RX_Out_Index := (This.RX_Out_Index + 1) mod FIFO_Size;
         This.RX_FIFO_CNT := This.RX_FIFO_CNT - 1;
      end return;
   end Last;

end USB.Device.MIDI;
