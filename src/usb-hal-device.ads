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

package USB.HAL.Device is

   type USB_Device_Controller is interface;
   type Any_USB_Device_Controller is access all USB_Device_Controller'Class;

   procedure Initialize (This : in out USB_Device_Controller)
   is abstract;

   procedure Start (This : in out USB_Device_Controller)
   is abstract;

   type UDC_Event_Kind is (None,
                           Reset,
                           Setup_Request,
                           Transfer_Complete);

   type UDC_Event (Kind : UDC_Event_Kind := None) is record
      case Kind is
         when Setup_Request =>
            Req    : Setup_Data;
            Req_EP : EP_Id;
         when Transfer_Complete =>
            EP   : EP_Addr;
            BCNT : Packet_Size;
         when others => null;
      end case;
   end record;

   No_Event : constant UDC_Event := (Kind => None);

   function Poll (This : in out USB_Device_Controller) return UDC_Event
   is abstract;

   procedure Reset (This : in out USB_Device_Controller)
   is abstract;
   --  Called when the host resets the device

   function Request_Buffer (This          : in out USB_Device_Controller;
                            Ep            :        EP_Addr;
                            Len           :        Packet_Size)
                            return System.Address
   is abstract;
   --  Allocate a buffer for the given End-Point, either from RAM or internal
   --  USB Controller memory depending on the controller. UDC must keep track
   --  of the return address to perform IN/OUT transfers (see EP_Send_Packet
   --  and EP_Ready_For_Data).

   function Valid_EP_Id (This : in out USB_Device_Controller;
                         EP   :        EP_Id)
                         return Boolean
   is abstract;
   --  Return True if the given EP is valid for this UDC. This is used by the
   --  stack to know the number of EPs available.

   procedure EP_Send_Packet (This : in out USB_Device_Controller;
                             Ep   :        EP_Id;
                             Len  :        Packet_Size)
   is abstract;
   --  Len has to be less than or equal to the requested buffer size. The
   --  data to transfer is read from the previously requested EP buffer
   --  (see Request_Buffer).

   procedure EP_Setup (This : in out USB_Device_Controller;
                       EP   :        EP_Addr;
                       Typ  :        EP_Type)
   is abstract;
   --  Setup the given End-Point for the given type of transfer

   procedure EP_Ready_For_Data (This    : in out USB_Device_Controller;
                                EP      :        EP_Id;
                                Max_Len :        Packet_Size;
                                Ready   :        Boolean := True)
   is abstract;
   --  Max_Len has to be less than or equal to the requested buffer size. The
   --  transfered data will be written to the previously requested EP buffer
   --  (see Request_Buffer).

   procedure EP_Stall (This : in out USB_Device_Controller;
                       EP   :        EP_Addr;
                       Set  :        Boolean := True)
   is abstract;

   procedure Set_Address (This : in out USB_Device_Controller;
                          Addr :        UInt7)
   is abstract;

   function Early_Address (This : USB_Device_Controller) return Boolean
   is abstract;
   --  This function should return True if Set_Address must be called during
   --  the processing of the SET_ADDRESS setup request instead of at the end of
   --  the setup request. For some reason this is required for the USB
   --  controller of the STM32F series.

   function Img (Evt : UDC_Event) return String
   is (case Evt.Kind is
          when Setup_Request =>
             Evt.Kind'Img & " " & Img (EP_Addr'(Evt.Req_EP, EP_Out)) & " " &
               Img (Evt.Req),
          when Transfer_Complete =>
             Evt.Kind'Img & " " & Img (Evt.EP) & " BCNT:" & Evt.BCNT'Img,
          when others =>
            Evt.Kind'Img);

end USB.HAL.Device;
