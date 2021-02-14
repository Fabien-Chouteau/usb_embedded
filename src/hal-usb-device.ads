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

with System;

package HAL.USB.Device is

   type USB_Device_Controller is interface;
   type Any_USB_Device_Controller is access all USB_Device_Controller'Class;

   procedure Initialize (This : in out USB_Device_Controller)
   is abstract;

   procedure Start (This : in out USB_Device_Controller)
   is abstract;

   type UDC_Event_Kind is (None,
                           Reset,
                           Setup_Request,
                           Data_Ready,
                           Transfer_Complete);

   type UDC_Event (Kind : UDC_Event_Kind := None) is record
      case Kind is
         when Setup_Request =>
            Req    : Setup_Data;
            Req_EP : EP_Id;
         when Data_Ready =>
            RX_EP   : EP_Id;
            RX_BCNT : UInt11;  -- Byte count (0 .. 1024)
         when Transfer_Complete =>
            T_EP : EP_Addr;
         when others => null;
      end case;
   end record;

   No_Event : constant UDC_Event := (Kind => None);

   function Poll (This : in out USB_Device_Controller) return UDC_Event
   is abstract;

   type EP_Callback is access procedure
     (This : in out USB_Device_Controller'Class;
      EP   : EP_Id);

   type Setup_Callback is access procedure
     (This : in out USB_Device_Controller'Class;
      EP   : EP_Id;
      Req  : Setup_Data);

   procedure Set_EP_Callback (This     : in out USB_Device_Controller;
                              EP       : EP_Addr;
                              Callback : EP_Callback)
   is abstract;

   procedure Set_Setup_Callback (This     : in out USB_Device_Controller;
                                 EP       : EP_Id;
                                 Callback : Setup_Callback)
   is abstract;

   procedure EP_Read_Packet (This : in out USB_Device_Controller;
                             Ep   : EP_Id;
                             Addr : System.Address;
                             Len  : UInt32)
   is abstract;

   procedure EP_Write_Packet (This : in out USB_Device_Controller;
                              Ep   : EP_Id;
                              Addr : System.Address;
                              Len  : UInt32)
   is abstract;

   procedure EP_Setup (This     : in out USB_Device_Controller;
                       EP       : EP_Addr;
                       Typ      : EP_Type;
                       Max_Size : UInt16;
                       Callback : EP_Callback)
   is abstract;

   procedure EP_Set_NAK (This : in out USB_Device_Controller;
                         EP   : EP_Addr;
                         NAK  : Boolean)
   is abstract
     with Pre'Class => EP.Dir = EP_Out;

   procedure EP_Set_Stall (This : in out USB_Device_Controller;
                           EP   : EP_Addr)
   is abstract;

   procedure Set_Address (This : in out USB_Device_Controller;
                          Addr : UInt7)
   is abstract;

   function Early_Address (This : USB_Device_Controller) return Boolean
   is abstract;
   --  This function return True if Set_Address should be called during the
   --  processing of the SET_ADDRESS setup request instead of at the end of the
   --  setup request. For some reason, this is required for the USB controller
   --  of the STM32F series.

   function Img (Evt : UDC_Event) return String
   is (case Evt.Kind is
          when Setup_Request =>
             Evt.Kind'Img & " " & Img (EP_Addr'(Evt.Req_EP, EP_Out)) & " " &
               Img (Evt.Req),
          when Data_Ready =>
             Evt.Kind'Img & " " & Img (EP_Addr'(Evt.RX_EP, EP_Out)) &
               " BCNT:" & Evt.RX_BCNT'Img,
          when Transfer_Complete =>
             Evt.Kind'Img & " " & Img (Evt.T_EP),
          when others =>
            Evt.Kind'Img);

end HAL.USB.Device;
