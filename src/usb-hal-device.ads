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
                           Data_Ready,
                           Transfer_Complete);

   type UDC_Event (Kind : UDC_Event_Kind := None) is record
      case Kind is
         when Setup_Request =>
            Req    : Setup_Data;
            Req_EP : EP_Id;
         when Data_Ready =>
            RX_BCNT : UInt11;  -- Byte count (0 .. 1024)
            RX_EP   : EP_Id;
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

end USB.HAL.Device;
