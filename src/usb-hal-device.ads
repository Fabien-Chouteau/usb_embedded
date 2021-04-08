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
            BCNT : UInt11;  -- Byte count (0 .. 1024)
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
                            Len           :        UInt11;
                            Min_Alignment :        UInt8 := 1)
                            return System.Address
   is abstract;
   --  Allocate a buffer for the given End-Point, either from RAM or interal USB
   --  Controller memory depending on the controller.

   procedure EP_Write_Packet (This : in out USB_Device_Controller;
                              Ep   : EP_Id;
                              Addr : System.Address;
                              Len  : UInt32)
   is abstract;

   procedure EP_Setup (This     : in out USB_Device_Controller;
                       EP       : EP_Addr;
                       Typ      : EP_Type;
                       Max_Size : UInt16)
   is abstract;

   procedure EP_Ready_For_Data (This    : in out USB_Device_Controller;
                                EP      : EP_Id;
                                Addr    : System.Address;
                                Max_Len : UInt32;
                                Ready   : Boolean := True)
   is abstract;

   procedure EP_Stall (This : in out USB_Device_Controller;
                       EP   :        EP_Addr;
                       Set  :        Boolean := True)
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
          when Transfer_Complete =>
             Evt.Kind'Img & " " & Img (Evt.EP) & " BCNT:" & Evt.BCNT'Img,
          when others =>
            Evt.Kind'Img);

end USB.HAL.Device;
