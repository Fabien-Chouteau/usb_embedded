--  This test is check control data out with a fake class request
with USB_Testing;               use USB_Testing;
with USB_Testing.UDC_Stub;      use USB_Testing.UDC_Stub;
with USB_Testing.UDC_Scenarios;

with HAL; use HAL;
with Interfaces; use Interfaces;

with USB;            use USB;
with USB.Device;     use USB.Device;
with USB.Device.HID;
with USB.HAL.Device; use USB.HAL.Device;

procedure Main is

   Scenario : aliased constant UDC_Stub.Stub_Scenario :=
     UDC_Scenarios.Enumeration (Verbose => False) &
     UDC_Scenarios.Set_Config (Verbose => True, Config_Id => 1) &
     Stub_Scenario'(1 => (Kind => Set_Verbose, Verbose => True),
                    2 => (Kind    => UDC_Event_E,
                          Evt     => (Kind   => Setup_Request,
                                      --  Get Report descriptor
                                      Req    => ((Iface, 0, Stand, Device_To_Host),
                                                 6, 16#2200#, 0, 255),
                                      Req_EP => 0)),
                    --  ACK the IN transfer with a ZLP
                    3 =>  (Kind => UDC_Event_E,
                           Evt  => (Kind => Transfer_Complete,
                                    EP   => (0, EP_Out),
                                    BCNT => 0))

                   );

   RX_Data : aliased constant UInt8_Array := (1 .. 16 => 42);


   Configuration : aliased constant UInt8_Array := (0 .. 1 => 0);

   HID_Class : aliased USB.Device.HID.Default_HID_Class;

   UDC : aliased UDC_Stub.Controller (Scenario'Unchecked_Access,
                                      RX_Data'Unchecked_Access,
                                      Has_Early_Address => False,
                                      Max_Packet_Size   => 64,
                                      EP_Buffers_Size   => 256);
   Ctrl : USB.Device.USB_Device;

   S : Natural := 1;
begin

   Ctrl.Register_Class (HID_Class'Unchecked_Access);

   Ctrl.Initialize
     (Controller      => UDC'Unchecked_Access,
      Manufacturer    => To_USB_String ("Manufacturer"),
      Product         => To_USB_String ("Product"),
      Serial_Number   => To_USB_String ("Serial"),
      Max_Packet_Size => 64);

   Ctrl.Start;

   loop
      Ctrl.Poll;

      if HID_Class.Ready then
         case S is
            when 1 =>
               HID_Class.Set_Click (Btn1 => False,
                                    Btn2 => False,
                                    Btn3 => False);
               HID_Class.Set_Move (10, 0);
               HID_Class.Send_Report (UDC);
            when 2 =>
               HID_Class.Set_Click (Btn1 => False,
                                    Btn2 => False,
                                    Btn3 => False);
               HID_Class.Set_Move (0, -10);
               HID_Class.Send_Report (UDC);
            when 3 =>
               HID_Class.Set_Move (0, 0);
               HID_Class.Set_Click (Btn1 => True,
                                    Btn2 => False,
                                    Btn3 => False);
               HID_Class.Send_Report (UDC);
            when others =>
               null;
         end case;
         S := S + 1;
      end if;

      pragma Warnings (Off, "possible infinite loop");
      exit when UDC.End_Of_Scenario;
      pragma Warnings (On, "possible infinite loop");
   end loop;
end Main;
