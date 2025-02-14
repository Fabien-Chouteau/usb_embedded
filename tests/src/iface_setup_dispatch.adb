with AAA.Strings;

with HAL; use HAL;

with USB_Testing.UDC_Stub; use USB_Testing.UDC_Stub;
with USB_Testing.UDC_Scenarios;
with USB_Testing; use USB_Testing;
with USB.HAL.Device; use USB.HAL.Device;
with USB; use USB;

procedure Iface_Setup_Dispatch is

   --  The Stub classes only have one interface each, so the interface IDs
   --  should be:
   --   -- Class 1 -> interface ID 0
   --   -- Class 2 -> interface ID 1

   Scenario : aliased constant UDC_Stub.Stub_Scenario :=
     UDC_Scenarios.Enumeration (Verbose => False) &
     Stub_Scenario'(1 => (Kind => Set_Verbose, Verbose => True),
                    2 => (Kind    => UDC_Event_E,
                          Evt     => (Kind   => Setup_Request,
                                      Req    => ((Iface, 0, Stand, Host_To_Device),
                                                 0, 0,
                                                 0, -- Interface ID of class 1
                                                 0),
                                      Req_EP => 0)),
                    3 =>  (Kind    => UDC_Event_E,
                           Evt     => (Kind   => Setup_Request,
                                       Req    => ((Iface, 0, Stand, Host_To_Device),
                                                  0, 0,
                                                  1, -- Interface ID of class 2
                                                  0),
                                       Req_EP => 0)),
                    4 =>  (Kind    => UDC_Event_E,
                           Evt     => (Kind   => Setup_Request,
                                       Req    => ((Iface, 0, Stand, Host_To_Device),
                                                  0, 0,
                                                  2, -- Invalid interface ID
                                                  0),
                                       Req_EP => 0))
                   );

   RX_Data : aliased constant UInt8_Array := (0 .. 1 => 0);

   Expected : constant AAA.Strings.Vector := AAA.Strings.Empty_Vector
     .Append ("UDC Verbose on")
     .Append ("UDC Poll -> SETUP_REQUEST [EP_OUT 0] Type: (HOST_TO_DEVICE,STAND,IFACE) Req: 0 Val: 0 Index: 0 Len: 0")
     .Append ("UDC EP_Ready_For_Data [EP_OUT 0] FALSE")
     .Append ("USB Class 1 Setup_Request Type: (HOST_TO_DEVICE,STAND,IFACE) Req: 0 Val: 0 Index: 0 Len: 0")
     .Append ("UDC EP_Write_Packet [EP_IN 0] ZLP")
     .Append ("UDC Poll -> TRANSFER_COMPLETE [EP_IN 0] BCNT: 0")
     .Append ("UDC Poll -> SETUP_REQUEST [EP_OUT 0] Type: (HOST_TO_DEVICE,STAND,IFACE) Req: 0 Val: 0 Index: 1 Len: 0")
     .Append ("UDC EP_Ready_For_Data [EP_OUT 0] FALSE")
     .Append ("USB Class 2 Setup_Request Type: (HOST_TO_DEVICE,STAND,IFACE) Req: 0 Val: 0 Index: 1 Len: 0")
     .Append ("UDC EP_Write_Packet [EP_IN 0] ZLP")
     .Append ("UDC Poll -> TRANSFER_COMPLETE [EP_IN 0] BCNT: 0")
     .Append ("UDC Poll -> SETUP_REQUEST [EP_OUT 0] Type: (HOST_TO_DEVICE,STAND,IFACE) Req: 0 Val: 0 Index: 2 Len: 0")
     .Append ("UDC EP_Ready_For_Data [EP_OUT 0] FALSE")
     .Append ("UDC EP_Stall [EP_IN 0] TRUE")
     .Append ("UDC EP_Stall [EP_OUT 0] TRUE")
     .Append ("UDC Poll -> NONE");
begin
   USB_Testing.UDC_Scenarios.Two_Classes_UDC_Test (Scenario,
                                                   Expected,
                                                   RX_Data);
end Iface_Setup_Dispatch;
