--  Check that interface setup request are dispatched to the right class

with USB_Testing;               use USB_Testing;
with USB_Testing.UDC_Stub;      use USB_Testing.UDC_Stub;
with USB_Testing.UDC_Scenarios;

with HAL;            use HAL;
with USB.HAL.Device; use USB.HAL.Device;
with USB;            use USB;

procedure Main is

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

begin

   USB_Testing.UDC_Scenarios.Two_Classes_UDC_Test (Scenario, RX_Data);

end Main;
