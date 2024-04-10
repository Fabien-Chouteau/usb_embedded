with AAA.Strings;

with HAL; use HAL;

with USB_Testing.UDC_Stub; use USB_Testing.UDC_Stub;
with USB_Testing.UDC_Scenarios;
with USB_Testing; use USB_Testing;
with USB.HAL.Device; use USB.HAL.Device;
with USB; use USB;

package body Tests.Device is

   ------------
   -- Set_Up --
   ------------

   overriding
   procedure Set_Up (T : in out UDC_Stub_Fixture) is
   begin
      null;
   end Set_Up;

   -----------------------
   -- Device_Descriptor --
   -----------------------

   procedure Device_Descriptor (Unused : in out UDC_Stub_Fixture) is

      Scenario : aliased constant UDC_Stub.Stub_Scenario :=
        UDC_Scenarios.Enumeration (Verbose => True);

      RX_Data : aliased constant UInt8_Array := (0 .. 1 => 0);

      Expected : constant AAA.Strings.Vector := AAA.Strings.Empty_Vector
        .Append ("UDC Request_Buffer ([EP_IN 0], Len => 64)")
        .Append ("UDC Request_Buffer ([EP_OUT 0], Len => 64)")
        .Append ("UDC Initialize")
        .Append ("UDC Start")
        .Append ("UDC Poll -> RESET")
        .Append ("UDC Reset")
        .Append ("UDC EP_Setup [EP_IN 0] Type: CONTROL")
        .Append ("UDC EP_Setup [EP_OUT 0] Type: CONTROL")
        .Append ("UDC Set_Address 0")
        .Append ("UDC EP_Ready_For_Data [EP_OUT 0] TRUE")
        .Append ("UDC Reset")
        .Append ("UDC Poll -> SETUP_REQUEST [EP_OUT 0] Type: (DEVICE_TO_HOST,STAND,DEV) Req: 6 Val: 256 Index: 0 Len: 64")
        .Append ("UDC EP_Ready_For_Data [EP_OUT 0] FALSE")
        .Append ("UDC EP_Write_Packet [EP_IN 0] 18 bytes")
        .Append ("0000_0000_0000_0000: 12 01 10 01 00 00 00 40 66 66 42 42 21 01 01 02 .......@ffBB!...")
        .Append ("0000_0000_0000_0010: 03 01                                           ..")
        .Append ("UDC Poll -> TRANSFER_COMPLETE [EP_IN 0] BCNT: 18")
        .Append ("UDC EP_Ready_For_Data [EP_OUT 0] TRUE")
        .Append ("UDC Poll -> TRANSFER_COMPLETE [EP_OUT 0] BCNT: 0")
        .Append ("UDC Poll -> NONE");

   begin
      USB_Testing.UDC_Scenarios.Basic_UDC_Test (Scenario,
                                                Expected,
                                                RX_Data,
                                                Early_Address => False,
                                                Init_Verbose  => True);
   end Device_Descriptor;

   -----------------------
   -- No_Status_Out_ZLP --
   -----------------------

   procedure No_Status_Out_ZLP (Unused : in out UDC_Stub_Fixture) is
      Scenario : aliased constant UDC_Stub.Stub_Scenario :=
        UDC_Scenarios.Enumeration (Verbose => False) &
        Stub_Scenario'(1 => (Kind => Set_Verbose, Verbose => False),
                       2 => (Kind    => UDC_Event_E,
                             Evt     => (Kind   => Setup_Request,
                                         --  Get Device descriptor
                                         Req    => ((Dev, 0, Stand, Device_To_Host),
                                                    6, 16#0100#, 0, 64),
                                         Req_EP => 0)),
                       --  Do NOT ACK the IN transfer with a ZLP, but send another
                       --  request instead.
                       3 => (Kind => Set_Verbose, Verbose => True),
                       4 =>  (Kind    => UDC_Event_E,
                              Evt     => (Kind   => Setup_Request,
                                          --  Get Device descriptor
                                          Req    => ((Dev, 0, Stand, Device_To_Host),
                                                     6, 16#0100#, 0, 64),
                                          Req_EP => 0))
                      );

      RX_Data : aliased constant UInt8_Array := (0 .. 1 => 0);

      Expected : constant AAA.Strings.Vector := AAA.Strings.Empty_Vector
        .Append ("UDC Verbose on")
        .Append ("UDC Poll -> SETUP_REQUEST [EP_OUT 0] Type: (DEVICE_TO_HOST,STAND,DEV) Req: 6 Val: 256 Index: 0 Len: 64")
        .Append ("UDC EP_Ready_For_Data [EP_OUT 0] FALSE")
        .Append ("UDC EP_Write_Packet [EP_IN 0] 18 bytes")
        .Append ("0000_0000_0000_0000: 12 01 10 01 00 00 00 40 66 66 42 42 21 01 01 02 .......@ffBB!...")
        .Append ("0000_0000_0000_0010: 03 01                                           ..")
        .Append ("UDC Poll -> TRANSFER_COMPLETE [EP_IN 0] BCNT: 18")
        .Append ("UDC EP_Ready_For_Data [EP_OUT 0] TRUE")
        .Append ("UDC Poll -> NONE");
   begin
      USB_Testing.UDC_Scenarios.Basic_UDC_Test (Scenario,
                                                Expected,
                                                RX_Data,
                                                Early_Address => False);
   end No_Status_Out_ZLP;

   -----------------
   -- Set_Address --
   -----------------

   procedure Set_Address (Unused : in out UDC_Stub_Fixture) is
      Scenario : aliased constant UDC_Stub.Stub_Scenario :=
        UDC_Scenarios.Enumeration (Verbose => False) &
        UDC_Scenarios.Set_Address (Verbose => True);

      RX_Data : aliased constant UInt8_Array := (0 .. 1 => 0);

      Expected : constant AAA.Strings.Vector := AAA.Strings.Empty_Vector
        .Append ("UDC Verbose on")
        .Append ("UDC Poll -> SETUP_REQUEST [EP_OUT 0] Type: (HOST_TO_DEVICE,STAND,DEV) Req: 5 Val: 42 Index: 0 Len: 0")
        .Append ("UDC EP_Ready_For_Data [EP_OUT 0] FALSE")
        .Append ("UDC EP_Write_Packet [EP_IN 0] ZLP")
        .Append ("UDC Poll -> TRANSFER_COMPLETE [EP_IN 0] BCNT: 0")
        .Append ("UDC Set_Address 42")
        .Append ("UDC Poll -> NONE");

   begin
      USB_Testing.UDC_Scenarios.Basic_UDC_Test (Scenario,
                                                Expected,
                                                RX_Data,
                                                Early_Address => False);
   end Set_Address;

   -----------------------
   -- Set_Early_Address --
   -----------------------

   procedure Set_Early_Address (Unused : in out UDC_Stub_Fixture) is
      Scenario : aliased constant UDC_Stub.Stub_Scenario :=
        UDC_Scenarios.Enumeration (Verbose => False) &
        UDC_Scenarios.Set_Address (Verbose => True);

      RX_Data : aliased constant UInt8_Array := (0 .. 1 => 0);

      Expected : constant AAA.Strings.Vector := AAA.Strings.Empty_Vector
        .Append ("UDC Verbose on")
        .Append ("UDC Poll -> SETUP_REQUEST [EP_OUT 0] Type: (HOST_TO_DEVICE,STAND,DEV) Req: 5 Val: 42 Index: 0 Len: 0")
        .Append ("UDC EP_Ready_For_Data [EP_OUT 0] FALSE")
        .Append ("UDC Set_Address 42")
        .Append ("UDC EP_Write_Packet [EP_IN 0] ZLP")
        .Append ("UDC Poll -> TRANSFER_COMPLETE [EP_IN 0] BCNT: 0")
        .Append ("UDC Set_Address 42")
        .Append ("UDC Poll -> NONE");

   begin
      USB_Testing.UDC_Scenarios.Basic_UDC_Test (Scenario,
                                                Expected,
                                                RX_Data,
                                                Early_Address => True);
   end Set_Early_Address;

   ----------------
   -- Get_Status --
   ----------------

   procedure Get_Status (Unused : in out UDC_Stub_Fixture) is
      Scenario : aliased constant UDC_Stub.Stub_Scenario :=
        UDC_Scenarios.Enumeration (Verbose => False) &
        UDC_Scenarios.Get_Status  (Verbose => True);

      RX_Data : aliased constant UInt8_Array := (1 .. 2 => 0);

      Expected : constant AAA.Strings.Vector := AAA.Strings.Empty_Vector
        .Append ("UDC Verbose on")
        .Append ("UDC Poll -> SETUP_REQUEST [EP_OUT 0] Type: (DEVICE_TO_HOST,STAND,DEV) Req: 0 Val: 0 Index: 0 Len: 2")
        .Append ("UDC EP_Ready_For_Data [EP_OUT 0] FALSE")
        .Append ("UDC EP_Write_Packet [EP_IN 0] 2 bytes")
        .Append ("0000_0000_0000_0000: 00 00                                           ..")
        .Append ("UDC Poll -> TRANSFER_COMPLETE [EP_IN 0] BCNT: 2")
        .Append ("UDC EP_Ready_For_Data [EP_OUT 0] TRUE")
        .Append ("UDC Poll -> NONE");
   begin

      USB_Testing.UDC_Scenarios.Basic_UDC_Test (Scenario,
                                                Expected,
                                                RX_Data,
                                                Early_Address => True);
   end Get_Status;

   ----------------------
   -- Control_Data_Out --
   ----------------------

   procedure Control_Data_Out (Unused : in out UDC_Stub_Fixture) is
      Scenario : aliased constant UDC_Stub.Stub_Scenario :=
        UDC_Scenarios.Enumeration (Verbose => False) &
        Stub_Scenario'(1 => (Kind => Set_Verbose, Verbose => True),
                       2 => (Kind => UDC_Event_E,
                             Evt  => (Kind   => Setup_Request,

                                      --  Use an invalid request to trigger a stall
                                      --  after the data transfer.
                                      Req     => ((Dev, 0, Class, Host_To_Device),
                                                  42, 0, 0, 16),
                                      Req_EP  => 0)),
                       3 => (Kind => UDC_Event_E,
                             Evt  =>  (Kind => Transfer_Complete,
                                       EP   => (0, EP_Out),
                                       BCNT => 16)
                            )
                      );

      RX_Data : aliased constant UInt8_Array := (1 .. 16 => 42);

      Expected : constant AAA.Strings.Vector := AAA.Strings.Empty_Vector
        .Append ("UDC Verbose on")
        .Append ("UDC Poll -> SETUP_REQUEST [EP_OUT 0] Type: (HOST_TO_DEVICE,CLASS,DEV) Req: 42 Val: 0 Index: 0 Len: 16")
        .Append ("UDC EP_Ready_For_Data [EP_OUT 0] FALSE")
        .Append ("UDC EP_Ready_For_Data [EP_OUT 0] TRUE")
        .Append ("UDC Poll -> TRANSFER_COMPLETE [EP_OUT 0] BCNT: 16")
        .Append ("UDC OUT Transfer [EP_OUT 0] 16 bytes")
        .Append ("0000_0000_0000_0000: 2A 2A 2A 2A 2A 2A 2A 2A 2A 2A 2A 2A 2A 2A 2A 2A ****************")
        .Append ("USB Class 1 Setup_Request Type: (HOST_TO_DEVICE,CLASS,DEV) Req: 42 Val: 0 Index: 0 Len: 16")
        .Append ("0000_0000_0000_0000: 2A 2A 2A 2A 2A 2A 2A 2A 2A 2A 2A 2A 2A 2A 2A 2A ****************")
        .Append ("UDC EP_Stall [EP_IN 0] TRUE")
        .Append ("UDC EP_Stall [EP_OUT 0] TRUE")
        .Append ("UDC Poll -> NONE");

   begin
      USB_Testing.UDC_Scenarios.Basic_UDC_Test (Scenario,
                                                Expected,
                                                RX_Data,
                                                Early_Address => True);
   end Control_Data_Out;

   -----------------------
   -- Get_Device_Config --
   -----------------------

   procedure Get_Device_Config (Unused : in out UDC_Stub_Fixture) is

      Scenario : aliased constant UDC_Stub.Stub_Scenario :=
        UDC_Scenarios.Enumeration (Verbose => False) &
        UDC_Scenarios.Set_Address (Verbose => False, Addr => 42) &
        UDC_Scenarios.Get_Config (Verbose => True);

      RX_Data : aliased constant UInt8_Array := (0 .. 1 => 0);

      Expected : constant AAA.Strings.Vector := AAA.Strings.Empty_Vector
        .Append ("UDC Verbose on")
        .Append ("UDC Poll -> SETUP_REQUEST [EP_OUT 0] Type: (DEVICE_TO_HOST,STAND,DEV) Req: 6 Val: 512 Index: 0 Len: 255")
        .Append ("UDC EP_Ready_For_Data [EP_OUT 0] FALSE")
        .Append ("UDC EP_Write_Packet [EP_IN 0] 59 bytes")
        .Append ("0000_0000_0000_0000: 09 02 3B 00 01 01 00 80 32 00 00 00 00 00 00 00 ..;.....2.......")
        .Append ("0000_0000_0000_0010: 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 ................")
        .Append ("0000_0000_0000_0020: 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 ................")
        .Append ("0000_0000_0000_0030: 00 00 00 00 00 00 00 00 00 00 00                ...........")
        .Append ("UDC Poll -> TRANSFER_COMPLETE [EP_IN 0] BCNT: 59")
        .Append ("UDC EP_Ready_For_Data [EP_OUT 0] TRUE")
        .Append ("UDC Poll -> TRANSFER_COMPLETE [EP_OUT 0] BCNT: 0")
        .Append ("UDC Poll -> NONE");
   begin
      USB_Testing.UDC_Scenarios.Basic_UDC_Test (Scenario,
                                                Expected,
                                                RX_Data,
                                                Early_Address => True);
   end Get_Device_Config;

   -----------------------
   -- String_Descriptor --
   -----------------------

   procedure String_Descriptor (Unused : in out UDC_Stub_Fixture) is

      Scenario : aliased constant UDC_Stub.Stub_Scenario :=
        UDC_Scenarios.Enumeration (Verbose => False)
        & UDC_Scenarios.Get_Config (Verbose => False)

        --  Get string #1 usually manufacturer from device desc
        & UDC_Scenarios.Get_String (Verbose => True, Id => 1,  Ack => True)
        --  Get string #2 usually product from device desc
        & UDC_Scenarios.Get_String (Verbose => True, Id => 2,  Ack => True)
        --  Get string #3 usually serial number from device desc
        & UDC_Scenarios.Get_String (Verbose => True, Id => 3,  Ack => True)
        --  Get invalid string
        & UDC_Scenarios.Get_String (Verbose => True, Id => 42, Ack => False);

      RX_Data : aliased constant UInt8_Array := (0 .. 1 => 0);

      Expected : constant AAA.Strings.Vector := AAA.Strings.Empty_Vector
        .Append ("UDC Verbose on")
        .Append ("UDC Poll -> SETUP_REQUEST [EP_OUT 0] Type: (DEVICE_TO_HOST,STAND,DEV) Req: 6 Val: 769 Index: 0 Len: 255")
        .Append ("UDC EP_Ready_For_Data [EP_OUT 0] FALSE")
        .Append ("UDC EP_Write_Packet [EP_IN 0] 26 bytes")
        .Append ("0000_0000_0000_0000: 1A 03 4D 00 61 00 6E 00 75 00 66 00 61 00 63 00 ..M.a.n.u.f.a.c.")
        .Append ("0000_0000_0000_0010: 74 00 75 00 72 00 65 00 72 00                   t.u.r.e.r.")
        .Append ("UDC Poll -> TRANSFER_COMPLETE [EP_IN 0] BCNT: 26")
        .Append ("UDC EP_Ready_For_Data [EP_OUT 0] TRUE")
        .Append ("UDC Poll -> TRANSFER_COMPLETE [EP_OUT 0] BCNT: 0")
        .Append ("UDC Poll -> SETUP_REQUEST [EP_OUT 0] Type: (DEVICE_TO_HOST,STAND,DEV) Req: 6 Val: 770 Index: 0 Len: 255")
        .Append ("UDC EP_Ready_For_Data [EP_OUT 0] FALSE")
        .Append ("UDC EP_Write_Packet [EP_IN 0] 16 bytes")
        .Append ("0000_0000_0000_0000: 10 03 50 00 72 00 6F 00 64 00 75 00 63 00 74 00 ..P.r.o.d.u.c.t.")
        .Append ("UDC Poll -> TRANSFER_COMPLETE [EP_IN 0] BCNT: 16")
        .Append ("UDC EP_Ready_For_Data [EP_OUT 0] TRUE")
        .Append ("UDC Poll -> TRANSFER_COMPLETE [EP_OUT 0] BCNT: 0")
        .Append ("UDC Poll -> SETUP_REQUEST [EP_OUT 0] Type: (DEVICE_TO_HOST,STAND,DEV) Req: 6 Val: 771 Index: 0 Len: 255")
        .Append ("UDC EP_Ready_For_Data [EP_OUT 0] FALSE")
        .Append ("UDC EP_Write_Packet [EP_IN 0] 14 bytes")
        .Append ("0000_0000_0000_0000: 0E 03 53 00 65 00 72 00 69 00 61 00 6C 00       ..S.e.r.i.a.l.")
        .Append ("UDC Poll -> TRANSFER_COMPLETE [EP_IN 0] BCNT: 14")
        .Append ("UDC EP_Ready_For_Data [EP_OUT 0] TRUE")
        .Append ("UDC Poll -> TRANSFER_COMPLETE [EP_OUT 0] BCNT: 0")
        .Append ("UDC Poll -> SETUP_REQUEST [EP_OUT 0] Type: (DEVICE_TO_HOST,STAND,DEV) Req: 6 Val: 810 Index: 0 Len: 255")
        .Append ("UDC EP_Ready_For_Data [EP_OUT 0] FALSE")
        .Append ("UDC EP_Stall [EP_IN 0] TRUE")
        .Append ("UDC EP_Stall [EP_OUT 0] TRUE")
        .Append ("UDC Poll -> NONE");
   begin
      USB_Testing.UDC_Scenarios.Basic_UDC_Test (Scenario,
                                                Expected,
                                                RX_Data,
                                                Early_Address => True);
   end String_Descriptor;

   --------------------------
   -- Iface_Setup_Dispatch --
   --------------------------

   procedure Iface_Setup_Dispatch (Unused : in out UDC_Stub_Fixture) is

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

begin

   Suite.Add_Test (UDC_Stub_Caller.Create ("Device_Descriptor", Device_Descriptor'Access));
   Suite.Add_Test (UDC_Stub_Caller.Create ("Get_Device_Config", Get_Device_Config'Access));
   Suite.Add_Test (UDC_Stub_Caller.Create ("No Status Out ZLP", No_Status_Out_ZLP'Access));
   Suite.Add_Test (UDC_Stub_Caller.Create ("Set_Address", Set_Address'Access));
   Suite.Add_Test (UDC_Stub_Caller.Create ("Set_Early_Address", Set_Early_Address'Access));
   Suite.Add_Test (UDC_Stub_Caller.Create ("Get_Status", Get_Status'Access));
   Suite.Add_Test (UDC_Stub_Caller.Create ("Control_Data_Out", Control_Data_Out'Access));
   Suite.Add_Test (UDC_Stub_Caller.Create ("String Descriptor", String_Descriptor'Access));
   Suite.Add_Test (UDC_Stub_Caller.Create ("iface Setup Dispatch", Iface_Setup_Dispatch'Access));

end Tests.Device;
