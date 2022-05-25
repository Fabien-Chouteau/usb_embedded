with Interfaces; use Interfaces;

with AUnit.Assertions;

with AAA.Strings;

with HAL; use HAL;
with USB_Testing.Output;

with USB_Testing.UDC_Stub; use USB_Testing.UDC_Stub;
with USB_Testing.UDC_Scenarios;
with USB_Testing; use USB_Testing;
with USB.HAL.Device; use USB.HAL.Device;

with USB; use USB;
with USB.Device.HID.Mouse;
with USB.Device; use USB.Device;

package body Tests.Device.HID is

   -----------
   -- Mouse --
   -----------

   procedure Mouse (Unused : in out UDC_Stub_Fixture) is
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

      HID_Class : aliased USB.Device.HID.Mouse.Instance;

      Output : aliased USB_Testing.Output.Text_Output;

      UDC : aliased UDC_Stub.Controller (Output'Unchecked_Access,
                                         Scenario'Unchecked_Access,
                                         RX_Data'Unchecked_Access,
                                         Has_Early_Address => False,
                                         Max_Packet_Size   => 64,
                                         EP_Buffers_Size   => 256,
                                         Number_Of_EPs     => 1,
                                         Init_Verbose      => False);
      Stack : USB.Device.USB_Device_Stack (Max_Classes => 1);
      Result : USB.Device.Init_Result;

      Expected : constant AAA.Strings.Vector := AAA.Strings.Empty_Vector
        .Append ("UDC Verbose on")
        .Append ("UDC Poll -> SETUP_REQUEST [EP_OUT 0] Type: (HOST_TO_DEVICE,STAND,DEV) Req: 9 Val: 1 Index: 0 Len: 0")
        .Append ("UDC EP_Ready_For_Data [EP_OUT 0] FALSE")
        .Append ("UDC EP_Setup [EP_IN 1] Type: INTERRUPT")
        .Append ("UDC EP_Write_Packet [EP_IN 0] ZLP")
        .Append ("UDC Poll -> TRANSFER_COMPLETE [EP_IN 0] BCNT: 0")
        .Append ("UDC Poll -> SETUP_REQUEST [EP_OUT 0] Type: (DEVICE_TO_HOST,STAND,IFACE) Req: 6 Val: 8704 Index: 0 Len: 255")
        .Append ("UDC EP_Ready_For_Data [EP_OUT 0] FALSE")
        .Append ("UDC EP_Write_Packet [EP_IN 0] 50 bytes")
        .Append ("0000_0000_0000_0000: 05 01 09 02 A1 01 09 01 A1 00 05 09 19 01 29 03 ..............).")
        .Append ("0000_0000_0000_0010: 15 00 25 01 95 03 75 01 81 02 95 01 75 05 81 03 ..%...u.....u...")
        .Append ("0000_0000_0000_0020: 05 01 09 30 09 31 15 81 25 7F 75 08 95 02 81 06 ...0.1..%.u.....")
        .Append ("0000_0000_0000_0030: C0 C0                                           ..")
        .Append ("UDC Poll -> TRANSFER_COMPLETE [EP_IN 0] BCNT: 50")
        .Append ("UDC EP_Ready_For_Data [EP_OUT 0] TRUE")
        .Append ("UDC Poll -> TRANSFER_COMPLETE [EP_OUT 0] BCNT: 0")
        .Append ("UDC Poll -> NONE")
        .Append ("UDC EP_Write_Packet [EP_IN 1] 3 bytes")
        .Append ("0000_0000_0000_0000: 00 0A 00                                        ...")
        .Append ("UDC Poll -> TRANSFER_COMPLETE [EP_IN 1] BCNT: 3")
        .Append ("UDC Poll -> NONE")
        .Append ("UDC EP_Write_Packet [EP_IN 1] 3 bytes")
        .Append ("0000_0000_0000_0000: 00 00 F6                                        ...")
        .Append ("UDC Poll -> TRANSFER_COMPLETE [EP_IN 1] BCNT: 3")
        .Append ("UDC Poll -> NONE")
        .Append ("UDC EP_Write_Packet [EP_IN 1] 3 bytes")
        .Append ("0000_0000_0000_0000: 01 00 00                                        ...")
        .Append ("UDC Poll -> TRANSFER_COMPLETE [EP_IN 1] BCNT: 3")
        .Append ("UDC Poll -> NONE");

      S : Natural := 1;
   begin

      AUnit.Assertions.Assert
        (Stack.Register_Class (HID_Class'Unchecked_Access),
         "USB STACK register class failed");

      Result := Stack.Initialize
        (Controller      => UDC'Unchecked_Access,
         Manufacturer    => To_USB_String ("Manufacturer"),
         Product         => To_USB_String ("Product"),
         Serial_Number   => To_USB_String ("Serial"),
         Max_Packet_Size => 64);

      AUnit.Assertions.Assert
        (Result = Ok, "USB STACK Init failed: " & Result'Img);

      Stack.Start;

      loop
         Stack.Poll;

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

      declare
         use USB_Testing.Output;

         Actual : constant AAA.Strings.Vector := Output.Dump;
      begin
         AUnit.Assertions.Assert
           (Equal (Expected, Actual),
            "Diff in output: " & ASCII.LF &
              Diff (Expected, Actual, "Expected", "Actual").Flatten (ASCII.LF));

      end;

   end Mouse;

begin

   Suite.Add_Test (UDC_Stub_Caller.Create ("Device HID Mouse", Mouse'Access));

end Tests.Device.HID;
