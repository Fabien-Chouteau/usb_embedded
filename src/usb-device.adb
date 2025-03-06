------------------------------------------------------------------------------
--                                                                          --
--                     Copyright (C) 2018-2021, AdaCore                     --
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

with Ada.Text_IO;
with System.Storage_Elements; use System.Storage_Elements;

with USB.Device.Control;

with USB.Logging.Device;

with USB.Utils;

package body USB.Device is

   procedure Put_Line (Str : String);
   procedure Put_Line (Str : String) is
   begin
      if Verbose then
         Ada.Text_IO.Put_Line (Str);
      end if;
   end Put_Line;

   ----------------------
   -- Stall_Control_EP --
   ----------------------

   procedure Stall_Control_EP (This : in out USB_Device_Stack) is
   begin
      This.UDC.EP_Stall ((0, EP_In));
      This.UDC.EP_Stall ((0, EP_Out));
      This.Ctrl.State := Idle;
   end Stall_Control_EP;

   ----------------
   -- Get_Status --
   ----------------

   function Get_Status (This : in out USB_Device_Stack;
                        Req  : Setup_Data)
                        return Setup_Request_Answer
   is
   begin
      if Req.Value = 0 and then Req.Index = 0 and then Req.Length = 2 then
         This.Ctrl.Buffer (1) := 0;  --  Reserved
         This.Ctrl.Buffer (2) := 0;  --  B1: Remote Wakeup; B0: Self Powered
         This.Ctrl.Buf := This.Ctrl.Buffer'Address;
         This.Ctrl.Len := Storage_Offset (2);
         return Handled;
      end if;

      return Not_Supported;
   end Get_Status;

   ----------------
   -- Get_String --
   ----------------

   function Get_String (This  : in out USB_Device_Stack;
                        Index : String_Id)
                        return Setup_Request_Answer
   is
   begin

      --  Send string descriptor using internal control buffer
      pragma Compile_Time_Error (This.Ctrl.Buffer'Length < 256,
                                 "Internal control buffer too small for " &
                                   "max string descriptor");

      if Index = 0 then
         This.Ctrl.Buffer (1) := 4; -- bLength
         This.Ctrl.Buffer (2) := Dt_String'Enum_Rep; -- 0x03

         --  LANG_EN_US
         This.Ctrl.Buffer (3) := ASCII.HT'Enum_Rep;  -- 0x04
         This.Ctrl.Buffer (4) := ASCII.EOT'Enum_Rep; -- 0x09
         This.Ctrl.Buf := This.Ctrl.Buffer'Address;
         This.Ctrl.Len := Storage_Offset (This.Ctrl.Buffer (1));
         return Handled;
      end if;

      if Index not in 1 .. This.Last_String_Id then
         return Not_Supported;
      end if;

      declare
         Info : String_Info renames This.String_Indexes (Index);
         Len : constant Natural := Info.To - Info.From + 1;
         Dst : String (1 .. Len)
           with Address => This.Ctrl.Buffer (3)'Address;
      begin
         This.Ctrl.Buffer (1) := UInt8 (Len + 2); -- bLength
         This.Ctrl.Buffer (2) := Dt_String'Enum_Rep;
         Dst := This.String_Buffer (Info.From .. Info.To);
      end;

      This.Ctrl.Buf := This.Ctrl.Buffer'Address;
      This.Ctrl.Len := Storage_Offset (This.Ctrl.Buffer (1));
      return Handled;
   end Get_String;

   --------------------
   -- Get_Descriptor --
   --------------------

   function Get_Descriptor (This : in out USB_Device_Stack;
                            Req  : Setup_Data)
                            return Setup_Request_Answer
   is
      Index     : constant UInt8 := Utils.Low (Req.Value);
      Desc_Type : constant UInt8 := Utils.High (Req.Value);
   begin

      case Desc_Type is
         when Dt_Device'Enum_Rep =>
            Put_Line ("DT_DEVICE");
            This.Build_Device_Descriptor;
            return Handled;

         when Dt_Configuration'Enum_Rep =>
            Put_Line ("DT_CONFIGURATION");
            This.Build_Config_Descriptor;
            return Handled;

         when Dt_String'Enum_Rep =>
            Put_Line ("DT_STRING");
            return Get_String (This, String_Id (Index));

         when Dt_Qualifier'Enum_Rep =>
            Put_Line ("DT_QUALIFIER");
            This.Build_Device_Qualifier;
            return Handled;

         when others =>
            Put_Line ("Descriptor not implemented:" & Desc_Type'Img);
            return Not_Supported;
      end case;
   end Get_Descriptor;

   -----------------
   -- Set_Address --
   -----------------

   function Set_Address (This : in out USB_Device_Stack;
                         Req  : Setup_Data)
                         return Setup_Request_Answer
   is
   begin
      This.Dev_Addr := UInt7 (Req.Value and 16#7F#);

      Put_Line ("Set Address: " & This.Dev_Addr'Img);

      if This.UDC.Early_Address then
         Put_Line ("Set early Address: " & This.Dev_Addr'Img);

         --  The DWC OTG USB requires the address to be set at this point...
         This.UDC.Set_Address (This.Dev_Addr);
      end if;

      --  Reply with Zero-Length-Packet
      This.Ctrl.Buf := System.Null_Address;
      This.Ctrl.Len := 0;
      return Handled;
   end Set_Address;

   -----------------------
   -- Set_Configuration --
   -----------------------

   function Set_Configuration (This : in out USB_Device_Stack;
                               Req  : Setup_Data)
                               return Setup_Request_Answer
   is
      Answer : Setup_Request_Answer := Handled;
   begin

      for Class of This.Classes loop
         exit when Class.Ptr = null;
         Answer := Class.Ptr.Configure (This.UDC.all, Req.Value);
         exit when Answer /= Handled;
      end loop;

      This.Dev_State := Configured;

      return Answer;
   end Set_Configuration;

   -----------------
   -- Initialized --
   -----------------

   function Initialized (This : USB_Device_Stack) return Boolean
   is (This.Is_Init);

   --------------------
   -- Register_Class --
   --------------------

   function Register_Class (This  : in out USB_Device_Stack;
                            Class : not null Any_USB_Device_Class)
                            return Boolean
   is
   begin

      for Class_I of This.Classes loop
         if Class_I.Ptr = null then
            Class_I.Ptr := Class;
            return True;
         end if;
      end loop;

      return False;
   end Register_Class;

   ----------------------
   -- Request_Endpoint --
   ----------------------

   function Request_Endpoint (This : in out USB_Device_Stack;
                              Typ  :        EP_Type;
                              EP   : out EP_Id)
                              return Boolean
   is
   begin
      if This.Initializing = null then
         return False;
      end if;

      for Id in EP_Id range 1 .. EP_Id'Last loop

         if This.UDC.Valid_EP_Id (Id) -- Check if the EP exists on the UDC
           and then
            This.Endpoints (Id).Assigned_To = null -- Check if not already used
         then
            EP := Id;
            This.Endpoints (Id).Assigned_To := This.Initializing;
            return True;
         end if;
      end loop;

      return False;
   end Request_Endpoint;

   --------------------
   -- Request_Buffer --
   --------------------

   function Request_Buffer (This : in out USB_Device_Stack;
                            EP   :        EP_Addr;
                            Len  :        Packet_Size)
                            return System.Address
   is
   begin
      if This.UDC /= null then
         return This.UDC.Request_Buffer (EP, Len);
      else
         return System.Null_Address;
      end if;
   end Request_Buffer;

   ---------------------
   -- Register_String --
   ---------------------

   function Register_String (This : in out USB_Device_Stack;
                             Str  : USB_String)
                             return String_Id
   is
   begin
      if Str'Length = 0
        or else
         This.Last_String_Id = Max_Strings
        or else
         (Max_Total_String_Chars - This.Last_String_Index) < Str'Length
      then
         return Invalid_String_Id;
      end if;

      This.Last_String_Id := This.Last_String_Id + 1;

      declare
         From : constant Natural := This.Last_String_Index + 1;
         To   : constant Natural := This.Last_String_Index + Str'Length;
         Dst  : USB_String (1 .. Str'Length)
           with Address => This.String_Buffer (From)'Address;
      begin
         Dst := Str;
         This.String_Indexes (This.Last_String_Id).From := From;
         This.String_Indexes (This.Last_String_Id).To := To;
         This.Last_String_Index := To;
      end;

      This.Last_String_Index := This.Last_String_Index + 1;
      return This.Last_String_Id;
   end Register_String;

   ---------------
   -- Initalize --
   ---------------

   function Initialize
     (This            : in out USB_Device_Stack;
      Controller      : not null Any_USB_Device_Controller;
      Manufacturer    : USB_String;
      Product         : USB_String;
      Serial_Number   : USB_String;
      Max_Packet_Size : Control_Packet_Size;
      Vendor_Id       : UInt16 := 16#6666#;
      Product_Id      : UInt16 := 16#4242#;
      Bcd_Device      : UInt16 := 16#0121#)
      return Init_Result
   is
      use System;

      Number_Of_Interfaces : Interface_Id;
      Unused : Natural;
      Iface_Id : Interface_Id := 0;
      Result : Init_Result;
   begin

      This.Max_Packet_Size := Max_Packet_Size;

      This.UDC := Controller;

      --  Control EP buffers

      This.Ctrl.EP_In_Addr := This.UDC.Request_Buffer
        (Ep  => (0, EP_In),
         Len => This.Max_Packet_Size);

      if This.Ctrl.EP_In_Addr = System.Null_Address then
         return Not_Enough_EP_Buffer;
      end if;

      This.Ctrl.EP_Out_Addr := This.UDC.Request_Buffer
        (Ep  => (0, EP_Out),
         Len => This.Max_Packet_Size);

      if This.Ctrl.EP_Out_Addr = System.Null_Address then
         return Not_Enough_EP_Buffer;
      end if;

      --  Init classes

      for Class of This.Classes loop
         exit when Class.Ptr = null;

         Class.Ptr.Get_Class_Info
           (Number_Of_Interfaces     => Number_Of_Interfaces,
            Config_Descriptor_Length => Unused);

         --  Mark which class we are initializing
         This.Initializing := Class.Ptr;

         --  Initialize the class
         declare
            --  Workaround gnatprove warning on Class.Ptr
            Class_Ptr : constant Any_USB_Device_Class := Class.Ptr;
         begin
            Result := Class_Ptr.Initialize (This, Iface_Id);
         end;

         if Result /= Ok then
            return Result;
         end if;

         --  Record the range of interface for this class:
         Class.First_Iface := Iface_Id;
         Class.Last_Iface := Iface_Id + Number_Of_Interfaces - 1;

         --  Move interface index for the next class
         Iface_Id := Iface_Id + Number_Of_Interfaces;
      end loop;

      This.Initializing := null;

      This.Vendor_Id  := Vendor_Id;
      This.Product_Id := Product_Id;
      This.Bcd_Device := Bcd_Device;

      --  Register mendatory strings
      This.Manufacturer_Str := This.Register_String (Manufacturer);
      This.Product_Str := This.Register_String (Product);
      This.Serial_Str := This.Register_String (Serial_Number);

      This.Is_Init := True;

      return Ok;
   end Initialize;

   -----------
   -- Start --
   -----------

   procedure Start (This : in out USB_Device_Stack) is
   begin
      This.UDC.Initialize; --  This should actually init
      This.UDC.Start;
   end Start;

   -----------
   -- Reset --
   -----------

   procedure Reset (This : in out USB_Device_Stack) is
   begin
      This.UDC.EP_Setup ((0, EP_In), USB.Control);

      This.UDC.EP_Setup ((0, EP_Out), USB.Control);

      This.UDC.Set_Address (0);

      This.UDC.EP_Ready_For_Data (0, 8, True);

      This.Ctrl.State := Idle;

      This.UDC.Reset;
   end Reset;

   -----------------------
   -- Transfer_Complete --
   -----------------------

   procedure Transfer_Complete (This : in out USB_Device_Stack;
                                EP   :        EP_Addr;
                                CNT  :        Packet_Size)
   is
      Assigned_To : Any_USB_Device_Class
      renames This.Endpoints (EP.Num).Assigned_To;
   begin
      if EP = (0, EP_In) then
         USB.Device.Control.Control_In (This);

      elsif EP = (0, EP_Out) then
         USB.Device.Control.Control_Out (This, CNT);

      elsif Assigned_To /= null then
         Assigned_To.Transfer_Complete (This.UDC.all, EP, CNT);

      end if;
   end Transfer_Complete;

   -----------------------------
   -- Build_Device_Descriptor --
   -----------------------------

   procedure Build_Device_Descriptor (This : in out USB_Device_Stack) is
      Desc : Device_Descriptor
        with Address => This.Ctrl.Buffer'Address;
   begin
      Desc := (bLength            => Desc'Size / 8,
               bDescriptorType    => Dt_Device'Enum_Rep,
               bcdUSB             => 16#0110#,
               bDeviceClass       => 0,
               bDeviceSubClass    => 0,
               bDeviceProtocol    => 0,
               bMaxPacketSize0    => UInt8 (This.Max_Packet_Size),
               idVendor           => This.Vendor_Id,
               idProduct          => This.Product_Id,
               bcdDevice          => This.Bcd_Device,

               --  String IDs
               iManufacturer      => This.Manufacturer_Str,
               iProduct           => This.Product_Str,
               iSerialNumber      => This.Serial_Str,
               bNumConfigurations => 1);

      This.Ctrl.Buf := This.Ctrl.Buffer'Address;
      This.Ctrl.Len := Desc'Size / 8;
   end Build_Device_Descriptor;

   ----------------------------
   -- Build_Device_Qualifier --
   ----------------------------

   procedure Build_Device_Qualifier (This : in out USB_Device_Stack) is
      Desc : Device_Qualifier
        with Address => This.Ctrl.Buffer'Address;
   begin
      Desc := (bLength            => Desc'Size / 8,
               bDescriptorType    => Dt_Qualifier'Enum_Rep,
               bcdUSB             => 16#0200#,
               bDeviceClass       => 0,
               bDeviceSubClass    => 0,
               bDeviceProtocol    => 0,
               bMaxPacketSize0    => UInt8 (This.Max_Packet_Size),
               bNumConfigurations => 1,
               bReserved          => 0);

      This.Ctrl.Buf := This.Ctrl.Buffer'Address;
      This.Ctrl.Len := Desc'Size / 8;
   end Build_Device_Qualifier;

   -----------------------------
   -- Build_Config_Descriptor --
   -----------------------------

   procedure Build_Config_Descriptor (This : in out USB_Device_Stack) is
      Total_Length : Natural;
      Len : Natural;
      First : constant Natural := This.Ctrl.Buffer'First;
      Number_Of_Interfaces : Interface_Id := 0;
      Total_Number_Of_Interfaces : Interface_Id := 0;
   begin
      Total_Length := 9; -- Size of configuration descriptor

      for Class of This.Classes loop
         exit when Class.Ptr = null;

         Class.Ptr.Get_Class_Info (Number_Of_Interfaces,
                                   Config_Descriptor_Length => Len);

         if Total_Length + Len > This.Ctrl.Buffer'Length then
            raise Program_Error with "Not enought space in control buffer" &
              " for configuration descriptor";
         end if;

         declare
            From  : constant Natural := First + Total_Length;
            To    : constant Natural := From + Len - 1;
         begin
            Class.Ptr.Fill_Config_Descriptor (This.Ctrl.Buffer (From .. To));
         end;

         Total_Length := Total_Length + Len;
         Total_Number_Of_Interfaces :=
           Total_Number_Of_Interfaces + Number_Of_Interfaces;
      end loop;

      --  Now that we know the total length of the configuration we can write
      --  the configuration descriptor.

      declare
         Len_Low : constant UInt8 := UInt8 (UInt32 (Total_Length) and 16#FF#);

         Len_High : constant UInt8 :=
           UInt8 (Shift_Right (UInt32 (Total_Length) and 16#FF00#, 8));

         USB_DESC_TYPE_CONFIGURATION : constant := 2;
         USB_CFG_MAX_BUS_POWER : constant := 100;

      begin
         This.Ctrl.Buffer (First .. First + 9 - 1) :=
           (   --  USB configuration descriptor
            9, --  sizeof(usbDescrConfig): length of descriptor in bytes
            USB_DESC_TYPE_CONFIGURATION, --  descriptor type
            Len_Low, Len_High, --  total length of data returned
            UInt8 (Total_Number_Of_Interfaces), -- Ifaces in this configuration
            1, --  index of this configuration
            0, --  configuration name string index
            Shift_Left (1, 7), --  attributes
            USB_CFG_MAX_BUS_POWER / 2 --  max USB current in 2mA units */);
           );
      end;

      This.Ctrl.Buf := This.Ctrl.Buffer'Address;
      This.Ctrl.Len := Storage_Offset (Total_Length);

   end Build_Config_Descriptor;

   ----------
   -- Poll --
   ----------

   procedure Poll (This : in out USB_Device_Stack) is
   begin

      loop
         declare
            Evt : constant UDC_Event := This.UDC.Poll;
         begin

            USB.Logging.Device.Log (Evt);

            case Evt.Kind is
            when Reset =>
               Put_Line ("Poll: Reset");
               Reset (This);
            when Setup_Request =>
               Put_Line ("Poll: Setup_Request");
               This.Ctrl.Req := Evt.Req;
               USB.Device.Control.Setup (This, Evt.Req_EP);
            when Transfer_Complete =>
               Put_Line ("Poll: Transfer_Complete");
               This.Transfer_Complete (Evt.EP, Evt.BCNT);
            when None =>
               Put_Line ("Poll: None");
               return;
            end case;
         end;
      end loop;
   end Poll;

   ----------------
   -- Controller --
   ----------------

   function Controller (This : USB_Device_Stack)
                        return not null Any_USB_Device_Controller
   is (This.UDC);

end USB.Device;
