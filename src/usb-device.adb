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

with Ada.Text_IO;
with System.Storage_Elements; use System.Storage_Elements;

with USB.Device.Control;

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

   procedure Stall_Control_EP (This : in out USB_Device) is
   begin
      This.UDC.EP_Set_Stall ((0, EP_In));
      This.UDC.EP_Set_Stall ((0, EP_Out));
      This.Ctrl.State := Idle;
   end Stall_Control_EP;

   ----------------
   -- Get_String --
   ----------------

   function Get_String (This  : in out USB_Device;
                        Index : UInt8)
                        return Setup_Request_Answer
   is
   begin
      for Str of This.Strings.all loop
         if Str.Index = Index then

            This.Ctrl.Buf := Str.Str.all'Address;
            This.Ctrl.Len := Str.Str.all'Size / 8;
            return Handled;
         end if;
      end loop;

      return Not_Supported;
   end Get_String;

   --------------------
   -- Get_Descriptor --
   --------------------

   function Get_Descriptor (This : in out USB_Device;
                            Req  : Setup_Data)
                            return Setup_Request_Answer
   is
      Index     : constant UInt8 := UInt8 (Req.Value and 16#FF#);
      Desc_Type : constant UInt8 :=
        UInt8 (Shift_Right (Req.Value, 8) and 16#FF#);
   begin

      case Desc_Type is
         when 1 => -- DT_DEVICE
            Put_Line ("DT_DEVICE");
            This.Build_Device_Descriptor;
            return Handled;
         when 2 => -- DT_CONFIGURATION
            Put_Line ("DT_CONFIGURATION");

            This.Build_Config_Descriptor;
            return Handled;
         when 3 => -- DT_STRING
            Put_Line ("DT_STRING");
            return Get_String (This, Index);
         when 6 => -- DT_QUALIFIER

            Put_Line ("DT_QUALIFIER");

            --  Qualifier descriptor is only available on HS device. This is not
            --  supported yet.
            return Not_Supported;

         when others =>
            raise Program_Error with "Descriptor not implemented " &
              Desc_Type'Img;
            return Not_Supported;
      end case;
   end Get_Descriptor;

   -----------------
   -- Set_Address --
   -----------------

   function Set_Address (This : in out USB_Device;
                         Req  : Setup_Data)
                         return Setup_Request_Answer
   is
   begin
      This.Dev_Addr := UInt7 (Req.Value and 16#7F#);

      if Verbose then
         Put_Line ("Set Address: " & This.Dev_Addr'Img);
      end if;

      if This.UDC.Early_Address then
         if Verbose then
            Put_Line ("Set early Address: " & This.Dev_Addr'Img);
         end if;

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

   function Set_Configuration (This : in out USB_Device;
                               Req  : Setup_Data)
                               return Setup_Request_Answer
   is
      Answer : Setup_Request_Answer := Handled;
   begin

      for Index in Class_Index loop
         exit when This.Classes (Index) = null;
         Answer := This.Classes (Index).Configure (This.UDC.all, Req.Value);
         exit when Answer /= Handled;
      end loop;

      return Answer;
   end Set_Configuration;

   -----------------
   -- Initialized --
   -----------------

   function Initialized (This : USB_Device) return Boolean
   is (This.UDC /= null);

   --------------------
   -- Register_Class --
   --------------------

   procedure Register_Class (This  : in out USB_Device;
                             Class : not null Any_USB_Device_Class)
   is
   begin

      for Index in Class_Index loop
         if This.Classes (Index) = null then
            This.Classes (Index) := Class;
            return;
         end if;
      end loop;

      raise Program_Error with "Max classes limit reached";
   end Register_Class;

   ----------------------
   -- Request_Endpoint --
   ----------------------

   function Request_Endpoint (This : in out USB_Device;
                              EP   : out EP_Id)
                              return Boolean
   is
   begin
      if This.Initializing = null then
         return False;
      end if;

      for Id in EP_Id range 1 .. EP_Id'Last loop
         if This.Endpoints (Id).Assigned_To = null then
            EP := Id;
            This.Endpoints (Id).Assigned_To := This.Initializing;
            return True;
         end if;
      end loop;
      return False;
   end Request_Endpoint;

   ---------------
   -- Initalize --
   ---------------

   procedure Initalize (This            : in out USB_Device;
                        Controller      : not null Any_USB_Device_Controller;
                        Strings         : not null access constant String_Array;
                        Max_Packet_Size : UInt8)
   is
   begin

      for Index in Class_Index loop
         exit when This.Classes (Index) = null;

         This.Initializing := This.Classes (Index);

         This.Classes (Index).Initialize (This, Index);

      end loop;

      This.Initializing := null;

      This.UDC := Controller;
      This.Strings := Strings;
      This.Max_Packet_Size := Max_Packet_Size;

   end Initalize;

   -----------
   -- Start --
   -----------

   procedure Start (This : in out USB_Device) is
   begin

      --  TODO: Clear previous Class

      --  TODO: Set descriptor

      --  TODO: This.State := Default
      --  TODO: This.Id := Id;

      This.UDC.Initialize; --  This should actually init

      This.UDC.Start;

      --  TODO: Register class
   end Start;

   -----------
   -- Reset --
   -----------

   procedure Reset (This : in out USB_Device) is
   begin
      This.UDC.EP_Setup ((0, EP_In), USB.Control,
                         UInt16 (This.Max_Packet_Size), null);

      This.UDC.EP_Setup ((0, EP_Out), USB.Control,
                         UInt16 (This.Max_Packet_Size), null);

      This.UDC.Set_Address (0);

      --  TODO: reset callback
   end Reset;

   -----------------------
   -- Transfer_Complete --
   -----------------------

   procedure Transfer_Complete (This : in out USB_Device;
                                EP   :        EP_Addr)
   is
      Assigned_To : Any_USB_Device_Class
      renames This.Endpoints (EP.Num).Assigned_To;
   begin
      if EP = (0, EP_In) then
         USB.Device.Control.Control_In (This);

      elsif Assigned_To /= null then
         Assigned_To.Transfer_Complete (This.UDC.all, EP);

      end if;
   end Transfer_Complete;

   ----------------
   -- Data_Ready --
   ----------------

   procedure Data_Ready (This  : in out USB_Device;
                         EP    :        EP_Id;
                         Count :        UInt11)
   is
      Assigned_To : Any_USB_Device_Class
      renames This.Endpoints (EP).Assigned_To;
   begin
      if EP = 0 then
         USB.Device.Control.Control_Out (This, Count);

      elsif Assigned_To /= null then

         Assigned_To.Data_Ready (This.UDC.all, EP, UInt32 (Count));
      end if;
   end Data_Ready;

   -----------------------------
   -- Build_Device_Descriptor --
   -----------------------------

   procedure Build_Device_Descriptor (This : in out USB_Device) is
      Desc : Device_Descriptor
        with Address => This.Ctrl.RX_Buf'Address;
   begin
      Desc := (bLength            => USB.Device_Descriptor'Size / 8,
               bDescriptorType    => 1, -- DT_DEVICE
               bcdUSB             => 16#0200#,
               bDeviceClass       => 0,
               bDeviceSubClass    => 0,
               bDeviceProtocol    => 0,
               bMaxPacketSize0    => This.Max_Packet_Size,
               idVendor           => 16#6666#,
               idProduct          => 16#4242#,
               bcdDevice          => 16#0100#,

               --  String IDs
               iManufacturer      => 1,
               iProduct           => 2,
               iSerialNumber      => 3,
               bNumConfigurations => 1);

      This.Ctrl.Buf := This.Ctrl.RX_Buf'Address;
      This.Ctrl.Len := Desc'Size / 8;
   end Build_Device_Descriptor;

   -----------------------------
   -- Build_Config_Descriptor --
   -----------------------------

   procedure Build_Config_Descriptor (This : in out USB_Device) is
      Total_Length : Natural;
      Len : Natural;
      First : constant Natural := This.Ctrl.RX_Buf'First;
   begin
      Total_Length := 9; -- Size of configuration descriptor

      for Index in Class_Index loop
         exit when This.Classes (Index) = null;

         Len := This.Classes (Index).Config_Descriptor_Length;

         if Total_Length + Len > This.Ctrl.RX_Buf'Length then
            raise Program_Error with "Not enought space in control buffer" &
              " for configuration descriptor";
         end if;

         declare
            From  : constant Natural := First + Total_Length;
            To    : constant Natural := From + Len - 1;
         begin
            This.Classes (Index).Fill_Config_Descriptor
              (This.Ctrl.RX_Buf (From .. To));
         end;

         Total_Length := Total_Length + Len;
      end loop;

      --  Now that we know the total length of the configuration we can write
      --  the configuration descriptor.

      declare
         Len_Low : constant UInt8 := UInt8 (UInt32 (Total_Length) and 16#FF#);

         Len_High : constant UInt8 :=
           UInt8 (Shift_Right (UInt32 (Total_Length) and 16#FF00#, 8));

         USB_DESC_TYPE_CONFIGURATION : constant := 2;
         USB_CFG_MAX_BUS_POWER : constant := 2;

      begin
         This.Ctrl.RX_Buf (First .. First + 9 - 1) :=
           (   --  USB configuration descriptor
            9, --  sizeof(usbDescrConfig): length of descriptor in bytes
            USB_DESC_TYPE_CONFIGURATION, --  descriptor type
            Len_Low, Len_High, --  total length of data returned
            2, --  number of interfaces in this configuration
            1, --  index of this configuration
            0, --  configuration name string index
            Shift_Left (1, 7), --  attributes
            USB_CFG_MAX_BUS_POWER / 2 --  max USB current in 2mA units */);
           );
      end;

      This.Ctrl.Buf := This.Ctrl.RX_Buf'Address;
      This.Ctrl.Len := Storage_Offset (Total_Length);

   end Build_Config_Descriptor;

   ----------
   -- Poll --
   ----------

   procedure Poll (This : in out USB_Device) is
   begin

      loop
         declare
            Evt : constant UDC_Event := This.UDC.Poll;
         begin
            case Evt.Kind is
            when Reset =>
               Put_Line ("Poll: Reset");
               Reset (This);
            when Setup_Request =>
               Put_Line ("Poll: Setup_Request");
               This.Ctrl.Req := Evt.Req;
               USB.Device.Control.Setup (This, Evt.Req_EP);
            when Data_Ready =>
               Put_Line ("Poll: Data_Ready");
               This.Data_Ready (Evt.RX_EP, Evt.RX_BCNT);
            when Transfer_Complete =>
               Put_Line ("Poll: Transfer_Complete");
               This.Transfer_Complete (Evt.T_EP);
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

   function Controller (This : USB_Device)
                        return not null Any_USB_Device_Controller
   is (This.UDC);

end USB.Device;
