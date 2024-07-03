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

with USB.HAL.Device; use USB.HAL.Device;

package USB.Device is

   subtype Class_Index is UInt8;

   --  Forward declaration of Class
   type USB_Device_Class;
   pragma Warnings (Off, "*applying 'Class to an untagged incomplete type*");
   type Any_USB_Device_Class is access all USB_Device_Class'Class;
   pragma Warnings (On, "*applying 'Class to an untagged incomplete type*");

   -- Device Stack --

   type USB_Device_Stack (Max_Classes : Class_Index) is tagged private;

   function Initialized (This : USB_Device_Stack) return Boolean;

   function Register_Class (This  : in out USB_Device_Stack;
                            Class : not null Any_USB_Device_Class)
                            return Boolean
     with Pre => not This.Initialized;
   --  Return False if there is no space left to register the class

   function Request_Endpoint (This : in out USB_Device_Stack;
                              Typ  :        EP_Type;
                              EP   :    out EP_Id)
                              return Boolean
     with Pre => not This.Initialized;
   --  Allocate a non-control End-Point to be used by the class

   function Request_Buffer (This : in out USB_Device_Stack;
                            EP   :        EP_Addr;
                            Len  :        Packet_Size)
                            return System.Address
     with Pre => not This.Initialized;
   --  Allocate a buffer for the corresponding End-Point. This buffer should
   --  then be used by the class for IN or OUT transfers depending on the given
   --  EP address.
   --
   --  Return Null_Address if resquest fails (e.g. no memory available).

   function Register_String (This : in out USB_Device_Stack;
                             Str  : USB_String)
                             return String_Id
     with Pre => not This.Initialized
                and then
                 --  The USB String descriptor must fit in the control buffer,
                 --  this is constraint on the length string that can be used
                 --  in the device.
                 Str'Length <= Control_Buffer_Size - 2;
   --  Register a USB string

   type Init_Result is (Ok, Not_Enough_EPs, Not_Enough_EP_Buffer);

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
     with Post => (if Initialize'Result = Ok then This.Initialized);

   procedure Start (This : in out USB_Device_Stack)
     with Pre => This.Initialized;

   procedure Reset (This : in out USB_Device_Stack)
     with Pre => This.Initialized;

   procedure Poll (This : in out USB_Device_Stack)
     with Pre => This.Initialized;

   function Controller (This : USB_Device_Stack)
                        return not null Any_USB_Device_Controller
     with Pre => This.Initialized;

   -- Device Class Interface --

   type USB_Device_Class is limited interface;

   function Initialize (This                 : in out USB_Device_Class;
                        Dev                  : in out USB_Device_Stack'Class;
                        Base_Interface_Index :        Interface_Id)
                        return Init_Result
   is abstract;

   procedure Get_Class_Info
     (This                     : in out USB_Device_Class;
      Number_Of_Interfaces     :    out Interface_Id;
      Config_Descriptor_Length :    out Natural)
   is abstract;

   procedure Fill_Config_Descriptor (This : in out USB_Device_Class;
                                     Data :    out UInt8_Array)
   is abstract;

   function Configure (This  : in out USB_Device_Class;
                       UDC   : in out USB_Device_Controller'Class;
                       Index : UInt16)
                       return Setup_Request_Answer
   is abstract;

   function Setup_Read_Request (This  : in out USB_Device_Class;
                                Req   : Setup_Data;
                                Buf   : out System.Address;
                                Len   : out Buffer_Len)
                                return Setup_Request_Answer
   is abstract;

   function Setup_Write_Request (This  : in out USB_Device_Class;
                                 Req   : Setup_Data;
                                 Data  : UInt8_Array)
                                 return Setup_Request_Answer
   is abstract;

   procedure Transfer_Complete (This : in out USB_Device_Class;
                                UDC  : in out USB_Device_Controller'Class;
                                EP   :        EP_Addr;
                                CNT  :        Packet_Size)
   is abstract;

private

   type Control_State is (Idle,

                          --  IN means Device to Host
                          Data_In,
                          Last_Data_In,
                          Status_In,

                          --  OUT means Host to Device
                          Data_Out,
                          Last_Data_Out,
                          Status_Out);

   type Device_State is (Idle, Addressed, Configured, Suspended);

   type Control_Machine is record
      --  For better performances this buffer has to be word aligned. So we put
      --  it as the first field of this record.
      Buffer : UInt8_Array (1 .. Control_Buffer_Size);
      --  This Buffer is used to store various data used by the control machine
      --  such as descriptors or incomming setup data. It's content will
      --  be copied to/from EP_In/Out_Addr buffer allocated from the UDC.
      --  Sometimes the class will provide its own data buffer for transfer
      --  in which case this Buffer is not used.

      EP_Out_Addr : System.Address := System.Null_Address;
      EP_In_Addr  : System.Address := System.Null_Address;
      --  Endpoint buffer addresses allocated from the UDC

      Req : Setup_Data;

      Buf : System.Address;
      --  Pointer to the current chunk of in/out data. Can point to Buffer
      --  above or another buffer provided by the class.

      Len : Buffer_Len := 0;
      --  For Out control the number of bytes received so far.
      --  For In control the number of remaining bytes to send.

      State : Control_State := Idle;
      Need_ZLP : Boolean := False;
   end record;

   type Class_Info is record
      Ptr : Any_USB_Device_Class := null;
      First_Iface : Interface_Id := Interface_Id'Last;
      Last_Iface  : Interface_Id := Interface_Id'First;
   end record;
   type Class_Array is array (Class_Index range <>) of Class_Info;

   type Endpoint_Status is record
      Assigned_To : Any_USB_Device_Class := null;
   end record;

   type Endpoint_Status_Array is array (USB.EP_Id) of Endpoint_Status;

   type String_Info is record
      From, To : Natural := 0;
   end record;

   type String_Info_Array
   is array (String_Id range 1 .. Max_Strings) of String_Info;

   type USB_Device_Stack (Max_Classes : Class_Index)
   is tagged record

      Ctrl : Control_Machine;

      Max_Packet_Size : Control_Packet_Size := 0;

      Is_Init : Boolean := False;
      UDC     : Any_USB_Device_Controller := null;
      Classes : Class_Array (1 .. Max_Classes);
      Endpoints : Endpoint_Status_Array;

      Dev_Addr  : UInt7 := 0;
      Dev_State : Device_State := Idle;

      Initializing : Any_USB_Device_Class := null;
      --  Tracks which class is currently being initialized

      Last_String_Id : String_Id := 0;
      Last_String_Index : Natural := 0;
      String_Buffer : String (1 .. Max_Total_String_Chars);
      String_Indexes : String_Info_Array;

      Vendor_Id  : UInt16;
      Product_Id : UInt16;
      Bcd_Device : UInt16;

      Manufacturer_Str : String_Id := Invalid_String_Id;
      Product_Str      : String_Id := Invalid_String_Id;
      Serial_Str       : String_Id := Invalid_String_Id;
   end record;

   procedure Stall_Control_EP (This : in out USB_Device_Stack);

   function Get_Status (This  : in out USB_Device_Stack;
                        Req  : Setup_Data)
                        return Setup_Request_Answer;

   function Get_String (This  : in out USB_Device_Stack;
                        Index : String_Id)
                        return Setup_Request_Answer;

   function Get_Descriptor (This : in out USB_Device_Stack;
                            Req  : Setup_Data)
                            return Setup_Request_Answer;

   function Set_Address (This : in out USB_Device_Stack;
                            Req  : Setup_Data)
                            return Setup_Request_Answer;
   function Set_Configuration (This : in out USB_Device_Stack;
                               Req  : Setup_Data)
                               return Setup_Request_Answer;

   procedure Transfer_Complete (This : in out USB_Device_Stack;
                                EP   :        EP_Addr;
                                CNT  :        Packet_Size);

   procedure Build_Config_Descriptor (This : in out USB_Device_Stack);
   --  Build the configuration descriptor in the control buffer from classes'
   --  interface descriptors.

   procedure Build_Device_Descriptor (This : in out USB_Device_Stack);
   --  Build the device descriptor in the control buffer

   procedure Build_Device_Qualifier (This : in out USB_Device_Stack);
   --  Build the device qualifier in the control buffer

   type Transfer_Buffer is new System.Address;
   No_Buffer : constant Transfer_Buffer :=
     Transfer_Buffer (System.Null_Address);

   -------------
   -- To_Addr --
   -------------

   function To_Addr (B : Transfer_Buffer) return System.Address
   is (System.Address (B));

end USB.Device;
