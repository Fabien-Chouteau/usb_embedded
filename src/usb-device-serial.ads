------------------------------------------------------------------------------
--                                                                          --
--                        Copyright (C) 2021, AdaCore                       --
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

with BBqueue;
private with BBqueue.Buffers;
private with Atomic;

package USB.Device.Serial is
   use type System.Storage_Elements.Storage_Offset;

   --  @summary Default USB serial device class implementation
   --  @description
   --  This limited type provides a simple CDC-ACM (virtual serial port)
   --  implementation on top of the USB device controller. It uses two BBqueue
   --  ring buffers for transmit and receive data.
   --
   --  **Recommended buffer sizing**
   --  * `TX_Buffer_Size`: at least 2 × USB packet size (128 bytes minimum for
   --    full-speed USB).
   --  * `RX_Buffer_Size`: at least as large as the largest single read
   --    operation expected by the application.
   --
   type Default_Serial_Class (TX_Buffer_Size, RX_Buffer_Size : BBqueue.Count)
   is limited new USB_Device_Class with private;

   type CDC_Line_Coding is record
      Bitrate   : UInt32;
      Stop_Bit  : UInt8;
      Parity    : UInt8;
      Data_Bits : UInt8;
   end record
     with Pack, Size => 56;

   type CDC_Line_Control_State is record
      DTE_Is_Present   : Boolean;
      Half_Duplex_Carrier_control : Boolean;
      Reserved : UInt14;
   end record
     with Pack, Size => 16;

   function Line_Coding (This : Default_Serial_Class)
                         return CDC_Line_Coding;

   function List_Ctrl_State (This : Default_Serial_Class)
                             return CDC_Line_Control_State;

   procedure Set_Interface_String (This  : in out Default_Serial_Class;
                                   Stack : in out USB_Device_Stack'Class;
                                   Str   :        String);
   --  Only has effect when called before enumeration

   ------------------------------------
   -- Reading Data from the USB Host --
   ------------------------------------

   --  @summary Read data received from the USB host
   --  @description
   --  Attempts to read up to `Len` bytes from the receive ring buffer into the
   --  provided memory location. The actual number of bytes transferred is
   --  returned in the `Len` parameter.
   --
   --  Longer messages than the available buffer space will not be transferred
   --  in a single call. On return the caller must check the updated `Len` value
   --  and retry if more data is required.
   --
   --  A short delay between retries is recommended to avoid saturating the
   --  receive ring buffer. Because of possible fragmentation inside the ring
   --  buffer a retry may be necessary even for relatively small requests.
   --
   --  @param This The serial device instance
   --  @param Buf  Address of the buffer to receive the data
   --  @param Len  On entry: maximum number of bytes to read.
   --              On exit:  actual number of bytes transferred.
   procedure Read (This : in out Default_Serial_Class;
                   Buf  :        System.Address;
                   Len  : in out UInt32) with
     Post => (Len <= Len'Old) and then Len <= UInt32 (This.Rx_Buffer_Size);

   --  @summary Read data received from the USB host into a string
   --  @description
   --  Same semantics as the address-based `Read` procedure, but writes the data
   --  directly into an Ada `String`. The string must be large enough to hold
   --  the requested number of characters.
   --
   --  See the address-based `Read` for details on partial transfers and retry
   --  behaviour.
   --
   --  @param This The serial device instance
   --  @param Str  Output string that will receive the data
   --  @param Len  On exit:  actual number of characters transferred.
   procedure Read (This : in out Default_Serial_Class;
                   Str  :    out String;
                   Len  :    out UInt32) with
     Post => (Len <= Len'Old) and then Len <= UInt32 (This.Rx_Buffer_Size);

   ----------------------------------
   -- Writing Data to the USB Host --
   ----------------------------------

   --  @summary Write data to the USB host
   --  @description
   --  Attempts to queue up to `Len` bytes for transmission to the USB host. The
   --  actual number of bytes accepted by the driver is returned in the `Len`
   --  parameter.
   --
   --  The driver will never accept more bytes than supplied, but may accept
   --  fewer due to buffer space or USB endpoint constraints. Longer messages
   --  must be sent in multiple calls.
   --
   --  The caller is responsible for retrying any remaining data after a short
   --  delay. Sending data too quickly without delays can completely deadlock
   --  the USB stack with no automatic recovery.
   --
   --  @param This The serial device instance
   --  @param UDC  The underlying USB device controller
   --  @param Buf  Address of the data to be transmitted
   --  @param Len  On entry: number of bytes to send.
   --              On exit:  actual number of bytes queued.
   procedure Write (This : in out Default_Serial_Class;
                    UDC  : in out USB_Device_Controller'Class;
                    Buf  :        System.Address;
                    Len  : in out UInt32) with
     Post => (Len <= Len'Old) and then Len <= UInt32 (This.TX_Buffer_Size) / 2;

   --  @summary Write data to the USB host from a string
   --  @description
   --  Same semantics as the address-based `Write` procedure, but
   --  reads the data directly from an Ada `String`.
   --
   --  See the address-based `Write` for details on partial transfers,
   --  retry behaviour.
   --
   --  @param This The serial device instance
   --  @param UDC  The underlying USB device controller
   --  @param Str  String containing the data to be transmitted
   --  @param Len  On exit: actual number of characters queued.
   procedure Write (This : in out Default_Serial_Class;
                    UDC  : in out USB_Device_Controller'Class;
                    Str  :        String;
                    Len  :    out UInt32) with
     Post => Len <= Str'Length and then Len <= UInt32 (This.TX_Buffer_Size) / 2;

private

   type Default_Serial_Class (TX_Buffer_Size, RX_Buffer_Size : BBqueue.Count)
   is limited new USB_Device_Class with record
      Interface_Index : Interface_Id;
      Int_EP          : USB.EP_Id;
      Bulk_EP         : USB.EP_Id;
      Iface_Str       : USB.String_Id := Invalid_String_Id;

      Int_Buf         : System.Address := System.Null_Address;
      Bulk_Out_Buf    : System.Address := System.Null_Address;
      Bulk_In_Buf     : System.Address := System.Null_Address;

      TX_Queue : BBqueue.Buffers.Buffer (TX_Buffer_Size);
      RX_Queue : BBqueue.Buffers.Buffer (RX_Buffer_Size);

      TX_In_Progress : aliased Atomic.Flag := Atomic.Init (False);

      Coding : CDC_Line_Coding;
      State  : CDC_Line_Control_State;
   end record;

   procedure Setup_RX (This : in out Default_Serial_Class;
                       UDC  : in out USB_Device_Controller'Class);

   procedure Setup_TX (This : in out Default_Serial_Class;
                       UDC  : in out USB_Device_Controller'Class);

   overriding
   function Initialize (This                 : in out Default_Serial_Class;
                        Dev                  : in out USB_Device_Stack'Class;
                        Base_Interface_Index :        Interface_Id)
                        return Init_Result;

   overriding
   procedure Get_Class_Info
     (This                     : in out Default_Serial_Class;
      Number_Of_Interfaces     :    out Interface_Id;
      Config_Descriptor_Length :    out Natural);

   overriding
   procedure Fill_Config_Descriptor (This : in out Default_Serial_Class;
                                     Data :    out UInt8_Array);
   overriding
   function Configure (This  : in out Default_Serial_Class;
                       UDC   : in out USB_Device_Controller'Class;
                       Index : UInt16)
                       return Setup_Request_Answer;

   overriding
   function Setup_Read_Request (This  : in out Default_Serial_Class;
                                Req   : Setup_Data;
                                Buf   : out System.Address;
                                Len   : out Buffer_Len)
                                return Setup_Request_Answer;

   overriding
   function Setup_Write_Request (This  : in out Default_Serial_Class;
                                 Req   : Setup_Data;
                                 Data  : UInt8_Array)
                                 return Setup_Request_Answer;

   overriding
   procedure Transfer_Complete (This : in out Default_Serial_Class;
                                UDC  : in out USB_Device_Controller'Class;
                                EP   :        EP_Addr;
                                CNT  :        Packet_Size);

end USB.Device.Serial;
