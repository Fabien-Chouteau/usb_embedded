# usb_embedded
An Ada USB stack for embedded devices

This project and its design are inspired by the USB device stack of the
[libopencm3 project](https://github.com/libopencm3/libopencm3) and
[TinyUSB](https://github.com/hathach/tinyusb).

# How to use the USB Embedded Device Stack:

## What you need:

 - A USB Device Controller (a.k.a UDC), this is the low level driver for the
   USB controller of your microcontroller. It should be provided by the HAL
   (Hardware Abstraction Layer) or BSP (Board Support Package) for your
   microcontroller.

   If you are developing a new HAL or BSP for a microcontroller, see the
   Porting USB Embedded section for writing a UDC implementation.

 - One or more Device Classes. These are the features that your USB device will
   provide (mouse, keyboard, serial port, MIDI, etc.). This crate provides a
   few of the most common classes that can be used out-of-the box, but you can
   also implement your own class (see doc).

 - A USB Embedded Device Stack, this is what implements the USB control
   protocol and links the UDC and classes together.


For example with an UDC driver for samd51 and two classes:

```ada
   UDC : aliased SAM.USB.UDC
     (Periph          => SAM_SVD.USB.USB_Periph'Access,
      EP_Buffers_Size => 256,
      Max_Packet_Size => 64);

   Class_Serial : aliased USB.Device.Serial.Default_Serial_Class (...);
   Class_HID    : aliased USB.Device.HID.Default_HID_Class (..);

   Stack : USB.Device.USB_Device_Stack;
```

## Initialization


There are 4 major steps in the USB Device stack initialization process:

 - Initialize the UDC: This step depends on the microcontroller and might
   require:
    - Setting up IO pins
    - Setting up clocks
    - Setting up Direct Memory Access (DMA)
   Please follow the documentation the of HAL/BSP for your microcontroller.

 - Register classes: This is when the application decides what features will be
   provided by the USB Device. It can be serial, HID (mouse/keyboard), both or
   more.

 - Initialize the stack: During this step the classes will request resources
   from the Stack and the UDC. If the classes cannot get the resources they
   need, an error is returned and the stack cannot be started.

 - Start the stack: After this step the stack is ready to work and receive the
   first packets from the USB Host.

```ada
begin

   --  TODO: UDC Initialization here

   --  Register classes
   Stack.Register_Class (Class_Serial'Access);
   Stack.Register_Class (Class_HID'Access);

   --  Initialize the stack
   if Stack.Initialize (UDC'Access,
                        USB.To_USB_String ("Wee Noise Makers"),
                        USB.To_USB_String ("Noise Nugget"),
                        USB.To_USB_String ("v1.0"),
                        UDC.Max_Packet_Size) /= Ok
   then
      raise Program_Error with "Cannot initialize USB Stack";
   end if;

   --  Start the stack
   Stack.Start;
```

## Polling or interrupt driven:

There are currently two ways to use the USB stack:

### Polling

With this method, the application must call the `Poll` sub-program of the stack
at a regular interval. The stack will then ask the UDC if any event occurred on
the USB bus since the last call and handles those events.

### Interrupt driven

With this method the application will still call the `Poll` sub-program of the
stack, but only in the interrupt handler of the UDC.

# Writing a custom Device Class

## Initialization

If a class is registered, its `Initialize` procedure will be called during
initialization of the stack. The body of the procedure `Initialize` is the
*only* place where the classes can request resources such as end-points and
end-points buffers.

### Requesting End-Points

Classes use end-points to send or receive data to/from the host.

Unfortunately every UDC only has a limited number of end-points available for
classes, so an end-points request can potentially fail.

### Requesting End-Point Buffers

Some UDC have internal DMA controller to access any data buffers in RAM
(samd51), others can only access special memory banks attached to the
controller (RP2040).

In theory the internal DMA controller allow a design with one less copy of
memory during the transfers as data doesn't have to be copied to a special
memory area. In practice there are constraints on what the DMA control can do,
for instance data has to be aligned on 4 bytes or data must not be in flash
(samd51). The result is that USB class drivers that will work on any hardware
will not benefit always from this optimization (less copy) and handling the
different cases will increase the complexity.

For the design of this USB stack the choice is to ignore DMA controllers and
always use memory allocated by the UDC. The stack provides a way for classes to
request buffers that will be used with end-points. UDC will either return a
pointer in RAM (samd51) or in dedicated memory, regardless the provided memory
must be accessible by the CPU to read/write data from/to it.

Classes *must always* use memory allocated from this API with end-points and
nothing else. To that effect the EP_Ready_For_Data and EP_Send_Packet do not
have a buffer address argument, the buffer address is always the one return by
Request_Buffer.

## Configuration

*WORK IN PROGRESS*

### Setting up End-Points

*WORK IN PROGRESS*

## Host to Device transfers (OUT)

*WORK IN PROGRESS*

## Device to Host transfers (IN)

*WORK IN PROGRESS*

# Porting USB Embedded

Porting the USB Embedded stack to a new microcontroller mostly means
implementing the `USB.HAL.Device.USB_Device_Controller` interface for the USB
Device controller.

*WORK IN PROGRESS*

# Design

It is highly recommended to read the invaluable ["USB in a
NutShell"](https://www.beyondlogic.org/usbnutshell/usb1.shtml) website to
understand the following design documentation.

## Device Control Transfers

The main part of the USB Stack is the handling of control transfers (Setup
Requests). The control part is mostly a state machine that handles Setup
Requests and the data payload associated with them. The payload can be either
sent by the Host with the Setup Packet (bmRequestType = `Host to Device`), or
sent by the Device as an answer to the Setup Packet (bmRequestTyoe = `Device to
Host`). A zero-length-packet (ZLP) is sometimes used for acknowledgement of the
Setup Request. If the Device doesn't support a given Setup Packet sent by the
Host, the control end-points are stalled to indicate an error.

The control state machine starts in an Idle state. When a Setup Packet is
received, it is decoded and analyzed.

 - If the Setup Packet is of type `Host to Device` with a payload, the state
   machine is set to `Data_Out`/`Last_Data_Out`. These states receive the
   payload from the Host by chunks. The `Last_Data_Out` state is used when the
   next chunk is expected to be the last one for the payload. Once the full
   payload is received, the Setup Packet is processed.

 - If the Setup Packet is of type `Host to Device` but without a payload,
   packet is processed immediately.

 - If the Setup Packet is of type `Device to Host`, the packet is processed
   immediately. If the answer to the packet has a payload (Req.Length > 0), The
   state machine is set to `Data_In`/`Last_Data_In`. These states send the
   payload to the Host by chunks. The `Last_Data_In` state is used when the
   next chunk is expected to be the last one for the payload.


# Dispatching of Setup Requests

*WORK IN PROGRESS*
