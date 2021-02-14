# usb_embedded
An Ada USB stack for embedded devices 


Inspired by the USB device stack of the libopencm3 project (link).

The control part of the USB device stack is mostly a state machine that handles
Setup Packets and the data payload associated with them. The payload can be
either sent by the Host with the Setup Packet (bmRequestType = `Host to
Device`), or sent by the Device as an answer to the Setup Packet (bmRequestTyoe
= `Device to Host`). A zero-length-packet (ZLP) is sometimes used for
acknowledgement of the Setup Request. If the Device doesn't support a given
Setup Packet sent by the Host, the control end-points are stalled to indicate
an error.

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
   immediately. If the answer to the packet has payload (Req.Length > 0), The
   state machine is set to `Data_In`/`Last_Data_In`. These states send the
   payload to the Host by chunks. The `Last_Data_In` state is used when the
   next chunk is expected to be the last one for the payload.
 
