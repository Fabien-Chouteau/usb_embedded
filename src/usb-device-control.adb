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

with Ada.Text_IO; use Ada.Text_IO;

with System.Storage_Elements; use System.Storage_Elements;

with USB.Utils;

package body USB.Device.Control is

   function Need_ZLP (Len     : Buffer_Len;
                      wLength : UInt16;
                      EP_Size : Packet_Size)
                      return Boolean;

   --------------
   -- Need_ZLP --
   --------------

   function Need_ZLP (Len     : Buffer_Len;
                      wLength : UInt16;
                      EP_Size : Packet_Size)
                      return Boolean
   is (Len < Buffer_Len (wLength)
       and then
       Len /= 0
       and then
       Len mod Buffer_Len (EP_Size) = 0);

   -----------
   -- Setup --
   -----------

   procedure Setup (This : in out USB_Device_Stack;
                            EP   : EP_Id)
   is
      Req : Setup_Data renames This.Ctrl.Req;
   begin

      if This.Ctrl.State not in
        Idle |

         --  We accept the Status_Out state here because some UDC (samdx1) will
         --  skip the control ZLP OUT if a new setup request is received.
        Status_Out
      then
         raise Program_Error with "Not expecting setup in '" &
           This.Ctrl.State'Img & "' control state";
      end if;

      This.Ctrl.State := Idle;

      This.UDC.EP_Ready_For_Data (EP, 0, False);

      if Verbose then
         Put_Line ("Req: " & Img (This.Ctrl.Req));
      end if;

      if Req.RType.Dir = Device_To_Host then
         Setup_Read (This);
      else
         Setup_Write (This);
      end if;
   end Setup;

   ----------------
   -- Control_In --
   ----------------

   procedure Control_In (This : in out USB_Device_Stack) is
   begin
      case This.Ctrl.State is
         when Data_In =>
            Send_Chunk (This);

         when Last_Data_In =>
            This.Ctrl.State := Status_Out;
            This.UDC.EP_Ready_For_Data (0,
                                        This.Max_Packet_Size,
                                        True);

         when Status_In =>
            if Verbose then
               Put_Line ("Status_In");
            end if;

            --  Exception: handle set address request here
            if This.Ctrl.Req.RType = (Dev, 0, Stand, Host_To_Device)
              and then
                This.Ctrl.Req.Request = Req_Set_Address'Enum_Rep

            then
               This.UDC.Set_Address (UInt7 (This.Ctrl.Req.Value and 16#7F#));
               This.Dev_State := Addressed;
            end if;

            This.Ctrl.State := Idle;

         when others =>
            if Verbose then
               Put_Line ("Control_In: Unexpected state '" &
                           This.Ctrl.State'Img & "'");
            end if;

            This.Stall_Control_EP;
      end case;
   end Control_In;

   -----------------
   -- Control_Out --
   -----------------

   procedure Control_Out (This : in out USB_Device_Stack;
                          BCNT :        Packet_Size)
   is
   begin
      case This.Ctrl.State is
         when Status_Out =>

            if BCNT /= 0 then
               raise Program_Error with "ZLP expected for Status_Out";
            end if;

            This.Ctrl.State := Idle;

         when Data_Out =>

            --  Receive a chunk
            Receive_Chunk (This);

            --  Check if the next chunk is going to be the last
            if (Buffer_Len (This.Ctrl.Req.Length) - This.Ctrl.Len) <=
              Buffer_Len (This.Max_Packet_Size)
            then
               This.Ctrl.State := Last_Data_Out;
            end if;

         when Last_Data_Out =>

            --  Receive the last chunk
            Receive_Chunk (This);

            --  Handle the request now that we have the full payload
            Handle_Write_Request (This);

         when others =>
            if Verbose then
               Put_Line ("Control_Out: Unexpected state '" &
                           This.Ctrl.State'Img & "'");
            end if;

            This.Stall_Control_EP;
      end case;
   end Control_Out;

   ----------------
   -- Setup_Read --
   ----------------

   procedure Setup_Read (This : in out USB_Device_Stack)
   is
   begin
      if Verbose then
         Put_Line ("Control_Setup_Read");
      end if;
      Handle_Read_Request (This);
   end Setup_Read;

   -----------------
   -- Setup_Write --
   -----------------

   procedure Setup_Write (This : in out USB_Device_Stack) is
      Req : Setup_Data renames This.Ctrl.Req;
   begin
      if Verbose then
         Put_Line ("Control_Setup_Write");
      end if;

      if Req.Length = 0 then
         Handle_Write_Request (This);
         return;
      end if;

      if Req.Length > Control_Buffer_Size then
         Stall_Control_EP (This);
         return;
      end if;

      --  Get ready to receive the data.

      This.Ctrl.Len := 0;
      This.Ctrl.Buf := This.Ctrl.Buffer'Address;

      --  The UDC will write data to EP_Out_Addr. Upon transfer complete, we
      --  will copy to internal control buffer.

      if Req.Length > UInt16 (This.Max_Packet_Size) then
         This.Ctrl.State := Data_Out;
         This.UDC.EP_Ready_For_Data (0,
                                     This.Max_Packet_Size,
                                     True);
      else
         This.UDC.EP_Ready_For_Data (0,
                                     Packet_Size (Req.Length),
                                     True);
         This.Ctrl.State := Last_Data_Out;
      end if;

   end Setup_Write;

   --------------------
   -- Device_Request --
   --------------------

   function Device_Request  (This : in out USB_Device_Stack)
                             return Setup_Request_Answer
   is
      Req : Setup_Data renames This.Ctrl.Req;
   begin
      if Verbose then
         Put_Line ("Control Dev Req");
      end if;

      case Req.Request is
         when Req_Get_Status'Enum_Rep =>
            return Get_Status (This, Req);
         when Req_Clear_Feature'Enum_Rep =>
            raise Program_Error with "CLEAR_FEATURE not implemented";
         when Req_Set_Feature'Enum_Rep =>
            raise Program_Error with "SET_FEATURE not implemented";
         when Req_Set_Address'Enum_Rep =>
            return Set_Address (This, Req);
         when Req_Get_Descriptor'Enum_Rep =>
            return Get_Descriptor (This, Req);
         when Req_Set_Descriptor'Enum_Rep =>
            raise Program_Error with "SET_DESCRIPTOR not implemented";
         when Req_Get_Configuration'Enum_Rep =>
            raise Program_Error with "GET_CONFIGURATION not implemented";
         when Req_Set_Configuration'Enum_Rep =>
            return Set_Configuration (This, Req);
         when others =>
            return Not_Supported;
      end case;
   end Device_Request;

   ----------------------
   -- Endpoint_Request --
   ----------------------

   function Endpoint_Request (This : in out USB_Device_Stack)
                              return Setup_Request_Answer
   is
      Req   : Setup_Data renames This.Ctrl.Req;
      Id    : constant EP_Id := EP_Id (Req.Index and 16#0F#);
      Dir   : constant EP_Dir := (if (Req.Index and 16#08#) /= 0
                                  then EP_In
                                  else EP_Out);

      EP : constant EP_Addr := (Id, Dir);
   begin
      case Req.Request is
         when Req_Get_Status'Enum_Rep =>
            raise Program_Error with "EP GET_STATUS not implemented";

         when Req_Clear_Feature'Enum_Rep =>
            case Req.Value is
               when 0 => -- HALT ENDPOINT
                  This.UDC.EP_Stall (EP, False);
                  return Handled;
               when others =>
                  raise Program_Error with "Invalid EP CLEAR_FEATURE";
            end case;

         when Req_Set_Feature'Enum_Rep =>
            case Req.Value is
               when 0 => -- HALT ENDPOINT
                  This.UDC.EP_Stall (EP, True);
                  return Handled;
               when others =>
                  raise Program_Error with "Invalid EP SET_FEATURE";
            end case;

         when Req_Sync_Feature'Enum_Rep =>
            raise Program_Error with "EP SYNCH_FEATURE not implemented";

         when others =>
            raise Program_Error with "Invalid EP request";
      end case;
   end Endpoint_Request;

   -------------------------------
   -- Dispatch_Request_To_Class --
   -------------------------------

   function Dispatch_Request_To_Class (This : in out USB_Device_Stack)
                                       return Setup_Request_Answer
   is
      Req : Setup_Data renames This.Ctrl.Req;

      Res : Setup_Request_Answer;
   begin
      for Class of This.Classes loop

         exit when Class .Ptr = null;

         if Req.RType.Recipient /= Iface
           or else

         --  Check to dispatch an interface request to the right class
           (Interface_Id (Req.Index)
            in Class.First_Iface .. Class.Last_Iface)
         then

            if Req.RType.Dir = Device_To_Host then

               Res := Class.Ptr.Setup_Read_Request
                 (Req, This.Ctrl.Buf, This.Ctrl.Len);

               return Res;
            else
               return Class.Ptr.Setup_Write_Request
                 (Req, This.Ctrl.Buffer (1 .. Natural (Req.Length)));
            end if;
         end if;
      end loop;

      return Not_Supported;
   end Dispatch_Request_To_Class;

   ----------------------
   -- Dispatch_Request --
   ----------------------

   function Dispatch_Request (This : in out USB_Device_Stack)
                              return Setup_Request_Answer
   is
      Req : Setup_Data renames This.Ctrl.Req;
      Answer : Setup_Request_Answer;
   begin
      if Verbose then
         Put_Line ("Control_Dispatch_Request");
      end if;

      --  Standard handling

      if Req.RType.Typ in Class | Vendor then
         Answer := Dispatch_Request_To_Class (This);
         if Answer /= Next_Callback then
            return Answer;
         end if;
      end if;

      case Req.RType.Recipient is
         when Dev =>
            return Device_Request (This);
         when Iface =>
            --  Send interface request to the class
            return Dispatch_Request_To_Class (This);
         when Endpoint =>
            return Endpoint_Request (This);
         when Other =>
            Put_Line ("Control Other Req not impl");
      end case;

      return Not_Supported;
   end Dispatch_Request;

   -------------------------
   -- Handle_Read_Request --
   -------------------------

   procedure Handle_Read_Request (This : in out USB_Device_Stack) is
      Req : Setup_Data renames This.Ctrl.Req;
   begin
      if Dispatch_Request (This) /= Not_Supported then

         if Req.Length > 0 then

            This.Ctrl.Len :=
              Buffer_Len'Min (This.Ctrl.Len, Buffer_Len (Req.Length));

            This.Ctrl.Need_ZLP := Need_ZLP (This.Ctrl.Len,
                                            Req.Length,
                                            This.Max_Packet_Size);

            Send_Chunk (This);
         else
            if Verbose then
               Put_Line ("zero-length-packet to ack the setup req");
            end if;

            --  zero-length-packet to ack the setup req
            This.UDC.EP_Send_Packet (0, 0);
            This.Ctrl.State := Status_In;
         end if;
      else
         --  Stall transaction to indicate an error
         This.Stall_Control_EP;
      end if;
   end Handle_Read_Request;

   --------------------------
   -- Handle_Write_Request --
   --------------------------

   procedure Handle_Write_Request (This : in out USB_Device_Stack) is
   begin
      if Dispatch_Request (This) /= Not_Supported then
         --  zero-length-packet to ack the setup req
         This.UDC.EP_Send_Packet (0, 0);
         This.Ctrl.State := Status_In;
      else
         --  Stall transaction to indicate an error
         This.Stall_Control_EP;
      end if;
   end Handle_Write_Request;

   ----------------
   -- Send_Chunk --
   ----------------

   procedure Send_Chunk (This : in out USB_Device_Stack) is
   begin
      if This.Ctrl.Len > Buffer_Len (This.Max_Packet_Size) then

         USB.Utils.Copy (Src   => This.Ctrl.Buf,
                         Dst   => This.Ctrl.EP_In_Addr,
                         Count => This.Max_Packet_Size);

         This.UDC.EP_Send_Packet (0, This.Max_Packet_Size);

         This.Ctrl.Buf := This.Ctrl.Buf +
           Buffer_Len (This.Max_Packet_Size);

         This.Ctrl.Len := This.Ctrl.Len - Buffer_Len (This.Max_Packet_Size);

         This.Ctrl.State := Data_In;

      else
         --  Copy data to the UDC EP buffer
         USB.Utils.Copy (Src   => This.Ctrl.Buf,
                         Dst   => This.Ctrl.EP_In_Addr,
                         Count => Packet_Size (This.Ctrl.Len));

         This.UDC.EP_Send_Packet (0, Packet_Size (This.Ctrl.Len));

         if This.Ctrl.Need_ZLP then
            This.Ctrl.State := Data_In;
         else
            This.Ctrl.State := Last_Data_In;
         end if;

         This.Ctrl.Buf := System.Null_Address;
         This.Ctrl.Len := 0;
         This.Ctrl.Need_ZLP := False;
      end if;
   end Send_Chunk;

   -------------------
   -- Receive_Chunk --
   -------------------

   procedure Receive_Chunk (This : in out USB_Device_Stack) is
      Read_Size : constant Buffer_Len :=
        Buffer_Len'Min (Buffer_Len (This.Max_Packet_Size),
                        Buffer_Len (This.Ctrl.Req.Length) - This.Ctrl.Len);
   begin
      --  Copy data from the UDC EP buffer
      USB.Utils.Copy (Src   => This.Ctrl.EP_Out_Addr,
                      Dst   => This.Ctrl.Buf,
                      Count => Packet_Size (Read_Size));

      This.Ctrl.Len := This.Ctrl.Len + Read_Size;
      This.Ctrl.Buf := This.Ctrl.Buf + Read_Size;
   end Receive_Chunk;

end USB.Device.Control;
