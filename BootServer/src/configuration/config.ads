with Ada.Strings.Unbounded;
with Boot_Images;

package Config is
   use Ada.Strings.Unbounded;

   type Server_Config is record
      BOOTP_Port           : Integer := 67
      TFTP_Port            : Integer := 69;
      Listen_Address       : Unbounded_String := To_Unbounded_String("0.0.0.0");
      Log_Devel            : Integer := 1;
      Log_File             : Unbounded_String := To_Unbounded_String("logs/boot_server.log");
      Client_Mappings      : Unbounded_String := To_Unbounded_String("config/client_mappings.conf");
      Default_Boot_Image   : Unbounded_String := To_Unbounded_String("images/default.bin");
   end record;

   Current_Config : Server_Config;

   procedure Load(Filename : in String);
   procedure Save(Filename : in String);

   function Get_Client_IP(MAC_Address : String) return String;
   function Get_Client_Boot_Image(MAC_Address : String) return String;
end Config;
