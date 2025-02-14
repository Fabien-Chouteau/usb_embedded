package body Tests is

   ---------------
   -- Get_Suite --
   ---------------

   function Get_Suite return AUnit.Test_Suites.Access_Test_Suite
   is (Suite'Access);

end Tests;
