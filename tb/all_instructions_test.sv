// Test all instructions and inputs exclude interrupts
`include "environment.sv"

class test;

   environment env;
   transaction trans;

   function new(virtual cpu_if vif);

      env = new(vif, "z80_test_set.txt");

   endfunction // new

   virtual task run();
      fork
	 env.run();
      join_none
   endtask // run
endclass // test
