/*=====================================================================
 Environment
 =====================================================================*/
`include "transaction.sv"
`include "generator.sv"
`include "driver.sv"
`include "monitor.sv"
`include "scoreboard.sv"

class environment;

   // Generator and driver instance
   generator  gen;
   driver     driv;
   monitor    mon;
   scoreboard scb;

   // mailbox handles
   mailbox gen2driv;
   mailbox mon2scb;

   // virtual interface
   virtual cpu_if vif;

   function new(virtual cpu_if vif, string test_file);
      // get the interface from test
      this.vif = vif;

      // creating the mailbox (same handle will be shared across gen and driv)
      gen2driv = new(1);
      mon2scb  = new();

      // create generator and driver
      gen  = new(gen2driv, test_file);
      driv = new(vif, gen2driv);
      mon  = new(vif, mon2scb);
      scb  = new(mon2scb);
      
   endfunction // new

   task pre_test();
      driv.reset();
   endtask // pre_test

   task test();
      fork
	 gen.main();
	 driv.main();
	 mon.main();
	 scb.main();
      join_any
   endtask // test

   task post_test();
      wait(gen.ended.triggered);
      wait(gen.no_transactions == driv.no_transactions);
   endtask // post_test

   task run();
      pre_test();
      test();
      post_test();
      $display("----------------------------------------------");
      if (scb.errors == 0)
	$display("                   PASSED ");
      else
	$display("                   FAILD : %d ", scb.errors);
      $display("----------------------------------------------");
      $finish;
   endtask // run
   
endclass // env
