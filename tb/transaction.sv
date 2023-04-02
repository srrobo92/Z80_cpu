class transaction;

   bit [7:0]      data_i;
   bit 		  wait_n;
   bit 		  interrupt;
   bit 		  nmi;
   bit [7:0] 	  data_o;
   bit 		  m1;
   bit 		  mreq;
   bit 		  rd;
   bit 		  wr;
   bit 		  halt;
   bit 		  busreq;
   bit 		  busack;
   bit [15:0] 	  addr_bus;
   time 	  dur;
   bit            op_or_data;

   function void display(string name);
      $display("----------------------");
      $display("- %s ", name);
      $display("- Instruction/Data : %X", data_i);
      $display("- wait             : %b", wait_n);
      $display("- nmi              : %b", nmi);
      $display("- interrupt        : %b", interrupt);
      $display("- busreq           : %b", busreq);
      $display("- Instruct dur     : %d", dur);
      $display("- @ %t", $time);
      $display("----------------------");
   endfunction // display
      
endclass // transaction
