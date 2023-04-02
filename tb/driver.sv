// Gets packet from generator and drive the transaction packet items into interface
// interface is connected to DUT so the items driven into interface signal will get driven into DUT

class driver;

   // Number of transactions counted
   int no_transactions;

   // virtual interface
   virtual cpu_if vif;

   // mailbox handle
   mailbox gen2driv;

   // constructor
   function new(virtual cpu_if vif, mailbox gen2driv);
      this.vif = vif;
      this.gen2driv = gen2driv;
   endfunction // new

   task reset;
      wait(!vif.reset);
      $display("[DRIVER] ------ Reset Started ------ ");
      vif.busreq    <= 1'b1;
      vif.wait_n    <= 1'b1;
      vif.interrupt <= 1'b1;
      vif.nmi       <= 1'b1;
      vif.data_i    <= 8'h0;
      vif.dur       <= 0;
      vif.op_or_data <= 1'b1;
      wait(vif.reset);
      $display("[DRIVER] ------ Reset Ended   ------ ");
   endtask // reset

   task main();
      forever begin
	 transaction trans;
	 // Load signals to CPU
	 
	 gen2driv.get(trans);

	 
	 
	 if ( trans.op_or_data == 1 ) begin
	    @(negedge vif.m1);
	 end else
	   @(negedge vif.rd);	 
	 
	 trans.display("[Driver]");
	 vif.data_i <= trans.data_i;
	 vif.wait_n <= trans.wait_n;
	 vif.interrupt <= trans.interrupt;
	 vif.nmi <= trans.nmi;
	 vif.dur <= trans.dur;
	 vif.busreq <= trans.busreq;
	 vif.op_or_data <= trans.op_or_data;
	 if ( vif.wr )
	   trans.data_o = vif.data_o;
	 trans.m1 = vif.m1;
	 trans.wr = vif.wr;
	 trans.rd = vif.rd;
	 trans.halt = vif.halt;
	 trans.busack = vif.busack;
	 trans.addr_bus = vif.addr_bus;
	 trans.mreq = vif.mreq;
	 
	 no_transactions++;
	 wait(vif.m1);

      end
   endtask // main
endclass // driver

