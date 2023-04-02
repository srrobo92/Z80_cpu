vcom -2008 ../src/alu.vhd
vcom -2008 ../src/cpu_control.vhd
vcom -2008 ../src/z80_top.vhd

vlog -sv ../tb/environment.sv
vlog -sv +incdir+../tb ../tb/cpu_tb_top.sv
