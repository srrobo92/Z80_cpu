-------------------------------------------------------------------------------
-- Title      : Z80 CPU
-- Project    : 
-------------------------------------------------------------------------------
-- File       : z80_top.vhd
-- Author     : Steve Robichaud
-- Company    : Self
-- Created    : 2022-08-14
-- Last update: 2023-04-02
-- Platform   : 
-- Standard   : VHDL'08
-------------------------------------------------------------------------------
-- Description: Attempt at repoducing Z80 CPU
-------------------------------------------------------------------------------
-- Copyright (c) 2022 
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  Description
-- 2022-08-14  1.0      srrob   Created
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;


entity z80_top is

  port (
    CLK         : in    std_logic;
    ---------------------------------------------------------------------------
    -- System Control
    ---------------------------------------------------------------------------
    M1          : out   std_logic;      -- Active Low
    MREQ        : out   std_logic;      -- Active Low
    RD          : out   std_logic;      -- Active Low
    WR          : out   std_logic;      -- Active Low
    RFSH        : out   std_logic;      -- Active Low
    ---------------------------------------------------------------------------
    -- CPU Control
    ---------------------------------------------------------------------------
    WAIT_N      : in    std_logic;      -- Active Low
    HALT        : out   std_logic;      -- Active Low
    IME         : out   std_logic;      -- Send to RAM
    INT_EN      : in    std_logic_vector(7 downto 0);
    INT_FL      : in    std_logic_vector(7 downto 0);
    NMI         : in    std_logic;      -- Active Low (Not using)
    RESET       : in    std_logic;      -- Active Low
    ---------------------------------------------------------------------------
    -- CPU BUS Control (Not using)
    ---------------------------------------------------------------------------
    BUSREQ      : in    std_logic;      -- Active Low
    BUSACK      : out   std_logic;      -- Active Low
    ---------------------------------------------------------------------------
    -- BUSES
    ---------------------------------------------------------------------------
    ADDRESS_BUS : out   std_logic_vector(15 downto 0);
    DATA_BUS_I  : in    std_logic_vector(7 downto 0);
    DATA_BUS_O  : out   std_logic_vector(7 downto 0));

end entity z80_top;

architecture rtl of z80_top is

  -----------------------------------------------------------------------------
  -- Signals
  -----------------------------------------------------------------------------
  -- Registers
  -- Accumulator
  signal accumulator_reg              : std_logic_vector(7 downto 0);
  -- Flags
  -- 7 : Zero Flag
  -- 6 : Subtract Flag
  -- 5 : Half Carry Flag
  -- 4 : Carry Flag
  -- 3 : Not Used
  -- 2 : Not Used
  -- 1 : Not Used
  -- 0 : Not Used 
  signal flags_reg                    : std_logic_vector(7 downto 0);
  -- General Purpose Registers
  -- (Combined 8 bit regsisters using aliases for the actual 8 bit versions)
  signal bc_reg                       : std_logic_vector(15 downto 0);
  signal de_reg                       : std_logic_vector(15 downto 0);
  signal hl_reg                       : std_logic_vector(15 downto 0);
  -- Program counter
  signal program_counter              : std_logic_vector(15 downto 0);
  -- Interrupt signals
  signal interrupt_master_enable      : std_logic;
  signal interrupt_data               : std_logic_vector(4 downto 0);
  signal interrupt_local              : std_logic;
  signal interrupt_handled            : std_logic;
  -----------------------------------------------------------------------------
  -- CPU CONTROL SIGNALS
  signal rd_local       : std_logic;
  signal wr_local       : std_logic;
  signal stack_data     : std_logic_vector(15 downto 0);
  signal update_reg     : std_logic_vector(2 downto 0);
  signal increment_reg  : std_logic_vector(2 downto 0);
  signal decrement_reg  : std_logic_vector(2 downto 0);
  signal inc_8b_reg     : std_logic_vector(3 downto 0);
  signal dec_8b_reg     : std_logic_vector(3 downto 0);
  signal bit_reg        : std_logic_vector(2 downto 0);
  signal bit_cmd        : std_logic_vector(4 downto 0);
  signal use_cmd        : std_logic;
  signal db_io          : std_logic;
  signal inc_db         : std_logic;
  signal dec_db         : std_logic;
  signal add_reg        : std_logic_vector(3 downto 0);
  signal add_hl         : std_logic_vector(2 downto 0);
  signal hl_pl_sp       : std_logic;
  signal add_db         : std_logic;
  signal use_cf         : std_logic;
  signal sub_reg        : std_logic_vector(3 downto 0);
  signal sub_db         : std_logic;
  signal and_reg        : std_logic_vector(3 downto 0);
  signal and_db         : std_logic;
  signal or_reg         : std_logic_vector(3 downto 0);
  signal or_db          : std_logic;
  signal xor_reg        : std_logic_vector(3 downto 0);
  signal xor_db         : std_logic;
  signal load_regs      : std_logic_vector(6 downto 0);
  signal db_to_regs     : std_logic_vector(3 downto 0);
  signal reg_to_db      : std_logic_vector(3 downto 0);
  signal reg_16_to_db   : std_logic_vector(2 downto 0);
  signal push_reg       : std_logic_vector(2 downto 0);
  signal pop_reg        : std_logic_vector(2 downto 0);
  signal pc_to_sp       : std_logic;
  signal pc_to_sp_p3    : std_logic;
  signal sp_to_pc       : std_logic;
  signal sp_add         : std_logic;
  signal flag_only      : std_logic;
  signal preform_daa    : std_logic;
  signal not_acc        : std_logic;
  signal not_cf         : std_logic;
  signal set_cf         : std_logic;
  signal enable_inter   : std_logic;
  signal disable_inter  : std_logic;
  signal rotate_l_a     : std_logic;
  signal thru_cf        : std_logic;
  signal rotate_r_a     : std_logic;
  signal hl_ld_sp       : std_logic;
  signal exchange_de_hl : std_logic;
  signal exchange_af    : std_logic;
  signal exchange_prime : std_logic;
  -----------------------------------------------------------------------------
  -----------------------------------------------------------------------------
  -- Aliases
  alias b_reg : std_logic_vector(7 downto 0) is bc_reg(15 downto 8);
  alias c_reg : std_logic_vector(7 downto 0) is bc_reg(7 downto 0);
  alias d_reg : std_logic_vector(7 downto 0) is de_reg(15 downto 8);
  alias e_reg : std_logic_vector(7 downto 0) is de_reg(7 downto 0);
  alias h_reg : std_logic_vector(7 downto 0) is hl_reg(15 downto 8);
  alias l_reg : std_logic_vector(7 downto 0) is hl_reg(7 downto 0);  
  -----------------------------------------------------------------------------

begin

  RD <= rd_local;
  WR <= wr_local;


  -----------------------------------------------------------------------------
  -- Handle interrupts
  -----------------------------------------------------------------------------
  u_int : process(CLK, RESET)
  begin
    if RESET = '0' then
      IME                     <= '0';
      interrupt_local         <= '0';
      interrupt_data          <= (others => '0');
      interrupt_master_enable <= '0';
    elsif rising_edge(CLK) then
      IME <= interrupt_master_enable;
      if enable_inter = '1' then
        interrupt_master_enable <= '1';
      elsif disable_inter = '1' then
        interrupt_master_enable <= '0';
      end if;
      if INT_FL /= x"00" then
        interrupt_data <= INT_FL(4 downto 0) and INT_EN(4 downto 0);
      end if;
      if interrupt_data /= '0' & x"0" then
        interrupt_local <= '1';
      else
        interrupt_local <= '0';
      end if;
    end if;
  end process;
  
  -----------------------------------------------------------------------------
  -- CPU Control Module
  --
  -- Decodes and sequences intructions, PC, WR, RD, main control logic
  -----------------------------------------------------------------------------
  u_control: entity work.cpu_control
    port map (
      clk            => CLK,
      reset          => RESET,
      data_bus       => DATA_BUS_I,
      address_bus    => ADDRESS_BUS,
      halt           => HALT,
      wait_n         => WAIT_N,
      int            => interrupt_local,
      ime            => interrupt_master_enable,
      int_data       => interrupt_data,
      int_handled    => interrupt_handled,
      nmi            => NMI,
      bus_req        => BUSREQ,
      bus_ack        => BUSACK,
      mreq           => MREQ,
      rd             => rd_local,
      wr             => wr_local,
      m1_o           => M1,
      rfsh           => RFSH,
      -------------------------------------------------------------------------
      flags_reg      => flags_reg,
      register_bc    => bc_reg,
      register_de    => de_reg,
      register_hl    => hl_reg,
      register_c     => c_reg,
      stack_data     => stack_data,
      pc_out         => program_counter,
      update_reg     => update_reg,
      increment_reg  => increment_reg,
      decrement_reg  => decrement_reg,
      inc_8b_reg     => inc_8b_reg,
      dec_8b_reg     => dec_8b_reg,
      bit_reg        => bit_reg,
      bit_cmd        => bit_cmd,
      use_cmd        => use_cmd,
      inc_db         => inc_db,
      db_io          => db_io,
      dec_db         => dec_db,
      add_reg        => add_reg,
      add_hl         => add_hl,
      hl_pl_sp       => hl_pl_sp,
      add_db         => add_db,
      use_cf         => use_cf,
      sub_reg        => sub_reg,
      sub_db         => sub_db,
      and_reg        => and_reg,
      and_db         => and_db,
      or_reg         => or_reg,
      or_db          => or_db,
      xor_reg        => xor_reg,
      xor_db         => xor_db,
      load_regs      => load_regs,
      db_to_regs     => db_to_regs,
      reg_to_db      => reg_to_db,
      reg_16_to_db   => reg_16_to_db,
      push_reg       => push_reg,
      pop_reg        => pop_reg,
      pc_to_sp       => pc_to_sp,
      pc_to_sp_p3    => pc_to_sp_p3,
      sp_to_pc       => sp_to_pc,
      sp_add         => sp_add,
      flag_only      => flag_only,
      preform_daa    => preform_daa,
      not_acc        => not_acc,
      not_cf         => not_cf,
      set_cf         => set_cf,
      enable_inter   => enable_inter,
      disable_inter  => disable_inter,
      rotate_l_a     => rotate_l_a,
      thru_cf        => thru_cf,
      rotate_r_a     => rotate_r_a,
      hl_ld_sp       => hl_ld_sp,
      exchange_de_hl => exchange_de_hl,
      exchange_af    => exchange_af,
      exchange_prime => exchange_prime);

  -----------------------------------------------------------------------------
  -- Arithmetic Logic Unit
  -----------------------------------------------------------------------------
  u_alu : entity work.alu
    port map (
      clk             => CLK,
      reset           => RESET,
      wr              => wr_local,
      rd              => rd_local,
      data_bus_i      => DATA_BUS_I,
      data_bus_o      => DATA_BUS_O,
      -------------------------------------------------------------------------
      flags_reg       => flags_reg,
      b_reg           => b_reg,
      c_reg           => c_reg,
      d_reg           => d_reg,
      e_reg           => e_reg,
      accumulator_reg => accumulator_reg,
      h_reg           => h_reg,
      l_reg           => l_reg,
      stack_data      => stack_data,
      pc_in           => program_counter,
      update_reg      => update_reg,
      increment_reg   => increment_reg,
      decrement_reg   => decrement_reg,
      inc_8b_reg      => inc_8b_reg,
      dec_8b_reg      => dec_8b_reg,
      bit_reg         => bit_reg,
      bit_cmd         => bit_cmd,
      use_cmd         => use_cmd,
      inc_db          => inc_db,
      dec_db          => dec_db,
      db_io           => db_io,
      add_reg         => add_reg,
      add_hl          => add_hl,
      hl_pl_sp        => hl_pl_sp,
      add_db          => add_db,
      use_cf          => use_cf,
      sub_reg         => sub_reg,
      sub_db          => sub_db,
      and_reg         => and_reg,
      and_db          => and_db,
      or_reg          => or_reg,
      or_db           => or_db,
      xor_reg         => xor_reg,
      xor_db          => xor_db,
      load_regs       => load_regs,
      db_to_regs      => db_to_regs,
      reg_to_db       => reg_to_db,
      reg_16_to_db    => reg_16_to_db,
      push_reg        => push_reg,
      pop_reg         => pop_reg,
      pc_to_sp        => pc_to_sp,
      pc_to_sp_p3     => pc_to_sp_p3,
      sp_to_pc        => sp_to_pc,
      sp_add          => sp_add,
      flag_only       => flag_only,
      preform_daa     => preform_daa,
      not_acc         => not_acc,
      not_cf          => not_cf,
      set_cf          => set_cf,
      rotate_l_a      => rotate_l_a,
      thru_cf         => thru_cf,
      rotate_r_a      => rotate_r_a,
      hl_ld_sp        => hl_ld_sp);
  

end architecture rtl;
