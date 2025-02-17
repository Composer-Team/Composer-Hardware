package beethoven.Platforms.FPGA.Xilinx.F2

import beethoven.Protocol.AXI.AXI4Compat
import chipsalliance.rocketchip.config.Parameters
import os.Path

object Shell {
  def write(toPath: Path)(implicit p: Parameters): Unit = {
    val aw_ar_sigs = Seq("addr", "id", "len", "size", "burst", "valid", "ready")
    val aw_ar_ignore = Seq("lock", "cache", "prot", "region", "qos")

    val w_sigs = Seq("data", "strb", "last", "valid", "ready")
    val r_sigs = Seq("id", "resp", "last", "valid", "data", "ready")

    val b_sigs = Seq("id", "resp", "valid", "ready")

    val ddr_connects = {
      val aw_connect = aw_ar_sigs.map(sig => f".M00_AXI_aw$sig(lcl_cl_sh_ddr.aw$sig)")
      val ar_connect = aw_ar_sigs.map(sig => f".M00_AXI_ar$sig(lcl_cl_sh_ddr.ar$sig)")
      val w_connect = w_sigs.map(sig => f".M00_AXI_w$sig(lcl_cl_sh_ddr.w$sig)")
      val r_connect = r_sigs.map(sig => f".M00_AXI_r$sig(lcl_cl_sh_ddr.r$sig)")
      val b_connect = b_sigs.map(sig => f".M00_AXI_b$sig(lcl_cl_sh_ddr.b$sig)")
      val aw_ignore = aw_ar_ignore.map(sig => f".M00_AXI_aw$sig()")
      val ar_ignore = aw_ar_ignore.map(sig => f".M00_AXI_ar$sig()")

      (aw_connect ++ ar_connect ++ w_connect ++ r_connect ++ b_connect ++ aw_ignore ++ ar_ignore).mkString(",\n    ")
    }

    val dma_connects = {
      val aw_connect = aw_ar_sigs.map(sig => f".dma_aw$sig(sh_cl_dma_pcis_aw$sig)")
      val ar_connect = aw_ar_sigs.map(sig => f".dma_ar$sig(sh_cl_dma_pcis_ar$sig)")
      val w_connect = w_sigs.map(sig => f".dma_w$sig(sh_cl_dma_pcis_w$sig)")
      val r_connect = r_sigs.map(sig => f".dma_r$sig(sh_cl_dma_pcis_r$sig)")
      val b_connect = b_sigs.map(sig => f".dma_b$sig(sh_cl_dma_pcis_b$sig)")
      val aw_ignore = aw_ar_ignore.map(sig => f".dma_aw$sig()")
      val ar_ignore = aw_ar_ignore.map(sig => f".dma_ar$sig()")
      (aw_connect ++ ar_connect ++ w_connect ++ r_connect ++ b_connect ++ aw_ignore ++ ar_ignore).mkString(",\n    ")
    }

    val aw_ar_lsigs = Seq("addr", "valid", "ready")
    val aw_ar_lsigs_ignore = Seq(
      ("id", "0"),
      ("len", "0"),
      ("size", "6'h3"),
      ("burst", "1'b1"),
      ("lock", "0"),
      ("cache", "0"),
      ("prot", "1'b1"),
      ("region", "0"),
      ("qos", "0"))

    val wl_sigs = Seq("data", "strb", "valid", "ready")
    val rl_sigs = Seq("resp", "valid", "data", "ready")
    val bl_sigs = Seq("resp", "valid", "ready")

    val ocl_connects = {
      val aw_connect = aw_ar_lsigs.map(sig => f".S00_AXI_aw$sig(sh_ocl_bus.aw$sig)")
      val aw_ignore = aw_ar_lsigs_ignore.map{case (sig, fill) => f".S00_AXI_aw$sig($fill)"}
      val ar_connect = aw_ar_lsigs.map(sig => f".S00_AXI_ar$sig(sh_ocl_bus.ar$sig)")
      val ar_ignore = aw_ar_lsigs_ignore.map{case (sig, fill) => f".S00_AXI_ar$sig($fill)"}
      val w_connect = wl_sigs.map(sig => f".S00_AXI_w$sig(sh_ocl_bus.w$sig)")
      val r_connect = rl_sigs.map(sig => f".S00_AXI_r$sig(sh_ocl_bus.r$sig)")
      val b_connect = bl_sigs.map(sig => f".S00_AXI_b$sig(sh_ocl_bus.b$sig)")
      (aw_connect ++ aw_ignore ++ ar_connect ++ ar_ignore ++ w_connect ++ r_connect ++ b_connect).mkString(",\n    ")
    }


    os.write(toPath,
      f"""
         |module cl_beethoven_top
         |#(parameter EN_DDR = 1,
         |  parameter EN_HBM = 0) (
         |`include "cl_ports.vh"
         |);
         |`include "cl_id_defines.vh"
         |`define CL_NAME cl_beethoven_top
         |//Highly recommeneded.  For lib FIFO block, uses less async reset (take advantage of
         |// FPGA flop init capability).  This will help with routing resources.
         |`define FPGA_LESS_RST
         |`define SH_SDA
         |//uncomment below to make SH and CL async
         |`define SH_CL_ASYNC
         |`define DEF_AXSIZE    3'd6   // 64 Bytes per beat
         |`define DEF_AXBURST   2'd1   // INCR burst
         |`define DEF_AXCACHE   4'd3   // Bufferable, Modifiable
         |`define DEF_AXLOCK    1'd0   // Normal access
         |`define DEF_AXPROT    3'd2 // Unprivileged access, Non-Secure Access
         |`define DEF_AXQOS     4'd0   // Regular Identifier
         |`define DEF_AXREGION  4'd0   // Single region
         |
         | interface axi_bus_t #(DATA_WIDTH=32, ADDR_WIDTH=32, ID_WIDTH=16, AXLEN_WIDTH=8);
         |    logic [ADDR_WIDTH-1 : 0]  araddr  ;
         |    logic [1 : 0]             arburst ;
         |    logic [ID_WIDTH-1 : 0]    arid    ;
         |    logic [AXLEN_WIDTH-1 : 0] arlen   ;
         |    logic [2 : 0]             arsize  ;
         |    logic                     arvalid ;
         |    logic [ADDR_WIDTH-1 : 0]  awaddr  ;
         |    logic [1 : 0]             awburst ;
         |    logic [ID_WIDTH-1 : 0]    awid    ;
         |    logic [AXLEN_WIDTH-1 : 0] awlen   ;
         |    logic [2 : 0]             awsize  ;
         |    logic                     awvalid ;
         |    logic                     rready  ;
         |    logic                     bready  ;
         |    logic [ID_WIDTH-1:0]      wid;
         |    logic [DATA_WIDTH-1 : 0]           wdata   ;
         |    logic                     wlast   ;
         |    logic [(DATA_WIDTH/8)-1 : 0]            wstrb   ;
         |    logic                     wvalid  ;
         |    logic                     arready ;
         |    logic                     awready ;
         |    logic [DATA_WIDTH-1 : 0]           rdata   ;
         |    logic [ID_WIDTH-1 : 0]    rid     ;
         |    logic                     rlast   ;
         |    logic [1 : 0]             rresp   ;
         |    logic                     rvalid  ;
         |    logic                     wready  ;
         |    logic [ID_WIDTH-1 : 0]    bid     ;
         |    logic [1 : 0]             bresp   ;
         |    logic                     bvalid  ;
         |  endinterface;
         |
         |  `include "/home/ubuntu/aws-fpga/hdk/common/lib/interfaces.sv"
         |  //----------------------------
         |  // Internal interfaces
         |  //----------------------------
         |  axi_bus_t #(.ADDR_WIDTH(64), .ID_WIDTH(16), .DATA_WIDTH(512))   lcl_cl_sh_ddr();
         |  axi_bus_t #(.ADDR_WIDTH(64), .ID_WIDTH(16), .DATA_WIDTH(512))   axi_bus_tied();
         |  axi_bus_t #(.ADDR_WIDTH(64), .ID_WIDTH(16), .DATA_WIDTH(512))   sh_cl_dma_pcis_bus();
         |  axi_bus_t #(.ADDR_WIDTH(32), .ID_WIDTH(1), .DATA_WIDTH(32))   sh_ocl_bus();
         |
         |  //----------------------------
         |  // End internal interfaces
         |  //----------------------------
         |
         |
         |///////////////////////////////////////////////////////////////////////
         |///////////////// Unused signals //////////////////////////////////////
         |///////////////////////////////////////////////////////////////////////
         |
         |  // Tie off unused signals
         |  assign cl_sh_dma_rd_full  = 'b0;
         |  assign cl_sh_dma_wr_full  = 'b0;
         |
         |  assign cl_sh_pcim_awuser  = 'b0;
         |  assign cl_sh_pcim_aruser  = 'b0;
         |
         |  assign cl_sh_status0      = 'b0;
         |  assign cl_sh_status1      = 'b0;
         |  assign cl_sh_status2      = 'b0;
         |  // NOTE:
         |  // Using rst_main_n to feed CL_SDA_AXIL_XBAR. Since this XBAR is before AWS_CLK_GEN
         |  //-----------------------------------------
         |  // Clocking Block : AWS_CLK_GEN
         |  //-----------------------------------------
         |  logic   gen_clk_hbm_ref;
         |  logic   gen_clk_main_a0;
         |  logic   gen_clk_extra_a1;
         |  logic   gen_clk_extra_a2;
         |  logic   gen_clk_extra_a3;
         |  logic   gen_clk_extra_b0;
         |  logic   gen_clk_extra_b1;
         |  logic   gen_clk_extra_c0;
         |  logic   gen_clk_extra_c1;
         |  logic   gen_clk_hbm_axi;
         |  logic   gen_rst_hbm_axi_n;
         |  logic   gen_rst_hbm_ref_n;
         |  logic   gen_rst_c1_n;
         |  logic   gen_rst_c0_n;
         |  logic   gen_rst_b1_n;
         |  logic   gen_rst_b0_n;
         |  logic   gen_rst_a3_n;
         |  logic   gen_rst_a2_n;
         |  logic   gen_rst_a1_n;
         |  logic   gen_rst_main_n;
         |
         |  aws_clk_gen
         |  #(
         |    .CLK_GRP_A_EN           (1                        ),
         |    .CLK_GRP_B_EN           (1                        ),
         |    .CLK_GRP_C_EN           (1                        ),
         |    .CLK_HBM_EN             (1                        )
         |  )
         |  AWS_CLK_GEN
         |  (
         |    .i_clk_main_a0          (clk_main_a0     ),
         |    .i_rst_main_n           (rst_main_n      ),
         |    .i_clk_hbm_ref          (clk_hbm_ref     ),
         |
         |    .s_axil_ctrl_awaddr     (),
         |    .s_axil_ctrl_awvalid    (1'b0),
         |    .s_axil_ctrl_awready    (),
         |    .s_axil_ctrl_wdata      (),
         |    .s_axil_ctrl_wstrb      (),
         |    .s_axil_ctrl_wvalid     (1'b1),
         |    .s_axil_ctrl_wready     (),
         |    .s_axil_ctrl_bresp      (),
         |    .s_axil_ctrl_bvalid     (),
         |    .s_axil_ctrl_bready     (1'b1),
         |    .s_axil_ctrl_araddr     (),
         |    .s_axil_ctrl_arvalid    (1'b0),
         |    .s_axil_ctrl_arready    (),
         |    .s_axil_ctrl_rdata      (),
         |    .s_axil_ctrl_rresp      (),
         |    .s_axil_ctrl_rvalid     (),
         |    .s_axil_ctrl_rready     (1'b1),
         |
         |    .o_clk_hbm_ref          (gen_clk_hbm_ref          ),
         |    .o_clk_main_a0          (gen_clk_main_a0          ),
         |    .o_clk_extra_a1         (gen_clk_extra_a1         ),
         |    .o_clk_extra_a2         (gen_clk_extra_a2         ),
         |    .o_clk_extra_a3         (gen_clk_extra_a3         ),
         |    .o_clk_extra_b0         (gen_clk_extra_b0         ),
         |    .o_clk_extra_b1         (gen_clk_extra_b1         ),
         |    .o_clk_extra_c0         (gen_clk_extra_c0         ),
         |    .o_clk_extra_c1         (gen_clk_extra_c1         ),
         |    .o_clk_hbm_axi          (gen_clk_hbm_axi          ),
         |    .o_cl_rst_hbm_axi_n     (gen_rst_hbm_axi_n        ),
         |    .o_cl_rst_hbm_ref_n     (gen_rst_hbm_ref_n        ),
         |    .o_cl_rst_c1_n          (gen_rst_c1_n             ),
         |    .o_cl_rst_c0_n          (gen_rst_c0_n             ),
         |    .o_cl_rst_b1_n          (gen_rst_b1_n             ),
         |    .o_cl_rst_b0_n          (gen_rst_b0_n             ),
         |    .o_cl_rst_a3_n          (gen_rst_a3_n             ),
         |    .o_cl_rst_a2_n          (gen_rst_a2_n             ),
         |    .o_cl_rst_a1_n          (gen_rst_a1_n             ),
         |    .o_cl_rst_main_n        (gen_rst_main_n           )
         |  );
         |///////////////////////////////////////////////////////////////////////
         |///////////////// OCL SLAVE module ////////////////////////////////////
         |///////////////////////////////////////////////////////////////////////
         |
         |  assign sh_ocl_bus.awvalid       = ocl_cl_awvalid;
         |  assign sh_ocl_bus.awaddr[31:0]  = ocl_cl_awaddr;
         |  assign cl_ocl_awready           = sh_ocl_bus.awready;
         |
         |  assign sh_ocl_bus.wdata[31:0]   = ocl_cl_wdata;
         |  assign sh_ocl_bus.wstrb[3:0]    = ocl_cl_wstrb;
         |  assign sh_ocl_bus.wvalid        = ocl_cl_wvalid;
         |  assign cl_ocl_wready            = sh_ocl_bus.wready;
         |
         |  assign cl_ocl_bresp             = sh_ocl_bus.bresp;
         |  assign cl_ocl_bvalid            = sh_ocl_bus.bvalid;
         |  assign sh_ocl_bus.bready        = ocl_cl_bready;
         |
         |  assign sh_ocl_bus.araddr[31:0]  = ocl_cl_araddr;
         |  assign sh_ocl_bus.arvalid       = ocl_cl_arvalid;
         |  assign cl_ocl_arready           = sh_ocl_bus.arready;
         |
         |  assign cl_ocl_rresp             = sh_ocl_bus.rresp;
         |  assign cl_ocl_rdata             = sh_ocl_bus.rdata[31:0];
         |  assign cl_ocl_rvalid            = sh_ocl_bus.rvalid;
         |  assign sh_ocl_bus.rready        = ocl_cl_rready;
         |
         |  logic ocl_sync_rst_n;
         |
         |  xpm_cdc_async_rst CDC_ASYNC_RST_N_OCL
         |  (
         |    .src_arst               (gen_rst_main_n           ),
         |    .dest_clk               (gen_clk_main_a0          ),
         |    .dest_arst              (ocl_sync_rst_n           )
         |  );
         |
         |//////////////////// DDR module ///////////////////////////////////////
         |  logic         ddr_ready;
         |
         |  //-----------------------------------------
         |  // DDR controller instantiation
         |  //-----------------------------------------
         |  localparam    NUM_CFG_STGS_CL_DDR_ATG = 8;
         |
         |  logic  [7:0]  sh_ddr_stat_addr_q;
         |  logic         sh_ddr_stat_wr_q;
         |  logic         sh_ddr_stat_rd_q;
         |  logic [31:0]  sh_ddr_stat_wdata_q;
         |  logic         ddr_sh_stat_ack_q;
         |  logic [31:0]  ddr_sh_stat_rdata_q;
         |  logic  [7:0]  ddr_sh_stat_int_q;
         |
         |  // !!NOTE!!: Tie SH_DDR resets to rst_main_n ONLY!!
         |  logic         ddr_sync_rst_n;
         |
         |  xpm_cdc_async_rst CDC_ASYNC_RST_N_DDR
         |  (
         |    .src_arst               (rst_main_n               ),
         |    .dest_clk               (gen_clk_main_a0          ),
         |    .dest_arst              (ddr_sync_rst_n           )
         |  );
         |
         |  lib_pipe
         |  #(
         |    .WIDTH                  (1+1+8+32                 ),
         |    .STAGES                 (NUM_CFG_STGS_CL_DDR_ATG  )
         |  )
         |  PIPE_DDR_STAT0
         |  (
         |    .clk                    (gen_clk_main_a0          ),
         |    .rst_n                  (ddr_sync_rst_n           ),
         |    .in_bus                 ({sh_cl_ddr_stat_wr,
         |                              sh_cl_ddr_stat_rd,
         |                              sh_cl_ddr_stat_addr,
         |                              sh_cl_ddr_stat_wdata}   ),
         |    .out_bus                ({sh_ddr_stat_wr_q,
         |                              sh_ddr_stat_rd_q,
         |                              sh_ddr_stat_addr_q,
         |                              sh_ddr_stat_wdata_q}    )
         |  );
         |
         |  lib_pipe
         |  #(
         |    .WIDTH                  (1+8+32                   ),
         |    .STAGES                 (NUM_CFG_STGS_CL_DDR_ATG  )
         |  )
         |  PIPE_DDR_STAT_ACK0
         |  (
         |    .clk                    (gen_clk_main_a0          ),
         |    .rst_n                  (ddr_sync_rst_n           ),
         |    .in_bus                 ({ddr_sh_stat_ack_q,
         |                              ddr_sh_stat_int_q,
         |                              ddr_sh_stat_rdata_q}    ),
         |    .out_bus                ({cl_sh_ddr_stat_ack,
         |                              cl_sh_ddr_stat_int,
         |                              cl_sh_ddr_stat_rdata}   )
         |  );
         |
         |  // `define USE_AP_64GB_DDR_DIMM  // This is 64GB DDR controller with user-controlled Auto Precharge
         |
         |  sh_ddr
         |  #(
         |    .DDR_PRESENT            (EN_DDR                   )
         |  )
         |  SH_DDR
         |  (
         |    .clk                    (gen_clk_main_a0          ),
         |    .rst_n                  (ddr_sync_rst_n           ),
         |    .stat_clk               (gen_clk_main_a0          ),
         |    .stat_rst_n             (ddr_sync_rst_n           ),
         |
         |    .CLK_DIMM_DP            (CLK_DIMM_DP              ),
         |    .CLK_DIMM_DN            (CLK_DIMM_DN              ),
         |    .M_ACT_N                (M_ACT_N                  ),
         |    .M_MA                   (M_MA                     ),
         |    .M_BA                   (M_BA                     ),
         |    .M_BG                   (M_BG                     ),
         |    .M_CKE                  (M_CKE                    ),
         |    .M_ODT                  (M_ODT                    ),
         |    .M_CS_N                 (M_CS_N                   ),
         |    .M_CLK_DN               (M_CLK_DN                 ),
         |    .M_CLK_DP               (M_CLK_DP                 ),
         |    .M_PAR                  (M_PAR                    ),
         |    .M_DQ                   (M_DQ                     ),
         |    .M_ECC                  (M_ECC                    ),
         |    .M_DQS_DP               (M_DQS_DP                 ),
         |    .M_DQS_DN               (M_DQS_DN                 ),
         |    .cl_RST_DIMM_N          (RST_DIMM_N               ),
         |
         |    .cl_sh_ddr_axi_awid     (lcl_cl_sh_ddr.awid      ),
         |    .cl_sh_ddr_axi_awaddr   (lcl_cl_sh_ddr.awaddr    ),
         |    .cl_sh_ddr_axi_awlen    (lcl_cl_sh_ddr.awlen     ),
         |    .cl_sh_ddr_axi_awsize   (lcl_cl_sh_ddr.awsize    ),
         |    .cl_sh_ddr_axi_awvalid  (lcl_cl_sh_ddr.awvalid   ),
         |    .cl_sh_ddr_axi_awburst  (lcl_cl_sh_ddr.awburst   ),
         |    .cl_sh_ddr_axi_awuser   (1'd0                     ),
         |    .cl_sh_ddr_axi_awready  (lcl_cl_sh_ddr.awready   ),
         |    .cl_sh_ddr_axi_wdata    (lcl_cl_sh_ddr.wdata     ),
         |    .cl_sh_ddr_axi_wstrb    (lcl_cl_sh_ddr.wstrb     ),
         |    .cl_sh_ddr_axi_wlast    (lcl_cl_sh_ddr.wlast     ),
         |    .cl_sh_ddr_axi_wvalid   (lcl_cl_sh_ddr.wvalid    ),
         |    .cl_sh_ddr_axi_wready   (lcl_cl_sh_ddr.wready    ),
         |    .cl_sh_ddr_axi_bid      (lcl_cl_sh_ddr.bid       ),
         |    .cl_sh_ddr_axi_bresp    (lcl_cl_sh_ddr.bresp     ),
         |    .cl_sh_ddr_axi_bvalid   (lcl_cl_sh_ddr.bvalid    ),
         |    .cl_sh_ddr_axi_bready   (lcl_cl_sh_ddr.bready    ),
         |    .cl_sh_ddr_axi_arid     (lcl_cl_sh_ddr.arid      ),
         |    .cl_sh_ddr_axi_araddr   (lcl_cl_sh_ddr.araddr    ),
         |    .cl_sh_ddr_axi_arlen    (lcl_cl_sh_ddr.arlen     ),
         |    .cl_sh_ddr_axi_arsize   (lcl_cl_sh_ddr.arsize    ),
         |    .cl_sh_ddr_axi_arvalid  (lcl_cl_sh_ddr.arvalid   ),
         |    .cl_sh_ddr_axi_arburst  (lcl_cl_sh_ddr.arburst   ),
         |    .cl_sh_ddr_axi_aruser   (1'd0                     ),
         |    .cl_sh_ddr_axi_arready  (lcl_cl_sh_ddr.arready   ),
         |    .cl_sh_ddr_axi_rid      (lcl_cl_sh_ddr.rid       ),
         |    .cl_sh_ddr_axi_rdata    (lcl_cl_sh_ddr.rdata     ),
         |    .cl_sh_ddr_axi_rresp    (lcl_cl_sh_ddr.rresp     ),
         |    .cl_sh_ddr_axi_rlast    (lcl_cl_sh_ddr.rlast     ),
         |    .cl_sh_ddr_axi_rvalid   (lcl_cl_sh_ddr.rvalid    ),
         |    .cl_sh_ddr_axi_rready   (lcl_cl_sh_ddr.rready    ),
         |
         |    .sh_ddr_stat_bus_addr   (sh_ddr_stat_addr_q       ),
         |    .sh_ddr_stat_bus_wdata  (sh_ddr_stat_wdata_q      ),
         |    .sh_ddr_stat_bus_wr     (sh_ddr_stat_wr_q         ),
         |    .sh_ddr_stat_bus_rd     (sh_ddr_stat_rd_q         ),
         |    .sh_ddr_stat_bus_ack    (ddr_sh_stat_ack_q        ),
         |    .sh_ddr_stat_bus_rdata  (ddr_sh_stat_rdata_q      ),
         |
         |    .ddr_sh_stat_int        (ddr_sh_stat_int_q        ),
         |    .sh_cl_ddr_is_ready     (ddr_ready                )
         |  );
         |
         |///////////////// DMA PCIS SLAVE module ///////////////////////////////
         |
         |  assign sh_cl_dma_pcis_bus.awaddr  = sh_cl_dma_pcis_awaddr;
         |  assign sh_cl_dma_pcis_bus.awid    = sh_cl_dma_pcis_awid;
         |  assign sh_cl_dma_pcis_bus.awlen   = sh_cl_dma_pcis_awlen;
         |  assign sh_cl_dma_pcis_bus.awsize  = sh_cl_dma_pcis_awsize;
         |  assign sh_cl_dma_pcis_bus.awvalid = sh_cl_dma_pcis_awvalid;
         |  assign cl_sh_dma_pcis_awready     = sh_cl_dma_pcis_bus.awready;
         |
         |  assign sh_cl_dma_pcis_bus.wid     = sh_cl_dma_pcis_wid;
         |  assign sh_cl_dma_pcis_bus.wdata   = sh_cl_dma_pcis_wdata;
         |  assign sh_cl_dma_pcis_bus.wstrb   = sh_cl_dma_pcis_wstrb;
         |  assign sh_cl_dma_pcis_bus.wlast   = sh_cl_dma_pcis_wlast;
         |  assign sh_cl_dma_pcis_bus.wvalid  = sh_cl_dma_pcis_wvalid;
         |  assign cl_sh_dma_pcis_wready      = sh_cl_dma_pcis_bus.wready;
         |
         |  assign cl_sh_dma_pcis_bresp       = sh_cl_dma_pcis_bus.bresp;
         |  assign cl_sh_dma_pcis_bid         = sh_cl_dma_pcis_bus.bid;
         |  assign cl_sh_dma_pcis_bvalid      = sh_cl_dma_pcis_bus.bvalid;
         |  assign sh_cl_dma_pcis_bus.bready  = sh_cl_dma_pcis_bready;
         |
         |  assign sh_cl_dma_pcis_bus.araddr  = sh_cl_dma_pcis_araddr;
         |  assign sh_cl_dma_pcis_bus.arid    = sh_cl_dma_pcis_arid;
         |  assign sh_cl_dma_pcis_bus.arlen   = sh_cl_dma_pcis_arlen;
         |  assign sh_cl_dma_pcis_bus.arsize  = sh_cl_dma_pcis_arsize;
         |  assign sh_cl_dma_pcis_bus.arvalid = sh_cl_dma_pcis_arvalid;
         |  assign cl_sh_dma_pcis_arready     = sh_cl_dma_pcis_bus.arready;
         |
         |  assign cl_sh_dma_pcis_rid         = sh_cl_dma_pcis_bus.rid;
         |  assign cl_sh_dma_pcis_rlast       = sh_cl_dma_pcis_bus.rlast;
         |  assign cl_sh_dma_pcis_rresp       = sh_cl_dma_pcis_bus.rresp;
         |  assign cl_sh_dma_pcis_rdata       = sh_cl_dma_pcis_bus.rdata;
         |  assign cl_sh_dma_pcis_rvalid      = sh_cl_dma_pcis_bus.rvalid;
         |  assign sh_cl_dma_pcis_bus.rready  = sh_cl_dma_pcis_rready;
         |
         |//////////////////// HBM module ///////////////////////////////////////
         |
         |//  logic hbm_sync_rst_n;
         |//
         |//  xpm_cdc_async_rst CDC_ASYNC_RST_N_HBM
         |//  (
         |//    .src_arst               (gen_rst_main_n           ),
         |//    .dest_clk               (gen_clk_main_a0          ),
         |//    .dest_arst              (hbm_sync_rst_n           )
         |//  );
         |//
         |//  logic hbm_axi_sync_rst_n;
         |//
         |//  xpm_cdc_async_rst CDC_ASYNC_RST_N_HBM_AXI
         |//  (
         |//    .src_arst               (gen_rst_hbm_axi_n        ),
         |//    .dest_clk               (gen_clk_hbm_axi          ),
         |//    .dest_arst              (hbm_axi_sync_rst_n       )
         |//  );
         |//
         |//  cl_mem_hbm_axi4
         |//  #(
         |//    .HBM_PRESENT            (EN_HBM                   )
         |//  )
         |//  CL_HBM
         |//  (
         |//    .clk_hbm_ref            (gen_clk_hbm_ref          ),
         |//    .clk_hbm_axi            (gen_clk_hbm_axi          ),
         |//    .hbm_axi_rst_n          (hbm_axi_sync_rst_n       ),
         |//    .clk                    (gen_clk_main_a0          ),
         |//    .rst_n                  (hbm_sync_rst_n           ),
         |//    .hbm_axi4_bus           (lcl_cl_sh_ddrb           ),
         |//    .hbm_stat_bus           (hbm_stat_cfg_bus         ),
         |//    .hbm_kern_cfg_bus       (hbm_kern_cfg_bus         ),
         |//    .i_hbm_apb_preset_n_1   (hbm_apb_preset_n_1       ),
         |//    .o_hbm_apb_paddr_1      (hbm_apb_paddr_1          ),
         |//    .o_hbm_apb_pprot_1      (hbm_apb_pprot_1          ),
         |//    .o_hbm_apb_psel_1       (hbm_apb_psel_1           ),
         |//    .o_hbm_apb_penable_1    (hbm_apb_penable_1        ),
         |//    .o_hbm_apb_pwrite_1     (hbm_apb_pwrite_1         ),
         |//    .o_hbm_apb_pwdata_1     (hbm_apb_pwdata_1         ),
         |//    .o_hbm_apb_pstrb_1      (hbm_apb_pstrb_1          ),
         |//    .o_hbm_apb_pready_1     (hbm_apb_pready_1         ),
         |//    .o_hbm_apb_prdata_1     (hbm_apb_prdata_1         ),
         |//    .o_hbm_apb_pslverr_1    (hbm_apb_pslverr_1        ),
         |//    .i_hbm_apb_preset_n_0   (hbm_apb_preset_n_0       ),
         |//    .o_hbm_apb_paddr_0      (hbm_apb_paddr_0          ),
         |//    .o_hbm_apb_pprot_0      (hbm_apb_pprot_0          ),
         |//    .o_hbm_apb_psel_0       (hbm_apb_psel_0           ),
         |//    .o_hbm_apb_penable_0    (hbm_apb_penable_0        ),
         |//    .o_hbm_apb_pwrite_0     (hbm_apb_pwrite_0         ),
         |//    .o_hbm_apb_pwdata_0     (hbm_apb_pwdata_0         ),
         |//    .o_hbm_apb_pstrb_0      (hbm_apb_pstrb_0          ),
         |//    .o_hbm_apb_pready_0     (hbm_apb_pready_0         ),
         |//    .o_hbm_apb_prdata_0     (hbm_apb_prdata_0         ),
         |//    .o_hbm_apb_pslverr_0    (hbm_apb_pslverr_0        ),
         |//    .o_cl_sh_hbm_stat_int   (                         ),
         |//    .o_hbm_ready            (hbm_ready                )
         |//  );
         |
         |///////////////// PCIM MSTR module ////////////////////////////////////
         |`include "unused_pcim_template.inc"
         |
         |//////////////////// SDA module ///////////////////////////////////////
         |  `include "unused_cl_sda_template.inc"
         |
         |//////////////////// IRQ module ///////////////////////////////////////
         |`include "unused_apppf_irq_template.inc"
         |
         |//////////////////// FLR module ///////////////////////////////////////
         |`include "unused_flr_template.inc"
         |////////////////// Frequency module ///////////////////////////////////
         |
         |
         |///////////////////////////////////////////////////////////////////////
         |//////////////////// Debug module /////////////////////////////////////
         |///////////////////////////////////////////////////////////////////////
         |
         |//`ifndef DISABLE_VJTAG_DEBUG
         |//
         |//  cl_ila
         |//  #(
         |//    .DDR_A_PRESENT          (`DDR_A_PRESENT           )
         |//  )
         |//  CL_ILA
         |//  (
         |//    .aclk                   (gen_clk_main_a0          ),
         |//    .drck                   (drck                     ),
         |//    .shift                  (shift                    ),
         |//    .tdi                    (tdi                      ),
         |//    .update                 (update                   ),
         |//    .sel                    (sel                      ),
         |//    .tdo                    (tdo                      ),
         |//    .tms                    (tms                      ),
         |//    .tck                    (tck                      ),
         |//    .runtest                (runtest                  ),
         |//    .reset                  (reset                    ),
         |//    .capture                (capture                  ),
         |//    .bscanid_en             (bscanid_en               ),
         |//    .sh_cl_dma_pcis_q       (sh_cl_dma_pcis_q         ),
         |// `ifndef DDR_A_ABSENT
         |//    .lcl_cl_sh_ddr         (lcl_cl_sh_ddr           )
         |// `else
         |//    .lcl_cl_sh_ddr         (axi_bus_tied             )
         |// `endif
         |//  );
         |//
         |//  cl_vio CL_VIO
         |//  (
         |//    .clk_extra_a1           (clk_extra_a1             )
         |//  );
         |//
         |//`endif //  `ifndef DISABLE_VJTAG_DEBUG
         |
         |  //-----------------------------------------
         |  // Virtual JATG ILA Debug core example
         |  //-----------------------------------------
         |
         |  // tie off for ILA port when probing block not present
         |  assign axi_bus_tied.awvalid = 'b0;
         |  assign axi_bus_tied.awaddr  = 'b0;
         |  assign axi_bus_tied.awready = 'b0;
         |  assign axi_bus_tied.wvalid  = 'b0;
         |  assign axi_bus_tied.wstrb   = 'b0;
         |  assign axi_bus_tied.wlast   = 'b0;
         |  assign axi_bus_tied.wready  = 'b0;
         |  assign axi_bus_tied.wdata   = 'b0;
         |  assign axi_bus_tied.arready = 'b0;
         |  assign axi_bus_tied.rdata   = 'b0;
         |  assign axi_bus_tied.araddr  = 'b0;
         |  assign axi_bus_tied.arvalid = 'b0;
         |  assign axi_bus_tied.awid    = 'b0;
         |  assign axi_bus_tied.arid    = 'b0;
         |  assign axi_bus_tied.awlen   = 'b0;
         |  assign axi_bus_tied.rlast   = 'b0;
         |  assign axi_bus_tied.rresp   = 'b0;
         |  assign axi_bus_tied.rid     = 'b0;
         |  assign axi_bus_tied.rvalid  = 'b0;
         |  assign axi_bus_tied.arlen   = 'b0;
         |  assign axi_bus_tied.bresp   = 'b0;
         |  assign axi_bus_tied.rready  = 'b0;
         |  assign axi_bus_tied.bvalid  = 'b0;
         |  assign axi_bus_tied.bid     = 'b0;
         |  assign axi_bus_tied.bready  = 'b0;
         |
         |  BeethovenTop (
         |    .clock(gen_clk_main_a0),
         |    .RESETn(ddr_sync_rst_n),
         |    $ddr_connects,
         |    $dma_connects,
         |    $ocl_connects);
         |
         |
         |
         |endmodule
         |""".stripMargin)
  }
}
