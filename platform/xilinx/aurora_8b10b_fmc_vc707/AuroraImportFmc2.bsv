package AuroraImportFmc2;

import FIFO::*;
import FIFOF::*;
import Clocks :: *;
import DefaultValue :: *;
import Xilinx :: *;
import XilinxCells :: *;
import ConnectalXilinxCells::*;

import AuroraCommon::*;

import AuroraGearbox::*;

(* synthesize *)
module mkAuroraIntra2#(Clock gt_clk_p, Clock gt_clk_n, Clock clk110, Reset rst110) (AuroraIfc);

	Clock cur_clk <- exposeCurrentClock; // assuming 200MHz clock
	Reset cur_rst <- exposeCurrentReset;
	

`ifndef BSIM
	// ClockDividerIfc auroraIntraClockDiv4 <- mkDCMClockDivider(11, 25, clocked_by clk250);
	//ClockDividerIfc auroraIntraClockDiv4 <- mkDCMClockDivider(4, 5);
   
   
   // ClockGenerator7Params clkgenParams = defaultValue;
   // clkgenParams.clkin1_period    = 4.000;
   // clkgenParams.clkin_buffer     = False;
   // clkgenParams.clkfbout_mult_f  = 4; // 1000MHz //11
   // clkgenParams.clkout0_divide_f = 9.091;
   // clkgenParams.clkout1_divide     = 4;
   // clkgenParams.clkout1_duty_cycle = 0.5;
   // clkgenParams.clkout1_phase      = 0.0000;
   // ClockGenerator7 auroraIntraClockGen <- mkClockGenerator7(clkgenParams, clocked_by clk250, reset_by rst250);
   // Clock clk110 = auroraIntraClockGen.clkout0;   
   // Reset defaultReset <- exposeCurrentReset;
	// Clock clk110 = auroraIntraClockDiv4.slowClock;
	MakeResetIfc rst110ifc2 <- mkReset(8, True, clk110);
	//Reset rst50 = rst50ifc.new_rst;
	// Reset rst110 <- mkAsyncReset(2, defaultReset, clk110);
	Reset rst110_2 = rst110ifc2.new_rst;
	Reset rst110_2a <- mkAsyncReset(2, rst110_2, clk110);
   
////////////////////////////////////////////////////////////////////////////////
///  GT_Ref clocks IBUFDS
////////////////////////////////////////////////////////////////////////////////

      let fmc2_gt_clk_i <- mkClockIBUFDS_GTE(
                                            `ifdef ClockDefaultParam
                                            defaultValue,
                                            `endif
                                            True, gt_clk_p, gt_clk_n);
   

   	AuroraImportIfc#(4) auroraIntraImport <- mkAuroraImport_8b10b_fmc2(fmc2_gt_clk_i.gen_clk, clk110, rst110, rst110_2a);
`else
	//Clock gtx_clk = cur_clk;
	AuroraImportIfc#(4) auroraIntraImport <- mkAuroraImport_8b10b_bsim;
`endif


	Clock aclk = auroraIntraImport.aurora_clk;
	Reset arst = auroraIntraImport.aurora_rst;


	Reg#(Bit#(32)) gearboxSendCnt <- mkReg(0);
	Reg#(Bit#(32)) gearboxRecCnt <- mkReg(0);
	Reg#(Bit#(32)) auroraSendCnt <- mkReg(0, clocked_by aclk, reset_by arst);
	Reg#(Bit#(32)) auroraRecCnt <- mkReg(0, clocked_by aclk, reset_by arst);
	Reg#(Bit#(32)) auroraSendCntCC <- mkSyncRegToCC(0, aclk, arst);
	Reg#(Bit#(32)) auroraRecCntCC <- mkSyncRegToCC(0, aclk, arst);
	rule syncCnt;
		auroraSendCntCC <= auroraSendCnt;
		auroraRecCntCC <= auroraRecCnt;
	endrule


	AuroraGearboxIfc auroraGearbox <- mkAuroraGearbox(aclk, arst);
	rule auroraOut if (auroraIntraImport.user.channel_up==1);
		let d <- auroraGearbox.auroraSend;
		//if ( auroraIntraImport.user.channel_up == 1 ) begin
			$display("Gearbox send out: %x", d);
			auroraSendCnt <= auroraSendCnt + 1;
			auroraIntraImport.user.send(d);
		//end
	endrule
	/*
	rule resetDeadLink ( auroraIntraImport.user.channel_up == 0 );
		auroraGearbox.resetLink;
		$display("Gearbox reset link");
	endrule
	*/
	rule auroraIn;
		let d <- auroraIntraImport.user.receive;
		$display("Gearbox received: %x", d);
		auroraRecCnt <= auroraRecCnt + 1;
		auroraGearbox.auroraRecv(d);
	endrule
		

	method Tuple4#(Bit#(32), Bit#(32), Bit#(32), Bit#(32)) getDebugCnts;
		return tuple4(gearboxSendCnt, gearboxRecCnt, auroraSendCntCC, auroraRecCntCC);
	endmethod


	method Action send(DataIfc data, PacketType ptype);
		auroraGearbox.send(data, ptype);
		gearboxSendCnt <= gearboxSendCnt + 1;
	endmethod
	method ActionValue#(Tuple2#(DataIfc, PacketType)) receive;
		let d <- auroraGearbox.recv;
		gearboxRecCnt <= gearboxRecCnt + 1;
		return d;
	endmethod

	method channel_up = auroraIntraImport.user.channel_up;
	method lane_up = auroraIntraImport.user.lane_up;
	method hard_err = auroraIntraImport.user.hard_err;
	method soft_err = auroraIntraImport.user.soft_err;
	method data_err_count = auroraIntraImport.user.data_err_count;

	interface Clock clk = auroraIntraImport.aurora_clk;
	interface Reset rst = auroraIntraImport.aurora_rst;

	interface Aurora_Pins aurora = auroraIntraImport.aurora;
endmodule

module mkAuroraImport_8b10b_bsim (AuroraImportIfc#(4));
	Clock clk <- exposeCurrentClock;
	Reset rst <- exposeCurrentReset;

	FIFOF#(Bit#(128)) mirrorQ <- mkSizedFIFOF(2);
	Reg#(Bit#(1)) laneUpR <- mkReg(0);
	Reg#(Bit#(32)) laneUpDelay <- mkReg(500);

	rule updateLaneUp;
		if (laneUpDelay ==0) begin
			laneUpR <= 1;
		end
		else begin
			laneUpDelay <= laneUpDelay - 1;
		end
	endrule


	rule detectFull if (!mirrorQ.notFull);
		$display("WARNING: mirrorQ is full!");
	endrule

	interface aurora_clk = clk;
	interface aurora_rst = rst;
	interface AuroraControllerIfc user;
		interface Reset aurora_rst_n = rst;

		method Bit#(1) channel_up;
			return laneUpR; 
		endmethod
		method Bit#(4) lane_up;
			return signExtend(laneUpR);
		endmethod
		method Bit#(1) hard_err;
			return 0;
		endmethod
		method Bit#(1) soft_err;
			return 0;
		endmethod
		method Bit#(8) data_err_count;
			return 0;
		endmethod

		method Action send(Bit#(128) data);
			mirrorQ.enq(data);
		endmethod
		method ActionValue#(Bit#(128)) receive;
			mirrorQ.deq;
			return mirrorQ.first;
		endmethod
	endinterface
endmodule

import "BVI" aurora_8b10b_fmc2_exdes =
module mkAuroraImport_8b10b_fmc2#(Clock gt_clk_in, Clock init_clk, Reset init_rst_n, Reset gt_rst_n) (AuroraImportIfc#(4));
	default_clock no_clock;
	default_reset no_reset;

	input_clock (INIT_CLK_IN) = init_clk;
	input_reset (RESET_N) = init_rst_n;
	input_reset (GT_RESET_N) = gt_rst_n;

	output_clock aurora_clk(USER_CLK);
	output_reset aurora_rst(USER_RST_N) clocked_by (aurora_clk);

	input_clock (GT_REFCLK) = gt_clk_in;
   
	interface Aurora_Pins aurora;
		method rxn_in(RXN) enable((*inhigh*) rx_n_en) reset_by(no_reset) clocked_by(gt_clk_in);
		method rxp_in(RXP) enable((*inhigh*) rx_p_en) reset_by(no_reset) clocked_by(gt_clk_in);
		method TXN txn_out() reset_by(no_reset) clocked_by(gt_clk_in); 
		method TXP txp_out() reset_by(no_reset) clocked_by(gt_clk_in);
	endinterface

	interface AuroraControllerIfc user;
		output_reset aurora_rst_n(USER_RST) clocked_by (aurora_clk);

		method CHANNEL_UP channel_up;
		method LANE_UP lane_up;
		method HARD_ERR hard_err;
		method SOFT_ERR soft_err;
		method ERR_COUNT data_err_count;

		method send(TX_DATA) enable(tx_en) ready(tx_rdy) clocked_by(aurora_clk) reset_by(aurora_rst);
		method RX_DATA receive() enable((*inhigh*) rx_en) ready(rx_rdy) clocked_by(aurora_clk) reset_by(aurora_rst);
	endinterface
	
	schedule (aurora_rxn_in, aurora_rxp_in, aurora_txn_out, aurora_txp_out, user_channel_up, user_lane_up, user_hard_err, user_soft_err, user_data_err_count) CF 
	(aurora_rxn_in, aurora_rxp_in, aurora_txn_out, aurora_txp_out, user_channel_up, user_lane_up, user_hard_err, user_soft_err, user_data_err_count);
	schedule (user_send) CF (aurora_rxn_in, aurora_rxp_in, aurora_txn_out, aurora_txp_out, user_channel_up, user_lane_up, user_hard_err, user_soft_err, user_data_err_count);

	schedule (user_receive) CF (aurora_rxn_in, aurora_rxp_in, aurora_txn_out, aurora_txp_out, user_channel_up, user_lane_up, user_hard_err, user_soft_err, user_data_err_count);

	schedule (user_receive) SB (user_send);
	schedule (user_send) C (user_send);
	schedule (user_receive) C (user_receive);

endmodule

endpackage: AuroraImportFmc2
