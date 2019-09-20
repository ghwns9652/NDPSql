

##    run: run the program
##         pass parameters to software via 'make RUN_ARGS= run'
RUN_ARGS?=

NDPDIR=/home/chamdoo/FlashDriver_vcu/lower/vcu108
export DTOP=$(NDPDIR)/platform_tests/flash_dual_no_ind_vcu108/vcu108
CONNECTALDIR=$(NDPDIR)/platform/build_tools/connectal
BSVPATH = $(NDPDIR)/platform_tests/flash_dual_no_ind_vcu108:$(NDPDIR)/platform/build_tools/connectal/bsv:$(NDPDIR)/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv:$(NDPDIR)/platform/xilinx/aurora_8b10b_fmc_vcu108:$(NDPDIR)/platform/controller/src/common:$(NDPDIR)/platform/controller/src/model_virtex:$(NDPDIR)/platform/controller/src/hw_virtex:$(NDPDIR)/platform/lib/:$(NDPDIR)/bsvlib:$(NDPDIR)/platform/build_tools/connectal/lib/bsv:$(NDPDIR)/platform/build_tools/connectal/generated/xilinx:$(NDPDIR)/platform/build_tools/connectal/generated/altera

BOARD=vcu108
PROJECTDIR=$(NDPDIR)/platform_tests/flash_dual_no_ind_vcu108/vcu108
MKTOP=mkPcieTop
OS=ubuntu
TOOLCHAIN?=
DUT=mkpcietop

export INTERFACES = MemServerRequest MMURequest MemServerIndication MMUIndication FlashRequest FlashIndication
BSVFILES = $(NDPDIR)/platform_tests/flash_dual_no_ind_vcu108/FlashTop.bsv $(NDPDIR)/platform/build_tools/connectal/bsv/ConnectalMemory.bsv $(NDPDIR)/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/IfcNames.bsv $(NDPDIR)/platform/build_tools/connectal/bsv/DisplayInd.bsv
XCIFILES = $(NDPDIR)/platform/build_tools/connectal/out/vcu108/aurora_8b10b_fmc1/aurora_8b10b_fmc1.xci $(NDPDIR)/platform/build_tools/connectal/out/vcu108/aurora_8b10b_fmc2/aurora_8b10b_fmc2.xci $(NDPDIR)/platform/build_tools/connectal/out/vcu108/pcie3_ultrascale_0/pcie3_ultrascale_0.xci

BSCFLAGS_PROJECT = +RTS -K46777216 -RTS
SIM_CXX_PROJECT = 
CFLAGS_PROJECT = 
CXXFLAGS_PROJECT = 
XELABFLAGS = 
XSIMFLAGS  = -R
TOPBSVFILE = $(NDPDIR)/platform/build_tools/connectal/bsv/PcieTop.bsv
BSVDEFINES =  -D IMPORT_HOSTIF -D XILINX_SYS_CLK -D DataBusWidth=256 -D USE_WIDE_WIDTH -D ConnectalVersion=18.08.1 -D NumberOfMasters=1 -D PinType=Top_Pins -D PinTypeInclude=FlashTop -D NumberOfUserTiles=1 -D SlaveDataBusWidth=32 -D SlaveControlAddrWidth=5 -D BurstLenSize=10 -D project_dir=$(DTOP) -D MainClockPeriod=8 -D DerivedClockPeriod=9.091000 -D PcieClockPeriod=4 -D XILINX=1 -D VirtexUltrascale -D XilinxUltrascale -D PCIE -D PCIE3 -D PcieHostInterface -D PhysAddrWidth=40 -D NUMBER_OF_LEDS=2 -D PcieLanes=8 -D CONNECTAL_BITS_DEPENDENCES=hw/mkTop.bit -D CONNECTAL_RUN_SCRIPT=$(CONNECTALDIR)/scripts/run.pcietest -D BOARD_vcu108
QTUSED = 
export BSVDEFINES_LIST = IMPORT_HOSTIF XILINX_SYS_CLK DataBusWidth=256 USE_WIDE_WIDTH ConnectalVersion=18.08.1 NumberOfMasters=1 PinType=Top_Pins PinTypeInclude=FlashTop NumberOfUserTiles=1 SlaveDataBusWidth=32 SlaveControlAddrWidth=5 BurstLenSize=10 project_dir=$(DTOP) MainClockPeriod=8 DerivedClockPeriod=9.091000 PcieClockPeriod=4 XILINX=1 VirtexUltrascale XilinxUltrascale PCIE PCIE3 PcieHostInterface PhysAddrWidth=40 NUMBER_OF_LEDS=2 PcieLanes=8 CONNECTAL_BITS_DEPENDENCES=hw/mkTop.bit CONNECTAL_RUN_SCRIPT=$(CONNECTALDIR)/scripts/run.pcietest BOARD_vcu108
export DUT_NAME = mkPcieTop





IMPORT_HOSTIF=IMPORT_HOSTIF
XILINX_SYS_CLK=XILINX_SYS_CLK
DataBusWidth=256
USE_WIDE_WIDTH=USE_WIDE_WIDTH
ConnectalVersion=18.08.1
NumberOfMasters=1
PinType=Top_Pins
PinTypeInclude=FlashTop
NumberOfUserTiles=1
SlaveDataBusWidth=32
SlaveControlAddrWidth=5
BurstLenSize=10
project_dir=$(DTOP)
MainClockPeriod=8
DerivedClockPeriod=9.091000
PcieClockPeriod=4
XILINX=1
VirtexUltrascale=VirtexUltrascale
XilinxUltrascale=XilinxUltrascale
PCIE=PCIE
PCIE3=PCIE3
PcieHostInterface=PcieHostInterface
PhysAddrWidth=40
NUMBER_OF_LEDS=2
PcieLanes=8
CONNECTAL_BITS_DEPENDENCES=hw/mkTop.bit
CONNECTAL_RUN_SCRIPT=$(CONNECTALDIR)/scripts/run.pcietest
BOARD_vcu108=BOARD_vcu108


include $(CONNECTALDIR)/scripts/Makefile.connectal.build


export VERILOG_PATH=verilog $(NDPDIR)/platform/xilinx/aurora_8b10b_fmc_vcu108 $(NDPDIR)/platform/build_tools/connectal/verilog/xilinx $(NDPDIR)/platform/build_tools/connectal/verilog $(BLUESPEC_VERILOG)
MODELSIM_FILES= 
FPGAMAKE=$(CONNECTALDIR)/../fpgamake/fpgamake
fpgamake.mk: $(VFILE) Makefile prepare_bin_target
	$(Q)if [ -f ../synth-ip.tcl ]; then vivado -mode batch -source ../synth-ip.tcl; fi
	$(Q)$(FPGAMAKE) $(FPGAMAKE_VERBOSE) -o fpgamake.mk --board=vcu108 --part=xcvu095-ffva2104-2-e  --floorplan= --constraint=$(NDPDIR)/platform/build_tools/connectal/constraints/xilinx/vcu108.xdc --implconstraint=$(NDPDIR)/platform/build_tools/connectal/constraints/xilinx/vcu108.xdc --implconstraint=$(NDPDIR)/platform/build_tools/connectal/constraints/xilinx/pcie-clocks.xdc --implconstraint=$(NDPDIR)/platform/xilinx/constraints/pcieclock-vcu108.xdc --implconstraint=$(NDPDIR)/platform/xilinx/aurora_8b10b_fmc_vcu108/aurora_8b10b_fmc_exdes.xdc --xci=$(NDPDIR)/platform/build_tools/connectal/out/vcu108/aurora_8b10b_fmc1/aurora_8b10b_fmc1.xci --xci=$(NDPDIR)/platform/build_tools/connectal/out/vcu108/aurora_8b10b_fmc2/aurora_8b10b_fmc2.xci --xci=$(NDPDIR)/platform/build_tools/connectal/out/vcu108/pcie3_ultrascale_0/pcie3_ultrascale_0.xci --tcl=$(NDPDIR)/platform/build_tools/connectal/constraints/xilinx/cdc.tcl   -t $(MKTOP)   -b hw/mkTop.bit   $(VERILOG_PATH)

synth.%:fpgamake.mk
	$(MAKE) -f fpgamake.mk Synth/$*/$*-synth.dcp

hw/mkTop.bit: prepare_bin_target  fpgamake.mk
	$(Q)mkdir -p hw
	$(Q)$(MAKE) -f fpgamake.mk
ifneq ($(XILINX),)
	$(Q)rsync -rav --include="*/" --include="*.rpt" --exclude="*" Impl/ bin
else ifneq ($(ALTERA),)
	$(Q)cp -f $(MKTOP).sof bin
endif




