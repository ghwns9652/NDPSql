# BSV dependences
#  -DIMPORT_HOSTIF
#  -DXILINX_SYS_CLK
#  -DDataBusWidth=256
#  -DUSE_WIDE_WIDTH
#  -DConnectalVersion=18.08.1
#  -DNumberOfMasters=1
#  -DPinType=Top_Pins
#  -DPinTypeInclude=FlashTop
#  -DNumberOfUserTiles=1
#  -DSlaveDataBusWidth=32
#  -DSlaveControlAddrWidth=5
#  -DBurstLenSize=10
#  -Dproject_dir=/home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108
#  -DMainClockPeriod=8
#  -DDerivedClockPeriod=9.091000
#  -DPcieClockPeriod=4
#  -DXILINX=1
#  -DVirtexUltrascale
#  -DXilinxUltrascale
#  -DPCIE
#  -DPCIE3
#  -DPcieHostInterface
#  -DPhysAddrWidth=40
#  -DNUMBER_OF_LEDS=2
#  -DPcieLanes=8
#  -DCONNECTAL_BITS_DEPENDENCES=hw/mkTop.bit
#  -DCONNECTAL_RUN_SCRIPT=/home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/scripts/run.pcietest
#  -DBOARD_vcu108
OBJMAKEFILE_DEP = $(wildcard /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/*.bsv) $(wildcard /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/*.bsv) $(wildcard /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/*.bsv) $(wildcard /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/xilinx/aurora_8b10b_fmc_vcu108/*.bsv) $(wildcard /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/controller/src/common/*.bsv) $(wildcard /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/controller/src/model_virtex/*.bsv) $(wildcard /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/controller/src/hw_virtex/*.bsv) $(wildcard /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/lib//*.bsv) $(wildcard /home/hojun/git/FlashDriver_bulk/lower/vcu108/bsvlib/*.bsv) $(wildcard /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/lib/bsv/*.bsv) $(wildcard /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/generated/xilinx/*.bsv) $(wildcard /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/generated/altera/*.bsv)


FlashTypes_BO  = obj/FlashTypes.bo
FlashTypes_DEP = obj/ControllerTypes.bo
FlashTypes_INC = 
FlashTypes_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/FlashTypes.bsv

$(eval $(call BSV_BO_RULE, $(FlashTypes_BO), $(FlashTypes_BSV), $(FlashTypes_DEP), $(FlashTypes_INC)))

FlashTop_BO  = obj/FlashTop.bo
FlashTop_DEP = obj/FIFO.bo obj/FIFOF.bo obj/Vector.bo obj/BuildVector.bo obj/Connectable.bo obj/HostInterface.bo obj/Assert.bo obj/GetPut.bo obj/ClientServer.bo obj/ClientServerHelper.bo obj/ControllerTypes.bo obj/AuroraCommon.bo obj/AuroraImportFmc1.bo obj/FlashCtrlVirtex.bo obj/FlashCtrlModel.bo obj/ConnectalConfig.bo obj/ConnectalMemTypes.bo obj/MemReadEngine.bo obj/MemWriteEngine.bo obj/Pipe.bo obj/FlashTypes.bo obj/RenameTable.bo obj/BRAMFIFOVector.bo obj/BRAM.bo obj/Clocks.bo
FlashTop_INC = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/ConnectalProjectConfig.bsv
FlashTop_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/FlashTop.bsv

$(eval $(call BSV_BO_RULE, $(FlashTop_BO), $(FlashTop_BSV), $(FlashTop_DEP), $(FlashTop_INC)))

EmptyFlash_BO  = obj/EmptyFlash.bo
EmptyFlash_DEP = obj/ControllerTypes.bo obj/AuroraCommon.bo obj/AuroraImportFmc1.bo obj/FlashCtrlVirtex.bo obj/FlashCtrlModel.bo obj/GetPut.bo obj/FIFO.bo
EmptyFlash_INC = 
EmptyFlash_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/EmptyFlash.bsv

$(eval $(call BSV_BO_RULE, $(EmptyFlash_BO), $(EmptyFlash_BSV), $(EmptyFlash_DEP), $(EmptyFlash_INC)))

EmptyFlash_MOD = mkEmptyFlashCtrl
EmptyFlash_V   = verilog/mkEmptyFlashCtrl.v
EmptyFlash_BO  = obj/EmptyFlash.bo
EmptyFlash_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/EmptyFlash.bsv
EmptyFlash_DEP = obj/ControllerTypes.bo obj/AuroraCommon.bo obj/AuroraImportFmc1.bo obj/FlashCtrlVirtex.bo obj/FlashCtrlModel.bo obj/GetPut.bo obj/FIFO.bo
EmptyFlash_INC = 

$(eval $(call BSV_V_RULE, $(EmptyFlash_MOD), $(EmptyFlash_V), $(EmptyFlash_BSV), $(EmptyFlash_DEP), $(EmptyFlash_INC)))

ZynqTop_BO  = obj/ZynqTop.bo
ZynqTop_DEP = obj/ConnectalConfig.bo obj/ConnectalClocks.bo obj/Clocks.bo obj/DefaultValue.bo obj/Vector.bo obj/Connectable.bo obj/ConnectableWithTrace.bo obj/Portal.bo obj/ConnectalMemTypes.bo obj/AxiMasterSlave.bo obj/XilinxCells.bo obj/ConnectalXilinxCells.bo obj/PS7LIB.bo obj/PS7Trace.bo obj/PPS7LIB.bo obj/CtrlMux.bo obj/AxiDma.bo obj/Top.bo obj/Bscan.bo obj/HostInterface.bo obj/Platform.bo obj/FlashTop.bo
ZynqTop_INC = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/ConnectalProjectConfig.bsv
ZynqTop_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/ZynqTop.bsv

$(eval $(call BSV_BO_RULE, $(ZynqTop_BO), $(ZynqTop_BSV), $(ZynqTop_DEP), $(ZynqTop_INC)))

PcieGearbox_BO  = obj/PcieGearbox.bo
PcieGearbox_DEP = obj/Vector.bo obj/GetPut.bo obj/PCIE.bo obj/Clocks.bo obj/DefaultValue.bo obj/TieOff.bo obj/XilinxCells.bo obj/ClientServer.bo obj/PCIEWRAPPER.bo obj/Connectable.bo obj/Reserved.bo obj/DReg.bo obj/Gearbox.bo obj/FIFO.bo obj/FIFOF.bo obj/SpecialFIFOs.bo
PcieGearbox_INC = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/ConnectalProjectConfig.bsv
PcieGearbox_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/PcieGearbox.bsv

$(eval $(call BSV_BO_RULE, $(PcieGearbox_BO), $(PcieGearbox_BSV), $(PcieGearbox_DEP), $(PcieGearbox_INC)))

PcieCsr_BO  = obj/PcieCsr.bo
PcieCsr_DEP = obj/Vector.bo obj/BRAM.bo obj/FIFOF.bo obj/BRAMFIFO.bo obj/GetPut.bo obj/Connectable.bo obj/PCIE.bo obj/Clocks.bo obj/PcieTracer.bo obj/ConnectalMemTypes.bo obj/AddressGenerator.bo obj/Pipe.bo
PcieCsr_INC = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/ConnectalProjectConfig.bsv
PcieCsr_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/PcieCsr.bsv

$(eval $(call BSV_BO_RULE, $(PcieCsr_BO), $(PcieCsr_BSV), $(PcieCsr_DEP), $(PcieCsr_INC)))

PcieCsr_MOD = mkPcieControlAndStatusRegs
PcieCsr_V   = verilog/mkPcieControlAndStatusRegs.v
PcieCsr_BO  = obj/PcieCsr.bo
PcieCsr_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/PcieCsr.bsv
PcieCsr_DEP = obj/Vector.bo obj/BRAM.bo obj/FIFOF.bo obj/BRAMFIFO.bo obj/GetPut.bo obj/Connectable.bo obj/PCIE.bo obj/Clocks.bo obj/PcieTracer.bo obj/ConnectalMemTypes.bo obj/AddressGenerator.bo obj/Pipe.bo
PcieCsr_INC = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/ConnectalProjectConfig.bsv

$(eval $(call BSV_V_RULE, $(PcieCsr_MOD), $(PcieCsr_V), $(PcieCsr_BSV), $(PcieCsr_DEP), $(PcieCsr_INC)))

Pcie3EndpointX7_BO  = obj/Pcie3EndpointX7.bo
Pcie3EndpointX7_DEP = obj/BRAMFIFO.bo obj/Clocks.bo obj/Vector.bo obj/BuildVector.bo obj/Connectable.bo obj/GetPut.bo obj/Reserved.bo obj/TieOff.bo obj/DefaultValue.bo obj/DReg.bo obj/Gearbox.bo obj/FIFO.bo obj/FIFOF.bo obj/ConnectalFIFO.bo obj/SpecialFIFOs.bo obj/ClientServer.bo obj/Real.bo obj/XilinxVirtex7PCIE.bo obj/BUtils.bo obj/Probe.bo obj/ConnectalConfig.bo obj/ConnectalClocks.bo obj/ConnectalXilinxCells.bo obj/XilinxCells.bo obj/PCIE.bo obj/PCIEWRAPPER3u.bo obj/Bufgctrl.bo obj/PcieGearbox.bo obj/Pipe.bo
Pcie3EndpointX7_INC = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/ConnectalProjectConfig.bsv
Pcie3EndpointX7_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/Pcie3EndpointX7.bsv

$(eval $(call BSV_BO_RULE, $(Pcie3EndpointX7_BO), $(Pcie3EndpointX7_BSV), $(Pcie3EndpointX7_DEP), $(Pcie3EndpointX7_INC)))

Pcie3EndpointX7_MOD = mkPcieEndpointX7
Pcie3EndpointX7_V   = verilog/mkPcieEndpointX7.v
Pcie3EndpointX7_BO  = obj/Pcie3EndpointX7.bo
Pcie3EndpointX7_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/Pcie3EndpointX7.bsv
Pcie3EndpointX7_DEP = obj/BRAMFIFO.bo obj/Clocks.bo obj/Vector.bo obj/BuildVector.bo obj/Connectable.bo obj/GetPut.bo obj/Reserved.bo obj/TieOff.bo obj/DefaultValue.bo obj/DReg.bo obj/Gearbox.bo obj/FIFO.bo obj/FIFOF.bo obj/ConnectalFIFO.bo obj/SpecialFIFOs.bo obj/ClientServer.bo obj/Real.bo obj/XilinxVirtex7PCIE.bo obj/BUtils.bo obj/Probe.bo obj/ConnectalConfig.bo obj/ConnectalClocks.bo obj/ConnectalXilinxCells.bo obj/XilinxCells.bo obj/PCIE.bo obj/PCIEWRAPPER3u.bo obj/Bufgctrl.bo obj/PcieGearbox.bo obj/Pipe.bo
Pcie3EndpointX7_INC = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/ConnectalProjectConfig.bsv

$(eval $(call BSV_V_RULE, $(Pcie3EndpointX7_MOD), $(Pcie3EndpointX7_V), $(Pcie3EndpointX7_BSV), $(Pcie3EndpointX7_DEP), $(Pcie3EndpointX7_INC)))

MemServer_BO  = obj/MemServer.bo
MemServer_DEP = obj/FIFO.bo obj/Vector.bo obj/List.bo obj/GetPut.bo obj/ClientServer.bo obj/Assert.bo obj/StmtFSM.bo obj/SpecialFIFOs.bo obj/Connectable.bo obj/HostInterface.bo obj/ConnectalMemTypes.bo obj/ConnectalConfig.bo obj/ConnectalMemory.bo obj/ConnectalMMU.bo obj/MemServerInternal.bo
MemServer_INC = 
MemServer_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/MemServer.bsv

$(eval $(call BSV_BO_RULE, $(MemServer_BO), $(MemServer_BSV), $(MemServer_DEP), $(MemServer_INC)))

ConnectalClocks_BO  = obj/ConnectalClocks.bo
ConnectalClocks_DEP = obj/Clocks.bo
ConnectalClocks_INC = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/ConnectalProjectConfig.bsv
ConnectalClocks_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/ConnectalClocks.bsv

$(eval $(call BSV_BO_RULE, $(ConnectalClocks_BO), $(ConnectalClocks_BSV), $(ConnectalClocks_DEP), $(ConnectalClocks_INC)))

OldEHR_BO  = obj/OldEHR.bo
OldEHR_DEP = obj/Vector.bo obj/RWire.bo obj/Probe.bo
OldEHR_INC = 
OldEHR_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/OldEHR.bsv

$(eval $(call BSV_BO_RULE, $(OldEHR_BO), $(OldEHR_BSV), $(OldEHR_DEP), $(OldEHR_INC)))

OldEHR_MOD = mkEHR2BSV
OldEHR_V   = verilog/mkEHR2BSV.v
OldEHR_BO  = obj/OldEHR.bo
OldEHR_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/OldEHR.bsv
OldEHR_DEP = obj/Vector.bo obj/RWire.bo obj/Probe.bo
OldEHR_INC = 

$(eval $(call BSV_V_RULE, $(OldEHR_MOD), $(OldEHR_V), $(OldEHR_BSV), $(OldEHR_DEP), $(OldEHR_INC)))

Pcie1EndpointX7_BO  = obj/Pcie1EndpointX7.bo
Pcie1EndpointX7_DEP = obj/ConnectalConfig.bo obj/Clocks.bo obj/Vector.bo obj/Connectable.bo obj/GetPut.bo obj/Reserved.bo obj/TieOff.bo obj/DefaultValue.bo obj/DReg.bo obj/Gearbox.bo obj/FIFO.bo obj/FIFOF.bo obj/SpecialFIFOs.bo obj/ClientServer.bo obj/Real.bo obj/ConnectalClocks.bo obj/ConnectalXilinxCells.bo obj/XilinxCells.bo obj/PCIE.bo obj/PCIEWRAPPER.bo obj/Bufgctrl.bo obj/PcieGearbox.bo obj/PcieStateChanges.bo obj/Pipe.bo
Pcie1EndpointX7_INC = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/ConnectalProjectConfig.bsv
Pcie1EndpointX7_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/Pcie1EndpointX7.bsv

$(eval $(call BSV_BO_RULE, $(Pcie1EndpointX7_BO), $(Pcie1EndpointX7_BSV), $(Pcie1EndpointX7_DEP), $(Pcie1EndpointX7_INC)))

Pcie1EndpointX7_MOD = mkPcieEndpointX7
Pcie1EndpointX7_V   = verilog/mkPcieEndpointX7.v
Pcie1EndpointX7_BO  = obj/Pcie1EndpointX7.bo
Pcie1EndpointX7_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/Pcie1EndpointX7.bsv
Pcie1EndpointX7_DEP = obj/ConnectalConfig.bo obj/Clocks.bo obj/Vector.bo obj/Connectable.bo obj/GetPut.bo obj/Reserved.bo obj/TieOff.bo obj/DefaultValue.bo obj/DReg.bo obj/Gearbox.bo obj/FIFO.bo obj/FIFOF.bo obj/SpecialFIFOs.bo obj/ClientServer.bo obj/Real.bo obj/ConnectalClocks.bo obj/ConnectalXilinxCells.bo obj/XilinxCells.bo obj/PCIE.bo obj/PCIEWRAPPER.bo obj/Bufgctrl.bo obj/PcieGearbox.bo obj/PcieStateChanges.bo obj/Pipe.bo
Pcie1EndpointX7_INC = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/ConnectalProjectConfig.bsv

$(eval $(call BSV_V_RULE, $(Pcie1EndpointX7_MOD), $(Pcie1EndpointX7_V), $(Pcie1EndpointX7_BSV), $(Pcie1EndpointX7_DEP), $(Pcie1EndpointX7_INC)))

AxiBits_BO  = obj/AxiBits.bo
AxiBits_DEP = obj/Vector.bo obj/Clocks.bo obj/FIFOF.bo obj/ConnectalFIFO.bo obj/GetPut.bo obj/Probe.bo obj/ConnectalMemTypes.bo obj/ConnectalEHR.bo obj/Axi4MasterSlave.bo obj/Connectable.bo obj/ConnectalBramFifo.bo
AxiBits_INC = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/ConnectalProjectConfig.bsv
AxiBits_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/AxiBits.bsv

$(eval $(call BSV_BO_RULE, $(AxiBits_BO), $(AxiBits_BSV), $(AxiBits_DEP), $(AxiBits_INC)))

PS8LIB_BO  = obj/PS8LIB.bo
PS8LIB_DEP = obj/ConnectalConfig.bo obj/BuildVector.bo obj/Clocks.bo obj/DefaultValue.bo obj/FIFOF.bo obj/GetPut.bo obj/Connectable.bo obj/ConnectableWithTrace.bo obj/Bscan.bo obj/Vector.bo obj/XilinxCells.bo obj/ConnectalXilinxCells.bo obj/ConnectalClocks.bo obj/AxiMasterSlave.bo obj/Axi4MasterSlave.bo obj/AxiDma.bo obj/AxiBits.bo obj/ConnectalMemTypes.bo obj/Platform.bo obj/ZYNQ_ULTRA.bo obj/Probe.bo
PS8LIB_INC = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/ConnectalProjectConfig.bsv
PS8LIB_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/PS8LIB.bsv

$(eval $(call BSV_BO_RULE, $(PS8LIB_BO), $(PS8LIB_BSV), $(PS8LIB_DEP), $(PS8LIB_INC)))

ConnectalEHR_BO  = obj/ConnectalEHR.bo
ConnectalEHR_DEP = obj/Vector.bo obj/RWire.bo
ConnectalEHR_INC = 
ConnectalEHR_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/ConnectalEHR.bsv

$(eval $(call BSV_BO_RULE, $(ConnectalEHR_BO), $(ConnectalEHR_BSV), $(ConnectalEHR_DEP), $(ConnectalEHR_INC)))

ParallellaTop_BO  = obj/ParallellaTop.bo
ParallellaTop_DEP = obj/Clocks.bo obj/Vector.bo obj/Connectable.bo obj/ConnectableWithTrace.bo obj/Portal.bo obj/ConnectalMemTypes.bo obj/AxiMasterSlave.bo obj/XilinxCells.bo obj/ConnectalXilinxCells.bo obj/PS7LIB.bo obj/PS7Trace.bo obj/PPS7LIB.bo obj/CtrlMux.bo obj/AxiDma.bo obj/Top.bo obj/Bscan.bo obj/HostInterface.bo obj/FlashTop.bo
ParallellaTop_INC = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/ConnectalProjectConfig.bsv
ParallellaTop_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/ParallellaTop.bsv

$(eval $(call BSV_BO_RULE, $(ParallellaTop_BO), $(ParallellaTop_BSV), $(ParallellaTop_DEP), $(ParallellaTop_INC)))

PcieTracer_BO  = obj/PcieTracer.bo
PcieTracer_DEP = obj/Vector.bo obj/Clocks.bo obj/GetPut.bo obj/Connectable.bo obj/FIFO.bo obj/FIFOF.bo obj/PCIE.bo obj/BRAM.bo obj/BramMux.bo obj/ConnectalBram.bo
PcieTracer_INC = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/ConnectalProjectConfig.bsv
PcieTracer_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/PcieTracer.bsv

$(eval $(call BSV_BO_RULE, $(PcieTracer_BO), $(PcieTracer_BSV), $(PcieTracer_DEP), $(PcieTracer_INC)))

PcieTracer_MOD = mkPcieTracer
PcieTracer_V   = verilog/mkPcieTracer.v
PcieTracer_BO  = obj/PcieTracer.bo
PcieTracer_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/PcieTracer.bsv
PcieTracer_DEP = obj/Vector.bo obj/Clocks.bo obj/GetPut.bo obj/Connectable.bo obj/FIFO.bo obj/FIFOF.bo obj/PCIE.bo obj/BRAM.bo obj/BramMux.bo obj/ConnectalBram.bo
PcieTracer_INC = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/ConnectalProjectConfig.bsv

$(eval $(call BSV_V_RULE, $(PcieTracer_MOD), $(PcieTracer_V), $(PcieTracer_BSV), $(PcieTracer_DEP), $(PcieTracer_INC)))

AxiStream_BO  = obj/AxiStream.bo
AxiStream_DEP = obj/Connectable.bo obj/FIFOF.bo obj/GetPut.bo obj/GetPutM.bo obj/Probe.bo
AxiStream_INC = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/ConnectalProjectConfig.bsv
AxiStream_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/AxiStream.bsv

$(eval $(call BSV_BO_RULE, $(AxiStream_BO), $(AxiStream_BSV), $(AxiStream_DEP), $(AxiStream_INC)))

ZynqUltraTop_BO  = obj/ZynqUltraTop.bo
ZynqUltraTop_DEP = obj/ConnectalConfig.bo obj/ConnectalClocks.bo obj/Clocks.bo obj/DefaultValue.bo obj/Vector.bo obj/Connectable.bo obj/ConnectableWithTrace.bo obj/Portal.bo obj/ConnectalMemTypes.bo obj/AxiMasterSlave.bo obj/XilinxCells.bo obj/ConnectalXilinxCells.bo obj/PS8LIB.bo obj/ZYNQ_ULTRA.bo obj/CtrlMux.bo obj/AxiDma.bo obj/Top.bo obj/Bscan.bo obj/HostInterface.bo obj/Platform.bo obj/FlashTop.bo
ZynqUltraTop_INC = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/ConnectalProjectConfig.bsv
ZynqUltraTop_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/ZynqUltraTop.bsv

$(eval $(call BSV_BO_RULE, $(ZynqUltraTop_BO), $(ZynqUltraTop_BSV), $(ZynqUltraTop_DEP), $(ZynqUltraTop_INC)))

SimDma_BO  = obj/SimDma.bo
SimDma_DEP = obj/Vector.bo obj/FIFO.bo obj/FIFOF.bo obj/GetPut.bo obj/ConnectalMemTypes.bo obj/ConnectalConfig.bo
SimDma_INC = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/ConnectalProjectConfig.bsv
SimDma_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/SimDma.bsv

$(eval $(call BSV_BO_RULE, $(SimDma_BO), $(SimDma_BSV), $(SimDma_DEP), $(SimDma_INC)))

PS7Trace_BO  = obj/PS7Trace.bo
PS7Trace_DEP = obj/ConnectalConfig.bo obj/Connectable.bo obj/ConnectableWithTrace.bo obj/Vector.bo obj/HostInterface.bo obj/PS7LIB.bo obj/Portal.bo obj/AxiMasterSlave.bo obj/AxiDma.bo obj/AxiBits.bo obj/AxiGather.bo obj/Platform.bo
PS7Trace_INC = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/ConnectalProjectConfig.bsv
PS7Trace_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/PS7Trace.bsv

$(eval $(call BSV_BO_RULE, $(PS7Trace_BO), $(PS7Trace_BSV), $(PS7Trace_DEP), $(PS7Trace_INC)))

PcieRootPortX7_BO  = obj/PcieRootPortX7.bo
PcieRootPortX7_DEP = obj/Clocks.bo obj/Vector.bo obj/Connectable.bo obj/GetPut.bo obj/Reserved.bo obj/TieOff.bo obj/DefaultValue.bo obj/DReg.bo obj/Gearbox.bo obj/FIFO.bo obj/FIFOF.bo obj/SpecialFIFOs.bo obj/ClientServer.bo obj/Real.bo obj/ConnectalClocks.bo obj/ConnectalXilinxCells.bo obj/XilinxCells.bo obj/PCIE.bo obj/PCIEWRAPPER.bo obj/Bufgctrl.bo
PcieRootPortX7_INC = 
PcieRootPortX7_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/PcieRootPortX7.bsv

$(eval $(call BSV_BO_RULE, $(PcieRootPortX7_BO), $(PcieRootPortX7_BSV), $(PcieRootPortX7_DEP), $(PcieRootPortX7_INC)))

PcieRootPortX7_MOD = mkPcieRootPortX7
PcieRootPortX7_V   = verilog/mkPcieRootPortX7.v
PcieRootPortX7_BO  = obj/PcieRootPortX7.bo
PcieRootPortX7_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/PcieRootPortX7.bsv
PcieRootPortX7_DEP = obj/Clocks.bo obj/Vector.bo obj/Connectable.bo obj/GetPut.bo obj/Reserved.bo obj/TieOff.bo obj/DefaultValue.bo obj/DReg.bo obj/Gearbox.bo obj/FIFO.bo obj/FIFOF.bo obj/SpecialFIFOs.bo obj/ClientServer.bo obj/Real.bo obj/ConnectalClocks.bo obj/ConnectalXilinxCells.bo obj/XilinxCells.bo obj/PCIE.bo obj/PCIEWRAPPER.bo obj/Bufgctrl.bo
PcieRootPortX7_INC = 

$(eval $(call BSV_V_RULE, $(PcieRootPortX7_MOD), $(PcieRootPortX7_V), $(PcieRootPortX7_BSV), $(PcieRootPortX7_DEP), $(PcieRootPortX7_INC)))

BpiFlash_BO  = obj/BpiFlash.bo
BpiFlash_DEP = obj/Vector.bo obj/Clocks.bo obj/Connectable.bo obj/GetPut.bo obj/FIFOF.bo obj/BRAM.bo obj/Probe.bo obj/StmtFSM.bo obj/TriState.bo obj/ConnectalXilinxCells.bo
BpiFlash_INC = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/ConnectalProjectConfig.bsv
BpiFlash_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/BpiFlash.bsv

$(eval $(call BSV_BO_RULE, $(BpiFlash_BO), $(BpiFlash_BSV), $(BpiFlash_DEP), $(BpiFlash_INC)))

PcieTop_BO  = obj/PcieTop.bo
PcieTop_DEP = obj/Vector.bo obj/Clocks.bo obj/GetPut.bo obj/FIFO.bo obj/Connectable.bo obj/ClientServer.bo obj/DefaultValue.bo obj/ConnectalConfig.bo obj/PcieSplitter.bo obj/Xilinx.bo obj/Portal.bo obj/Top.bo obj/PcieCsr.bo obj/ConnectalMemTypes.bo obj/Bscan.bo obj/ConnectalClocks.bo obj/GetPutWithClocks.bo obj/PCIEWRAPPER3u.bo obj/Pcie3EndpointX7.bo obj/PcieHost.bo obj/HostInterface.bo obj/FlashTop.bo obj/Platform.bo
PcieTop_INC = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/ConnectalProjectConfig.bsv
PcieTop_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/PcieTop.bsv

$(eval $(call BSV_BO_RULE, $(PcieTop_BO), $(PcieTop_BSV), $(PcieTop_DEP), $(PcieTop_INC)))

MemReadEngine_BO  = obj/MemReadEngine.bo
MemReadEngine_DEP = obj/Vector.bo obj/BuildVector.bo obj/Cntrs.bo obj/FIFOF.bo obj/FIFO.bo obj/GetPut.bo obj/Assert.bo obj/ClientServer.bo obj/BRAM.bo obj/BRAMFIFO.bo obj/ConfigCounter.bo obj/Connectable.bo obj/ConnectalMemory.bo obj/ConnectalBramFifo.bo obj/ConnectalMemTypes.bo obj/Pipe.bo obj/ConnectalMemUtils.bo obj/ConnectalConfig.bo
MemReadEngine_INC = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/ConnectalProjectConfig.bsv
MemReadEngine_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/MemReadEngine.bsv

$(eval $(call BSV_BO_RULE, $(MemReadEngine_BO), $(MemReadEngine_BSV), $(MemReadEngine_DEP), $(MemReadEngine_INC)))

XsimIF_BO  = obj/XsimIF.bo
XsimIF_DEP = 
XsimIF_INC = 
XsimIF_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/XsimIF.bsv

$(eval $(call BSV_BO_RULE, $(XsimIF_BO), $(XsimIF_BSV), $(XsimIF_DEP), $(XsimIF_INC)))

PcieEndpointS5_BO  = obj/PcieEndpointS5.bo
PcieEndpointS5_DEP = obj/ConnectalConfig.bo obj/Clocks.bo obj/Vector.bo obj/Connectable.bo obj/GetPut.bo obj/Reserved.bo obj/TieOff.bo obj/DefaultValue.bo obj/DReg.bo obj/Gearbox.bo obj/FIFO.bo obj/FIFOF.bo obj/SpecialFIFOs.bo obj/ClientServer.bo obj/Real.bo obj/PCIE.bo
PcieEndpointS5_INC = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/ConnectalProjectConfig.bsv
PcieEndpointS5_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/PcieEndpointS5.bsv

$(eval $(call BSV_BO_RULE, $(PcieEndpointS5_BO), $(PcieEndpointS5_BSV), $(PcieEndpointS5_DEP), $(PcieEndpointS5_INC)))

PcieEndpointS5_MOD = mkPcieEndpointS5
PcieEndpointS5_V   = verilog/mkPcieEndpointS5.v
PcieEndpointS5_BO  = obj/PcieEndpointS5.bo
PcieEndpointS5_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/PcieEndpointS5.bsv
PcieEndpointS5_DEP = obj/ConnectalConfig.bo obj/Clocks.bo obj/Vector.bo obj/Connectable.bo obj/GetPut.bo obj/Reserved.bo obj/TieOff.bo obj/DefaultValue.bo obj/DReg.bo obj/Gearbox.bo obj/FIFO.bo obj/FIFOF.bo obj/SpecialFIFOs.bo obj/ClientServer.bo obj/Real.bo obj/PCIE.bo
PcieEndpointS5_INC = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/ConnectalProjectConfig.bsv

$(eval $(call BSV_V_RULE, $(PcieEndpointS5_MOD), $(PcieEndpointS5_V), $(PcieEndpointS5_BSV), $(PcieEndpointS5_DEP), $(PcieEndpointS5_INC)))

ConnectalMimo_BO  = obj/ConnectalMimo.bo
ConnectalMimo_DEP = obj/Vector.bo obj/DefaultValue.bo obj/BUtils.bo obj/FIFO.bo obj/FIFOF.bo obj/BRAMFIFO.bo obj/Counter.bo obj/Clocks.bo obj/MIMO.bo obj/ConnectalBramFifo.bo
ConnectalMimo_INC = 
ConnectalMimo_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/ConnectalMimo.bsv

$(eval $(call BSV_BO_RULE, $(ConnectalMimo_BO), $(ConnectalMimo_BSV), $(ConnectalMimo_DEP), $(ConnectalMimo_INC)))

AxiDma_BO  = obj/AxiDma.bo
AxiDma_DEP = obj/ConnectalConfig.bo obj/FIFO.bo obj/GetPut.bo obj/ClientServer.bo obj/AxiMasterSlave.bo obj/Axi4MasterSlave.bo obj/ConnectalMemTypes.bo obj/ConnectalMemory.bo
AxiDma_INC = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/ConnectalProjectConfig.bsv
AxiDma_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/AxiDma.bsv

$(eval $(call BSV_BO_RULE, $(AxiDma_BO), $(AxiDma_BSV), $(AxiDma_DEP), $(AxiDma_INC)))

AvalonSplitter_BO  = obj/AvalonSplitter.bo
AvalonSplitter_DEP = obj/FIFO.bo obj/FIFOF.bo obj/GetPut.bo obj/Vector.bo obj/AvalonMasterSlave.bo
AvalonSplitter_INC = 
AvalonSplitter_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/AvalonSplitter.bsv

$(eval $(call BSV_BO_RULE, $(AvalonSplitter_BO), $(AvalonSplitter_BSV), $(AvalonSplitter_DEP), $(AvalonSplitter_INC)))

AvalonBits_BO  = obj/AvalonBits.bo
AvalonBits_DEP = obj/Vector.bo obj/Connectable.bo
AvalonBits_INC = 
AvalonBits_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/AvalonBits.bsv

$(eval $(call BSV_BO_RULE, $(AvalonBits_BO), $(AvalonBits_BSV), $(AvalonBits_DEP), $(AvalonBits_INC)))

ConnectalFIFO_BO  = obj/ConnectalFIFO.bo
ConnectalFIFO_DEP = obj/FIFOF.bo obj/ConnectalEHR.bo
ConnectalFIFO_INC = 
ConnectalFIFO_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/ConnectalFIFO.bsv

$(eval $(call BSV_BO_RULE, $(ConnectalFIFO_BO), $(ConnectalFIFO_BSV), $(ConnectalFIFO_DEP), $(ConnectalFIFO_INC)))

PcieRootDevice_BO  = obj/PcieRootDevice.bo
PcieRootDevice_DEP = obj/GetPut.bo obj/Connectable.bo
PcieRootDevice_INC = 
PcieRootDevice_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/PcieRootDevice.bsv

$(eval $(call BSV_BO_RULE, $(PcieRootDevice_BO), $(PcieRootDevice_BSV), $(PcieRootDevice_DEP), $(PcieRootDevice_INC)))

Dsp48E1_BO  = obj/Dsp48E1.bo
Dsp48E1_DEP = obj/GetPut.bo obj/FIFO.bo obj/FIFOF.bo obj/Clocks.bo obj/ConnectalClocks.bo
Dsp48E1_INC = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/ConnectalProjectConfig.bsv
Dsp48E1_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/Dsp48E1.bsv

$(eval $(call BSV_BO_RULE, $(Dsp48E1_BO), $(Dsp48E1_BSV), $(Dsp48E1_DEP), $(Dsp48E1_INC)))

Dsp48E1_MOD = mkDsp48E1
Dsp48E1_V   = verilog/mkDsp48E1.v
Dsp48E1_BO  = obj/Dsp48E1.bo
Dsp48E1_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/Dsp48E1.bsv
Dsp48E1_DEP = obj/GetPut.bo obj/FIFO.bo obj/FIFOF.bo obj/Clocks.bo obj/ConnectalClocks.bo
Dsp48E1_INC = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/ConnectalProjectConfig.bsv

$(eval $(call BSV_V_RULE, $(Dsp48E1_MOD), $(Dsp48E1_V), $(Dsp48E1_BSV), $(Dsp48E1_DEP), $(Dsp48E1_INC)))

SyncBits_BO  = obj/SyncBits.bo
SyncBits_DEP = obj/Vector.bo obj/Clocks.bo
SyncBits_INC = 
SyncBits_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/SyncBits.bsv

$(eval $(call BSV_BO_RULE, $(SyncBits_BO), $(SyncBits_BSV), $(SyncBits_DEP), $(SyncBits_INC)))

AvalonDdr3Controller_BO  = obj/AvalonDdr3Controller.bo
AvalonDdr3Controller_DEP = obj/Clocks.bo obj/GetPut.bo obj/ClientServer.bo obj/ConnectalClocks.bo obj/ALTERA_DDR3_WRAPPER.bo
AvalonDdr3Controller_INC = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/ConnectalProjectConfig.bsv
AvalonDdr3Controller_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/AvalonDdr3Controller.bsv

$(eval $(call BSV_BO_RULE, $(AvalonDdr3Controller_BO), $(AvalonDdr3Controller_BSV), $(AvalonDdr3Controller_DEP), $(AvalonDdr3Controller_INC)))

AvalonDdr3Controller_MOD = mkDdr3
AvalonDdr3Controller_V   = verilog/mkDdr3.v
AvalonDdr3Controller_BO  = obj/AvalonDdr3Controller.bo
AvalonDdr3Controller_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/AvalonDdr3Controller.bsv
AvalonDdr3Controller_DEP = obj/Clocks.bo obj/GetPut.bo obj/ClientServer.bo obj/ConnectalClocks.bo obj/ALTERA_DDR3_WRAPPER.bo
AvalonDdr3Controller_INC = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/ConnectalProjectConfig.bsv

$(eval $(call BSV_V_RULE, $(AvalonDdr3Controller_MOD), $(AvalonDdr3Controller_V), $(AvalonDdr3Controller_BSV), $(AvalonDdr3Controller_DEP), $(AvalonDdr3Controller_INC)))

GetPutWithClocks_BO  = obj/GetPutWithClocks.bo
GetPutWithClocks_DEP = obj/GetPut.bo obj/Connectable.bo obj/ClientServer.bo obj/Clocks.bo obj/ConnectalMemTypes.bo obj/FIFOF.bo obj/ConnectalBramFifo.bo obj/Pipe.bo obj/Probe.bo obj/SyncAxisFifo32x8.bo obj/AxiStream.bo obj/Vector.bo
GetPutWithClocks_INC = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/ConnectalProjectConfig.bsv
GetPutWithClocks_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/GetPutWithClocks.bsv

$(eval $(call BSV_BO_RULE, $(GetPutWithClocks_BO), $(GetPutWithClocks_BSV), $(GetPutWithClocks_DEP), $(GetPutWithClocks_INC)))

Trace_BO  = obj/Trace.bo
Trace_DEP = obj/BRAM.bo obj/BRAMFIFO.bo obj/ConnectalFIFO.bo obj/DefaultValue.bo obj/FIFOF.bo obj/GetPut.bo obj/ConnectalMemTypes.bo
Trace_INC = 
Trace_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/Trace.bsv

$(eval $(call BSV_BO_RULE, $(Trace_BO), $(Trace_BSV), $(Trace_DEP), $(Trace_INC)))

ConnectalMemory_BO  = obj/ConnectalMemory.bo
ConnectalMemory_DEP = obj/GetPut.bo obj/Vector.bo
ConnectalMemory_INC = 
ConnectalMemory_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/ConnectalMemory.bsv

$(eval $(call BSV_BO_RULE, $(ConnectalMemory_BO), $(ConnectalMemory_BSV), $(ConnectalMemory_DEP), $(ConnectalMemory_INC)))

Pcie3RootPortX7_BO  = obj/Pcie3RootPortX7.bo
Pcie3RootPortX7_DEP = obj/BRAMFIFO.bo obj/Clocks.bo obj/Vector.bo obj/BuildVector.bo obj/Connectable.bo obj/GetPut.bo obj/Reserved.bo obj/TieOff.bo obj/DefaultValue.bo obj/DReg.bo obj/Gearbox.bo obj/FIFO.bo obj/FIFOF.bo obj/CFFIFO.bo obj/SpecialFIFOs.bo obj/ClientServer.bo obj/Real.bo obj/XilinxVirtex7PCIE.bo obj/BUtils.bo obj/Probe.bo obj/ConnectalConfig.bo obj/ConnectalClocks.bo obj/ConnectalXilinxCells.bo obj/XilinxCells.bo obj/PCIE.bo obj/ROOTPCIEWRAPPER3.bo obj/Bufgctrl.bo obj/PcieGearbox.bo obj/Pipe.bo
Pcie3RootPortX7_INC = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/ConnectalProjectConfig.bsv
Pcie3RootPortX7_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/Pcie3RootPortX7.bsv

$(eval $(call BSV_BO_RULE, $(Pcie3RootPortX7_BO), $(Pcie3RootPortX7_BSV), $(Pcie3RootPortX7_DEP), $(Pcie3RootPortX7_INC)))

Pcie3RootPortX7_MOD = mkPcieRootPortX7
Pcie3RootPortX7_V   = verilog/mkPcieRootPortX7.v
Pcie3RootPortX7_BO  = obj/Pcie3RootPortX7.bo
Pcie3RootPortX7_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/Pcie3RootPortX7.bsv
Pcie3RootPortX7_DEP = obj/BRAMFIFO.bo obj/Clocks.bo obj/Vector.bo obj/BuildVector.bo obj/Connectable.bo obj/GetPut.bo obj/Reserved.bo obj/TieOff.bo obj/DefaultValue.bo obj/DReg.bo obj/Gearbox.bo obj/FIFO.bo obj/FIFOF.bo obj/CFFIFO.bo obj/SpecialFIFOs.bo obj/ClientServer.bo obj/Real.bo obj/XilinxVirtex7PCIE.bo obj/BUtils.bo obj/Probe.bo obj/ConnectalConfig.bo obj/ConnectalClocks.bo obj/ConnectalXilinxCells.bo obj/XilinxCells.bo obj/PCIE.bo obj/ROOTPCIEWRAPPER3.bo obj/Bufgctrl.bo obj/PcieGearbox.bo obj/Pipe.bo
Pcie3RootPortX7_INC = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/ConnectalProjectConfig.bsv

$(eval $(call BSV_V_RULE, $(Pcie3RootPortX7_MOD), $(Pcie3RootPortX7_V), $(Pcie3RootPortX7_BSV), $(Pcie3RootPortX7_DEP), $(Pcie3RootPortX7_INC)))

HostInterface_BO  = obj/HostInterface.bo
HostInterface_DEP = obj/ConnectalConfig.bo obj/Vector.bo obj/GetPut.bo obj/ClientServer.bo obj/BRAM.bo obj/PCIE.bo obj/Bscan.bo obj/PcieCsr.bo obj/PcieTracer.bo obj/ConnectalMemTypes.bo obj/Pipe.bo obj/PCIEWRAPPER3u.bo obj/Pcie3EndpointX7.bo
HostInterface_INC = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/ConnectalProjectConfig.bsv
HostInterface_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/HostInterface.bsv

$(eval $(call BSV_BO_RULE, $(HostInterface_BO), $(HostInterface_BSV), $(HostInterface_DEP), $(HostInterface_INC)))

AxiGather_BO  = obj/AxiGather.bo
AxiGather_DEP = obj/GetPut.bo obj/Vector.bo obj/PPS7LIB.bo obj/AxiMasterSlave.bo obj/Axi4MasterSlave.bo obj/AxiBits.bo obj/GetPutM.bo obj/FIFOF.bo
AxiGather_INC = 
AxiGather_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/AxiGather.bsv

$(eval $(call BSV_BO_RULE, $(AxiGather_BO), $(AxiGather_BSV), $(AxiGather_DEP), $(AxiGather_INC)))

CnocPortal_BO  = obj/CnocPortal.bo
CnocPortal_DEP = obj/ConnectalConfig.bo obj/FIFOF.bo obj/Vector.bo obj/ConnectalMemTypes.bo obj/Pipe.bo obj/Portal.bo obj/HostInterface.bo
CnocPortal_INC = 
CnocPortal_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/CnocPortal.bsv

$(eval $(call BSV_BO_RULE, $(CnocPortal_BO), $(CnocPortal_BSV), $(CnocPortal_DEP), $(CnocPortal_INC)))

CtrlMux_BO  = obj/CtrlMux.bo
CtrlMux_DEP = obj/Vector.bo obj/GetPut.bo obj/FIFOF.bo obj/SpecialFIFOs.bo obj/FIFO.bo obj/ConnectalConfig.bo obj/HostInterface.bo obj/Connectable.bo obj/AddressGenerator.bo obj/Portal.bo obj/ConnectalMemTypes.bo obj/Arith.bo obj/Pipe.bo
CtrlMux_INC = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/ConnectalProjectConfig.bsv
CtrlMux_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/CtrlMux.bsv

$(eval $(call BSV_BO_RULE, $(CtrlMux_BO), $(CtrlMux_BSV), $(CtrlMux_DEP), $(CtrlMux_INC)))

ConnectalMemUtils_BO  = obj/ConnectalMemUtils.bo
ConnectalMemUtils_DEP = obj/BRAM.bo obj/BRAMFIFO.bo obj/FIFO.bo obj/Vector.bo obj/Gearbox.bo obj/FIFOF.bo obj/SpecialFIFOs.bo obj/GetPut.bo obj/ConnectalConfig.bo obj/ConnectalMemTypes.bo obj/ConfigCounter.bo obj/BRAMFIFOFLevel.bo
ConnectalMemUtils_INC = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/ConnectalProjectConfig.bsv
ConnectalMemUtils_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/ConnectalMemUtils.bsv

$(eval $(call BSV_BO_RULE, $(ConnectalMemUtils_BO), $(ConnectalMemUtils_BSV), $(ConnectalMemUtils_DEP), $(ConnectalMemUtils_INC)))

ConnectalXilinxCells_BO  = obj/ConnectalXilinxCells.bo
ConnectalXilinxCells_DEP = obj/Clocks.bo obj/DefaultValue.bo obj/XilinxCells.bo obj/Vector.bo
ConnectalXilinxCells_INC = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/ConnectalProjectConfig.bsv
ConnectalXilinxCells_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/ConnectalXilinxCells.bsv

$(eval $(call BSV_BO_RULE, $(ConnectalXilinxCells_BO), $(ConnectalXilinxCells_BSV), $(ConnectalXilinxCells_DEP), $(ConnectalXilinxCells_INC)))

Adapter_BO  = obj/Adapter.bo
Adapter_DEP = obj/GetPut.bo obj/FIFOF.bo obj/SpecialFIFOs.bo obj/StmtFSM.bo obj/Assert.bo obj/Pipe.bo
Adapter_INC = 
Adapter_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/Adapter.bsv

$(eval $(call BSV_BO_RULE, $(Adapter_BO), $(Adapter_BSV), $(Adapter_DEP), $(Adapter_INC)))

Axi4MasterSlave_BO  = obj/Axi4MasterSlave.bo
Axi4MasterSlave_DEP = obj/FIFOF.bo obj/FIFO.bo obj/GetPut.bo obj/Connectable.bo obj/RegFile.bo obj/ConnectalMemTypes.bo
Axi4MasterSlave_INC = 
Axi4MasterSlave_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/Axi4MasterSlave.bsv

$(eval $(call BSV_BO_RULE, $(Axi4MasterSlave_BO), $(Axi4MasterSlave_BSV), $(Axi4MasterSlave_DEP), $(Axi4MasterSlave_INC)))

ConnectalAlteraCells_BO  = obj/ConnectalAlteraCells.bo
ConnectalAlteraCells_DEP = obj/Clocks.bo obj/DefaultValue.bo obj/Vector.bo obj/ConnectalClocks.bo
ConnectalAlteraCells_INC = 
ConnectalAlteraCells_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/ConnectalAlteraCells.bsv

$(eval $(call BSV_BO_RULE, $(ConnectalAlteraCells_BO), $(ConnectalAlteraCells_BSV), $(ConnectalAlteraCells_DEP), $(ConnectalAlteraCells_INC)))

PcieHost_BO  = obj/PcieHost.bo
PcieHost_DEP = obj/Vector.bo obj/BuildVector.bo obj/Clocks.bo obj/GetPut.bo obj/FIFO.bo obj/Connectable.bo obj/ClientServer.bo obj/BRAM.bo obj/DefaultValue.bo obj/ConnectalConfig.bo obj/PcieSplitter.bo obj/PcieTracer.bo obj/Xilinx.bo obj/ConnectalXilinxCells.bo obj/Bscan.bo obj/Portal.bo obj/MemToPcie.bo obj/PcieToMem.bo obj/PcieCsr.bo obj/ConnectalMemTypes.bo obj/Pcie3EndpointX7.bo obj/HostInterface.bo
PcieHost_INC = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/ConnectalProjectConfig.bsv
PcieHost_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/PcieHost.bsv

$(eval $(call BSV_BO_RULE, $(PcieHost_BO), $(PcieHost_BSV), $(PcieHost_DEP), $(PcieHost_INC)))

PcieHost_MOD = mkMemToPcieSynth
PcieHost_V   = verilog/mkMemToPcieSynth.v
PcieHost_BO  = obj/PcieHost.bo
PcieHost_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/PcieHost.bsv
PcieHost_DEP = obj/Vector.bo obj/BuildVector.bo obj/Clocks.bo obj/GetPut.bo obj/FIFO.bo obj/Connectable.bo obj/ClientServer.bo obj/BRAM.bo obj/DefaultValue.bo obj/ConnectalConfig.bo obj/PcieSplitter.bo obj/PcieTracer.bo obj/Xilinx.bo obj/ConnectalXilinxCells.bo obj/Bscan.bo obj/Portal.bo obj/MemToPcie.bo obj/PcieToMem.bo obj/PcieCsr.bo obj/ConnectalMemTypes.bo obj/Pcie3EndpointX7.bo obj/HostInterface.bo
PcieHost_INC = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/ConnectalProjectConfig.bsv

$(eval $(call BSV_V_RULE, $(PcieHost_MOD), $(PcieHost_V), $(PcieHost_BSV), $(PcieHost_DEP), $(PcieHost_INC)))

PcieHost_MOD = mkPcieHost
PcieHost_V   = verilog/mkPcieHost.v
PcieHost_BO  = obj/PcieHost.bo
PcieHost_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/PcieHost.bsv
PcieHost_DEP = obj/Vector.bo obj/BuildVector.bo obj/Clocks.bo obj/GetPut.bo obj/FIFO.bo obj/Connectable.bo obj/ClientServer.bo obj/BRAM.bo obj/DefaultValue.bo obj/ConnectalConfig.bo obj/PcieSplitter.bo obj/PcieTracer.bo obj/Xilinx.bo obj/ConnectalXilinxCells.bo obj/Bscan.bo obj/Portal.bo obj/MemToPcie.bo obj/PcieToMem.bo obj/PcieCsr.bo obj/ConnectalMemTypes.bo obj/Pcie3EndpointX7.bo obj/HostInterface.bo
PcieHost_INC = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/ConnectalProjectConfig.bsv

$(eval $(call BSV_V_RULE, $(PcieHost_MOD), $(PcieHost_V), $(PcieHost_BSV), $(PcieHost_DEP), $(PcieHost_INC)))

AxiMasterSlave_BO  = obj/AxiMasterSlave.bo
AxiMasterSlave_DEP = obj/FIFOF.bo obj/FIFO.bo obj/GetPut.bo obj/GetPutWithClocks.bo obj/Connectable.bo obj/RegFile.bo
AxiMasterSlave_INC = 
AxiMasterSlave_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/AxiMasterSlave.bsv

$(eval $(call BSV_BO_RULE, $(AxiMasterSlave_BO), $(AxiMasterSlave_BSV), $(AxiMasterSlave_DEP), $(AxiMasterSlave_INC)))

MemServerInternal_BO  = obj/MemServerInternal.bo
MemServerInternal_DEP = obj/BRAMFIFO.bo obj/FIFO.bo obj/FIFOF.bo obj/Vector.bo obj/GetPut.bo obj/ClientServer.bo obj/Assert.bo obj/BRAM.bo obj/DefaultValue.bo obj/ConnectalMemTypes.bo obj/ConnectalMemory.bo obj/ConnectalClocks.bo obj/ConnectalMMU.bo obj/ConnectalCompletionBuffer.bo obj/ConnectalConfig.bo obj/RegFile.bo
MemServerInternal_INC = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/ConnectalProjectConfig.bsv
MemServerInternal_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/MemServerInternal.bsv

$(eval $(call BSV_BO_RULE, $(MemServerInternal_BO), $(MemServerInternal_BSV), $(MemServerInternal_DEP), $(MemServerInternal_INC)))

AddressGenerator_BO  = obj/AddressGenerator.bo
AddressGenerator_DEP = obj/FIFOF.bo obj/FIFO.bo obj/GetPut.bo obj/Connectable.bo obj/RegFile.bo obj/ConnectalMemTypes.bo obj/ConnectalConfig.bo
AddressGenerator_INC = 
AddressGenerator_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/AddressGenerator.bsv

$(eval $(call BSV_BO_RULE, $(AddressGenerator_BO), $(AddressGenerator_BSV), $(AddressGenerator_DEP), $(AddressGenerator_INC)))

AvalonDma_BO  = obj/AvalonDma.bo
AvalonDma_DEP = obj/ConnectalConfig.bo obj/FIFO.bo obj/GetPut.bo obj/Vector.bo obj/ClientServer.bo obj/ConnectalMemTypes.bo obj/ConnectalMemory.bo obj/AvalonMasterSlave.bo obj/AvalonBits.bo obj/AddressGenerator.bo obj/AvalonSplitter.bo obj/Connectable.bo
AvalonDma_INC = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/ConnectalProjectConfig.bsv
AvalonDma_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/AvalonDma.bsv

$(eval $(call BSV_BO_RULE, $(AvalonDma_BO), $(AvalonDma_BSV), $(AvalonDma_DEP), $(AvalonDma_INC)))

AsicTop_BO  = obj/AsicTop.bo
AsicTop_DEP = obj/ConnectalConfig.bo obj/Vector.bo obj/GetPut.bo obj/Connectable.bo obj/Portal.bo obj/Top.bo obj/HostInterface.bo obj/Pipe.bo obj/CnocPortal.bo obj/ConnectalMemTypes.bo obj/ConnectalMMU.bo obj/MemServer.bo obj/MMURequest.bo obj/MMUIndication.bo obj/MemServerIndication.bo obj/MemServerRequest.bo obj/SimDma.bo obj/IfcNames.bo obj/BuildVector.bo
AsicTop_INC = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/ConnectalProjectConfig.bsv
AsicTop_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/AsicTop.bsv

$(eval $(call BSV_BO_RULE, $(AsicTop_BO), $(AsicTop_BSV), $(AsicTop_DEP), $(AsicTop_INC)))

TraceMemClient_BO  = obj/TraceMemClient.bo
TraceMemClient_DEP = obj/FIFOF.bo obj/Pipe.bo obj/GetPut.bo obj/ConnectalMemTypes.bo obj/ConnectalMemory.bo
TraceMemClient_INC = 
TraceMemClient_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/TraceMemClient.bsv

$(eval $(call BSV_BO_RULE, $(TraceMemClient_BO), $(TraceMemClient_BSV), $(TraceMemClient_DEP), $(TraceMemClient_INC)))

ConnectalPrelude_BO  = obj/ConnectalPrelude.bo
ConnectalPrelude_DEP = 
ConnectalPrelude_INC = 
ConnectalPrelude_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/ConnectalPrelude.bsv

$(eval $(call BSV_BO_RULE, $(ConnectalPrelude_BO), $(ConnectalPrelude_BSV), $(ConnectalPrelude_DEP), $(ConnectalPrelude_INC)))

ConnectalCompletionBuffer_BO  = obj/ConnectalCompletionBuffer.bo
ConnectalCompletionBuffer_DEP = obj/BRAMFIFO.bo obj/FIFO.bo obj/FIFOF.bo obj/Vector.bo obj/GetPut.bo obj/ClientServer.bo obj/Assert.bo obj/BRAM.bo obj/RegFile.bo obj/ConnectalMemTypes.bo obj/ConnectalMemory.bo obj/ConfigCounter.bo
ConnectalCompletionBuffer_INC = 
ConnectalCompletionBuffer_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/ConnectalCompletionBuffer.bsv

$(eval $(call BSV_BO_RULE, $(ConnectalCompletionBuffer_BO), $(ConnectalCompletionBuffer_BSV), $(ConnectalCompletionBuffer_DEP), $(ConnectalCompletionBuffer_INC)))

PS5LIB_BO  = obj/PS5LIB.bo
PS5LIB_DEP = obj/Clocks.bo obj/Vector.bo obj/Connectable.bo obj/ConnectalAlteraCells.bo obj/ConnectalClocks.bo obj/ALTERA_XCVR_RECONFIG_WRAPPER.bo obj/ALTERA_PCIE_RECONFIG_DRIVER_WRAPPER.bo obj/ALTERA_PCIE_SV_WRAPPER.bo
PS5LIB_INC = 
PS5LIB_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/PS5LIB.bsv

$(eval $(call BSV_BO_RULE, $(PS5LIB_BO), $(PS5LIB_BSV), $(PS5LIB_DEP), $(PS5LIB_INC)))

PS5LIB_MOD = mkAlteraPcieHipRs
PS5LIB_V   = verilog/mkAlteraPcieHipRs.v
PS5LIB_BO  = obj/PS5LIB.bo
PS5LIB_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/PS5LIB.bsv
PS5LIB_DEP = obj/Clocks.bo obj/Vector.bo obj/Connectable.bo obj/ConnectalAlteraCells.bo obj/ConnectalClocks.bo obj/ALTERA_XCVR_RECONFIG_WRAPPER.bo obj/ALTERA_PCIE_RECONFIG_DRIVER_WRAPPER.bo obj/ALTERA_PCIE_SV_WRAPPER.bo
PS5LIB_INC = 

$(eval $(call BSV_V_RULE, $(PS5LIB_MOD), $(PS5LIB_V), $(PS5LIB_BSV), $(PS5LIB_DEP), $(PS5LIB_INC)))

ConnectalBram_BO  = obj/ConnectalBram.bo
ConnectalBram_DEP = obj/FIFO.bo obj/FIFOF.bo obj/Vector.bo obj/BRAM.bo obj/BRAMCore.bo obj/GetPut.bo obj/ClientServer.bo obj/OldEHR.bo obj/ConfigCounter.bo
ConnectalBram_INC = 
ConnectalBram_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/ConnectalBram.bsv

$(eval $(call BSV_BO_RULE, $(ConnectalBram_BO), $(ConnectalBram_BSV), $(ConnectalBram_DEP), $(ConnectalBram_INC)))

PS4LIB_BO  = obj/PS4LIB.bo
PS4LIB_DEP = obj/Clocks.bo obj/Vector.bo obj/Connectable.bo obj/ConnectalAlteraCells.bo obj/ConnectalClocks.bo obj/ALTERA_PCIE_SIV_WRAPPER.bo
PS4LIB_INC = 
PS4LIB_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/PS4LIB.bsv

$(eval $(call BSV_BO_RULE, $(PS4LIB_BO), $(PS4LIB_BSV), $(PS4LIB_DEP), $(PS4LIB_INC)))

ConnectableWithTrace_BO  = obj/ConnectableWithTrace.bo
ConnectableWithTrace_DEP = obj/AxiMasterSlave.bo obj/Connectable.bo obj/BRAM.bo obj/Bscan.bo obj/Vector.bo obj/FIFOF.bo
ConnectableWithTrace_INC = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/ConnectalProjectConfig.bsv
ConnectableWithTrace_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/ConnectableWithTrace.bsv

$(eval $(call BSV_BO_RULE, $(ConnectableWithTrace_BO), $(ConnectableWithTrace_BSV), $(ConnectableWithTrace_DEP), $(ConnectableWithTrace_INC)))

MemServerPortal_BO  = obj/MemServerPortal.bo
MemServerPortal_DEP = obj/Connectable.bo obj/GetPut.bo obj/FIFOF.bo obj/ConfigCounter.bo obj/ConnectalFIFO.bo obj/ConnectalConfig.bo obj/ConnectalMemTypes.bo
MemServerPortal_INC = 
MemServerPortal_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/MemServerPortal.bsv

$(eval $(call BSV_BO_RULE, $(MemServerPortal_BO), $(MemServerPortal_BSV), $(MemServerPortal_DEP), $(MemServerPortal_INC)))

AvalonMasterSlave_BO  = obj/AvalonMasterSlave.bo
AvalonMasterSlave_DEP = obj/GetPut.bo obj/Connectable.bo
AvalonMasterSlave_INC = 
AvalonMasterSlave_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/AvalonMasterSlave.bsv

$(eval $(call BSV_BO_RULE, $(AvalonMasterSlave_BO), $(AvalonMasterSlave_BSV), $(AvalonMasterSlave_DEP), $(AvalonMasterSlave_INC)))

ConnectalBramFifo_BO  = obj/ConnectalBramFifo.bo
ConnectalBramFifo_DEP = obj/Clocks.bo obj/Vector.bo obj/FIFO.bo obj/FIFOF.bo obj/BRAMFIFO.bo obj/CBus.bo obj/Arith.bo obj/ConnectalClocks.bo
ConnectalBramFifo_INC = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/ConnectalProjectConfig.bsv
ConnectalBramFifo_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/ConnectalBramFifo.bsv

$(eval $(call BSV_BO_RULE, $(ConnectalBramFifo_BO), $(ConnectalBramFifo_BSV), $(ConnectalBramFifo_DEP), $(ConnectalBramFifo_INC)))

PhysMemSlaveFromBram_BO  = obj/PhysMemSlaveFromBram.bo
PhysMemSlaveFromBram_DEP = obj/FIFOF.bo obj/FIFO.bo obj/GetPut.bo obj/ConnectalMemTypes.bo obj/AddressGenerator.bo obj/BRAM.bo obj/Memory.bo
PhysMemSlaveFromBram_INC = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/ConnectalProjectConfig.bsv
PhysMemSlaveFromBram_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/PhysMemSlaveFromBram.bsv

$(eval $(call BSV_BO_RULE, $(PhysMemSlaveFromBram_BO), $(PhysMemSlaveFromBram_BSV), $(PhysMemSlaveFromBram_DEP), $(PhysMemSlaveFromBram_INC)))

LinkerLib_BO  = obj/LinkerLib.bo
LinkerLib_DEP = obj/GetPut.bo
LinkerLib_INC = 
LinkerLib_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/LinkerLib.bsv

$(eval $(call BSV_BO_RULE, $(LinkerLib_BO), $(LinkerLib_BSV), $(LinkerLib_DEP), $(LinkerLib_INC)))

MemPipe_BO  = obj/MemPipe.bo
MemPipe_DEP = obj/FIFO.bo obj/FIFOF.bo obj/GetPut.bo obj/ClientServer.bo obj/BRAM.bo obj/Vector.bo obj/Probe.bo obj/ConnectalMemTypes.bo obj/Pipe.bo obj/ConfigCounter.bo
MemPipe_INC = 
MemPipe_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/MemPipe.bsv

$(eval $(call BSV_BO_RULE, $(MemPipe_BO), $(MemPipe_BSV), $(MemPipe_DEP), $(MemPipe_INC)))

Pipe_BO  = obj/Pipe.bo
Pipe_DEP = obj/FIFOF.bo obj/GetPut.bo obj/Connectable.bo obj/Vector.bo obj/BuildVector.bo obj/MIMO.bo obj/DefaultValue.bo obj/Gearbox.bo obj/Clocks.bo obj/AxiStream.bo
Pipe_INC = 
Pipe_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/Pipe.bsv

$(eval $(call BSV_BO_RULE, $(Pipe_BO), $(Pipe_BSV), $(Pipe_DEP), $(Pipe_INC)))

Portal_BO  = obj/Portal.bo
Portal_DEP = obj/ConnectalConfig.bo obj/Vector.bo obj/ConnectalMemTypes.bo obj/Pipe.bo obj/ConnectalMemory.bo obj/HostInterface.bo obj/FlashTop.bo
Portal_INC = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/ConnectalProjectConfig.bsv
Portal_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/Portal.bsv

$(eval $(call BSV_BO_RULE, $(Portal_BO), $(Portal_BSV), $(Portal_DEP), $(Portal_INC)))

ConnectalConfig_BO  = obj/ConnectalConfig.bo
ConnectalConfig_DEP = 
ConnectalConfig_INC = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/ConnectalProjectConfig.bsv
ConnectalConfig_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/ConnectalConfig.bsv

$(eval $(call BSV_BO_RULE, $(ConnectalConfig_BO), $(ConnectalConfig_BSV), $(ConnectalConfig_DEP), $(ConnectalConfig_INC)))

MemWriteEngine_BO  = obj/MemWriteEngine.bo
MemWriteEngine_DEP = obj/Vector.bo obj/BuildVector.bo obj/Cntrs.bo obj/FIFOF.bo obj/FIFO.bo obj/GetPut.bo obj/Connectable.bo obj/BRAMFIFO.bo obj/ConfigCounter.bo obj/ConnectalMemTypes.bo obj/Pipe.bo obj/ConnectalConfig.bo
MemWriteEngine_INC = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/ConnectalProjectConfig.bsv
MemWriteEngine_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/MemWriteEngine.bsv

$(eval $(call BSV_BO_RULE, $(MemWriteEngine_BO), $(MemWriteEngine_BSV), $(MemWriteEngine_DEP), $(MemWriteEngine_INC)))

Platform_BO  = obj/Platform.bo
Platform_DEP = obj/ConnectalConfig.bo obj/Vector.bo obj/BuildVector.bo obj/Portal.bo obj/HostInterface.bo obj/ConnectalMMU.bo obj/MemServer.bo obj/ConnectalMemTypes.bo obj/CtrlMux.bo obj/FIFO.bo obj/GetPut.bo obj/SpecialFIFOs.bo obj/Pipe.bo obj/ConnectalMemory.bo obj/MMURequest.bo obj/MMUIndication.bo obj/MemServerIndication.bo obj/MemServerRequest.bo obj/IfcNames.bo obj/FlashTop.bo
Platform_INC = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/ConnectalProjectConfig.bsv
Platform_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/Platform.bsv

$(eval $(call BSV_BO_RULE, $(Platform_BO), $(Platform_BSV), $(Platform_DEP), $(Platform_INC)))

PcieSplitter_BO  = obj/PcieSplitter.bo
PcieSplitter_DEP = obj/GetPut.bo obj/Connectable.bo obj/Vector.bo obj/FIFO.bo obj/FIFOF.bo obj/Counter.bo obj/PCIE.bo obj/Clocks.bo obj/ClientServer.bo
PcieSplitter_INC = 
PcieSplitter_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/PcieSplitter.bsv

$(eval $(call BSV_BO_RULE, $(PcieSplitter_BO), $(PcieSplitter_BSV), $(PcieSplitter_DEP), $(PcieSplitter_INC)))

PcieSplitter_MOD = mkTLPDispatcher
PcieSplitter_V   = verilog/mkTLPDispatcher.v
PcieSplitter_BO  = obj/PcieSplitter.bo
PcieSplitter_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/PcieSplitter.bsv
PcieSplitter_DEP = obj/GetPut.bo obj/Connectable.bo obj/Vector.bo obj/FIFO.bo obj/FIFOF.bo obj/Counter.bo obj/PCIE.bo obj/Clocks.bo obj/ClientServer.bo
PcieSplitter_INC = 

$(eval $(call BSV_V_RULE, $(PcieSplitter_MOD), $(PcieSplitter_V), $(PcieSplitter_BSV), $(PcieSplitter_DEP), $(PcieSplitter_INC)))

PcieSplitter_MOD = mkTLPArbiter
PcieSplitter_V   = verilog/mkTLPArbiter.v
PcieSplitter_BO  = obj/PcieSplitter.bo
PcieSplitter_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/PcieSplitter.bsv
PcieSplitter_DEP = obj/GetPut.bo obj/Connectable.bo obj/Vector.bo obj/FIFO.bo obj/FIFOF.bo obj/Counter.bo obj/PCIE.bo obj/Clocks.bo obj/ClientServer.bo
PcieSplitter_INC = 

$(eval $(call BSV_V_RULE, $(PcieSplitter_MOD), $(PcieSplitter_V), $(PcieSplitter_BSV), $(PcieSplitter_DEP), $(PcieSplitter_INC)))

MemToPcie_BO  = obj/MemToPcie.bo
MemToPcie_DEP = obj/FIFO.bo obj/FIFOF.bo obj/GetPut.bo obj/Connectable.bo obj/PCIE.bo obj/DefaultValue.bo obj/Vector.bo obj/ClientServer.bo obj/MIMO.bo obj/Probe.bo obj/ConnectalMimo.bo obj/MIFO.bo obj/ConnectalMemTypes.bo obj/ConfigCounter.bo obj/ConnectalConfig.bo
MemToPcie_INC = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/ConnectalProjectConfig.bsv
MemToPcie_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/MemToPcie.bsv

$(eval $(call BSV_BO_RULE, $(MemToPcie_BO), $(MemToPcie_BSV), $(MemToPcie_DEP), $(MemToPcie_INC)))

GearboxGetPut_BO  = obj/GearboxGetPut.bo
GearboxGetPut_DEP = obj/Vector.bo obj/BuildVector.bo obj/GetPut.bo obj/Gearbox.bo
GearboxGetPut_INC = 
GearboxGetPut_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/GearboxGetPut.bsv

$(eval $(call BSV_BO_RULE, $(GearboxGetPut_BO), $(GearboxGetPut_BSV), $(GearboxGetPut_DEP), $(GearboxGetPut_INC)))

MIFO_BO  = obj/MIFO.bo
MIFO_DEP = obj/Vector.bo obj/Arith.bo obj/FIFOF.bo obj/GetPut.bo obj/Pipe.bo obj/MIMO.bo
MIFO_INC = 
MIFO_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/MIFO.bsv

$(eval $(call BSV_BO_RULE, $(MIFO_BO), $(MIFO_BSV), $(MIFO_DEP), $(MIFO_INC)))

SyncAxisFifo32x8_BO  = obj/SyncAxisFifo32x8.bo
SyncAxisFifo32x8_DEP = obj/Clocks.bo obj/ConnectalFIFO.bo obj/DefaultValue.bo obj/FIFOF.bo obj/XilinxCells.bo obj/GetPut.bo obj/Connectable.bo obj/AxiBits.bo obj/AxiStream.bo obj/Vector.bo
SyncAxisFifo32x8_INC = 
SyncAxisFifo32x8_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/SyncAxisFifo32x8.bsv

$(eval $(call BSV_BO_RULE, $(SyncAxisFifo32x8_BO), $(SyncAxisFifo32x8_BSV), $(SyncAxisFifo32x8_DEP), $(SyncAxisFifo32x8_INC)))

UntetheredTop_BO  = obj/UntetheredTop.bo
UntetheredTop_DEP = obj/Vector.bo obj/Clocks.bo obj/GetPut.bo obj/FIFO.bo obj/Connectable.bo obj/ClientServer.bo obj/DefaultValue.bo obj/Real.bo obj/ConnectalConfig.bo obj/Xilinx.bo obj/Portal.bo obj/Top.bo obj/ConnectalMemTypes.bo obj/ConnectalClocks.bo obj/GetPutWithClocks.bo obj/HostInterface.bo obj/FlashTop.bo obj/Platform.bo
UntetheredTop_INC = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/ConnectalProjectConfig.bsv
UntetheredTop_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/UntetheredTop.bsv

$(eval $(call BSV_BO_RULE, $(UntetheredTop_BO), $(UntetheredTop_BSV), $(UntetheredTop_DEP), $(UntetheredTop_INC)))

PcieToMem_BO  = obj/PcieToMem.bo
PcieToMem_DEP = obj/FIFOF.bo obj/Vector.bo obj/GetPut.bo obj/Connectable.bo obj/MIMO.bo obj/PCIE.bo obj/DefaultValue.bo obj/ClientServer.bo obj/ConnectalMemTypes.bo obj/ConnectalConfig.bo
PcieToMem_INC = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/ConnectalProjectConfig.bsv
PcieToMem_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/PcieToMem.bsv

$(eval $(call BSV_BO_RULE, $(PcieToMem_BO), $(PcieToMem_BSV), $(PcieToMem_DEP), $(PcieToMem_INC)))

PcieToMem_MOD = mkPcieToMem
PcieToMem_V   = verilog/mkPcieToMem.v
PcieToMem_BO  = obj/PcieToMem.bo
PcieToMem_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/PcieToMem.bsv
PcieToMem_DEP = obj/FIFOF.bo obj/Vector.bo obj/GetPut.bo obj/Connectable.bo obj/MIMO.bo obj/PCIE.bo obj/DefaultValue.bo obj/ClientServer.bo obj/ConnectalMemTypes.bo obj/ConnectalConfig.bo
PcieToMem_INC = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/ConnectalProjectConfig.bsv

$(eval $(call BSV_V_RULE, $(PcieToMem_MOD), $(PcieToMem_V), $(PcieToMem_BSV), $(PcieToMem_DEP), $(PcieToMem_INC)))

PcieToMem_MOD = mkMemInterrupt
PcieToMem_V   = verilog/mkMemInterrupt.v
PcieToMem_BO  = obj/PcieToMem.bo
PcieToMem_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/PcieToMem.bsv
PcieToMem_DEP = obj/FIFOF.bo obj/Vector.bo obj/GetPut.bo obj/Connectable.bo obj/MIMO.bo obj/PCIE.bo obj/DefaultValue.bo obj/ClientServer.bo obj/ConnectalMemTypes.bo obj/ConnectalConfig.bo
PcieToMem_INC = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/ConnectalProjectConfig.bsv

$(eval $(call BSV_V_RULE, $(PcieToMem_MOD), $(PcieToMem_V), $(PcieToMem_BSV), $(PcieToMem_DEP), $(PcieToMem_INC)))

BramMux_BO  = obj/BramMux.bo
BramMux_DEP = obj/ClientServer.bo obj/BRAM.bo obj/Vector.bo obj/FIFO.bo obj/SpecialFIFOs.bo
BramMux_INC = 
BramMux_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/BramMux.bsv

$(eval $(call BSV_BO_RULE, $(BramMux_BO), $(BramMux_BSV), $(BramMux_DEP), $(BramMux_INC)))

ConnectalMMU_BO  = obj/ConnectalMMU.bo
ConnectalMMU_DEP = obj/RegFile.bo obj/FIFO.bo obj/FIFOF.bo obj/Vector.bo obj/GetPut.bo obj/Connectable.bo obj/BRAMFIFO.bo obj/BRAM.bo obj/Probe.bo obj/ConnectalBram.bo obj/ConnectalMemTypes.bo obj/StmtFSM.bo obj/ClientServer.bo obj/ConnectalConfig.bo obj/ConnectalMemory.bo obj/ConnectalCompletionBuffer.bo obj/Pipe.bo obj/SimDma.bo
ConnectalMMU_INC = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/ConnectalProjectConfig.bsv
ConnectalMMU_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/ConnectalMMU.bsv

$(eval $(call BSV_BO_RULE, $(ConnectalMMU_BO), $(ConnectalMMU_BSV), $(ConnectalMMU_DEP), $(ConnectalMMU_INC)))

ConnectalMMU_MOD = mkMMUSynth
ConnectalMMU_V   = verilog/mkMMUSynth.v
ConnectalMMU_BO  = obj/ConnectalMMU.bo
ConnectalMMU_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/ConnectalMMU.bsv
ConnectalMMU_DEP = obj/RegFile.bo obj/FIFO.bo obj/FIFOF.bo obj/Vector.bo obj/GetPut.bo obj/Connectable.bo obj/BRAMFIFO.bo obj/BRAM.bo obj/Probe.bo obj/ConnectalBram.bo obj/ConnectalMemTypes.bo obj/StmtFSM.bo obj/ClientServer.bo obj/ConnectalConfig.bo obj/ConnectalMemory.bo obj/ConnectalCompletionBuffer.bo obj/Pipe.bo obj/SimDma.bo
ConnectalMMU_INC = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/ConnectalProjectConfig.bsv

$(eval $(call BSV_V_RULE, $(ConnectalMMU_MOD), $(ConnectalMMU_V), $(ConnectalMMU_BSV), $(ConnectalMMU_DEP), $(ConnectalMMU_INC)))

PS7LIB_BO  = obj/PS7LIB.bo
PS7LIB_DEP = obj/BuildVector.bo obj/Clocks.bo obj/DefaultValue.bo obj/GetPut.bo obj/Connectable.bo obj/ConnectableWithTrace.bo obj/Bscan.bo obj/Vector.bo obj/PPS7LIB.bo obj/AxiMasterSlave.bo obj/AxiDma.bo obj/XilinxCells.bo obj/ConnectalXilinxCells.bo obj/ConnectalClocks.bo obj/AxiBits.bo obj/AxiGather.bo
PS7LIB_INC = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/ConnectalProjectConfig.bsv
PS7LIB_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/PS7LIB.bsv

$(eval $(call BSV_BO_RULE, $(PS7LIB_BO), $(PS7LIB_BSV), $(PS7LIB_DEP), $(PS7LIB_INC)))

PcieStateChanges_BO  = obj/PcieStateChanges.bo
PcieStateChanges_DEP = obj/FIFOF.bo obj/Probe.bo obj/Pipe.bo
PcieStateChanges_INC = 
PcieStateChanges_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/PcieStateChanges.bsv

$(eval $(call BSV_BO_RULE, $(PcieStateChanges_BO), $(PcieStateChanges_BSV), $(PcieStateChanges_DEP), $(PcieStateChanges_INC)))

XsimTop_BO  = obj/XsimTop.bo
XsimTop_DEP = obj/ConnectalConfig.bo obj/Vector.bo obj/GetPut.bo obj/Connectable.bo obj/Portal.bo obj/Top.bo obj/HostInterface.bo obj/Pipe.bo obj/CnocPortal.bo obj/ConnectalMemTypes.bo obj/ConnectalMMU.bo obj/MemServer.bo obj/MMURequest.bo obj/MMUIndication.bo obj/MemServerIndication.bo obj/MemServerRequest.bo obj/SimDma.bo obj/IfcNames.bo obj/BuildVector.bo obj/FlashTop.bo
XsimTop_INC = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/ConnectalProjectConfig.bsv
XsimTop_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/XsimTop.bsv

$(eval $(call BSV_BO_RULE, $(XsimTop_BO), $(XsimTop_BSV), $(XsimTop_DEP), $(XsimTop_INC)))

Pcie2EndpointX7_BO  = obj/Pcie2EndpointX7.bo
Pcie2EndpointX7_DEP = obj/ConnectalConfig.bo obj/BRAMFIFO.bo obj/Clocks.bo obj/Vector.bo obj/BuildVector.bo obj/Connectable.bo obj/GetPut.bo obj/Reserved.bo obj/TieOff.bo obj/DefaultValue.bo obj/DReg.bo obj/Gearbox.bo obj/FIFO.bo obj/FIFOF.bo obj/SpecialFIFOs.bo obj/ClientServer.bo obj/Real.bo obj/ConnectalClocks.bo obj/ConnectalXilinxCells.bo obj/XilinxCells.bo obj/PCIE.bo obj/PCIEWRAPPER2.bo obj/Pipe.bo obj/PcieStateChanges.bo obj/Bufgctrl.bo
Pcie2EndpointX7_INC = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/ConnectalProjectConfig.bsv
Pcie2EndpointX7_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/Pcie2EndpointX7.bsv

$(eval $(call BSV_BO_RULE, $(Pcie2EndpointX7_BO), $(Pcie2EndpointX7_BSV), $(Pcie2EndpointX7_DEP), $(Pcie2EndpointX7_INC)))

Pcie2EndpointX7_MOD = mkPcieEndpointX7
Pcie2EndpointX7_V   = verilog/mkPcieEndpointX7.v
Pcie2EndpointX7_BO  = obj/Pcie2EndpointX7.bo
Pcie2EndpointX7_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/Pcie2EndpointX7.bsv
Pcie2EndpointX7_DEP = obj/ConnectalConfig.bo obj/BRAMFIFO.bo obj/Clocks.bo obj/Vector.bo obj/BuildVector.bo obj/Connectable.bo obj/GetPut.bo obj/Reserved.bo obj/TieOff.bo obj/DefaultValue.bo obj/DReg.bo obj/Gearbox.bo obj/FIFO.bo obj/FIFOF.bo obj/SpecialFIFOs.bo obj/ClientServer.bo obj/Real.bo obj/ConnectalClocks.bo obj/ConnectalXilinxCells.bo obj/XilinxCells.bo obj/PCIE.bo obj/PCIEWRAPPER2.bo obj/Pipe.bo obj/PcieStateChanges.bo obj/Bufgctrl.bo
Pcie2EndpointX7_INC = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/ConnectalProjectConfig.bsv

$(eval $(call BSV_V_RULE, $(Pcie2EndpointX7_MOD), $(Pcie2EndpointX7_V), $(Pcie2EndpointX7_BSV), $(Pcie2EndpointX7_DEP), $(Pcie2EndpointX7_INC)))

SimLink_BO  = obj/SimLink.bo
SimLink_DEP = obj/GetPut.bo obj/Connectable.bo obj/FIFOF.bo obj/Pipe.bo
SimLink_INC = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/ConnectalProjectConfig.bsv
SimLink_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/SimLink.bsv

$(eval $(call BSV_BO_RULE, $(SimLink_BO), $(SimLink_BSV), $(SimLink_DEP), $(SimLink_INC)))

AwsF1Top_BO  = obj/AwsF1Top.bo
AwsF1Top_DEP = obj/ConnectalConfig.bo obj/Vector.bo obj/GetPut.bo obj/Connectable.bo obj/Portal.bo obj/Platform.bo obj/Top.bo obj/HostInterface.bo obj/Pipe.bo obj/CnocPortal.bo obj/ConnectalMemTypes.bo obj/ConnectalMMU.bo obj/MemServer.bo obj/MMURequest.bo obj/MMUIndication.bo obj/MemServerIndication.bo obj/MemServerRequest.bo obj/SimDma.bo obj/IfcNames.bo obj/BuildVector.bo obj/Axi4MasterSlave.bo obj/AxiBits.bo obj/AxiDma.bo obj/FIFOF.bo obj/ConnectalFIFO.bo obj/FlashTop.bo
AwsF1Top_INC = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/ConnectalProjectConfig.bsv
AwsF1Top_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/AwsF1Top.bsv

$(eval $(call BSV_BO_RULE, $(AwsF1Top_BO), $(AwsF1Top_BSV), $(AwsF1Top_DEP), $(AwsF1Top_INC)))

AvalonGather_BO  = obj/AvalonGather.bo
AvalonGather_DEP = obj/FIFO.bo obj/GetPut.bo obj/ClientServer.bo obj/AvalonBits.bo obj/AvalonMasterSlave.bo
AvalonGather_INC = 
AvalonGather_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/AvalonGather.bsv

$(eval $(call BSV_BO_RULE, $(AvalonGather_BO), $(AvalonGather_BSV), $(AvalonGather_DEP), $(AvalonGather_INC)))

PcieEndpointS5Test_BO  = obj/PcieEndpointS5Test.bo
PcieEndpointS5Test_DEP = obj/Clocks.bo obj/Connectable.bo obj/ConnectalAlteraCells.bo obj/ALTERA_PCIE_ED_WRAPPER.bo obj/PS5LIB.bo
PcieEndpointS5Test_INC = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/ConnectalProjectConfig.bsv
PcieEndpointS5Test_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/PcieEndpointS5Test.bsv

$(eval $(call BSV_BO_RULE, $(PcieEndpointS5Test_BO), $(PcieEndpointS5Test_BSV), $(PcieEndpointS5Test_DEP), $(PcieEndpointS5Test_INC)))

AxiDdr3Controller_BO  = obj/AxiDdr3Controller.bo
AxiDdr3Controller_DEP = obj/Clocks.bo obj/GetPut.bo obj/ClientServer.bo obj/AxiDdr3Wrapper.bo obj/AxiBits.bo obj/AxiGather.bo obj/Axi4MasterSlave.bo obj/ConnectalClocks.bo
AxiDdr3Controller_INC = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/ConnectalProjectConfig.bsv
AxiDdr3Controller_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/AxiDdr3Controller.bsv

$(eval $(call BSV_BO_RULE, $(AxiDdr3Controller_BO), $(AxiDdr3Controller_BSV), $(AxiDdr3Controller_DEP), $(AxiDdr3Controller_INC)))

AxiDdr3Controller_MOD = mkDdr3
AxiDdr3Controller_V   = verilog/mkDdr3.v
AxiDdr3Controller_BO  = obj/AxiDdr3Controller.bo
AxiDdr3Controller_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/AxiDdr3Controller.bsv
AxiDdr3Controller_DEP = obj/Clocks.bo obj/GetPut.bo obj/ClientServer.bo obj/AxiDdr3Wrapper.bo obj/AxiBits.bo obj/AxiGather.bo obj/Axi4MasterSlave.bo obj/ConnectalClocks.bo
AxiDdr3Controller_INC = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/ConnectalProjectConfig.bsv

$(eval $(call BSV_V_RULE, $(AxiDdr3Controller_MOD), $(AxiDdr3Controller_V), $(AxiDdr3Controller_BSV), $(AxiDdr3Controller_DEP), $(AxiDdr3Controller_INC)))

ConnectalMemTypes_BO  = obj/ConnectalMemTypes.bo
ConnectalMemTypes_DEP = obj/FIFOF.bo obj/Adapter.bo obj/Vector.bo obj/Connectable.bo obj/BRAMFIFO.bo obj/GetPut.bo obj/ClientServer.bo obj/ConnectalConfig.bo obj/Pipe.bo obj/ConnectalMemory.bo obj/DefaultValue.bo obj/AxiStream.bo
ConnectalMemTypes_INC = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/ConnectalProjectConfig.bsv
ConnectalMemTypes_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/ConnectalMemTypes.bsv

$(eval $(call BSV_BO_RULE, $(ConnectalMemTypes_BO), $(ConnectalMemTypes_BSV), $(ConnectalMemTypes_DEP), $(ConnectalMemTypes_INC)))

GetPutM_BO  = obj/GetPutM.bo
GetPutM_DEP = obj/GetPut.bo
GetPutM_INC = 
GetPutM_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/GetPutM.bsv

$(eval $(call BSV_BO_RULE, $(GetPutM_BO), $(GetPutM_BSV), $(GetPutM_DEP), $(GetPutM_INC)))

DisplayInd_BO  = obj/DisplayInd.bo
DisplayInd_DEP = 
DisplayInd_INC = 
DisplayInd_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/bsv/DisplayInd.bsv

$(eval $(call BSV_BO_RULE, $(DisplayInd_BO), $(DisplayInd_BSV), $(DisplayInd_DEP), $(DisplayInd_INC)))

MemServerRequest_BO  = obj/MemServerRequest.bo
MemServerRequest_DEP = obj/FIFO.bo obj/FIFOF.bo obj/GetPut.bo obj/Connectable.bo obj/Clocks.bo obj/FloatingPoint.bo obj/Adapter.bo obj/Leds.bo obj/Vector.bo obj/SpecialFIFOs.bo obj/ConnectalConfig.bo obj/ConnectalMemory.bo obj/Portal.bo obj/CtrlMux.bo obj/ConnectalMemTypes.bo obj/Pipe.bo obj/HostInterface.bo obj/LinkerLib.bo obj/BuildVector.bo obj/Assert.bo obj/ClientServer.bo obj/ClientServerHelper.bo obj/ControllerTypes.bo obj/AuroraCommon.bo obj/AuroraImportFmc1.bo obj/FlashCtrlVirtex.bo obj/FlashCtrlModel.bo obj/MemReadEngine.bo obj/MemWriteEngine.bo obj/FlashTypes.bo obj/RenameTable.bo obj/BRAMFIFOVector.bo obj/BRAM.bo
MemServerRequest_INC = 
MemServerRequest_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/MemServerRequest.bsv

$(eval $(call BSV_BO_RULE, $(MemServerRequest_BO), $(MemServerRequest_BSV), $(MemServerRequest_DEP), $(MemServerRequest_INC)))

MemServerRequest_MOD = mkMemServerRequestInput
MemServerRequest_V   = verilog/mkMemServerRequestInput.v
MemServerRequest_BO  = obj/MemServerRequest.bo
MemServerRequest_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/MemServerRequest.bsv
MemServerRequest_DEP = obj/FIFO.bo obj/FIFOF.bo obj/GetPut.bo obj/Connectable.bo obj/Clocks.bo obj/FloatingPoint.bo obj/Adapter.bo obj/Leds.bo obj/Vector.bo obj/SpecialFIFOs.bo obj/ConnectalConfig.bo obj/ConnectalMemory.bo obj/Portal.bo obj/CtrlMux.bo obj/ConnectalMemTypes.bo obj/Pipe.bo obj/HostInterface.bo obj/LinkerLib.bo obj/BuildVector.bo obj/Assert.bo obj/ClientServer.bo obj/ClientServerHelper.bo obj/ControllerTypes.bo obj/AuroraCommon.bo obj/AuroraImportFmc1.bo obj/FlashCtrlVirtex.bo obj/FlashCtrlModel.bo obj/MemReadEngine.bo obj/MemWriteEngine.bo obj/FlashTypes.bo obj/RenameTable.bo obj/BRAMFIFOVector.bo obj/BRAM.bo
MemServerRequest_INC = 

$(eval $(call BSV_V_RULE, $(MemServerRequest_MOD), $(MemServerRequest_V), $(MemServerRequest_BSV), $(MemServerRequest_DEP), $(MemServerRequest_INC)))

MemServerRequest_MOD = mkMemServerRequestWrapperMemPortalPipes
MemServerRequest_V   = verilog/mkMemServerRequestWrapperMemPortalPipes.v
MemServerRequest_BO  = obj/MemServerRequest.bo
MemServerRequest_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/MemServerRequest.bsv
MemServerRequest_DEP = obj/FIFO.bo obj/FIFOF.bo obj/GetPut.bo obj/Connectable.bo obj/Clocks.bo obj/FloatingPoint.bo obj/Adapter.bo obj/Leds.bo obj/Vector.bo obj/SpecialFIFOs.bo obj/ConnectalConfig.bo obj/ConnectalMemory.bo obj/Portal.bo obj/CtrlMux.bo obj/ConnectalMemTypes.bo obj/Pipe.bo obj/HostInterface.bo obj/LinkerLib.bo obj/BuildVector.bo obj/Assert.bo obj/ClientServer.bo obj/ClientServerHelper.bo obj/ControllerTypes.bo obj/AuroraCommon.bo obj/AuroraImportFmc1.bo obj/FlashCtrlVirtex.bo obj/FlashCtrlModel.bo obj/MemReadEngine.bo obj/MemWriteEngine.bo obj/FlashTypes.bo obj/RenameTable.bo obj/BRAMFIFOVector.bo obj/BRAM.bo
MemServerRequest_INC = 

$(eval $(call BSV_V_RULE, $(MemServerRequest_MOD), $(MemServerRequest_V), $(MemServerRequest_BSV), $(MemServerRequest_DEP), $(MemServerRequest_INC)))

MemServerRequest_MOD = mkMemServerRequestOutputPipes
MemServerRequest_V   = verilog/mkMemServerRequestOutputPipes.v
MemServerRequest_BO  = obj/MemServerRequest.bo
MemServerRequest_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/MemServerRequest.bsv
MemServerRequest_DEP = obj/FIFO.bo obj/FIFOF.bo obj/GetPut.bo obj/Connectable.bo obj/Clocks.bo obj/FloatingPoint.bo obj/Adapter.bo obj/Leds.bo obj/Vector.bo obj/SpecialFIFOs.bo obj/ConnectalConfig.bo obj/ConnectalMemory.bo obj/Portal.bo obj/CtrlMux.bo obj/ConnectalMemTypes.bo obj/Pipe.bo obj/HostInterface.bo obj/LinkerLib.bo obj/BuildVector.bo obj/Assert.bo obj/ClientServer.bo obj/ClientServerHelper.bo obj/ControllerTypes.bo obj/AuroraCommon.bo obj/AuroraImportFmc1.bo obj/FlashCtrlVirtex.bo obj/FlashCtrlModel.bo obj/MemReadEngine.bo obj/MemWriteEngine.bo obj/FlashTypes.bo obj/RenameTable.bo obj/BRAMFIFOVector.bo obj/BRAM.bo
MemServerRequest_INC = 

$(eval $(call BSV_V_RULE, $(MemServerRequest_MOD), $(MemServerRequest_V), $(MemServerRequest_BSV), $(MemServerRequest_DEP), $(MemServerRequest_INC)))

MemServerRequest_MOD = mkMemServerRequestOutput
MemServerRequest_V   = verilog/mkMemServerRequestOutput.v
MemServerRequest_BO  = obj/MemServerRequest.bo
MemServerRequest_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/MemServerRequest.bsv
MemServerRequest_DEP = obj/FIFO.bo obj/FIFOF.bo obj/GetPut.bo obj/Connectable.bo obj/Clocks.bo obj/FloatingPoint.bo obj/Adapter.bo obj/Leds.bo obj/Vector.bo obj/SpecialFIFOs.bo obj/ConnectalConfig.bo obj/ConnectalMemory.bo obj/Portal.bo obj/CtrlMux.bo obj/ConnectalMemTypes.bo obj/Pipe.bo obj/HostInterface.bo obj/LinkerLib.bo obj/BuildVector.bo obj/Assert.bo obj/ClientServer.bo obj/ClientServerHelper.bo obj/ControllerTypes.bo obj/AuroraCommon.bo obj/AuroraImportFmc1.bo obj/FlashCtrlVirtex.bo obj/FlashCtrlModel.bo obj/MemReadEngine.bo obj/MemWriteEngine.bo obj/FlashTypes.bo obj/RenameTable.bo obj/BRAMFIFOVector.bo obj/BRAM.bo
MemServerRequest_INC = 

$(eval $(call BSV_V_RULE, $(MemServerRequest_MOD), $(MemServerRequest_V), $(MemServerRequest_BSV), $(MemServerRequest_DEP), $(MemServerRequest_INC)))

MemServerRequest_MOD = mkMemServerRequestInverter
MemServerRequest_V   = verilog/mkMemServerRequestInverter.v
MemServerRequest_BO  = obj/MemServerRequest.bo
MemServerRequest_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/MemServerRequest.bsv
MemServerRequest_DEP = obj/FIFO.bo obj/FIFOF.bo obj/GetPut.bo obj/Connectable.bo obj/Clocks.bo obj/FloatingPoint.bo obj/Adapter.bo obj/Leds.bo obj/Vector.bo obj/SpecialFIFOs.bo obj/ConnectalConfig.bo obj/ConnectalMemory.bo obj/Portal.bo obj/CtrlMux.bo obj/ConnectalMemTypes.bo obj/Pipe.bo obj/HostInterface.bo obj/LinkerLib.bo obj/BuildVector.bo obj/Assert.bo obj/ClientServer.bo obj/ClientServerHelper.bo obj/ControllerTypes.bo obj/AuroraCommon.bo obj/AuroraImportFmc1.bo obj/FlashCtrlVirtex.bo obj/FlashCtrlModel.bo obj/MemReadEngine.bo obj/MemWriteEngine.bo obj/FlashTypes.bo obj/RenameTable.bo obj/BRAMFIFOVector.bo obj/BRAM.bo
MemServerRequest_INC = 

$(eval $(call BSV_V_RULE, $(MemServerRequest_MOD), $(MemServerRequest_V), $(MemServerRequest_BSV), $(MemServerRequest_DEP), $(MemServerRequest_INC)))

MemServerRequest_MOD = mkMemServerRequestInverterV
MemServerRequest_V   = verilog/mkMemServerRequestInverterV.v
MemServerRequest_BO  = obj/MemServerRequest.bo
MemServerRequest_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/MemServerRequest.bsv
MemServerRequest_DEP = obj/FIFO.bo obj/FIFOF.bo obj/GetPut.bo obj/Connectable.bo obj/Clocks.bo obj/FloatingPoint.bo obj/Adapter.bo obj/Leds.bo obj/Vector.bo obj/SpecialFIFOs.bo obj/ConnectalConfig.bo obj/ConnectalMemory.bo obj/Portal.bo obj/CtrlMux.bo obj/ConnectalMemTypes.bo obj/Pipe.bo obj/HostInterface.bo obj/LinkerLib.bo obj/BuildVector.bo obj/Assert.bo obj/ClientServer.bo obj/ClientServerHelper.bo obj/ControllerTypes.bo obj/AuroraCommon.bo obj/AuroraImportFmc1.bo obj/FlashCtrlVirtex.bo obj/FlashCtrlModel.bo obj/MemReadEngine.bo obj/MemWriteEngine.bo obj/FlashTypes.bo obj/RenameTable.bo obj/BRAMFIFOVector.bo obj/BRAM.bo
MemServerRequest_INC = 

$(eval $(call BSV_V_RULE, $(MemServerRequest_MOD), $(MemServerRequest_V), $(MemServerRequest_BSV), $(MemServerRequest_DEP), $(MemServerRequest_INC)))

MemServerRequest_MOD = mkMemServerRequestProxySynth
MemServerRequest_V   = verilog/mkMemServerRequestProxySynth.v
MemServerRequest_BO  = obj/MemServerRequest.bo
MemServerRequest_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/MemServerRequest.bsv
MemServerRequest_DEP = obj/FIFO.bo obj/FIFOF.bo obj/GetPut.bo obj/Connectable.bo obj/Clocks.bo obj/FloatingPoint.bo obj/Adapter.bo obj/Leds.bo obj/Vector.bo obj/SpecialFIFOs.bo obj/ConnectalConfig.bo obj/ConnectalMemory.bo obj/Portal.bo obj/CtrlMux.bo obj/ConnectalMemTypes.bo obj/Pipe.bo obj/HostInterface.bo obj/LinkerLib.bo obj/BuildVector.bo obj/Assert.bo obj/ClientServer.bo obj/ClientServerHelper.bo obj/ControllerTypes.bo obj/AuroraCommon.bo obj/AuroraImportFmc1.bo obj/FlashCtrlVirtex.bo obj/FlashCtrlModel.bo obj/MemReadEngine.bo obj/MemWriteEngine.bo obj/FlashTypes.bo obj/RenameTable.bo obj/BRAMFIFOVector.bo obj/BRAM.bo
MemServerRequest_INC = 

$(eval $(call BSV_V_RULE, $(MemServerRequest_MOD), $(MemServerRequest_V), $(MemServerRequest_BSV), $(MemServerRequest_DEP), $(MemServerRequest_INC)))

FlashRequest_BO  = obj/FlashRequest.bo
FlashRequest_DEP = obj/FIFO.bo obj/FIFOF.bo obj/GetPut.bo obj/Connectable.bo obj/Clocks.bo obj/FloatingPoint.bo obj/Adapter.bo obj/Leds.bo obj/Vector.bo obj/SpecialFIFOs.bo obj/ConnectalConfig.bo obj/ConnectalMemory.bo obj/Portal.bo obj/CtrlMux.bo obj/ConnectalMemTypes.bo obj/Pipe.bo obj/HostInterface.bo obj/LinkerLib.bo obj/FlashTop.bo obj/BuildVector.bo obj/Assert.bo obj/ClientServer.bo obj/ClientServerHelper.bo obj/ControllerTypes.bo obj/AuroraCommon.bo obj/AuroraImportFmc1.bo obj/FlashCtrlVirtex.bo obj/FlashCtrlModel.bo obj/MemReadEngine.bo obj/MemWriteEngine.bo obj/FlashTypes.bo obj/RenameTable.bo obj/BRAMFIFOVector.bo obj/BRAM.bo
FlashRequest_INC = 
FlashRequest_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/FlashRequest.bsv

$(eval $(call BSV_BO_RULE, $(FlashRequest_BO), $(FlashRequest_BSV), $(FlashRequest_DEP), $(FlashRequest_INC)))

FlashRequest_MOD = mkFlashRequestInput
FlashRequest_V   = verilog/mkFlashRequestInput.v
FlashRequest_BO  = obj/FlashRequest.bo
FlashRequest_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/FlashRequest.bsv
FlashRequest_DEP = obj/FIFO.bo obj/FIFOF.bo obj/GetPut.bo obj/Connectable.bo obj/Clocks.bo obj/FloatingPoint.bo obj/Adapter.bo obj/Leds.bo obj/Vector.bo obj/SpecialFIFOs.bo obj/ConnectalConfig.bo obj/ConnectalMemory.bo obj/Portal.bo obj/CtrlMux.bo obj/ConnectalMemTypes.bo obj/Pipe.bo obj/HostInterface.bo obj/LinkerLib.bo obj/FlashTop.bo obj/BuildVector.bo obj/Assert.bo obj/ClientServer.bo obj/ClientServerHelper.bo obj/ControllerTypes.bo obj/AuroraCommon.bo obj/AuroraImportFmc1.bo obj/FlashCtrlVirtex.bo obj/FlashCtrlModel.bo obj/MemReadEngine.bo obj/MemWriteEngine.bo obj/FlashTypes.bo obj/RenameTable.bo obj/BRAMFIFOVector.bo obj/BRAM.bo
FlashRequest_INC = 

$(eval $(call BSV_V_RULE, $(FlashRequest_MOD), $(FlashRequest_V), $(FlashRequest_BSV), $(FlashRequest_DEP), $(FlashRequest_INC)))

FlashRequest_MOD = mkFlashRequestWrapperMemPortalPipes
FlashRequest_V   = verilog/mkFlashRequestWrapperMemPortalPipes.v
FlashRequest_BO  = obj/FlashRequest.bo
FlashRequest_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/FlashRequest.bsv
FlashRequest_DEP = obj/FIFO.bo obj/FIFOF.bo obj/GetPut.bo obj/Connectable.bo obj/Clocks.bo obj/FloatingPoint.bo obj/Adapter.bo obj/Leds.bo obj/Vector.bo obj/SpecialFIFOs.bo obj/ConnectalConfig.bo obj/ConnectalMemory.bo obj/Portal.bo obj/CtrlMux.bo obj/ConnectalMemTypes.bo obj/Pipe.bo obj/HostInterface.bo obj/LinkerLib.bo obj/FlashTop.bo obj/BuildVector.bo obj/Assert.bo obj/ClientServer.bo obj/ClientServerHelper.bo obj/ControllerTypes.bo obj/AuroraCommon.bo obj/AuroraImportFmc1.bo obj/FlashCtrlVirtex.bo obj/FlashCtrlModel.bo obj/MemReadEngine.bo obj/MemWriteEngine.bo obj/FlashTypes.bo obj/RenameTable.bo obj/BRAMFIFOVector.bo obj/BRAM.bo
FlashRequest_INC = 

$(eval $(call BSV_V_RULE, $(FlashRequest_MOD), $(FlashRequest_V), $(FlashRequest_BSV), $(FlashRequest_DEP), $(FlashRequest_INC)))

FlashRequest_MOD = mkFlashRequestOutputPipes
FlashRequest_V   = verilog/mkFlashRequestOutputPipes.v
FlashRequest_BO  = obj/FlashRequest.bo
FlashRequest_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/FlashRequest.bsv
FlashRequest_DEP = obj/FIFO.bo obj/FIFOF.bo obj/GetPut.bo obj/Connectable.bo obj/Clocks.bo obj/FloatingPoint.bo obj/Adapter.bo obj/Leds.bo obj/Vector.bo obj/SpecialFIFOs.bo obj/ConnectalConfig.bo obj/ConnectalMemory.bo obj/Portal.bo obj/CtrlMux.bo obj/ConnectalMemTypes.bo obj/Pipe.bo obj/HostInterface.bo obj/LinkerLib.bo obj/FlashTop.bo obj/BuildVector.bo obj/Assert.bo obj/ClientServer.bo obj/ClientServerHelper.bo obj/ControllerTypes.bo obj/AuroraCommon.bo obj/AuroraImportFmc1.bo obj/FlashCtrlVirtex.bo obj/FlashCtrlModel.bo obj/MemReadEngine.bo obj/MemWriteEngine.bo obj/FlashTypes.bo obj/RenameTable.bo obj/BRAMFIFOVector.bo obj/BRAM.bo
FlashRequest_INC = 

$(eval $(call BSV_V_RULE, $(FlashRequest_MOD), $(FlashRequest_V), $(FlashRequest_BSV), $(FlashRequest_DEP), $(FlashRequest_INC)))

FlashRequest_MOD = mkFlashRequestOutput
FlashRequest_V   = verilog/mkFlashRequestOutput.v
FlashRequest_BO  = obj/FlashRequest.bo
FlashRequest_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/FlashRequest.bsv
FlashRequest_DEP = obj/FIFO.bo obj/FIFOF.bo obj/GetPut.bo obj/Connectable.bo obj/Clocks.bo obj/FloatingPoint.bo obj/Adapter.bo obj/Leds.bo obj/Vector.bo obj/SpecialFIFOs.bo obj/ConnectalConfig.bo obj/ConnectalMemory.bo obj/Portal.bo obj/CtrlMux.bo obj/ConnectalMemTypes.bo obj/Pipe.bo obj/HostInterface.bo obj/LinkerLib.bo obj/FlashTop.bo obj/BuildVector.bo obj/Assert.bo obj/ClientServer.bo obj/ClientServerHelper.bo obj/ControllerTypes.bo obj/AuroraCommon.bo obj/AuroraImportFmc1.bo obj/FlashCtrlVirtex.bo obj/FlashCtrlModel.bo obj/MemReadEngine.bo obj/MemWriteEngine.bo obj/FlashTypes.bo obj/RenameTable.bo obj/BRAMFIFOVector.bo obj/BRAM.bo
FlashRequest_INC = 

$(eval $(call BSV_V_RULE, $(FlashRequest_MOD), $(FlashRequest_V), $(FlashRequest_BSV), $(FlashRequest_DEP), $(FlashRequest_INC)))

FlashRequest_MOD = mkFlashRequestInverter
FlashRequest_V   = verilog/mkFlashRequestInverter.v
FlashRequest_BO  = obj/FlashRequest.bo
FlashRequest_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/FlashRequest.bsv
FlashRequest_DEP = obj/FIFO.bo obj/FIFOF.bo obj/GetPut.bo obj/Connectable.bo obj/Clocks.bo obj/FloatingPoint.bo obj/Adapter.bo obj/Leds.bo obj/Vector.bo obj/SpecialFIFOs.bo obj/ConnectalConfig.bo obj/ConnectalMemory.bo obj/Portal.bo obj/CtrlMux.bo obj/ConnectalMemTypes.bo obj/Pipe.bo obj/HostInterface.bo obj/LinkerLib.bo obj/FlashTop.bo obj/BuildVector.bo obj/Assert.bo obj/ClientServer.bo obj/ClientServerHelper.bo obj/ControllerTypes.bo obj/AuroraCommon.bo obj/AuroraImportFmc1.bo obj/FlashCtrlVirtex.bo obj/FlashCtrlModel.bo obj/MemReadEngine.bo obj/MemWriteEngine.bo obj/FlashTypes.bo obj/RenameTable.bo obj/BRAMFIFOVector.bo obj/BRAM.bo
FlashRequest_INC = 

$(eval $(call BSV_V_RULE, $(FlashRequest_MOD), $(FlashRequest_V), $(FlashRequest_BSV), $(FlashRequest_DEP), $(FlashRequest_INC)))

FlashRequest_MOD = mkFlashRequestInverterV
FlashRequest_V   = verilog/mkFlashRequestInverterV.v
FlashRequest_BO  = obj/FlashRequest.bo
FlashRequest_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/FlashRequest.bsv
FlashRequest_DEP = obj/FIFO.bo obj/FIFOF.bo obj/GetPut.bo obj/Connectable.bo obj/Clocks.bo obj/FloatingPoint.bo obj/Adapter.bo obj/Leds.bo obj/Vector.bo obj/SpecialFIFOs.bo obj/ConnectalConfig.bo obj/ConnectalMemory.bo obj/Portal.bo obj/CtrlMux.bo obj/ConnectalMemTypes.bo obj/Pipe.bo obj/HostInterface.bo obj/LinkerLib.bo obj/FlashTop.bo obj/BuildVector.bo obj/Assert.bo obj/ClientServer.bo obj/ClientServerHelper.bo obj/ControllerTypes.bo obj/AuroraCommon.bo obj/AuroraImportFmc1.bo obj/FlashCtrlVirtex.bo obj/FlashCtrlModel.bo obj/MemReadEngine.bo obj/MemWriteEngine.bo obj/FlashTypes.bo obj/RenameTable.bo obj/BRAMFIFOVector.bo obj/BRAM.bo
FlashRequest_INC = 

$(eval $(call BSV_V_RULE, $(FlashRequest_MOD), $(FlashRequest_V), $(FlashRequest_BSV), $(FlashRequest_DEP), $(FlashRequest_INC)))

FlashRequest_MOD = mkFlashRequestProxySynth
FlashRequest_V   = verilog/mkFlashRequestProxySynth.v
FlashRequest_BO  = obj/FlashRequest.bo
FlashRequest_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/FlashRequest.bsv
FlashRequest_DEP = obj/FIFO.bo obj/FIFOF.bo obj/GetPut.bo obj/Connectable.bo obj/Clocks.bo obj/FloatingPoint.bo obj/Adapter.bo obj/Leds.bo obj/Vector.bo obj/SpecialFIFOs.bo obj/ConnectalConfig.bo obj/ConnectalMemory.bo obj/Portal.bo obj/CtrlMux.bo obj/ConnectalMemTypes.bo obj/Pipe.bo obj/HostInterface.bo obj/LinkerLib.bo obj/FlashTop.bo obj/BuildVector.bo obj/Assert.bo obj/ClientServer.bo obj/ClientServerHelper.bo obj/ControllerTypes.bo obj/AuroraCommon.bo obj/AuroraImportFmc1.bo obj/FlashCtrlVirtex.bo obj/FlashCtrlModel.bo obj/MemReadEngine.bo obj/MemWriteEngine.bo obj/FlashTypes.bo obj/RenameTable.bo obj/BRAMFIFOVector.bo obj/BRAM.bo
FlashRequest_INC = 

$(eval $(call BSV_V_RULE, $(FlashRequest_MOD), $(FlashRequest_V), $(FlashRequest_BSV), $(FlashRequest_DEP), $(FlashRequest_INC)))

MemServerIndication_BO  = obj/MemServerIndication.bo
MemServerIndication_DEP = obj/FIFO.bo obj/FIFOF.bo obj/GetPut.bo obj/Connectable.bo obj/Clocks.bo obj/FloatingPoint.bo obj/Adapter.bo obj/Leds.bo obj/Vector.bo obj/SpecialFIFOs.bo obj/ConnectalConfig.bo obj/ConnectalMemory.bo obj/Portal.bo obj/CtrlMux.bo obj/ConnectalMemTypes.bo obj/Pipe.bo obj/HostInterface.bo obj/LinkerLib.bo obj/BuildVector.bo obj/Assert.bo obj/ClientServer.bo obj/ClientServerHelper.bo obj/ControllerTypes.bo obj/AuroraCommon.bo obj/AuroraImportFmc1.bo obj/FlashCtrlVirtex.bo obj/FlashCtrlModel.bo obj/MemReadEngine.bo obj/MemWriteEngine.bo obj/FlashTypes.bo obj/RenameTable.bo obj/BRAMFIFOVector.bo obj/BRAM.bo
MemServerIndication_INC = 
MemServerIndication_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/MemServerIndication.bsv

$(eval $(call BSV_BO_RULE, $(MemServerIndication_BO), $(MemServerIndication_BSV), $(MemServerIndication_DEP), $(MemServerIndication_INC)))

MemServerIndication_MOD = mkMemServerIndicationInput
MemServerIndication_V   = verilog/mkMemServerIndicationInput.v
MemServerIndication_BO  = obj/MemServerIndication.bo
MemServerIndication_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/MemServerIndication.bsv
MemServerIndication_DEP = obj/FIFO.bo obj/FIFOF.bo obj/GetPut.bo obj/Connectable.bo obj/Clocks.bo obj/FloatingPoint.bo obj/Adapter.bo obj/Leds.bo obj/Vector.bo obj/SpecialFIFOs.bo obj/ConnectalConfig.bo obj/ConnectalMemory.bo obj/Portal.bo obj/CtrlMux.bo obj/ConnectalMemTypes.bo obj/Pipe.bo obj/HostInterface.bo obj/LinkerLib.bo obj/BuildVector.bo obj/Assert.bo obj/ClientServer.bo obj/ClientServerHelper.bo obj/ControllerTypes.bo obj/AuroraCommon.bo obj/AuroraImportFmc1.bo obj/FlashCtrlVirtex.bo obj/FlashCtrlModel.bo obj/MemReadEngine.bo obj/MemWriteEngine.bo obj/FlashTypes.bo obj/RenameTable.bo obj/BRAMFIFOVector.bo obj/BRAM.bo
MemServerIndication_INC = 

$(eval $(call BSV_V_RULE, $(MemServerIndication_MOD), $(MemServerIndication_V), $(MemServerIndication_BSV), $(MemServerIndication_DEP), $(MemServerIndication_INC)))

MemServerIndication_MOD = mkMemServerIndicationWrapperMemPortalPipes
MemServerIndication_V   = verilog/mkMemServerIndicationWrapperMemPortalPipes.v
MemServerIndication_BO  = obj/MemServerIndication.bo
MemServerIndication_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/MemServerIndication.bsv
MemServerIndication_DEP = obj/FIFO.bo obj/FIFOF.bo obj/GetPut.bo obj/Connectable.bo obj/Clocks.bo obj/FloatingPoint.bo obj/Adapter.bo obj/Leds.bo obj/Vector.bo obj/SpecialFIFOs.bo obj/ConnectalConfig.bo obj/ConnectalMemory.bo obj/Portal.bo obj/CtrlMux.bo obj/ConnectalMemTypes.bo obj/Pipe.bo obj/HostInterface.bo obj/LinkerLib.bo obj/BuildVector.bo obj/Assert.bo obj/ClientServer.bo obj/ClientServerHelper.bo obj/ControllerTypes.bo obj/AuroraCommon.bo obj/AuroraImportFmc1.bo obj/FlashCtrlVirtex.bo obj/FlashCtrlModel.bo obj/MemReadEngine.bo obj/MemWriteEngine.bo obj/FlashTypes.bo obj/RenameTable.bo obj/BRAMFIFOVector.bo obj/BRAM.bo
MemServerIndication_INC = 

$(eval $(call BSV_V_RULE, $(MemServerIndication_MOD), $(MemServerIndication_V), $(MemServerIndication_BSV), $(MemServerIndication_DEP), $(MemServerIndication_INC)))

MemServerIndication_MOD = mkMemServerIndicationOutputPipes
MemServerIndication_V   = verilog/mkMemServerIndicationOutputPipes.v
MemServerIndication_BO  = obj/MemServerIndication.bo
MemServerIndication_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/MemServerIndication.bsv
MemServerIndication_DEP = obj/FIFO.bo obj/FIFOF.bo obj/GetPut.bo obj/Connectable.bo obj/Clocks.bo obj/FloatingPoint.bo obj/Adapter.bo obj/Leds.bo obj/Vector.bo obj/SpecialFIFOs.bo obj/ConnectalConfig.bo obj/ConnectalMemory.bo obj/Portal.bo obj/CtrlMux.bo obj/ConnectalMemTypes.bo obj/Pipe.bo obj/HostInterface.bo obj/LinkerLib.bo obj/BuildVector.bo obj/Assert.bo obj/ClientServer.bo obj/ClientServerHelper.bo obj/ControllerTypes.bo obj/AuroraCommon.bo obj/AuroraImportFmc1.bo obj/FlashCtrlVirtex.bo obj/FlashCtrlModel.bo obj/MemReadEngine.bo obj/MemWriteEngine.bo obj/FlashTypes.bo obj/RenameTable.bo obj/BRAMFIFOVector.bo obj/BRAM.bo
MemServerIndication_INC = 

$(eval $(call BSV_V_RULE, $(MemServerIndication_MOD), $(MemServerIndication_V), $(MemServerIndication_BSV), $(MemServerIndication_DEP), $(MemServerIndication_INC)))

MemServerIndication_MOD = mkMemServerIndicationOutput
MemServerIndication_V   = verilog/mkMemServerIndicationOutput.v
MemServerIndication_BO  = obj/MemServerIndication.bo
MemServerIndication_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/MemServerIndication.bsv
MemServerIndication_DEP = obj/FIFO.bo obj/FIFOF.bo obj/GetPut.bo obj/Connectable.bo obj/Clocks.bo obj/FloatingPoint.bo obj/Adapter.bo obj/Leds.bo obj/Vector.bo obj/SpecialFIFOs.bo obj/ConnectalConfig.bo obj/ConnectalMemory.bo obj/Portal.bo obj/CtrlMux.bo obj/ConnectalMemTypes.bo obj/Pipe.bo obj/HostInterface.bo obj/LinkerLib.bo obj/BuildVector.bo obj/Assert.bo obj/ClientServer.bo obj/ClientServerHelper.bo obj/ControllerTypes.bo obj/AuroraCommon.bo obj/AuroraImportFmc1.bo obj/FlashCtrlVirtex.bo obj/FlashCtrlModel.bo obj/MemReadEngine.bo obj/MemWriteEngine.bo obj/FlashTypes.bo obj/RenameTable.bo obj/BRAMFIFOVector.bo obj/BRAM.bo
MemServerIndication_INC = 

$(eval $(call BSV_V_RULE, $(MemServerIndication_MOD), $(MemServerIndication_V), $(MemServerIndication_BSV), $(MemServerIndication_DEP), $(MemServerIndication_INC)))

MemServerIndication_MOD = mkMemServerIndicationInverter
MemServerIndication_V   = verilog/mkMemServerIndicationInverter.v
MemServerIndication_BO  = obj/MemServerIndication.bo
MemServerIndication_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/MemServerIndication.bsv
MemServerIndication_DEP = obj/FIFO.bo obj/FIFOF.bo obj/GetPut.bo obj/Connectable.bo obj/Clocks.bo obj/FloatingPoint.bo obj/Adapter.bo obj/Leds.bo obj/Vector.bo obj/SpecialFIFOs.bo obj/ConnectalConfig.bo obj/ConnectalMemory.bo obj/Portal.bo obj/CtrlMux.bo obj/ConnectalMemTypes.bo obj/Pipe.bo obj/HostInterface.bo obj/LinkerLib.bo obj/BuildVector.bo obj/Assert.bo obj/ClientServer.bo obj/ClientServerHelper.bo obj/ControllerTypes.bo obj/AuroraCommon.bo obj/AuroraImportFmc1.bo obj/FlashCtrlVirtex.bo obj/FlashCtrlModel.bo obj/MemReadEngine.bo obj/MemWriteEngine.bo obj/FlashTypes.bo obj/RenameTable.bo obj/BRAMFIFOVector.bo obj/BRAM.bo
MemServerIndication_INC = 

$(eval $(call BSV_V_RULE, $(MemServerIndication_MOD), $(MemServerIndication_V), $(MemServerIndication_BSV), $(MemServerIndication_DEP), $(MemServerIndication_INC)))

MemServerIndication_MOD = mkMemServerIndicationInverterV
MemServerIndication_V   = verilog/mkMemServerIndicationInverterV.v
MemServerIndication_BO  = obj/MemServerIndication.bo
MemServerIndication_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/MemServerIndication.bsv
MemServerIndication_DEP = obj/FIFO.bo obj/FIFOF.bo obj/GetPut.bo obj/Connectable.bo obj/Clocks.bo obj/FloatingPoint.bo obj/Adapter.bo obj/Leds.bo obj/Vector.bo obj/SpecialFIFOs.bo obj/ConnectalConfig.bo obj/ConnectalMemory.bo obj/Portal.bo obj/CtrlMux.bo obj/ConnectalMemTypes.bo obj/Pipe.bo obj/HostInterface.bo obj/LinkerLib.bo obj/BuildVector.bo obj/Assert.bo obj/ClientServer.bo obj/ClientServerHelper.bo obj/ControllerTypes.bo obj/AuroraCommon.bo obj/AuroraImportFmc1.bo obj/FlashCtrlVirtex.bo obj/FlashCtrlModel.bo obj/MemReadEngine.bo obj/MemWriteEngine.bo obj/FlashTypes.bo obj/RenameTable.bo obj/BRAMFIFOVector.bo obj/BRAM.bo
MemServerIndication_INC = 

$(eval $(call BSV_V_RULE, $(MemServerIndication_MOD), $(MemServerIndication_V), $(MemServerIndication_BSV), $(MemServerIndication_DEP), $(MemServerIndication_INC)))

MemServerIndication_MOD = mkMemServerIndicationProxySynth
MemServerIndication_V   = verilog/mkMemServerIndicationProxySynth.v
MemServerIndication_BO  = obj/MemServerIndication.bo
MemServerIndication_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/MemServerIndication.bsv
MemServerIndication_DEP = obj/FIFO.bo obj/FIFOF.bo obj/GetPut.bo obj/Connectable.bo obj/Clocks.bo obj/FloatingPoint.bo obj/Adapter.bo obj/Leds.bo obj/Vector.bo obj/SpecialFIFOs.bo obj/ConnectalConfig.bo obj/ConnectalMemory.bo obj/Portal.bo obj/CtrlMux.bo obj/ConnectalMemTypes.bo obj/Pipe.bo obj/HostInterface.bo obj/LinkerLib.bo obj/BuildVector.bo obj/Assert.bo obj/ClientServer.bo obj/ClientServerHelper.bo obj/ControllerTypes.bo obj/AuroraCommon.bo obj/AuroraImportFmc1.bo obj/FlashCtrlVirtex.bo obj/FlashCtrlModel.bo obj/MemReadEngine.bo obj/MemWriteEngine.bo obj/FlashTypes.bo obj/RenameTable.bo obj/BRAMFIFOVector.bo obj/BRAM.bo
MemServerIndication_INC = 

$(eval $(call BSV_V_RULE, $(MemServerIndication_MOD), $(MemServerIndication_V), $(MemServerIndication_BSV), $(MemServerIndication_DEP), $(MemServerIndication_INC)))

MMUIndication_BO  = obj/MMUIndication.bo
MMUIndication_DEP = obj/FIFO.bo obj/FIFOF.bo obj/GetPut.bo obj/Connectable.bo obj/Clocks.bo obj/FloatingPoint.bo obj/Adapter.bo obj/Leds.bo obj/Vector.bo obj/SpecialFIFOs.bo obj/ConnectalConfig.bo obj/ConnectalMemory.bo obj/Portal.bo obj/CtrlMux.bo obj/ConnectalMemTypes.bo obj/Pipe.bo obj/HostInterface.bo obj/LinkerLib.bo obj/BuildVector.bo obj/Assert.bo obj/ClientServer.bo obj/ClientServerHelper.bo obj/ControllerTypes.bo obj/AuroraCommon.bo obj/AuroraImportFmc1.bo obj/FlashCtrlVirtex.bo obj/FlashCtrlModel.bo obj/MemReadEngine.bo obj/MemWriteEngine.bo obj/FlashTypes.bo obj/RenameTable.bo obj/BRAMFIFOVector.bo obj/BRAM.bo
MMUIndication_INC = 
MMUIndication_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/MMUIndication.bsv

$(eval $(call BSV_BO_RULE, $(MMUIndication_BO), $(MMUIndication_BSV), $(MMUIndication_DEP), $(MMUIndication_INC)))

MMUIndication_MOD = mkMMUIndicationInput
MMUIndication_V   = verilog/mkMMUIndicationInput.v
MMUIndication_BO  = obj/MMUIndication.bo
MMUIndication_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/MMUIndication.bsv
MMUIndication_DEP = obj/FIFO.bo obj/FIFOF.bo obj/GetPut.bo obj/Connectable.bo obj/Clocks.bo obj/FloatingPoint.bo obj/Adapter.bo obj/Leds.bo obj/Vector.bo obj/SpecialFIFOs.bo obj/ConnectalConfig.bo obj/ConnectalMemory.bo obj/Portal.bo obj/CtrlMux.bo obj/ConnectalMemTypes.bo obj/Pipe.bo obj/HostInterface.bo obj/LinkerLib.bo obj/BuildVector.bo obj/Assert.bo obj/ClientServer.bo obj/ClientServerHelper.bo obj/ControllerTypes.bo obj/AuroraCommon.bo obj/AuroraImportFmc1.bo obj/FlashCtrlVirtex.bo obj/FlashCtrlModel.bo obj/MemReadEngine.bo obj/MemWriteEngine.bo obj/FlashTypes.bo obj/RenameTable.bo obj/BRAMFIFOVector.bo obj/BRAM.bo
MMUIndication_INC = 

$(eval $(call BSV_V_RULE, $(MMUIndication_MOD), $(MMUIndication_V), $(MMUIndication_BSV), $(MMUIndication_DEP), $(MMUIndication_INC)))

MMUIndication_MOD = mkMMUIndicationWrapperMemPortalPipes
MMUIndication_V   = verilog/mkMMUIndicationWrapperMemPortalPipes.v
MMUIndication_BO  = obj/MMUIndication.bo
MMUIndication_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/MMUIndication.bsv
MMUIndication_DEP = obj/FIFO.bo obj/FIFOF.bo obj/GetPut.bo obj/Connectable.bo obj/Clocks.bo obj/FloatingPoint.bo obj/Adapter.bo obj/Leds.bo obj/Vector.bo obj/SpecialFIFOs.bo obj/ConnectalConfig.bo obj/ConnectalMemory.bo obj/Portal.bo obj/CtrlMux.bo obj/ConnectalMemTypes.bo obj/Pipe.bo obj/HostInterface.bo obj/LinkerLib.bo obj/BuildVector.bo obj/Assert.bo obj/ClientServer.bo obj/ClientServerHelper.bo obj/ControllerTypes.bo obj/AuroraCommon.bo obj/AuroraImportFmc1.bo obj/FlashCtrlVirtex.bo obj/FlashCtrlModel.bo obj/MemReadEngine.bo obj/MemWriteEngine.bo obj/FlashTypes.bo obj/RenameTable.bo obj/BRAMFIFOVector.bo obj/BRAM.bo
MMUIndication_INC = 

$(eval $(call BSV_V_RULE, $(MMUIndication_MOD), $(MMUIndication_V), $(MMUIndication_BSV), $(MMUIndication_DEP), $(MMUIndication_INC)))

MMUIndication_MOD = mkMMUIndicationOutputPipes
MMUIndication_V   = verilog/mkMMUIndicationOutputPipes.v
MMUIndication_BO  = obj/MMUIndication.bo
MMUIndication_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/MMUIndication.bsv
MMUIndication_DEP = obj/FIFO.bo obj/FIFOF.bo obj/GetPut.bo obj/Connectable.bo obj/Clocks.bo obj/FloatingPoint.bo obj/Adapter.bo obj/Leds.bo obj/Vector.bo obj/SpecialFIFOs.bo obj/ConnectalConfig.bo obj/ConnectalMemory.bo obj/Portal.bo obj/CtrlMux.bo obj/ConnectalMemTypes.bo obj/Pipe.bo obj/HostInterface.bo obj/LinkerLib.bo obj/BuildVector.bo obj/Assert.bo obj/ClientServer.bo obj/ClientServerHelper.bo obj/ControllerTypes.bo obj/AuroraCommon.bo obj/AuroraImportFmc1.bo obj/FlashCtrlVirtex.bo obj/FlashCtrlModel.bo obj/MemReadEngine.bo obj/MemWriteEngine.bo obj/FlashTypes.bo obj/RenameTable.bo obj/BRAMFIFOVector.bo obj/BRAM.bo
MMUIndication_INC = 

$(eval $(call BSV_V_RULE, $(MMUIndication_MOD), $(MMUIndication_V), $(MMUIndication_BSV), $(MMUIndication_DEP), $(MMUIndication_INC)))

MMUIndication_MOD = mkMMUIndicationOutput
MMUIndication_V   = verilog/mkMMUIndicationOutput.v
MMUIndication_BO  = obj/MMUIndication.bo
MMUIndication_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/MMUIndication.bsv
MMUIndication_DEP = obj/FIFO.bo obj/FIFOF.bo obj/GetPut.bo obj/Connectable.bo obj/Clocks.bo obj/FloatingPoint.bo obj/Adapter.bo obj/Leds.bo obj/Vector.bo obj/SpecialFIFOs.bo obj/ConnectalConfig.bo obj/ConnectalMemory.bo obj/Portal.bo obj/CtrlMux.bo obj/ConnectalMemTypes.bo obj/Pipe.bo obj/HostInterface.bo obj/LinkerLib.bo obj/BuildVector.bo obj/Assert.bo obj/ClientServer.bo obj/ClientServerHelper.bo obj/ControllerTypes.bo obj/AuroraCommon.bo obj/AuroraImportFmc1.bo obj/FlashCtrlVirtex.bo obj/FlashCtrlModel.bo obj/MemReadEngine.bo obj/MemWriteEngine.bo obj/FlashTypes.bo obj/RenameTable.bo obj/BRAMFIFOVector.bo obj/BRAM.bo
MMUIndication_INC = 

$(eval $(call BSV_V_RULE, $(MMUIndication_MOD), $(MMUIndication_V), $(MMUIndication_BSV), $(MMUIndication_DEP), $(MMUIndication_INC)))

MMUIndication_MOD = mkMMUIndicationInverter
MMUIndication_V   = verilog/mkMMUIndicationInverter.v
MMUIndication_BO  = obj/MMUIndication.bo
MMUIndication_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/MMUIndication.bsv
MMUIndication_DEP = obj/FIFO.bo obj/FIFOF.bo obj/GetPut.bo obj/Connectable.bo obj/Clocks.bo obj/FloatingPoint.bo obj/Adapter.bo obj/Leds.bo obj/Vector.bo obj/SpecialFIFOs.bo obj/ConnectalConfig.bo obj/ConnectalMemory.bo obj/Portal.bo obj/CtrlMux.bo obj/ConnectalMemTypes.bo obj/Pipe.bo obj/HostInterface.bo obj/LinkerLib.bo obj/BuildVector.bo obj/Assert.bo obj/ClientServer.bo obj/ClientServerHelper.bo obj/ControllerTypes.bo obj/AuroraCommon.bo obj/AuroraImportFmc1.bo obj/FlashCtrlVirtex.bo obj/FlashCtrlModel.bo obj/MemReadEngine.bo obj/MemWriteEngine.bo obj/FlashTypes.bo obj/RenameTable.bo obj/BRAMFIFOVector.bo obj/BRAM.bo
MMUIndication_INC = 

$(eval $(call BSV_V_RULE, $(MMUIndication_MOD), $(MMUIndication_V), $(MMUIndication_BSV), $(MMUIndication_DEP), $(MMUIndication_INC)))

MMUIndication_MOD = mkMMUIndicationInverterV
MMUIndication_V   = verilog/mkMMUIndicationInverterV.v
MMUIndication_BO  = obj/MMUIndication.bo
MMUIndication_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/MMUIndication.bsv
MMUIndication_DEP = obj/FIFO.bo obj/FIFOF.bo obj/GetPut.bo obj/Connectable.bo obj/Clocks.bo obj/FloatingPoint.bo obj/Adapter.bo obj/Leds.bo obj/Vector.bo obj/SpecialFIFOs.bo obj/ConnectalConfig.bo obj/ConnectalMemory.bo obj/Portal.bo obj/CtrlMux.bo obj/ConnectalMemTypes.bo obj/Pipe.bo obj/HostInterface.bo obj/LinkerLib.bo obj/BuildVector.bo obj/Assert.bo obj/ClientServer.bo obj/ClientServerHelper.bo obj/ControllerTypes.bo obj/AuroraCommon.bo obj/AuroraImportFmc1.bo obj/FlashCtrlVirtex.bo obj/FlashCtrlModel.bo obj/MemReadEngine.bo obj/MemWriteEngine.bo obj/FlashTypes.bo obj/RenameTable.bo obj/BRAMFIFOVector.bo obj/BRAM.bo
MMUIndication_INC = 

$(eval $(call BSV_V_RULE, $(MMUIndication_MOD), $(MMUIndication_V), $(MMUIndication_BSV), $(MMUIndication_DEP), $(MMUIndication_INC)))

MMUIndication_MOD = mkMMUIndicationProxySynth
MMUIndication_V   = verilog/mkMMUIndicationProxySynth.v
MMUIndication_BO  = obj/MMUIndication.bo
MMUIndication_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/MMUIndication.bsv
MMUIndication_DEP = obj/FIFO.bo obj/FIFOF.bo obj/GetPut.bo obj/Connectable.bo obj/Clocks.bo obj/FloatingPoint.bo obj/Adapter.bo obj/Leds.bo obj/Vector.bo obj/SpecialFIFOs.bo obj/ConnectalConfig.bo obj/ConnectalMemory.bo obj/Portal.bo obj/CtrlMux.bo obj/ConnectalMemTypes.bo obj/Pipe.bo obj/HostInterface.bo obj/LinkerLib.bo obj/BuildVector.bo obj/Assert.bo obj/ClientServer.bo obj/ClientServerHelper.bo obj/ControllerTypes.bo obj/AuroraCommon.bo obj/AuroraImportFmc1.bo obj/FlashCtrlVirtex.bo obj/FlashCtrlModel.bo obj/MemReadEngine.bo obj/MemWriteEngine.bo obj/FlashTypes.bo obj/RenameTable.bo obj/BRAMFIFOVector.bo obj/BRAM.bo
MMUIndication_INC = 

$(eval $(call BSV_V_RULE, $(MMUIndication_MOD), $(MMUIndication_V), $(MMUIndication_BSV), $(MMUIndication_DEP), $(MMUIndication_INC)))

Top_BO  = obj/Top.bo
Top_DEP = obj/ConnectalConfig.bo obj/Vector.bo obj/BuildVector.bo obj/Portal.bo obj/CtrlMux.bo obj/HostInterface.bo obj/Connectable.bo obj/MemReadEngine.bo obj/MemWriteEngine.bo obj/ConnectalMemTypes.bo obj/MemServer.bo obj/IfcNames.bo obj/FlashTop.bo obj/FlashIndication.bo obj/FlashRequest.bo
Top_INC = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/ConnectalProjectConfig.bsv
Top_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/Top.bsv

$(eval $(call BSV_BO_RULE, $(Top_BO), $(Top_BSV), $(Top_DEP), $(Top_INC)))

FlashIndication_BO  = obj/FlashIndication.bo
FlashIndication_DEP = obj/FIFO.bo obj/FIFOF.bo obj/GetPut.bo obj/Connectable.bo obj/Clocks.bo obj/FloatingPoint.bo obj/Adapter.bo obj/Leds.bo obj/Vector.bo obj/SpecialFIFOs.bo obj/ConnectalConfig.bo obj/ConnectalMemory.bo obj/Portal.bo obj/CtrlMux.bo obj/ConnectalMemTypes.bo obj/Pipe.bo obj/HostInterface.bo obj/LinkerLib.bo obj/FlashTop.bo obj/BuildVector.bo obj/Assert.bo obj/ClientServer.bo obj/ClientServerHelper.bo obj/ControllerTypes.bo obj/AuroraCommon.bo obj/AuroraImportFmc1.bo obj/FlashCtrlVirtex.bo obj/FlashCtrlModel.bo obj/MemReadEngine.bo obj/MemWriteEngine.bo obj/FlashTypes.bo obj/RenameTable.bo obj/BRAMFIFOVector.bo obj/BRAM.bo
FlashIndication_INC = 
FlashIndication_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/FlashIndication.bsv

$(eval $(call BSV_BO_RULE, $(FlashIndication_BO), $(FlashIndication_BSV), $(FlashIndication_DEP), $(FlashIndication_INC)))

FlashIndication_MOD = mkFlashIndicationInput
FlashIndication_V   = verilog/mkFlashIndicationInput.v
FlashIndication_BO  = obj/FlashIndication.bo
FlashIndication_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/FlashIndication.bsv
FlashIndication_DEP = obj/FIFO.bo obj/FIFOF.bo obj/GetPut.bo obj/Connectable.bo obj/Clocks.bo obj/FloatingPoint.bo obj/Adapter.bo obj/Leds.bo obj/Vector.bo obj/SpecialFIFOs.bo obj/ConnectalConfig.bo obj/ConnectalMemory.bo obj/Portal.bo obj/CtrlMux.bo obj/ConnectalMemTypes.bo obj/Pipe.bo obj/HostInterface.bo obj/LinkerLib.bo obj/FlashTop.bo obj/BuildVector.bo obj/Assert.bo obj/ClientServer.bo obj/ClientServerHelper.bo obj/ControllerTypes.bo obj/AuroraCommon.bo obj/AuroraImportFmc1.bo obj/FlashCtrlVirtex.bo obj/FlashCtrlModel.bo obj/MemReadEngine.bo obj/MemWriteEngine.bo obj/FlashTypes.bo obj/RenameTable.bo obj/BRAMFIFOVector.bo obj/BRAM.bo
FlashIndication_INC = 

$(eval $(call BSV_V_RULE, $(FlashIndication_MOD), $(FlashIndication_V), $(FlashIndication_BSV), $(FlashIndication_DEP), $(FlashIndication_INC)))

FlashIndication_MOD = mkFlashIndicationWrapperMemPortalPipes
FlashIndication_V   = verilog/mkFlashIndicationWrapperMemPortalPipes.v
FlashIndication_BO  = obj/FlashIndication.bo
FlashIndication_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/FlashIndication.bsv
FlashIndication_DEP = obj/FIFO.bo obj/FIFOF.bo obj/GetPut.bo obj/Connectable.bo obj/Clocks.bo obj/FloatingPoint.bo obj/Adapter.bo obj/Leds.bo obj/Vector.bo obj/SpecialFIFOs.bo obj/ConnectalConfig.bo obj/ConnectalMemory.bo obj/Portal.bo obj/CtrlMux.bo obj/ConnectalMemTypes.bo obj/Pipe.bo obj/HostInterface.bo obj/LinkerLib.bo obj/FlashTop.bo obj/BuildVector.bo obj/Assert.bo obj/ClientServer.bo obj/ClientServerHelper.bo obj/ControllerTypes.bo obj/AuroraCommon.bo obj/AuroraImportFmc1.bo obj/FlashCtrlVirtex.bo obj/FlashCtrlModel.bo obj/MemReadEngine.bo obj/MemWriteEngine.bo obj/FlashTypes.bo obj/RenameTable.bo obj/BRAMFIFOVector.bo obj/BRAM.bo
FlashIndication_INC = 

$(eval $(call BSV_V_RULE, $(FlashIndication_MOD), $(FlashIndication_V), $(FlashIndication_BSV), $(FlashIndication_DEP), $(FlashIndication_INC)))

FlashIndication_MOD = mkFlashIndicationOutputPipes
FlashIndication_V   = verilog/mkFlashIndicationOutputPipes.v
FlashIndication_BO  = obj/FlashIndication.bo
FlashIndication_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/FlashIndication.bsv
FlashIndication_DEP = obj/FIFO.bo obj/FIFOF.bo obj/GetPut.bo obj/Connectable.bo obj/Clocks.bo obj/FloatingPoint.bo obj/Adapter.bo obj/Leds.bo obj/Vector.bo obj/SpecialFIFOs.bo obj/ConnectalConfig.bo obj/ConnectalMemory.bo obj/Portal.bo obj/CtrlMux.bo obj/ConnectalMemTypes.bo obj/Pipe.bo obj/HostInterface.bo obj/LinkerLib.bo obj/FlashTop.bo obj/BuildVector.bo obj/Assert.bo obj/ClientServer.bo obj/ClientServerHelper.bo obj/ControllerTypes.bo obj/AuroraCommon.bo obj/AuroraImportFmc1.bo obj/FlashCtrlVirtex.bo obj/FlashCtrlModel.bo obj/MemReadEngine.bo obj/MemWriteEngine.bo obj/FlashTypes.bo obj/RenameTable.bo obj/BRAMFIFOVector.bo obj/BRAM.bo
FlashIndication_INC = 

$(eval $(call BSV_V_RULE, $(FlashIndication_MOD), $(FlashIndication_V), $(FlashIndication_BSV), $(FlashIndication_DEP), $(FlashIndication_INC)))

FlashIndication_MOD = mkFlashIndicationOutput
FlashIndication_V   = verilog/mkFlashIndicationOutput.v
FlashIndication_BO  = obj/FlashIndication.bo
FlashIndication_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/FlashIndication.bsv
FlashIndication_DEP = obj/FIFO.bo obj/FIFOF.bo obj/GetPut.bo obj/Connectable.bo obj/Clocks.bo obj/FloatingPoint.bo obj/Adapter.bo obj/Leds.bo obj/Vector.bo obj/SpecialFIFOs.bo obj/ConnectalConfig.bo obj/ConnectalMemory.bo obj/Portal.bo obj/CtrlMux.bo obj/ConnectalMemTypes.bo obj/Pipe.bo obj/HostInterface.bo obj/LinkerLib.bo obj/FlashTop.bo obj/BuildVector.bo obj/Assert.bo obj/ClientServer.bo obj/ClientServerHelper.bo obj/ControllerTypes.bo obj/AuroraCommon.bo obj/AuroraImportFmc1.bo obj/FlashCtrlVirtex.bo obj/FlashCtrlModel.bo obj/MemReadEngine.bo obj/MemWriteEngine.bo obj/FlashTypes.bo obj/RenameTable.bo obj/BRAMFIFOVector.bo obj/BRAM.bo
FlashIndication_INC = 

$(eval $(call BSV_V_RULE, $(FlashIndication_MOD), $(FlashIndication_V), $(FlashIndication_BSV), $(FlashIndication_DEP), $(FlashIndication_INC)))

FlashIndication_MOD = mkFlashIndicationInverter
FlashIndication_V   = verilog/mkFlashIndicationInverter.v
FlashIndication_BO  = obj/FlashIndication.bo
FlashIndication_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/FlashIndication.bsv
FlashIndication_DEP = obj/FIFO.bo obj/FIFOF.bo obj/GetPut.bo obj/Connectable.bo obj/Clocks.bo obj/FloatingPoint.bo obj/Adapter.bo obj/Leds.bo obj/Vector.bo obj/SpecialFIFOs.bo obj/ConnectalConfig.bo obj/ConnectalMemory.bo obj/Portal.bo obj/CtrlMux.bo obj/ConnectalMemTypes.bo obj/Pipe.bo obj/HostInterface.bo obj/LinkerLib.bo obj/FlashTop.bo obj/BuildVector.bo obj/Assert.bo obj/ClientServer.bo obj/ClientServerHelper.bo obj/ControllerTypes.bo obj/AuroraCommon.bo obj/AuroraImportFmc1.bo obj/FlashCtrlVirtex.bo obj/FlashCtrlModel.bo obj/MemReadEngine.bo obj/MemWriteEngine.bo obj/FlashTypes.bo obj/RenameTable.bo obj/BRAMFIFOVector.bo obj/BRAM.bo
FlashIndication_INC = 

$(eval $(call BSV_V_RULE, $(FlashIndication_MOD), $(FlashIndication_V), $(FlashIndication_BSV), $(FlashIndication_DEP), $(FlashIndication_INC)))

FlashIndication_MOD = mkFlashIndicationInverterV
FlashIndication_V   = verilog/mkFlashIndicationInverterV.v
FlashIndication_BO  = obj/FlashIndication.bo
FlashIndication_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/FlashIndication.bsv
FlashIndication_DEP = obj/FIFO.bo obj/FIFOF.bo obj/GetPut.bo obj/Connectable.bo obj/Clocks.bo obj/FloatingPoint.bo obj/Adapter.bo obj/Leds.bo obj/Vector.bo obj/SpecialFIFOs.bo obj/ConnectalConfig.bo obj/ConnectalMemory.bo obj/Portal.bo obj/CtrlMux.bo obj/ConnectalMemTypes.bo obj/Pipe.bo obj/HostInterface.bo obj/LinkerLib.bo obj/FlashTop.bo obj/BuildVector.bo obj/Assert.bo obj/ClientServer.bo obj/ClientServerHelper.bo obj/ControllerTypes.bo obj/AuroraCommon.bo obj/AuroraImportFmc1.bo obj/FlashCtrlVirtex.bo obj/FlashCtrlModel.bo obj/MemReadEngine.bo obj/MemWriteEngine.bo obj/FlashTypes.bo obj/RenameTable.bo obj/BRAMFIFOVector.bo obj/BRAM.bo
FlashIndication_INC = 

$(eval $(call BSV_V_RULE, $(FlashIndication_MOD), $(FlashIndication_V), $(FlashIndication_BSV), $(FlashIndication_DEP), $(FlashIndication_INC)))

FlashIndication_MOD = mkFlashIndicationProxySynth
FlashIndication_V   = verilog/mkFlashIndicationProxySynth.v
FlashIndication_BO  = obj/FlashIndication.bo
FlashIndication_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/FlashIndication.bsv
FlashIndication_DEP = obj/FIFO.bo obj/FIFOF.bo obj/GetPut.bo obj/Connectable.bo obj/Clocks.bo obj/FloatingPoint.bo obj/Adapter.bo obj/Leds.bo obj/Vector.bo obj/SpecialFIFOs.bo obj/ConnectalConfig.bo obj/ConnectalMemory.bo obj/Portal.bo obj/CtrlMux.bo obj/ConnectalMemTypes.bo obj/Pipe.bo obj/HostInterface.bo obj/LinkerLib.bo obj/FlashTop.bo obj/BuildVector.bo obj/Assert.bo obj/ClientServer.bo obj/ClientServerHelper.bo obj/ControllerTypes.bo obj/AuroraCommon.bo obj/AuroraImportFmc1.bo obj/FlashCtrlVirtex.bo obj/FlashCtrlModel.bo obj/MemReadEngine.bo obj/MemWriteEngine.bo obj/FlashTypes.bo obj/RenameTable.bo obj/BRAMFIFOVector.bo obj/BRAM.bo
FlashIndication_INC = 

$(eval $(call BSV_V_RULE, $(FlashIndication_MOD), $(FlashIndication_V), $(FlashIndication_BSV), $(FlashIndication_DEP), $(FlashIndication_INC)))

MMURequest_BO  = obj/MMURequest.bo
MMURequest_DEP = obj/FIFO.bo obj/FIFOF.bo obj/GetPut.bo obj/Connectable.bo obj/Clocks.bo obj/FloatingPoint.bo obj/Adapter.bo obj/Leds.bo obj/Vector.bo obj/SpecialFIFOs.bo obj/ConnectalConfig.bo obj/ConnectalMemory.bo obj/Portal.bo obj/CtrlMux.bo obj/ConnectalMemTypes.bo obj/Pipe.bo obj/HostInterface.bo obj/LinkerLib.bo obj/BuildVector.bo obj/Assert.bo obj/ClientServer.bo obj/ClientServerHelper.bo obj/ControllerTypes.bo obj/AuroraCommon.bo obj/AuroraImportFmc1.bo obj/FlashCtrlVirtex.bo obj/FlashCtrlModel.bo obj/MemReadEngine.bo obj/MemWriteEngine.bo obj/FlashTypes.bo obj/RenameTable.bo obj/BRAMFIFOVector.bo obj/BRAM.bo
MMURequest_INC = 
MMURequest_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/MMURequest.bsv

$(eval $(call BSV_BO_RULE, $(MMURequest_BO), $(MMURequest_BSV), $(MMURequest_DEP), $(MMURequest_INC)))

MMURequest_MOD = mkMMURequestInput
MMURequest_V   = verilog/mkMMURequestInput.v
MMURequest_BO  = obj/MMURequest.bo
MMURequest_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/MMURequest.bsv
MMURequest_DEP = obj/FIFO.bo obj/FIFOF.bo obj/GetPut.bo obj/Connectable.bo obj/Clocks.bo obj/FloatingPoint.bo obj/Adapter.bo obj/Leds.bo obj/Vector.bo obj/SpecialFIFOs.bo obj/ConnectalConfig.bo obj/ConnectalMemory.bo obj/Portal.bo obj/CtrlMux.bo obj/ConnectalMemTypes.bo obj/Pipe.bo obj/HostInterface.bo obj/LinkerLib.bo obj/BuildVector.bo obj/Assert.bo obj/ClientServer.bo obj/ClientServerHelper.bo obj/ControllerTypes.bo obj/AuroraCommon.bo obj/AuroraImportFmc1.bo obj/FlashCtrlVirtex.bo obj/FlashCtrlModel.bo obj/MemReadEngine.bo obj/MemWriteEngine.bo obj/FlashTypes.bo obj/RenameTable.bo obj/BRAMFIFOVector.bo obj/BRAM.bo
MMURequest_INC = 

$(eval $(call BSV_V_RULE, $(MMURequest_MOD), $(MMURequest_V), $(MMURequest_BSV), $(MMURequest_DEP), $(MMURequest_INC)))

MMURequest_MOD = mkMMURequestWrapperMemPortalPipes
MMURequest_V   = verilog/mkMMURequestWrapperMemPortalPipes.v
MMURequest_BO  = obj/MMURequest.bo
MMURequest_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/MMURequest.bsv
MMURequest_DEP = obj/FIFO.bo obj/FIFOF.bo obj/GetPut.bo obj/Connectable.bo obj/Clocks.bo obj/FloatingPoint.bo obj/Adapter.bo obj/Leds.bo obj/Vector.bo obj/SpecialFIFOs.bo obj/ConnectalConfig.bo obj/ConnectalMemory.bo obj/Portal.bo obj/CtrlMux.bo obj/ConnectalMemTypes.bo obj/Pipe.bo obj/HostInterface.bo obj/LinkerLib.bo obj/BuildVector.bo obj/Assert.bo obj/ClientServer.bo obj/ClientServerHelper.bo obj/ControllerTypes.bo obj/AuroraCommon.bo obj/AuroraImportFmc1.bo obj/FlashCtrlVirtex.bo obj/FlashCtrlModel.bo obj/MemReadEngine.bo obj/MemWriteEngine.bo obj/FlashTypes.bo obj/RenameTable.bo obj/BRAMFIFOVector.bo obj/BRAM.bo
MMURequest_INC = 

$(eval $(call BSV_V_RULE, $(MMURequest_MOD), $(MMURequest_V), $(MMURequest_BSV), $(MMURequest_DEP), $(MMURequest_INC)))

MMURequest_MOD = mkMMURequestOutputPipes
MMURequest_V   = verilog/mkMMURequestOutputPipes.v
MMURequest_BO  = obj/MMURequest.bo
MMURequest_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/MMURequest.bsv
MMURequest_DEP = obj/FIFO.bo obj/FIFOF.bo obj/GetPut.bo obj/Connectable.bo obj/Clocks.bo obj/FloatingPoint.bo obj/Adapter.bo obj/Leds.bo obj/Vector.bo obj/SpecialFIFOs.bo obj/ConnectalConfig.bo obj/ConnectalMemory.bo obj/Portal.bo obj/CtrlMux.bo obj/ConnectalMemTypes.bo obj/Pipe.bo obj/HostInterface.bo obj/LinkerLib.bo obj/BuildVector.bo obj/Assert.bo obj/ClientServer.bo obj/ClientServerHelper.bo obj/ControllerTypes.bo obj/AuroraCommon.bo obj/AuroraImportFmc1.bo obj/FlashCtrlVirtex.bo obj/FlashCtrlModel.bo obj/MemReadEngine.bo obj/MemWriteEngine.bo obj/FlashTypes.bo obj/RenameTable.bo obj/BRAMFIFOVector.bo obj/BRAM.bo
MMURequest_INC = 

$(eval $(call BSV_V_RULE, $(MMURequest_MOD), $(MMURequest_V), $(MMURequest_BSV), $(MMURequest_DEP), $(MMURequest_INC)))

MMURequest_MOD = mkMMURequestOutput
MMURequest_V   = verilog/mkMMURequestOutput.v
MMURequest_BO  = obj/MMURequest.bo
MMURequest_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/MMURequest.bsv
MMURequest_DEP = obj/FIFO.bo obj/FIFOF.bo obj/GetPut.bo obj/Connectable.bo obj/Clocks.bo obj/FloatingPoint.bo obj/Adapter.bo obj/Leds.bo obj/Vector.bo obj/SpecialFIFOs.bo obj/ConnectalConfig.bo obj/ConnectalMemory.bo obj/Portal.bo obj/CtrlMux.bo obj/ConnectalMemTypes.bo obj/Pipe.bo obj/HostInterface.bo obj/LinkerLib.bo obj/BuildVector.bo obj/Assert.bo obj/ClientServer.bo obj/ClientServerHelper.bo obj/ControllerTypes.bo obj/AuroraCommon.bo obj/AuroraImportFmc1.bo obj/FlashCtrlVirtex.bo obj/FlashCtrlModel.bo obj/MemReadEngine.bo obj/MemWriteEngine.bo obj/FlashTypes.bo obj/RenameTable.bo obj/BRAMFIFOVector.bo obj/BRAM.bo
MMURequest_INC = 

$(eval $(call BSV_V_RULE, $(MMURequest_MOD), $(MMURequest_V), $(MMURequest_BSV), $(MMURequest_DEP), $(MMURequest_INC)))

MMURequest_MOD = mkMMURequestInverter
MMURequest_V   = verilog/mkMMURequestInverter.v
MMURequest_BO  = obj/MMURequest.bo
MMURequest_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/MMURequest.bsv
MMURequest_DEP = obj/FIFO.bo obj/FIFOF.bo obj/GetPut.bo obj/Connectable.bo obj/Clocks.bo obj/FloatingPoint.bo obj/Adapter.bo obj/Leds.bo obj/Vector.bo obj/SpecialFIFOs.bo obj/ConnectalConfig.bo obj/ConnectalMemory.bo obj/Portal.bo obj/CtrlMux.bo obj/ConnectalMemTypes.bo obj/Pipe.bo obj/HostInterface.bo obj/LinkerLib.bo obj/BuildVector.bo obj/Assert.bo obj/ClientServer.bo obj/ClientServerHelper.bo obj/ControllerTypes.bo obj/AuroraCommon.bo obj/AuroraImportFmc1.bo obj/FlashCtrlVirtex.bo obj/FlashCtrlModel.bo obj/MemReadEngine.bo obj/MemWriteEngine.bo obj/FlashTypes.bo obj/RenameTable.bo obj/BRAMFIFOVector.bo obj/BRAM.bo
MMURequest_INC = 

$(eval $(call BSV_V_RULE, $(MMURequest_MOD), $(MMURequest_V), $(MMURequest_BSV), $(MMURequest_DEP), $(MMURequest_INC)))

MMURequest_MOD = mkMMURequestInverterV
MMURequest_V   = verilog/mkMMURequestInverterV.v
MMURequest_BO  = obj/MMURequest.bo
MMURequest_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/MMURequest.bsv
MMURequest_DEP = obj/FIFO.bo obj/FIFOF.bo obj/GetPut.bo obj/Connectable.bo obj/Clocks.bo obj/FloatingPoint.bo obj/Adapter.bo obj/Leds.bo obj/Vector.bo obj/SpecialFIFOs.bo obj/ConnectalConfig.bo obj/ConnectalMemory.bo obj/Portal.bo obj/CtrlMux.bo obj/ConnectalMemTypes.bo obj/Pipe.bo obj/HostInterface.bo obj/LinkerLib.bo obj/BuildVector.bo obj/Assert.bo obj/ClientServer.bo obj/ClientServerHelper.bo obj/ControllerTypes.bo obj/AuroraCommon.bo obj/AuroraImportFmc1.bo obj/FlashCtrlVirtex.bo obj/FlashCtrlModel.bo obj/MemReadEngine.bo obj/MemWriteEngine.bo obj/FlashTypes.bo obj/RenameTable.bo obj/BRAMFIFOVector.bo obj/BRAM.bo
MMURequest_INC = 

$(eval $(call BSV_V_RULE, $(MMURequest_MOD), $(MMURequest_V), $(MMURequest_BSV), $(MMURequest_DEP), $(MMURequest_INC)))

MMURequest_MOD = mkMMURequestProxySynth
MMURequest_V   = verilog/mkMMURequestProxySynth.v
MMURequest_BO  = obj/MMURequest.bo
MMURequest_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/MMURequest.bsv
MMURequest_DEP = obj/FIFO.bo obj/FIFOF.bo obj/GetPut.bo obj/Connectable.bo obj/Clocks.bo obj/FloatingPoint.bo obj/Adapter.bo obj/Leds.bo obj/Vector.bo obj/SpecialFIFOs.bo obj/ConnectalConfig.bo obj/ConnectalMemory.bo obj/Portal.bo obj/CtrlMux.bo obj/ConnectalMemTypes.bo obj/Pipe.bo obj/HostInterface.bo obj/LinkerLib.bo obj/BuildVector.bo obj/Assert.bo obj/ClientServer.bo obj/ClientServerHelper.bo obj/ControllerTypes.bo obj/AuroraCommon.bo obj/AuroraImportFmc1.bo obj/FlashCtrlVirtex.bo obj/FlashCtrlModel.bo obj/MemReadEngine.bo obj/MemWriteEngine.bo obj/FlashTypes.bo obj/RenameTable.bo obj/BRAMFIFOVector.bo obj/BRAM.bo
MMURequest_INC = 

$(eval $(call BSV_V_RULE, $(MMURequest_MOD), $(MMURequest_V), $(MMURequest_BSV), $(MMURequest_DEP), $(MMURequest_INC)))

IfcNames_BO  = obj/IfcNames.bo
IfcNames_DEP = 
IfcNames_INC = 
IfcNames_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/IfcNames.bsv

$(eval $(call BSV_BO_RULE, $(IfcNames_BO), $(IfcNames_BSV), $(IfcNames_DEP), $(IfcNames_INC)))

ConnectalProjectConfig_BO  = obj/ConnectalProjectConfig.bo
ConnectalProjectConfig_DEP = 
ConnectalProjectConfig_INC = 
ConnectalProjectConfig_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/ConnectalProjectConfig.bsv

$(eval $(call BSV_BO_RULE, $(ConnectalProjectConfig_BO), $(ConnectalProjectConfig_BSV), $(ConnectalProjectConfig_DEP), $(ConnectalProjectConfig_INC)))

AuroraImportFmc2_BO  = obj/AuroraImportFmc2.bo
AuroraImportFmc2_DEP = obj/FIFO.bo obj/FIFOF.bo obj/Clocks.bo obj/DefaultValue.bo obj/Xilinx.bo obj/XilinxCells.bo obj/ConnectalXilinxCells.bo obj/AuroraCommon.bo obj/AuroraGearbox.bo
AuroraImportFmc2_INC = 
AuroraImportFmc2_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/xilinx/aurora_8b10b_fmc_vcu108/AuroraImportFmc2.bsv

$(eval $(call BSV_BO_RULE, $(AuroraImportFmc2_BO), $(AuroraImportFmc2_BSV), $(AuroraImportFmc2_DEP), $(AuroraImportFmc2_INC)))

AuroraImportFmc2_MOD = mkAuroraIntra2
AuroraImportFmc2_V   = verilog/mkAuroraIntra2.v
AuroraImportFmc2_BO  = obj/AuroraImportFmc2.bo
AuroraImportFmc2_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/xilinx/aurora_8b10b_fmc_vcu108/AuroraImportFmc2.bsv
AuroraImportFmc2_DEP = obj/FIFO.bo obj/FIFOF.bo obj/Clocks.bo obj/DefaultValue.bo obj/Xilinx.bo obj/XilinxCells.bo obj/ConnectalXilinxCells.bo obj/AuroraCommon.bo obj/AuroraGearbox.bo
AuroraImportFmc2_INC = 

$(eval $(call BSV_V_RULE, $(AuroraImportFmc2_MOD), $(AuroraImportFmc2_V), $(AuroraImportFmc2_BSV), $(AuroraImportFmc2_DEP), $(AuroraImportFmc2_INC)))

AuroraImportFmc1_BO  = obj/AuroraImportFmc1.bo
AuroraImportFmc1_DEP = obj/FIFO.bo obj/FIFOF.bo obj/Clocks.bo obj/DefaultValue.bo obj/Xilinx.bo obj/XilinxCells.bo obj/ConnectalXilinxCells.bo obj/AuroraCommon.bo obj/AuroraGearbox.bo
AuroraImportFmc1_INC = 
AuroraImportFmc1_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/xilinx/aurora_8b10b_fmc_vcu108/AuroraImportFmc1.bsv

$(eval $(call BSV_BO_RULE, $(AuroraImportFmc1_BO), $(AuroraImportFmc1_BSV), $(AuroraImportFmc1_DEP), $(AuroraImportFmc1_INC)))

AuroraImportFmc1_MOD = mkAuroraIntra1
AuroraImportFmc1_V   = verilog/mkAuroraIntra1.v
AuroraImportFmc1_BO  = obj/AuroraImportFmc1.bo
AuroraImportFmc1_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/xilinx/aurora_8b10b_fmc_vcu108/AuroraImportFmc1.bsv
AuroraImportFmc1_DEP = obj/FIFO.bo obj/FIFOF.bo obj/Clocks.bo obj/DefaultValue.bo obj/Xilinx.bo obj/XilinxCells.bo obj/ConnectalXilinxCells.bo obj/AuroraCommon.bo obj/AuroraGearbox.bo
AuroraImportFmc1_INC = 

$(eval $(call BSV_V_RULE, $(AuroraImportFmc1_MOD), $(AuroraImportFmc1_V), $(AuroraImportFmc1_BSV), $(AuroraImportFmc1_DEP), $(AuroraImportFmc1_INC)))

ControllerTypes_BO  = obj/ControllerTypes.bo
ControllerTypes_DEP = obj/FShow.bo obj/GetPut.bo
ControllerTypes_INC = 
ControllerTypes_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/controller/src/common/ControllerTypes.bsv

$(eval $(call BSV_BO_RULE, $(ControllerTypes_BO), $(ControllerTypes_BSV), $(ControllerTypes_DEP), $(ControllerTypes_INC)))

FlashBusModel_BO  = obj/FlashBusModel.bo
FlashBusModel_DEP = obj/FIFOF.bo obj/FIFO.bo obj/BRAMFIFO.bo obj/BRAM.bo obj/GetPut.bo obj/ClientServer.bo obj/Vector.bo obj/RegFile.bo obj/ControllerTypes.bo
FlashBusModel_INC = 
FlashBusModel_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/controller/src/common/FlashBusModel.bsv

$(eval $(call BSV_BO_RULE, $(FlashBusModel_BO), $(FlashBusModel_BSV), $(FlashBusModel_DEP), $(FlashBusModel_INC)))

FlashBusModel_MOD = mkFlashBusModel
FlashBusModel_V   = verilog/mkFlashBusModel.v
FlashBusModel_BO  = obj/FlashBusModel.bo
FlashBusModel_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/controller/src/common/FlashBusModel.bsv
FlashBusModel_DEP = obj/FIFOF.bo obj/FIFO.bo obj/BRAMFIFO.bo obj/BRAM.bo obj/GetPut.bo obj/ClientServer.bo obj/Vector.bo obj/RegFile.bo obj/ControllerTypes.bo
FlashBusModel_INC = 

$(eval $(call BSV_V_RULE, $(FlashBusModel_MOD), $(FlashBusModel_V), $(FlashBusModel_BSV), $(FlashBusModel_DEP), $(FlashBusModel_INC)))

NullResetN_BO  = obj/NullResetN.bo
NullResetN_DEP = 
NullResetN_INC = 
NullResetN_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/controller/src/common/NullResetN.bsv

$(eval $(call BSV_BO_RULE, $(NullResetN_BO), $(NullResetN_BSV), $(NullResetN_DEP), $(NullResetN_INC)))

ChipscopeWrapper_BO  = obj/ChipscopeWrapper.bo
ChipscopeWrapper_DEP = 
ChipscopeWrapper_INC = 
ChipscopeWrapper_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/controller/src/common/ChipscopeWrapper.bsv

$(eval $(call BSV_BO_RULE, $(ChipscopeWrapper_BO), $(ChipscopeWrapper_BSV), $(ChipscopeWrapper_DEP), $(ChipscopeWrapper_INC)))

FlashCtrlModel_BO  = obj/FlashCtrlModel.bo
FlashCtrlModel_DEP = obj/FIFOF.bo obj/FIFO.bo obj/BRAMFIFO.bo obj/BRAM.bo obj/GetPut.bo obj/ClientServer.bo obj/Vector.bo obj/RegFile.bo obj/AuroraCommon.bo obj/AuroraImportFmc1.bo obj/ControllerTypes.bo obj/FlashCtrlVirtex.bo obj/FlashBusModel.bo
FlashCtrlModel_INC = 
FlashCtrlModel_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/controller/src/model_virtex/FlashCtrlModel.bsv

$(eval $(call BSV_BO_RULE, $(FlashCtrlModel_BO), $(FlashCtrlModel_BSV), $(FlashCtrlModel_DEP), $(FlashCtrlModel_INC)))

FlashCtrlModel_MOD = mkFlashCtrlModel
FlashCtrlModel_V   = verilog/mkFlashCtrlModel.v
FlashCtrlModel_BO  = obj/FlashCtrlModel.bo
FlashCtrlModel_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/controller/src/model_virtex/FlashCtrlModel.bsv
FlashCtrlModel_DEP = obj/FIFOF.bo obj/FIFO.bo obj/BRAMFIFO.bo obj/BRAM.bo obj/GetPut.bo obj/ClientServer.bo obj/Vector.bo obj/RegFile.bo obj/AuroraCommon.bo obj/AuroraImportFmc1.bo obj/ControllerTypes.bo obj/FlashCtrlVirtex.bo obj/FlashBusModel.bo
FlashCtrlModel_INC = 

$(eval $(call BSV_V_RULE, $(FlashCtrlModel_MOD), $(FlashCtrlModel_V), $(FlashCtrlModel_BSV), $(FlashCtrlModel_DEP), $(FlashCtrlModel_INC)))

FlashCtrlVirtex_BO  = obj/FlashCtrlVirtex.bo
FlashCtrlVirtex_DEP = obj/FIFOF.bo obj/FIFO.bo obj/BRAMFIFO.bo obj/BRAM.bo obj/GetPut.bo obj/ClientServer.bo obj/Vector.bo obj/Pipe.bo obj/AuroraGearbox.bo obj/AuroraCommon.bo obj/AuroraImportFmc1.bo obj/AuroraImportFmc2.bo obj/ControllerTypes.bo obj/StreamingSerDes.bo
FlashCtrlVirtex_INC = 
FlashCtrlVirtex_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/controller/src/hw_virtex/FlashCtrlVirtex.bsv

$(eval $(call BSV_BO_RULE, $(FlashCtrlVirtex_BO), $(FlashCtrlVirtex_BSV), $(FlashCtrlVirtex_DEP), $(FlashCtrlVirtex_INC)))

StreamingSerDes_BO  = obj/StreamingSerDes.bo
StreamingSerDes_DEP = obj/FIFOF.bo obj/FIFO.bo
StreamingSerDes_INC = 
StreamingSerDes_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/lib/StreamingSerDes.bsv

$(eval $(call BSV_BO_RULE, $(StreamingSerDes_BO), $(StreamingSerDes_BSV), $(StreamingSerDes_DEP), $(StreamingSerDes_INC)))

AuroraCommon_BO  = obj/AuroraCommon.bo
AuroraCommon_DEP = obj/FIFO.bo obj/Clocks.bo obj/DefaultValue.bo obj/Xilinx.bo obj/XilinxCells.bo obj/ConnectalXilinxCells.bo obj/ConnectalClocks.bo obj/AuroraGearbox.bo
AuroraCommon_INC = 
AuroraCommon_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/lib/AuroraCommon.bsv

$(eval $(call BSV_BO_RULE, $(AuroraCommon_BO), $(AuroraCommon_BSV), $(AuroraCommon_DEP), $(AuroraCommon_INC)))

AuroraCommon_MOD = mkGtClockImport
AuroraCommon_V   = verilog/mkGtClockImport.v
AuroraCommon_BO  = obj/AuroraCommon.bo
AuroraCommon_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/lib/AuroraCommon.bsv
AuroraCommon_DEP = obj/FIFO.bo obj/Clocks.bo obj/DefaultValue.bo obj/Xilinx.bo obj/XilinxCells.bo obj/ConnectalXilinxCells.bo obj/ConnectalClocks.bo obj/AuroraGearbox.bo
AuroraCommon_INC = 

$(eval $(call BSV_V_RULE, $(AuroraCommon_MOD), $(AuroraCommon_V), $(AuroraCommon_BSV), $(AuroraCommon_DEP), $(AuroraCommon_INC)))

XilinxIntDiv_BO  = obj/XilinxIntDiv.bo
XilinxIntDiv_DEP = obj/FIFOF.bo obj/FIFO.bo obj/SpecialFIFOs.bo obj/Vector.bo obj/Assert.bo
XilinxIntDiv_INC = 
XilinxIntDiv_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/lib/XilinxIntDiv.bsv

$(eval $(call BSV_BO_RULE, $(XilinxIntDiv_BO), $(XilinxIntDiv_BSV), $(XilinxIntDiv_DEP), $(XilinxIntDiv_INC)))

AuroraGearbox_BO  = obj/AuroraGearbox.bo
AuroraGearbox_DEP = obj/FIFO.bo obj/FIFOF.bo obj/Clocks.bo obj/BRAMFIFO.bo
AuroraGearbox_INC = 
AuroraGearbox_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/lib/AuroraGearbox.bsv

$(eval $(call BSV_BO_RULE, $(AuroraGearbox_BO), $(AuroraGearbox_BSV), $(AuroraGearbox_DEP), $(AuroraGearbox_INC)))

XilinxIntMul_BO  = obj/XilinxIntMul.bo
XilinxIntMul_DEP = obj/Vector.bo obj/FIFO.bo obj/FIFOF.bo obj/SpecialFIFOs.bo obj/Assert.bo
XilinxIntMul_INC = 
XilinxIntMul_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/lib/XilinxIntMul.bsv

$(eval $(call BSV_BO_RULE, $(XilinxIntMul_BO), $(XilinxIntMul_BSV), $(XilinxIntMul_DEP), $(XilinxIntMul_INC)))

RenameTable_BO  = obj/RenameTable.bo
RenameTable_DEP = obj/RWBramCore.bo obj/RegFile.bo obj/FIFO.bo obj/GetPut.bo obj/SpecialFIFOs.bo
RenameTable_INC = 
RenameTable_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/bsvlib/RenameTable.bsv

$(eval $(call BSV_BO_RULE, $(RenameTable_BO), $(RenameTable_BSV), $(RenameTable_DEP), $(RenameTable_INC)))

RWBramCore_BO  = obj/RWBramCore.bo
RWBramCore_DEP = obj/BRAMCore.bo obj/FIFOF.bo obj/SpecialFIFOs.bo
RWBramCore_INC = 
RWBramCore_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/bsvlib/RWBramCore.bsv

$(eval $(call BSV_BO_RULE, $(RWBramCore_BO), $(RWBramCore_BSV), $(RWBramCore_DEP), $(RWBramCore_INC)))

MyArbiter_BO  = obj/MyArbiter.bo
MyArbiter_DEP = obj/Vector.bo obj/BUtils.bo obj/Connectable.bo
MyArbiter_INC = 
MyArbiter_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/bsvlib/MyArbiter.bsv

$(eval $(call BSV_BO_RULE, $(MyArbiter_BO), $(MyArbiter_BSV), $(MyArbiter_DEP), $(MyArbiter_INC)))

TagAlloc_BO  = obj/TagAlloc.bo
TagAlloc_DEP = obj/ControllerTypes.bo obj/ClientServer.bo obj/Connectable.bo obj/FIFOF.bo obj/FIFO.bo obj/GetPut.bo obj/Vector.bo obj/MyArbiter.bo
TagAlloc_INC = 
TagAlloc_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/bsvlib/TagAlloc.bsv

$(eval $(call BSV_BO_RULE, $(TagAlloc_BO), $(TagAlloc_BSV), $(TagAlloc_DEP), $(TagAlloc_INC)))

DRAMBatchRdServer_BO  = obj/DRAMBatchRdServer.bo
DRAMBatchRdServer_DEP = obj/ClientServer.bo obj/ClientServerHelper.bo obj/DRAMControllerTypes.bo obj/FIFO.bo obj/GetPut.bo obj/Shifter.bo obj/SpecialFIFOs.bo obj/Vector.bo
DRAMBatchRdServer_INC = 
DRAMBatchRdServer_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/bsvlib/DRAMBatchRdServer.bsv

$(eval $(call BSV_BO_RULE, $(DRAMBatchRdServer_BO), $(DRAMBatchRdServer_BSV), $(DRAMBatchRdServer_DEP), $(DRAMBatchRdServer_INC)))

BRAMFIFOVector_BO  = obj/BRAMFIFOVector.bo
BRAMFIFOVector_DEP = obj/BRAM.bo obj/FIFO.bo obj/FIFOF.bo obj/Vector.bo
BRAMFIFOVector_INC = 
BRAMFIFOVector_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/bsvlib/BRAMFIFOVector.bsv

$(eval $(call BSV_BO_RULE, $(BRAMFIFOVector_BO), $(BRAMFIFOVector_BSV), $(BRAMFIFOVector_DEP), $(BRAMFIFOVector_INC)))

FlashCtrlIfc_BO  = obj/FlashCtrlIfc.bo
FlashCtrlIfc_DEP = obj/ControllerTypes.bo obj/FlashCtrlVirtex.bo obj/Connectable.bo
FlashCtrlIfc_INC = 
FlashCtrlIfc_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/bsvlib/FlashCtrlIfc.bsv

$(eval $(call BSV_BO_RULE, $(FlashCtrlIfc_BO), $(FlashCtrlIfc_BSV), $(FlashCtrlIfc_DEP), $(FlashCtrlIfc_INC)))

Multipliers_BO  = obj/Multipliers.bo
Multipliers_DEP = obj/FIFOF.bo obj/FIFO.bo obj/SpecialFIFOs.bo obj/Vector.bo obj/GetPut.bo
Multipliers_INC = 
Multipliers_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/bsvlib/Multipliers.bsv

$(eval $(call BSV_BO_RULE, $(Multipliers_BO), $(Multipliers_BSV), $(Multipliers_DEP), $(Multipliers_INC)))

EmulatedFlash_BO  = obj/EmulatedFlash.bo
EmulatedFlash_DEP = obj/ControllerTypes.bo obj/AuroraCommon.bo obj/AuroraImportFmc1.bo obj/FlashCtrlVirtex.bo obj/FlashCtrlModel.bo obj/GetPut.bo obj/FIFO.bo
EmulatedFlash_INC = 
EmulatedFlash_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/bsvlib/EmulatedFlash.bsv

$(eval $(call BSV_BO_RULE, $(EmulatedFlash_BO), $(EmulatedFlash_BSV), $(EmulatedFlash_DEP), $(EmulatedFlash_INC)))

EmulatedFlash_MOD = mkEmulatedFlashCtrl
EmulatedFlash_V   = verilog/mkEmulatedFlashCtrl.v
EmulatedFlash_BO  = obj/EmulatedFlash.bo
EmulatedFlash_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/bsvlib/EmulatedFlash.bsv
EmulatedFlash_DEP = obj/ControllerTypes.bo obj/AuroraCommon.bo obj/AuroraImportFmc1.bo obj/FlashCtrlVirtex.bo obj/FlashCtrlModel.bo obj/GetPut.bo obj/FIFO.bo
EmulatedFlash_INC = 

$(eval $(call BSV_V_RULE, $(EmulatedFlash_MOD), $(EmulatedFlash_V), $(EmulatedFlash_BSV), $(EmulatedFlash_DEP), $(EmulatedFlash_INC)))

FlashSwitch_BO  = obj/FlashSwitch.bo
FlashSwitch_DEP = obj/FlashCtrlIfc.bo obj/ControllerTypes.bo obj/FlashCtrlVirtex.bo obj/Vector.bo obj/FIFO.bo obj/RenameTable.bo obj/RegFile.bo obj/GetPut.bo
FlashSwitch_INC = 
FlashSwitch_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/bsvlib/FlashSwitch.bsv

$(eval $(call BSV_BO_RULE, $(FlashSwitch_BO), $(FlashSwitch_BSV), $(FlashSwitch_DEP), $(FlashSwitch_INC)))

ClientServerHelper_BO  = obj/ClientServerHelper.bo
ClientServerHelper_DEP = obj/ClientServer.bo obj/GetPut.bo obj/FIFO.bo obj/FIFOF.bo obj/Assert.bo
ClientServerHelper_INC = 
ClientServerHelper_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/bsvlib/ClientServerHelper.bsv

$(eval $(call BSV_BO_RULE, $(ClientServerHelper_BO), $(ClientServerHelper_BSV), $(ClientServerHelper_DEP), $(ClientServerHelper_INC)))

Compaction_BO  = obj/Compaction.bo
Compaction_DEP = obj/Vector.bo obj/FIFO.bo obj/SpecialFIFOs.bo obj/Pipe.bo obj/FIFOF.bo obj/GetPut.bo obj/Connectable.bo
Compaction_INC = 
Compaction_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/bsvlib/Compaction.bsv

$(eval $(call BSV_BO_RULE, $(Compaction_BO), $(Compaction_BSV), $(Compaction_DEP), $(Compaction_INC)))

Shifter_BO  = obj/Shifter.bo
Shifter_DEP = obj/GetPut.bo obj/FIFO.bo obj/BRAMFIFO.bo obj/SpecialFIFOs.bo obj/Vector.bo
Shifter_INC = 
Shifter_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/bsvlib/Shifter.bsv

$(eval $(call BSV_BO_RULE, $(Shifter_BO), $(Shifter_BSV), $(Shifter_DEP), $(Shifter_INC)))

EmptyFlash_BO  = obj/EmptyFlash.bo
EmptyFlash_DEP = obj/ControllerTypes.bo obj/AuroraCommon.bo obj/AuroraImportFmc1.bo obj/FlashCtrlVirtex.bo obj/FlashCtrlModel.bo obj/GetPut.bo obj/FIFO.bo
EmptyFlash_INC = 
EmptyFlash_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/bsvlib/EmptyFlash.bsv

$(eval $(call BSV_BO_RULE, $(EmptyFlash_BO), $(EmptyFlash_BSV), $(EmptyFlash_DEP), $(EmptyFlash_INC)))

SharedMemoryFifo_BO  = obj/SharedMemoryFifo.bo
SharedMemoryFifo_DEP = obj/Vector.bo obj/BuildVector.bo obj/GetPut.bo obj/ClientServer.bo obj/Gearbox.bo obj/DefaultValue.bo obj/FIFO.bo obj/FIFOF.bo obj/MIMO.bo obj/Pipe.bo obj/Portal.bo obj/ConnectalMemTypes.bo obj/ConnectalMemory.bo
SharedMemoryFifo_INC = 
SharedMemoryFifo_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/lib/bsv/SharedMemoryFifo.bsv

$(eval $(call BSV_BO_RULE, $(SharedMemoryFifo_BO), $(SharedMemoryFifo_BSV), $(SharedMemoryFifo_DEP), $(SharedMemoryFifo_INC)))

SpiRoot_BO  = obj/SpiRoot.bo
SpiRoot_DEP = obj/FIFO.bo obj/SpiTap.bo
SpiRoot_INC = 
SpiRoot_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/lib/bsv/SpiRoot.bsv

$(eval $(call BSV_BO_RULE, $(SpiRoot_BO), $(SpiRoot_BSV), $(SpiRoot_DEP), $(SpiRoot_INC)))

XADC_BO  = obj/XADC.bo
XADC_DEP = 
XADC_INC = 
XADC_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/lib/bsv/XADC.bsv

$(eval $(call BSV_BO_RULE, $(XADC_BO), $(XADC_BSV), $(XADC_DEP), $(XADC_INC)))

ConfigCounter_BO  = obj/ConfigCounter.bo
ConfigCounter_DEP = obj/GetPut.bo
ConfigCounter_INC = 
ConfigCounter_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/lib/bsv/ConfigCounter.bsv

$(eval $(call BSV_BO_RULE, $(ConfigCounter_BO), $(ConfigCounter_BSV), $(ConfigCounter_DEP), $(ConfigCounter_INC)))

IserdesDatadeser_BO  = obj/IserdesDatadeser.bo
IserdesDatadeser_DEP = obj/Vector.bo obj/Clocks.bo obj/FIFO.bo obj/FIFOF.bo obj/SyncBits.bo obj/ImageonVita.bo obj/IserdesDatadeserIF.bo
IserdesDatadeser_INC = 
IserdesDatadeser_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/lib/bsv/IserdesDatadeser.bsv

$(eval $(call BSV_BO_RULE, $(IserdesDatadeser_BO), $(IserdesDatadeser_BSV), $(IserdesDatadeser_DEP), $(IserdesDatadeser_INC)))

Dma2BRAM_BO  = obj/Dma2BRAM.bo
Dma2BRAM_DEP = obj/BRAM.bo obj/FIFO.bo obj/Vector.bo obj/Gearbox.bo obj/FIFOF.bo obj/SpecialFIFOs.bo obj/GetPut.bo obj/ClientServer.bo obj/ConnectalMemTypes.bo obj/MemWriteEngine.bo obj/ConnectalMemUtils.bo obj/Pipe.bo
Dma2BRAM_INC = 
Dma2BRAM_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/lib/bsv/Dma2BRAM.bsv

$(eval $(call BSV_BO_RULE, $(Dma2BRAM_BO), $(Dma2BRAM_BSV), $(Dma2BRAM_DEP), $(Dma2BRAM_INC)))

Arith_BO  = obj/Arith.bo
Arith_DEP = obj/Vector.bo
Arith_INC = 
Arith_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/lib/bsv/Arith.bsv

$(eval $(call BSV_BO_RULE, $(Arith_BO), $(Arith_BSV), $(Arith_DEP), $(Arith_INC)))

StackReg_BO  = obj/StackReg.bo
StackReg_DEP = obj/BRAM.bo
StackReg_INC = 
StackReg_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/lib/bsv/StackReg.bsv

$(eval $(call BSV_BO_RULE, $(StackReg_BO), $(StackReg_BSV), $(StackReg_DEP), $(StackReg_INC)))

HDMI_BO  = obj/HDMI.bo
HDMI_DEP = obj/Vector.bo obj/Clocks.bo obj/FIFO.bo obj/FIFOF.bo obj/SpecialFIFOs.bo obj/GetPut.bo obj/SyncBits.bo obj/YUV.bo obj/Arith.bo
HDMI_INC = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/ConnectalProjectConfig.bsv
HDMI_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/lib/bsv/HDMI.bsv

$(eval $(call BSV_BO_RULE, $(HDMI_BO), $(HDMI_BSV), $(HDMI_DEP), $(HDMI_INC)))

HDMI_MOD = mkRgb888ToYyuv
HDMI_V   = verilog/mkRgb888ToYyuv.v
HDMI_BO  = obj/HDMI.bo
HDMI_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/lib/bsv/HDMI.bsv
HDMI_DEP = obj/Vector.bo obj/Clocks.bo obj/FIFO.bo obj/FIFOF.bo obj/SpecialFIFOs.bo obj/GetPut.bo obj/SyncBits.bo obj/YUV.bo obj/Arith.bo
HDMI_INC = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/ConnectalProjectConfig.bsv

$(eval $(call BSV_V_RULE, $(HDMI_MOD), $(HDMI_V), $(HDMI_BSV), $(HDMI_DEP), $(HDMI_INC)))

Leds_BO  = obj/Leds.bo
Leds_DEP = 
Leds_INC = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/ConnectalProjectConfig.bsv
Leds_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/lib/bsv/Leds.bsv

$(eval $(call BSV_BO_RULE, $(Leds_BO), $(Leds_BSV), $(Leds_DEP), $(Leds_INC)))

HdmiDisplay_BO  = obj/HdmiDisplay.bo
HdmiDisplay_DEP = obj/FIFO.bo obj/BRAMFIFO.bo obj/Vector.bo obj/Clocks.bo obj/GetPut.bo obj/ClientServer.bo obj/Connectable.bo obj/ConnectalMemTypes.bo obj/MemReadEngine.bo obj/HDMI.bo obj/XADC.bo obj/YUV.bo obj/BlueScope.bo
HdmiDisplay_INC = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/ConnectalProjectConfig.bsv
HdmiDisplay_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/lib/bsv/HdmiDisplay.bsv

$(eval $(call BSV_BO_RULE, $(HdmiDisplay_BO), $(HdmiDisplay_BSV), $(HdmiDisplay_DEP), $(HdmiDisplay_INC)))

IserdesDatadeserIF_BO  = obj/IserdesDatadeserIF.bo
IserdesDatadeserIF_DEP = obj/Vector.bo
IserdesDatadeserIF_INC = 
IserdesDatadeserIF_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/lib/bsv/IserdesDatadeserIF.bsv

$(eval $(call BSV_BO_RULE, $(IserdesDatadeserIF_BO), $(IserdesDatadeserIF_BSV), $(IserdesDatadeserIF_DEP), $(IserdesDatadeserIF_INC)))

BlueScope_BO  = obj/BlueScope.bo
BlueScope_DEP = obj/Clocks.bo obj/FIFO.bo obj/FIFOF.bo obj/BRAMFIFO.bo obj/GetPut.bo obj/Connectable.bo obj/ConnectalMemTypes.bo obj/MemWriteEngine.bo obj/ClientServer.bo
BlueScope_INC = 
BlueScope_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/lib/bsv/BlueScope.bsv

$(eval $(call BSV_BO_RULE, $(BlueScope_BO), $(BlueScope_BSV), $(BlueScope_DEP), $(BlueScope_INC)))

FrequencyCounter_BO  = obj/FrequencyCounter.bo
FrequencyCounter_DEP = obj/Clocks.bo obj/FIFO.bo obj/StmtFSM.bo
FrequencyCounter_INC = 
FrequencyCounter_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/lib/bsv/FrequencyCounter.bsv

$(eval $(call BSV_BO_RULE, $(FrequencyCounter_BO), $(FrequencyCounter_BSV), $(FrequencyCounter_DEP), $(FrequencyCounter_INC)))

ConnectalSpi_BO  = obj/ConnectalSpi.bo
ConnectalSpi_DEP = obj/Clocks.bo obj/GetPut.bo obj/FIFOF.bo obj/Probe.bo obj/Connectable.bo obj/SpecialFIFOs.bo obj/SyncBits.bo obj/StmtFSM.bo obj/Assert.bo obj/XilinxCells.bo obj/Vector.bo obj/ConnectalClocks.bo
ConnectalSpi_INC = 
ConnectalSpi_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/lib/bsv/ConnectalSpi.bsv

$(eval $(call BSV_BO_RULE, $(ConnectalSpi_BO), $(ConnectalSpi_BSV), $(ConnectalSpi_DEP), $(ConnectalSpi_INC)))

PipeMul_BO  = obj/PipeMul.bo
PipeMul_DEP = obj/FIFO.bo obj/SpecialFIFOs.bo obj/Complex.bo obj/FIFOF.bo obj/Vector.bo obj/StmtFSM.bo
PipeMul_INC = 
PipeMul_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/lib/bsv/PipeMul.bsv

$(eval $(call BSV_BO_RULE, $(PipeMul_BO), $(PipeMul_BSV), $(PipeMul_DEP), $(PipeMul_INC)))

SharedMemoryPortal_BO  = obj/SharedMemoryPortal.bo
SharedMemoryPortal_DEP = obj/BuildVector.bo obj/ClientServer.bo obj/Connectable.bo obj/DefaultValue.bo obj/FIFOF.bo obj/Gearbox.bo obj/GetPut.bo obj/MIMO.bo obj/Probe.bo obj/Vector.bo obj/Pipe.bo obj/Portal.bo obj/ConnectalMemTypes.bo obj/ConnectalMemory.bo obj/SharedMemoryFifo.bo
SharedMemoryPortal_INC = 
SharedMemoryPortal_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/lib/bsv/SharedMemoryPortal.bsv

$(eval $(call BSV_BO_RULE, $(SharedMemoryPortal_BO), $(SharedMemoryPortal_BSV), $(SharedMemoryPortal_DEP), $(SharedMemoryPortal_INC)))

YUV_BO  = obj/YUV.bo
YUV_DEP = obj/Vector.bo obj/Arith.bo
YUV_INC = 
YUV_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/lib/bsv/YUV.bsv

$(eval $(call BSV_BO_RULE, $(YUV_BO), $(YUV_BSV), $(YUV_DEP), $(YUV_INC)))

Bscan_BO  = obj/Bscan.bo
Bscan_DEP = obj/FIFOF.bo obj/Clocks.bo obj/Vector.bo obj/BRAM.bo obj/BscanE2.bo obj/GetPut.bo obj/XilinxCells.bo obj/SyncBits.bo
Bscan_INC = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/ConnectalProjectConfig.bsv
Bscan_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/lib/bsv/Bscan.bsv

$(eval $(call BSV_BO_RULE, $(Bscan_BO), $(Bscan_BSV), $(Bscan_DEP), $(Bscan_INC)))

Stack_BO  = obj/Stack.bo
Stack_DEP = obj/BRAM.bo
Stack_INC = 
Stack_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/lib/bsv/Stack.bsv

$(eval $(call BSV_BO_RULE, $(Stack_BO), $(Stack_BSV), $(Stack_DEP), $(Stack_INC)))

BRAMFIFOFLevel_BO  = obj/BRAMFIFOFLevel.bo
BRAMFIFOFLevel_DEP = obj/GetPut.bo obj/BRAMFIFO.bo obj/FIFOF.bo
BRAMFIFOFLevel_INC = 
BRAMFIFOFLevel_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/lib/bsv/BRAMFIFOFLevel.bsv

$(eval $(call BSV_BO_RULE, $(BRAMFIFOFLevel_BO), $(BRAMFIFOFLevel_BSV), $(BRAMFIFOFLevel_DEP), $(BRAMFIFOFLevel_INC)))

SpiTap_BO  = obj/SpiTap.bo
SpiTap_DEP = obj/Connectable.bo
SpiTap_INC = 
SpiTap_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/lib/bsv/SpiTap.bsv

$(eval $(call BSV_BO_RULE, $(SpiTap_BO), $(SpiTap_BSV), $(SpiTap_DEP), $(SpiTap_INC)))

ImageonVita_BO  = obj/ImageonVita.bo
ImageonVita_DEP = obj/Vector.bo obj/Clocks.bo obj/DefaultValue.bo obj/XilinxCells.bo obj/ConnectalClocks.bo obj/ConnectalXilinxCells.bo
ImageonVita_INC = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/ConnectalProjectConfig.bsv
ImageonVita_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/lib/bsv/ImageonVita.bsv

$(eval $(call BSV_BO_RULE, $(ImageonVita_BO), $(ImageonVita_BSV), $(ImageonVita_DEP), $(ImageonVita_INC)))

ImageonVita_MOD = mkSerdesClock
ImageonVita_V   = verilog/mkSerdesClock.v
ImageonVita_BO  = obj/ImageonVita.bo
ImageonVita_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/lib/bsv/ImageonVita.bsv
ImageonVita_DEP = obj/Vector.bo obj/Clocks.bo obj/DefaultValue.bo obj/XilinxCells.bo obj/ConnectalClocks.bo obj/ConnectalXilinxCells.bo
ImageonVita_INC = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/ConnectalProjectConfig.bsv

$(eval $(call BSV_V_RULE, $(ImageonVita_MOD), $(ImageonVita_V), $(ImageonVita_BSV), $(ImageonVita_DEP), $(ImageonVita_INC)))

ImageonVita_MOD = mkImageClocks
ImageonVita_V   = verilog/mkImageClocks.v
ImageonVita_BO  = obj/ImageonVita.bo
ImageonVita_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/lib/bsv/ImageonVita.bsv
ImageonVita_DEP = obj/Vector.bo obj/Clocks.bo obj/DefaultValue.bo obj/XilinxCells.bo obj/ConnectalClocks.bo obj/ConnectalXilinxCells.bo
ImageonVita_INC = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/ConnectalProjectConfig.bsv

$(eval $(call BSV_V_RULE, $(ImageonVita_MOD), $(ImageonVita_V), $(ImageonVita_BSV), $(ImageonVita_DEP), $(ImageonVita_INC)))

BlueScopeEventPIO_BO  = obj/BlueScopeEventPIO.bo
BlueScopeEventPIO_DEP = obj/Clocks.bo obj/FIFO.bo obj/FIFOF.bo obj/BRAMFIFO.bo
BlueScopeEventPIO_INC = 
BlueScopeEventPIO_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/lib/bsv/BlueScopeEventPIO.bsv

$(eval $(call BSV_BO_RULE, $(BlueScopeEventPIO_BO), $(BlueScopeEventPIO_BSV), $(BlueScopeEventPIO_DEP), $(BlueScopeEventPIO_INC)))

BlueScopeEvent_BO  = obj/BlueScopeEvent.bo
BlueScopeEvent_DEP = obj/Clocks.bo obj/FIFO.bo obj/FIFOF.bo obj/BRAMFIFO.bo obj/GetPut.bo obj/Connectable.bo obj/ConnectalMemTypes.bo obj/MemWriteEngine.bo obj/ClientServer.bo
BlueScopeEvent_INC = 
BlueScopeEvent_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/lib/bsv/BlueScopeEvent.bsv

$(eval $(call BSV_BO_RULE, $(BlueScopeEvent_BO), $(BlueScopeEvent_BSV), $(BlueScopeEvent_DEP), $(BlueScopeEvent_INC)))

PCIEWRAPPER3_BO  = obj/PCIEWRAPPER3.bo
PCIEWRAPPER3_DEP = obj/Clocks.bo obj/DefaultValue.bo obj/XilinxCells.bo obj/GetPut.bo obj/AxiBits.bo
PCIEWRAPPER3_INC = 
PCIEWRAPPER3_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/generated/xilinx/PCIEWRAPPER3.bsv

$(eval $(call BSV_BO_RULE, $(PCIEWRAPPER3_BO), $(PCIEWRAPPER3_BSV), $(PCIEWRAPPER3_DEP), $(PCIEWRAPPER3_INC)))

ZYNQ_ULTRA_BO  = obj/ZYNQ_ULTRA.bo
ZYNQ_ULTRA_DEP = obj/Clocks.bo obj/DefaultValue.bo obj/XilinxCells.bo obj/GetPut.bo obj/AxiBits.bo
ZYNQ_ULTRA_INC = 
ZYNQ_ULTRA_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/generated/xilinx/ZYNQ_ULTRA.bsv

$(eval $(call BSV_BO_RULE, $(ZYNQ_ULTRA_BO), $(ZYNQ_ULTRA_BSV), $(ZYNQ_ULTRA_DEP), $(ZYNQ_ULTRA_INC)))

PCIEWRAPPER2_BO  = obj/PCIEWRAPPER2.bo
PCIEWRAPPER2_DEP = obj/Clocks.bo obj/DefaultValue.bo obj/XilinxCells.bo obj/GetPut.bo
PCIEWRAPPER2_INC = 
PCIEWRAPPER2_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/generated/xilinx/PCIEWRAPPER2.bsv

$(eval $(call BSV_BO_RULE, $(PCIEWRAPPER2_BO), $(PCIEWRAPPER2_BSV), $(PCIEWRAPPER2_DEP), $(PCIEWRAPPER2_INC)))

AxiDmaBvi_BO  = obj/AxiDmaBvi.bo
AxiDmaBvi_DEP = obj/Clocks.bo obj/DefaultValue.bo obj/XilinxCells.bo obj/GetPut.bo obj/ConnectalConfig.bo obj/AxiBits.bo obj/AxiStream.bo
AxiDmaBvi_INC = 
AxiDmaBvi_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/generated/xilinx/AxiDmaBvi.bsv

$(eval $(call BSV_BO_RULE, $(AxiDmaBvi_BO), $(AxiDmaBvi_BSV), $(AxiDmaBvi_DEP), $(AxiDmaBvi_INC)))

PCIEWRAPPER3u_BO  = obj/PCIEWRAPPER3u.bo
PCIEWRAPPER3u_DEP = obj/Clocks.bo obj/DefaultValue.bo obj/XilinxCells.bo obj/GetPut.bo obj/AxiBits.bo
PCIEWRAPPER3u_INC = 
PCIEWRAPPER3u_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/generated/xilinx/PCIEWRAPPER3u.bsv

$(eval $(call BSV_BO_RULE, $(PCIEWRAPPER3u_BO), $(PCIEWRAPPER3u_BSV), $(PCIEWRAPPER3u_DEP), $(PCIEWRAPPER3u_INC)))

Ddr3Wrapper_BO  = obj/Ddr3Wrapper.bo
Ddr3Wrapper_DEP = obj/Clocks.bo obj/DefaultValue.bo obj/XilinxCells.bo obj/GetPut.bo obj/AxiBits.bo
Ddr3Wrapper_INC = 
Ddr3Wrapper_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/generated/xilinx/Ddr3Wrapper.bsv

$(eval $(call BSV_BO_RULE, $(Ddr3Wrapper_BO), $(Ddr3Wrapper_BSV), $(Ddr3Wrapper_DEP), $(Ddr3Wrapper_INC)))

AxiDdr3Wrapper_BO  = obj/AxiDdr3Wrapper.bo
AxiDdr3Wrapper_DEP = obj/Clocks.bo obj/DefaultValue.bo obj/XilinxCells.bo obj/GetPut.bo obj/AxiBits.bo
AxiDdr3Wrapper_INC = 
AxiDdr3Wrapper_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/generated/xilinx/AxiDdr3Wrapper.bsv

$(eval $(call BSV_BO_RULE, $(AxiDdr3Wrapper_BO), $(AxiDdr3Wrapper_BSV), $(AxiDdr3Wrapper_DEP), $(AxiDdr3Wrapper_INC)))

AxiEth1000BaseX_BO  = obj/AxiEth1000BaseX.bo
AxiEth1000BaseX_DEP = obj/Clocks.bo obj/DefaultValue.bo obj/XilinxCells.bo obj/GetPut.bo obj/AxiBits.bo obj/Vector.bo obj/AxiStream.bo
AxiEth1000BaseX_INC = 
AxiEth1000BaseX_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/generated/xilinx/AxiEth1000BaseX.bsv

$(eval $(call BSV_BO_RULE, $(AxiEth1000BaseX_BO), $(AxiEth1000BaseX_BSV), $(AxiEth1000BaseX_DEP), $(AxiEth1000BaseX_INC)))

AxiEthBvi_BO  = obj/AxiEthBvi.bo
AxiEthBvi_DEP = obj/Clocks.bo obj/DefaultValue.bo obj/XilinxCells.bo obj/GetPut.bo obj/AxiBits.bo obj/Vector.bo obj/AxiStream.bo
AxiEthBvi_INC = 
AxiEthBvi_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/generated/xilinx/AxiEthBvi.bsv

$(eval $(call BSV_BO_RULE, $(AxiEthBvi_BO), $(AxiEthBvi_BSV), $(AxiEthBvi_DEP), $(AxiEthBvi_INC)))

PPS7LIB_BO  = obj/PPS7LIB.bo
PPS7LIB_DEP = obj/Clocks.bo obj/DefaultValue.bo obj/XilinxCells.bo obj/GetPut.bo obj/AxiBits.bo
PPS7LIB_INC = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform_tests/flash_dual_no_ind_vcu108/vcu108/generatedbsv/ConnectalProjectConfig.bsv
PPS7LIB_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/generated/xilinx/PPS7LIB.bsv

$(eval $(call BSV_BO_RULE, $(PPS7LIB_BO), $(PPS7LIB_BSV), $(PPS7LIB_DEP), $(PPS7LIB_INC)))

PCIEWRAPPER3uplus_BO  = obj/PCIEWRAPPER3uplus.bo
PCIEWRAPPER3uplus_DEP = obj/Clocks.bo obj/DefaultValue.bo obj/XilinxCells.bo obj/GetPut.bo obj/AxiBits.bo
PCIEWRAPPER3uplus_INC = 
PCIEWRAPPER3uplus_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/generated/xilinx/PCIEWRAPPER3uplus.bsv

$(eval $(call BSV_BO_RULE, $(PCIEWRAPPER3uplus_BO), $(PCIEWRAPPER3uplus_BSV), $(PCIEWRAPPER3uplus_DEP), $(PCIEWRAPPER3uplus_INC)))

PCIEWRAPPER_BO  = obj/PCIEWRAPPER.bo
PCIEWRAPPER_DEP = obj/Clocks.bo obj/DefaultValue.bo obj/XilinxCells.bo obj/GetPut.bo
PCIEWRAPPER_INC = 
PCIEWRAPPER_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/generated/xilinx/PCIEWRAPPER.bsv

$(eval $(call BSV_BO_RULE, $(PCIEWRAPPER_BO), $(PCIEWRAPPER_BSV), $(PCIEWRAPPER_DEP), $(PCIEWRAPPER_INC)))

PipeClock_BO  = obj/PipeClock.bo
PipeClock_DEP = obj/Clocks.bo obj/DefaultValue.bo obj/XilinxCells.bo obj/GetPut.bo
PipeClock_INC = 
PipeClock_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/generated/xilinx/PipeClock.bsv

$(eval $(call BSV_BO_RULE, $(PipeClock_BO), $(PipeClock_BSV), $(PipeClock_DEP), $(PipeClock_INC)))

Bufgctrl_BO  = obj/Bufgctrl.bo
Bufgctrl_DEP = obj/Clocks.bo obj/DefaultValue.bo obj/XilinxCells.bo obj/GetPut.bo
Bufgctrl_INC = 
Bufgctrl_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/generated/xilinx/Bufgctrl.bsv

$(eval $(call BSV_BO_RULE, $(Bufgctrl_BO), $(Bufgctrl_BSV), $(Bufgctrl_DEP), $(Bufgctrl_INC)))

PCIE_2_1_BO  = obj/PCIE_2_1.bo
PCIE_2_1_DEP = obj/Clocks.bo obj/DefaultValue.bo obj/XilinxCells.bo obj/GetPut.bo
PCIE_2_1_INC = 
PCIE_2_1_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/generated/xilinx/PCIE_2_1.bsv

$(eval $(call BSV_BO_RULE, $(PCIE_2_1_BO), $(PCIE_2_1_BSV), $(PCIE_2_1_DEP), $(PCIE_2_1_INC)))

AxiIntcBvi_BO  = obj/AxiIntcBvi.bo
AxiIntcBvi_DEP = obj/Clocks.bo obj/DefaultValue.bo obj/XilinxCells.bo obj/GetPut.bo obj/Vector.bo obj/AxiBits.bo
AxiIntcBvi_INC = 
AxiIntcBvi_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/generated/xilinx/AxiIntcBvi.bsv

$(eval $(call BSV_BO_RULE, $(AxiIntcBvi_BO), $(AxiIntcBvi_BSV), $(AxiIntcBvi_DEP), $(AxiIntcBvi_INC)))

BscanE2_BO  = obj/BscanE2.bo
BscanE2_DEP = obj/Clocks.bo obj/DefaultValue.bo obj/XilinxCells.bo obj/GetPut.bo
BscanE2_INC = 
BscanE2_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/generated/xilinx/BscanE2.bsv

$(eval $(call BSV_BO_RULE, $(BscanE2_BO), $(BscanE2_BSV), $(BscanE2_DEP), $(BscanE2_INC)))

ALTERA_PCIE_SIV_WRAPPER_BO  = obj/ALTERA_PCIE_SIV_WRAPPER.bo
ALTERA_PCIE_SIV_WRAPPER_DEP = obj/Clocks.bo obj/DefaultValue.bo obj/XilinxCells.bo obj/GetPut.bo
ALTERA_PCIE_SIV_WRAPPER_INC = 
ALTERA_PCIE_SIV_WRAPPER_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/generated/altera/ALTERA_PCIE_SIV_WRAPPER.bsv

$(eval $(call BSV_BO_RULE, $(ALTERA_PCIE_SIV_WRAPPER_BO), $(ALTERA_PCIE_SIV_WRAPPER_BSV), $(ALTERA_PCIE_SIV_WRAPPER_DEP), $(ALTERA_PCIE_SIV_WRAPPER_INC)))

ALTERA_PCIE_ED_WRAPPER_BO  = obj/ALTERA_PCIE_ED_WRAPPER.bo
ALTERA_PCIE_ED_WRAPPER_DEP = obj/Clocks.bo obj/DefaultValue.bo obj/XilinxCells.bo obj/GetPut.bo
ALTERA_PCIE_ED_WRAPPER_INC = 
ALTERA_PCIE_ED_WRAPPER_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/generated/altera/ALTERA_PCIE_ED_WRAPPER.bsv

$(eval $(call BSV_BO_RULE, $(ALTERA_PCIE_ED_WRAPPER_BO), $(ALTERA_PCIE_ED_WRAPPER_BSV), $(ALTERA_PCIE_ED_WRAPPER_DEP), $(ALTERA_PCIE_ED_WRAPPER_INC)))

ALTERA_ETH_PMA_RESET_CONTROL_WRAPPER_BO  = obj/ALTERA_ETH_PMA_RESET_CONTROL_WRAPPER.bo
ALTERA_ETH_PMA_RESET_CONTROL_WRAPPER_DEP = obj/Clocks.bo obj/DefaultValue.bo obj/XilinxCells.bo obj/GetPut.bo
ALTERA_ETH_PMA_RESET_CONTROL_WRAPPER_INC = 
ALTERA_ETH_PMA_RESET_CONTROL_WRAPPER_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/generated/altera/ALTERA_ETH_PMA_RESET_CONTROL_WRAPPER.bsv

$(eval $(call BSV_BO_RULE, $(ALTERA_ETH_PMA_RESET_CONTROL_WRAPPER_BO), $(ALTERA_ETH_PMA_RESET_CONTROL_WRAPPER_BSV), $(ALTERA_ETH_PMA_RESET_CONTROL_WRAPPER_DEP), $(ALTERA_ETH_PMA_RESET_CONTROL_WRAPPER_INC)))

ALTERA_PLL_WRAPPER_BO  = obj/ALTERA_PLL_WRAPPER.bo
ALTERA_PLL_WRAPPER_DEP = obj/Clocks.bo obj/DefaultValue.bo obj/XilinxCells.bo obj/GetPut.bo
ALTERA_PLL_WRAPPER_INC = 
ALTERA_PLL_WRAPPER_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/generated/altera/ALTERA_PLL_WRAPPER.bsv

$(eval $(call BSV_BO_RULE, $(ALTERA_PLL_WRAPPER_BO), $(ALTERA_PLL_WRAPPER_BSV), $(ALTERA_PLL_WRAPPER_DEP), $(ALTERA_PLL_WRAPPER_INC)))

ALTERA_PCIE_SV_WRAPPER_BO  = obj/ALTERA_PCIE_SV_WRAPPER.bo
ALTERA_PCIE_SV_WRAPPER_DEP = obj/Clocks.bo obj/DefaultValue.bo obj/XilinxCells.bo obj/GetPut.bo
ALTERA_PCIE_SV_WRAPPER_INC = 
ALTERA_PCIE_SV_WRAPPER_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/generated/altera/ALTERA_PCIE_SV_WRAPPER.bsv

$(eval $(call BSV_BO_RULE, $(ALTERA_PCIE_SV_WRAPPER_BO), $(ALTERA_PCIE_SV_WRAPPER_BSV), $(ALTERA_PCIE_SV_WRAPPER_DEP), $(ALTERA_PCIE_SV_WRAPPER_INC)))

ALTERA_ETH_PMA_RECONFIG_WRAPPER_BO  = obj/ALTERA_ETH_PMA_RECONFIG_WRAPPER.bo
ALTERA_ETH_PMA_RECONFIG_WRAPPER_DEP = obj/Clocks.bo obj/DefaultValue.bo obj/XilinxCells.bo obj/GetPut.bo
ALTERA_ETH_PMA_RECONFIG_WRAPPER_INC = 
ALTERA_ETH_PMA_RECONFIG_WRAPPER_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/generated/altera/ALTERA_ETH_PMA_RECONFIG_WRAPPER.bsv

$(eval $(call BSV_BO_RULE, $(ALTERA_ETH_PMA_RECONFIG_WRAPPER_BO), $(ALTERA_ETH_PMA_RECONFIG_WRAPPER_BSV), $(ALTERA_ETH_PMA_RECONFIG_WRAPPER_DEP), $(ALTERA_ETH_PMA_RECONFIG_WRAPPER_INC)))

ALTERA_PCIE_RECONFIG_DRIVER_WRAPPER_BO  = obj/ALTERA_PCIE_RECONFIG_DRIVER_WRAPPER.bo
ALTERA_PCIE_RECONFIG_DRIVER_WRAPPER_DEP = obj/Clocks.bo obj/DefaultValue.bo obj/XilinxCells.bo obj/GetPut.bo
ALTERA_PCIE_RECONFIG_DRIVER_WRAPPER_INC = 
ALTERA_PCIE_RECONFIG_DRIVER_WRAPPER_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/generated/altera/ALTERA_PCIE_RECONFIG_DRIVER_WRAPPER.bsv

$(eval $(call BSV_BO_RULE, $(ALTERA_PCIE_RECONFIG_DRIVER_WRAPPER_BO), $(ALTERA_PCIE_RECONFIG_DRIVER_WRAPPER_BSV), $(ALTERA_PCIE_RECONFIG_DRIVER_WRAPPER_DEP), $(ALTERA_PCIE_RECONFIG_DRIVER_WRAPPER_INC)))

ALTERA_DDR3_WRAPPER_BO  = obj/ALTERA_DDR3_WRAPPER.bo
ALTERA_DDR3_WRAPPER_DEP = obj/Clocks.bo obj/DefaultValue.bo obj/XilinxCells.bo obj/GetPut.bo obj/AxiBits.bo obj/Vector.bo
ALTERA_DDR3_WRAPPER_INC = 
ALTERA_DDR3_WRAPPER_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/generated/altera/ALTERA_DDR3_WRAPPER.bsv

$(eval $(call BSV_BO_RULE, $(ALTERA_DDR3_WRAPPER_BO), $(ALTERA_DDR3_WRAPPER_BSV), $(ALTERA_DDR3_WRAPPER_DEP), $(ALTERA_DDR3_WRAPPER_INC)))

ALTERA_ETH_PMA_WRAPPER_BO  = obj/ALTERA_ETH_PMA_WRAPPER.bo
ALTERA_ETH_PMA_WRAPPER_DEP = obj/Clocks.bo obj/DefaultValue.bo obj/XilinxCells.bo obj/GetPut.bo
ALTERA_ETH_PMA_WRAPPER_INC = 
ALTERA_ETH_PMA_WRAPPER_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/generated/altera/ALTERA_ETH_PMA_WRAPPER.bsv

$(eval $(call BSV_BO_RULE, $(ALTERA_ETH_PMA_WRAPPER_BO), $(ALTERA_ETH_PMA_WRAPPER_BSV), $(ALTERA_ETH_PMA_WRAPPER_DEP), $(ALTERA_ETH_PMA_WRAPPER_INC)))

ALTERA_XCVR_RECONFIG_WRAPPER_BO  = obj/ALTERA_XCVR_RECONFIG_WRAPPER.bo
ALTERA_XCVR_RECONFIG_WRAPPER_DEP = obj/Clocks.bo obj/DefaultValue.bo obj/XilinxCells.bo obj/GetPut.bo
ALTERA_XCVR_RECONFIG_WRAPPER_INC = 
ALTERA_XCVR_RECONFIG_WRAPPER_BSV = /home/hojun/git/FlashDriver_bulk/lower/vcu108/platform/build_tools/connectal/generated/altera/ALTERA_XCVR_RECONFIG_WRAPPER.bsv

$(eval $(call BSV_BO_RULE, $(ALTERA_XCVR_RECONFIG_WRAPPER_BO), $(ALTERA_XCVR_RECONFIG_WRAPPER_BSV), $(ALTERA_XCVR_RECONFIG_WRAPPER_DEP), $(ALTERA_XCVR_RECONFIG_WRAPPER_INC)))
