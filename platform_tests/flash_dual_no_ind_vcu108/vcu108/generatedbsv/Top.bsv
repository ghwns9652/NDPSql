
import ConnectalConfig::*;
import Vector::*;
import BuildVector::*;
import Portal::*;
import CtrlMux::*;
import HostInterface::*;
import Connectable::*;
import MemReadEngine::*;
import MemWriteEngine::*;
import ConnectalMemTypes::*;
import MemServer::*;
`include "ConnectalProjectConfig.bsv"
import IfcNames::*;
import `PinTypeInclude::*;
import FlashIndication::*;
import FlashTop::*;
import FlashRequest::*;



`ifndef IMPORT_HOSTIF
(* synthesize *)
`endif
module mkConnectalTop
`ifdef IMPORT_HOSTIF // no synthesis boundary
      #(HostInterface host)
`else
`ifdef IMPORT_HOST_CLOCKS // enables synthesis boundary
       #(Clock derivedClockIn, Reset derivedResetIn)
`else
// otherwise no params
`endif
`endif
       (ConnectalTop#(`PinType));
   Clock defaultClock <- exposeCurrentClock();
   Reset defaultReset <- exposeCurrentReset();
`ifdef IMPORT_HOST_CLOCKS // enables synthesis boundary
   HostInterface host = (interface HostInterface;
                           interface Clock derivedClock = derivedClockIn;
                           interface Reset derivedReset = derivedResetIn;
                         endinterface);
`endif
   FlashIndicationOutput lFlashIndicationOutput <- mkFlashIndicationOutput;
   FlashRequestInput lFlashRequestInput <- mkFlashRequestInput;

   let lFlashTop <- mkFlashTop(host, lFlashIndicationOutput.ifc);

   mkConnection(lFlashRequestInput.pipes, lFlashTop.request);

   Vector#(2,StdPortal) portals;
   PortalCtrlMemSlave#(SlaveControlAddrWidth,SlaveDataBusWidth) ctrlPort_0 <- mkPortalCtrlMemSlave(extend(pack(IfcNames_FlashIndicationH2S)), lFlashIndicationOutput.portalIfc.intr);
   let memslave_0 <- mkMemMethodMuxOut(ctrlPort_0.memSlave,lFlashIndicationOutput.portalIfc.indications);
   portals[0] = (interface MemPortal;
       interface PhysMemSlave slave = memslave_0;
       interface ReadOnly interrupt = ctrlPort_0.interrupt;
       interface WriteOnly num_portals = ctrlPort_0.num_portals;
       endinterface);
   PortalCtrlMemSlave#(SlaveControlAddrWidth,SlaveDataBusWidth) ctrlPort_1 <- mkPortalCtrlMemSlave(extend(pack(IfcNames_FlashRequestS2H)), lFlashRequestInput.portalIfc.intr);
   let memslave_1 <- mkMemMethodMuxIn(ctrlPort_1.memSlave,lFlashRequestInput.portalIfc.requests);
   portals[1] = (interface MemPortal;
       interface PhysMemSlave slave = memslave_1;
       interface ReadOnly interrupt = ctrlPort_1.interrupt;
       interface WriteOnly num_portals = ctrlPort_1.num_portals;
       endinterface);
   let ctrl_mux <- mkSlaveMux(portals);
   Vector#(NumWriteClients,MemWriteClient#(DataBusWidth)) nullWriters = replicate(null_mem_write_client());
   Vector#(NumReadClients,MemReadClient#(DataBusWidth)) nullReaders = replicate(null_mem_read_client());
   interface interrupt = getInterruptVector(portals);
   interface slave = ctrl_mux;
   interface readers = take(append(lFlashTop.dmaReadClient,nullReaders));
   interface writers = take(append(lFlashTop.dmaWriteClient,nullWriters));
`ifdef TOP_SOURCES_PORTAL_CLOCK
   interface portalClockSource = None;
`endif

      interface pins = lFlashTop.pins;
endmodule : mkConnectalTop
export mkConnectalTop;
export `PinTypeInclude::*;
