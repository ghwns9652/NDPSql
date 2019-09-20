package FlashIndication;

import FIFO::*;
import FIFOF::*;
import GetPut::*;
import Connectable::*;
import Clocks::*;
import FloatingPoint::*;
import Adapter::*;
import Leds::*;
import Vector::*;
import SpecialFIFOs::*;
import ConnectalConfig::*;
import ConnectalMemory::*;
import Portal::*;
import CtrlMux::*;
import ConnectalMemTypes::*;
import Pipe::*;
import HostInterface::*;
import LinkerLib::*;
import FlashTop::*;
import FIFO::*;
import FIFOF::*;
import Vector::*;
import BuildVector::*;
import Connectable::*;
import HostInterface::*;
import Assert::*;
import GetPut::*;
import ClientServer::*;
import ClientServerHelper::*;
import ControllerTypes::*;
import AuroraCommon::*;
import AuroraImportFmc1::*;
import FlashCtrlVirtex::*;
import FlashCtrlModel::*;
import ConnectalConfig::*;
import ConnectalMemTypes::*;
import MemReadEngine::*;
import MemWriteEngine::*;
import Pipe::*;
import FlashTypes::*;
import RenameTable::*;
import BRAMFIFOVector::*;
import BRAM::*;
import Clocks::*;




typedef struct {
    Bit#(32) tag;
    Bit#(64) cycles;
} ReadDone_Message deriving (Bits);

typedef struct {
    Bit#(32) tag;
    Bit#(64) cycles;
} WriteDone_Message deriving (Bits);

typedef struct {
    Bit#(32) tag;
    Bit#(32) status;
    Bit#(64) cycles;
} EraseDone_Message deriving (Bits);

typedef struct {
    Bit#(32) card;
    Bit#(32) debug0;
    Bit#(32) debug1;
    Bit#(32) debug2;
    Bit#(32) debug3;
    Bit#(32) debug4;
    Bit#(32) debug5;
} DebugDumpResp_Message deriving (Bits);

// exposed wrapper portal interface
interface FlashIndicationInputPipes;
    interface PipeOut#(ReadDone_Message) readDone_PipeOut;
    interface PipeOut#(WriteDone_Message) writeDone_PipeOut;
    interface PipeOut#(EraseDone_Message) eraseDone_PipeOut;
    interface PipeOut#(DebugDumpResp_Message) debugDumpResp_PipeOut;

endinterface
typedef PipePortal#(4, 0, SlaveDataBusWidth) FlashIndicationPortalInput;
interface FlashIndicationInput;
    interface FlashIndicationPortalInput portalIfc;
    interface FlashIndicationInputPipes pipes;
endinterface
interface FlashIndicationWrapperPortal;
    interface FlashIndicationPortalInput portalIfc;
endinterface
// exposed wrapper MemPortal interface
interface FlashIndicationWrapper;
    interface StdPortal portalIfc;
endinterface

instance Connectable#(FlashIndicationInputPipes,FlashIndication);
   module mkConnection#(FlashIndicationInputPipes pipes, FlashIndication ifc)(Empty);

    rule handle_readDone_request;
        let request <- toGet(pipes.readDone_PipeOut).get();
        ifc.readDone(request.tag, request.cycles);
    endrule

    rule handle_writeDone_request;
        let request <- toGet(pipes.writeDone_PipeOut).get();
        ifc.writeDone(request.tag, request.cycles);
    endrule

    rule handle_eraseDone_request;
        let request <- toGet(pipes.eraseDone_PipeOut).get();
        ifc.eraseDone(request.tag, request.status, request.cycles);
    endrule

    rule handle_debugDumpResp_request;
        let request <- toGet(pipes.debugDumpResp_PipeOut).get();
        ifc.debugDumpResp(request.card, request.debug0, request.debug1, request.debug2, request.debug3, request.debug4, request.debug5);
    endrule

   endmodule
endinstance

// exposed wrapper Portal implementation
(* synthesize *)
module mkFlashIndicationInput(FlashIndicationInput);
    Vector#(4, PipeIn#(Bit#(SlaveDataBusWidth))) requestPipeIn;

    AdapterFromBus#(SlaveDataBusWidth,ReadDone_Message) readDone_requestAdapter <- mkAdapterFromBus();
    requestPipeIn[0] = readDone_requestAdapter.in;

    AdapterFromBus#(SlaveDataBusWidth,WriteDone_Message) writeDone_requestAdapter <- mkAdapterFromBus();
    requestPipeIn[1] = writeDone_requestAdapter.in;

    AdapterFromBus#(SlaveDataBusWidth,EraseDone_Message) eraseDone_requestAdapter <- mkAdapterFromBus();
    requestPipeIn[2] = eraseDone_requestAdapter.in;

    AdapterFromBus#(SlaveDataBusWidth,DebugDumpResp_Message) debugDumpResp_requestAdapter <- mkAdapterFromBus();
    requestPipeIn[3] = debugDumpResp_requestAdapter.in;

    interface PipePortal portalIfc;
        interface PortalSize messageSize;
        method Bit#(16) size(Bit#(16) methodNumber);
            case (methodNumber)
            0: return fromInteger(valueOf(SizeOf#(ReadDone_Message)));
            1: return fromInteger(valueOf(SizeOf#(WriteDone_Message)));
            2: return fromInteger(valueOf(SizeOf#(EraseDone_Message)));
            3: return fromInteger(valueOf(SizeOf#(DebugDumpResp_Message)));
            endcase
        endmethod
        endinterface
        interface Vector requests = requestPipeIn;
        interface Vector indications = nil;
        interface PortalInterrupt intr;
           method Bool status();
              return False;
           endmethod
           method Bit#(dataWidth) channel();
              return -1;
           endmethod
        endinterface
    endinterface
    interface FlashIndicationInputPipes pipes;
        interface readDone_PipeOut = readDone_requestAdapter.out;
        interface writeDone_PipeOut = writeDone_requestAdapter.out;
        interface eraseDone_PipeOut = eraseDone_requestAdapter.out;
        interface debugDumpResp_PipeOut = debugDumpResp_requestAdapter.out;
    endinterface
endmodule

module mkFlashIndicationWrapperPortal#(FlashIndication ifc)(FlashIndicationWrapperPortal);
    let dut <- mkFlashIndicationInput;
    mkConnection(dut.pipes, ifc);
    interface PipePortal portalIfc = dut.portalIfc;
endmodule

interface FlashIndicationWrapperMemPortalPipes;
    interface FlashIndicationInputPipes pipes;
    interface MemPortal#(12,32) portalIfc;
endinterface

(* synthesize *)
module mkFlashIndicationWrapperMemPortalPipes#(Bit#(SlaveDataBusWidth) id)(FlashIndicationWrapperMemPortalPipes);

  let dut <- mkFlashIndicationInput;
  PortalCtrlMemSlave#(SlaveControlAddrWidth,SlaveDataBusWidth) ctrlPort <- mkPortalCtrlMemSlave(id, dut.portalIfc.intr);
  let memslave  <- mkMemMethodMuxIn(ctrlPort.memSlave,dut.portalIfc.requests);
  interface FlashIndicationInputPipes pipes = dut.pipes;
  interface MemPortal portalIfc = (interface MemPortal;
      interface PhysMemSlave slave = memslave;
      interface ReadOnly interrupt = ctrlPort.interrupt;
      interface WriteOnly num_portals = ctrlPort.num_portals;
    endinterface);
endmodule

// exposed wrapper MemPortal implementation
module mkFlashIndicationWrapper#(idType id, FlashIndication ifc)(FlashIndicationWrapper)
   provisos (Bits#(idType, a__),
	     Add#(b__, a__, SlaveDataBusWidth));
  let dut <- mkFlashIndicationWrapperMemPortalPipes(zeroExtend(pack(id)));
  mkConnection(dut.pipes, ifc);
  interface MemPortal portalIfc = dut.portalIfc;
endmodule

// exposed proxy interface
typedef PipePortal#(0, 4, SlaveDataBusWidth) FlashIndicationPortalOutput;
interface FlashIndicationOutput;
    interface FlashIndicationPortalOutput portalIfc;
    interface FlashTop::FlashIndication ifc;
endinterface
interface FlashIndicationProxy;
    interface StdPortal portalIfc;
    interface FlashTop::FlashIndication ifc;
endinterface

interface FlashIndicationOutputPipeMethods;
    interface PipeIn#(ReadDone_Message) readDone;
    interface PipeIn#(WriteDone_Message) writeDone;
    interface PipeIn#(EraseDone_Message) eraseDone;
    interface PipeIn#(DebugDumpResp_Message) debugDumpResp;

endinterface

interface FlashIndicationOutputPipes;
    interface FlashIndicationOutputPipeMethods methods;
    interface FlashIndicationPortalOutput portalIfc;
endinterface

function Bit#(16) getFlashIndicationMessageSize(Bit#(16) methodNumber);
    case (methodNumber)
            0: return fromInteger(valueOf(SizeOf#(ReadDone_Message)));
            1: return fromInteger(valueOf(SizeOf#(WriteDone_Message)));
            2: return fromInteger(valueOf(SizeOf#(EraseDone_Message)));
            3: return fromInteger(valueOf(SizeOf#(DebugDumpResp_Message)));
    endcase
endfunction

(* synthesize *)
module mkFlashIndicationOutputPipes(FlashIndicationOutputPipes);
    Vector#(4, PipeOut#(Bit#(SlaveDataBusWidth))) indicationPipes;

    AdapterToBus#(SlaveDataBusWidth,ReadDone_Message) readDone_responseAdapter <- mkAdapterToBus();
    indicationPipes[0] = readDone_responseAdapter.out;

    AdapterToBus#(SlaveDataBusWidth,WriteDone_Message) writeDone_responseAdapter <- mkAdapterToBus();
    indicationPipes[1] = writeDone_responseAdapter.out;

    AdapterToBus#(SlaveDataBusWidth,EraseDone_Message) eraseDone_responseAdapter <- mkAdapterToBus();
    indicationPipes[2] = eraseDone_responseAdapter.out;

    AdapterToBus#(SlaveDataBusWidth,DebugDumpResp_Message) debugDumpResp_responseAdapter <- mkAdapterToBus();
    indicationPipes[3] = debugDumpResp_responseAdapter.out;

    PortalInterrupt#(SlaveDataBusWidth) intrInst <- mkPortalInterrupt(indicationPipes);
    interface FlashIndicationOutputPipeMethods methods;
    interface readDone = readDone_responseAdapter.in;
    interface writeDone = writeDone_responseAdapter.in;
    interface eraseDone = eraseDone_responseAdapter.in;
    interface debugDumpResp = debugDumpResp_responseAdapter.in;

    endinterface
    interface PipePortal portalIfc;
        interface PortalSize messageSize;
            method size = getFlashIndicationMessageSize;
        endinterface
        interface Vector requests = nil;
        interface Vector indications = indicationPipes;
        interface PortalInterrupt intr = intrInst;
    endinterface
endmodule

(* synthesize *)
module mkFlashIndicationOutput(FlashIndicationOutput);
    let indicationPipes <- mkFlashIndicationOutputPipes;
    interface FlashTop::FlashIndication ifc;

    method Action readDone(Bit#(32) tag, Bit#(64) cycles);
        indicationPipes.methods.readDone.enq(ReadDone_Message {tag: tag, cycles: cycles});
        //$display("indicationMethod 'readDone' invoked");
    endmethod
    method Action writeDone(Bit#(32) tag, Bit#(64) cycles);
        indicationPipes.methods.writeDone.enq(WriteDone_Message {tag: tag, cycles: cycles});
        //$display("indicationMethod 'writeDone' invoked");
    endmethod
    method Action eraseDone(Bit#(32) tag, Bit#(32) status, Bit#(64) cycles);
        indicationPipes.methods.eraseDone.enq(EraseDone_Message {tag: tag, status: status, cycles: cycles});
        //$display("indicationMethod 'eraseDone' invoked");
    endmethod
    method Action debugDumpResp(Bit#(32) card, Bit#(32) debug0, Bit#(32) debug1, Bit#(32) debug2, Bit#(32) debug3, Bit#(32) debug4, Bit#(32) debug5);
        indicationPipes.methods.debugDumpResp.enq(DebugDumpResp_Message {card: card, debug0: debug0, debug1: debug1, debug2: debug2, debug3: debug3, debug4: debug4, debug5: debug5});
        //$display("indicationMethod 'debugDumpResp' invoked");
    endmethod
    endinterface
    interface PipePortal portalIfc = indicationPipes.portalIfc;
endmodule
instance PortalMessageSize#(FlashIndicationOutput);
   function Bit#(16) portalMessageSize(FlashIndicationOutput p, Bit#(16) methodNumber);
      return getFlashIndicationMessageSize(methodNumber);
   endfunction
endinstance


interface FlashIndicationInverse;
    method ActionValue#(ReadDone_Message) readDone;
    method ActionValue#(WriteDone_Message) writeDone;
    method ActionValue#(EraseDone_Message) eraseDone;
    method ActionValue#(DebugDumpResp_Message) debugDumpResp;

endinterface

interface FlashIndicationInverter;
    interface FlashTop::FlashIndication ifc;
    interface FlashIndicationInverse inverseIfc;
endinterface

instance Connectable#(FlashIndicationInverse, FlashIndicationOutputPipeMethods);
   module mkConnection#(FlashIndicationInverse in, FlashIndicationOutputPipeMethods out)(Empty);
    mkConnection(in.readDone, out.readDone);
    mkConnection(in.writeDone, out.writeDone);
    mkConnection(in.eraseDone, out.eraseDone);
    mkConnection(in.debugDumpResp, out.debugDumpResp);

   endmodule
endinstance

(* synthesize *)
module mkFlashIndicationInverter(FlashIndicationInverter);
    FIFOF#(ReadDone_Message) fifo_readDone <- mkFIFOF();
    FIFOF#(WriteDone_Message) fifo_writeDone <- mkFIFOF();
    FIFOF#(EraseDone_Message) fifo_eraseDone <- mkFIFOF();
    FIFOF#(DebugDumpResp_Message) fifo_debugDumpResp <- mkFIFOF();

    interface FlashTop::FlashIndication ifc;

    method Action readDone(Bit#(32) tag, Bit#(64) cycles);
        fifo_readDone.enq(ReadDone_Message {tag: tag, cycles: cycles});
    endmethod
    method Action writeDone(Bit#(32) tag, Bit#(64) cycles);
        fifo_writeDone.enq(WriteDone_Message {tag: tag, cycles: cycles});
    endmethod
    method Action eraseDone(Bit#(32) tag, Bit#(32) status, Bit#(64) cycles);
        fifo_eraseDone.enq(EraseDone_Message {tag: tag, status: status, cycles: cycles});
    endmethod
    method Action debugDumpResp(Bit#(32) card, Bit#(32) debug0, Bit#(32) debug1, Bit#(32) debug2, Bit#(32) debug3, Bit#(32) debug4, Bit#(32) debug5);
        fifo_debugDumpResp.enq(DebugDumpResp_Message {card: card, debug0: debug0, debug1: debug1, debug2: debug2, debug3: debug3, debug4: debug4, debug5: debug5});
    endmethod
    endinterface
    interface FlashIndicationInverse inverseIfc;

    method ActionValue#(ReadDone_Message) readDone;
        fifo_readDone.deq;
        return fifo_readDone.first;
    endmethod
    method ActionValue#(WriteDone_Message) writeDone;
        fifo_writeDone.deq;
        return fifo_writeDone.first;
    endmethod
    method ActionValue#(EraseDone_Message) eraseDone;
        fifo_eraseDone.deq;
        return fifo_eraseDone.first;
    endmethod
    method ActionValue#(DebugDumpResp_Message) debugDumpResp;
        fifo_debugDumpResp.deq;
        return fifo_debugDumpResp.first;
    endmethod
    endinterface
endmodule

(* synthesize *)
module mkFlashIndicationInverterV(FlashIndicationInverter);
    PutInverter#(ReadDone_Message) inv_readDone <- mkPutInverter();
    PutInverter#(WriteDone_Message) inv_writeDone <- mkPutInverter();
    PutInverter#(EraseDone_Message) inv_eraseDone <- mkPutInverter();
    PutInverter#(DebugDumpResp_Message) inv_debugDumpResp <- mkPutInverter();

    interface FlashTop::FlashIndication ifc;

    method Action readDone(Bit#(32) tag, Bit#(64) cycles);
        inv_readDone.mod.put(ReadDone_Message {tag: tag, cycles: cycles});
    endmethod
    method Action writeDone(Bit#(32) tag, Bit#(64) cycles);
        inv_writeDone.mod.put(WriteDone_Message {tag: tag, cycles: cycles});
    endmethod
    method Action eraseDone(Bit#(32) tag, Bit#(32) status, Bit#(64) cycles);
        inv_eraseDone.mod.put(EraseDone_Message {tag: tag, status: status, cycles: cycles});
    endmethod
    method Action debugDumpResp(Bit#(32) card, Bit#(32) debug0, Bit#(32) debug1, Bit#(32) debug2, Bit#(32) debug3, Bit#(32) debug4, Bit#(32) debug5);
        inv_debugDumpResp.mod.put(DebugDumpResp_Message {card: card, debug0: debug0, debug1: debug1, debug2: debug2, debug3: debug3, debug4: debug4, debug5: debug5});
    endmethod
    endinterface
    interface FlashIndicationInverse inverseIfc;

    method ActionValue#(ReadDone_Message) readDone;
        let v <- inv_readDone.inverse.get;
        return v;
    endmethod
    method ActionValue#(WriteDone_Message) writeDone;
        let v <- inv_writeDone.inverse.get;
        return v;
    endmethod
    method ActionValue#(EraseDone_Message) eraseDone;
        let v <- inv_eraseDone.inverse.get;
        return v;
    endmethod
    method ActionValue#(DebugDumpResp_Message) debugDumpResp;
        let v <- inv_debugDumpResp.inverse.get;
        return v;
    endmethod
    endinterface
endmodule

// synthesizeable proxy MemPortal
(* synthesize *)
module mkFlashIndicationProxySynth#(Bit#(SlaveDataBusWidth) id)(FlashIndicationProxy);
  let dut <- mkFlashIndicationOutput();
  PortalCtrlMemSlave#(SlaveControlAddrWidth,SlaveDataBusWidth) ctrlPort <- mkPortalCtrlMemSlave(id, dut.portalIfc.intr);
  let memslave  <- mkMemMethodMuxOut(ctrlPort.memSlave,dut.portalIfc.indications);
  interface MemPortal portalIfc = (interface MemPortal;
      interface PhysMemSlave slave = memslave;
      interface ReadOnly interrupt = ctrlPort.interrupt;
      interface WriteOnly num_portals = ctrlPort.num_portals;
    endinterface);
  interface FlashTop::FlashIndication ifc = dut.ifc;
endmodule

// exposed proxy MemPortal
module mkFlashIndicationProxy#(idType id)(FlashIndicationProxy)
   provisos (Bits#(idType, a__),
	     Add#(b__, a__, SlaveDataBusWidth));
   let rv <- mkFlashIndicationProxySynth(extend(pack(id)));
   return rv;
endmodule
endpackage: FlashIndication
