package FlashRequest;

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
    Bit#(32) card;
    Bit#(32) bus;
    Bit#(32) chip;
    Bit#(32) block;
    Bit#(32) page;
    Bit#(32) tag;
} ReadPage_Message deriving (Bits);

typedef struct {
    Bit#(32) card;
    Bit#(32) bus;
    Bit#(32) chip;
    Bit#(32) block;
    Bit#(32) page;
    Bit#(32) tag;
} WritePage_Message deriving (Bits);

typedef struct {
    Bit#(32) card;
    Bit#(32) bus;
    Bit#(32) chip;
    Bit#(32) block;
    Bit#(32) tag;
} EraseBlock_Message deriving (Bits);

typedef struct {
    Bit#(32) sgId;
} SetDmaReadRef_Message deriving (Bits);

typedef struct {
    Bit#(32) sgId;
} SetDmaWriteRef_Message deriving (Bits);

typedef struct {
    Bit#(32) dummy;
} Start_Message deriving (Bits);

typedef struct {
    Bit#(32) card;
} DebugDumpReq_Message deriving (Bits);

typedef struct {
    Bit#(32) flag;
    Bit#(32) debugDelay;
} SetDebugVals_Message deriving (Bits);

// exposed wrapper portal interface
interface FlashRequestInputPipes;
    interface PipeOut#(ReadPage_Message) readPage_PipeOut;
    interface PipeOut#(WritePage_Message) writePage_PipeOut;
    interface PipeOut#(EraseBlock_Message) eraseBlock_PipeOut;
    interface PipeOut#(SetDmaReadRef_Message) setDmaReadRef_PipeOut;
    interface PipeOut#(SetDmaWriteRef_Message) setDmaWriteRef_PipeOut;
    interface PipeOut#(Start_Message) start_PipeOut;
    interface PipeOut#(DebugDumpReq_Message) debugDumpReq_PipeOut;
    interface PipeOut#(SetDebugVals_Message) setDebugVals_PipeOut;

endinterface
typedef PipePortal#(8, 0, SlaveDataBusWidth) FlashRequestPortalInput;
interface FlashRequestInput;
    interface FlashRequestPortalInput portalIfc;
    interface FlashRequestInputPipes pipes;
endinterface
interface FlashRequestWrapperPortal;
    interface FlashRequestPortalInput portalIfc;
endinterface
// exposed wrapper MemPortal interface
interface FlashRequestWrapper;
    interface StdPortal portalIfc;
endinterface

instance Connectable#(FlashRequestInputPipes,FlashRequest);
   module mkConnection#(FlashRequestInputPipes pipes, FlashRequest ifc)(Empty);

    rule handle_readPage_request;
        let request <- toGet(pipes.readPage_PipeOut).get();
        ifc.readPage(request.card, request.bus, request.chip, request.block, request.page, request.tag);
    endrule

    rule handle_writePage_request;
        let request <- toGet(pipes.writePage_PipeOut).get();
        ifc.writePage(request.card, request.bus, request.chip, request.block, request.page, request.tag);
    endrule

    rule handle_eraseBlock_request;
        let request <- toGet(pipes.eraseBlock_PipeOut).get();
        ifc.eraseBlock(request.card, request.bus, request.chip, request.block, request.tag);
    endrule

    rule handle_setDmaReadRef_request;
        let request <- toGet(pipes.setDmaReadRef_PipeOut).get();
        ifc.setDmaReadRef(request.sgId);
    endrule

    rule handle_setDmaWriteRef_request;
        let request <- toGet(pipes.setDmaWriteRef_PipeOut).get();
        ifc.setDmaWriteRef(request.sgId);
    endrule

    rule handle_start_request;
        let request <- toGet(pipes.start_PipeOut).get();
        ifc.start(request.dummy);
    endrule

    rule handle_debugDumpReq_request;
        let request <- toGet(pipes.debugDumpReq_PipeOut).get();
        ifc.debugDumpReq(request.card);
    endrule

    rule handle_setDebugVals_request;
        let request <- toGet(pipes.setDebugVals_PipeOut).get();
        ifc.setDebugVals(request.flag, request.debugDelay);
    endrule

   endmodule
endinstance

// exposed wrapper Portal implementation
(* synthesize *)
module mkFlashRequestInput(FlashRequestInput);
    Vector#(8, PipeIn#(Bit#(SlaveDataBusWidth))) requestPipeIn;

    AdapterFromBus#(SlaveDataBusWidth,ReadPage_Message) readPage_requestAdapter <- mkAdapterFromBus();
    requestPipeIn[0] = readPage_requestAdapter.in;

    AdapterFromBus#(SlaveDataBusWidth,WritePage_Message) writePage_requestAdapter <- mkAdapterFromBus();
    requestPipeIn[1] = writePage_requestAdapter.in;

    AdapterFromBus#(SlaveDataBusWidth,EraseBlock_Message) eraseBlock_requestAdapter <- mkAdapterFromBus();
    requestPipeIn[2] = eraseBlock_requestAdapter.in;

    AdapterFromBus#(SlaveDataBusWidth,SetDmaReadRef_Message) setDmaReadRef_requestAdapter <- mkAdapterFromBus();
    requestPipeIn[3] = setDmaReadRef_requestAdapter.in;

    AdapterFromBus#(SlaveDataBusWidth,SetDmaWriteRef_Message) setDmaWriteRef_requestAdapter <- mkAdapterFromBus();
    requestPipeIn[4] = setDmaWriteRef_requestAdapter.in;

    AdapterFromBus#(SlaveDataBusWidth,Start_Message) start_requestAdapter <- mkAdapterFromBus();
    requestPipeIn[5] = start_requestAdapter.in;

    AdapterFromBus#(SlaveDataBusWidth,DebugDumpReq_Message) debugDumpReq_requestAdapter <- mkAdapterFromBus();
    requestPipeIn[6] = debugDumpReq_requestAdapter.in;

    AdapterFromBus#(SlaveDataBusWidth,SetDebugVals_Message) setDebugVals_requestAdapter <- mkAdapterFromBus();
    requestPipeIn[7] = setDebugVals_requestAdapter.in;

    interface PipePortal portalIfc;
        interface PortalSize messageSize;
        method Bit#(16) size(Bit#(16) methodNumber);
            case (methodNumber)
            0: return fromInteger(valueOf(SizeOf#(ReadPage_Message)));
            1: return fromInteger(valueOf(SizeOf#(WritePage_Message)));
            2: return fromInteger(valueOf(SizeOf#(EraseBlock_Message)));
            3: return fromInteger(valueOf(SizeOf#(SetDmaReadRef_Message)));
            4: return fromInteger(valueOf(SizeOf#(SetDmaWriteRef_Message)));
            5: return fromInteger(valueOf(SizeOf#(Start_Message)));
            6: return fromInteger(valueOf(SizeOf#(DebugDumpReq_Message)));
            7: return fromInteger(valueOf(SizeOf#(SetDebugVals_Message)));
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
    interface FlashRequestInputPipes pipes;
        interface readPage_PipeOut = readPage_requestAdapter.out;
        interface writePage_PipeOut = writePage_requestAdapter.out;
        interface eraseBlock_PipeOut = eraseBlock_requestAdapter.out;
        interface setDmaReadRef_PipeOut = setDmaReadRef_requestAdapter.out;
        interface setDmaWriteRef_PipeOut = setDmaWriteRef_requestAdapter.out;
        interface start_PipeOut = start_requestAdapter.out;
        interface debugDumpReq_PipeOut = debugDumpReq_requestAdapter.out;
        interface setDebugVals_PipeOut = setDebugVals_requestAdapter.out;
    endinterface
endmodule

module mkFlashRequestWrapperPortal#(FlashRequest ifc)(FlashRequestWrapperPortal);
    let dut <- mkFlashRequestInput;
    mkConnection(dut.pipes, ifc);
    interface PipePortal portalIfc = dut.portalIfc;
endmodule

interface FlashRequestWrapperMemPortalPipes;
    interface FlashRequestInputPipes pipes;
    interface MemPortal#(12,32) portalIfc;
endinterface

(* synthesize *)
module mkFlashRequestWrapperMemPortalPipes#(Bit#(SlaveDataBusWidth) id)(FlashRequestWrapperMemPortalPipes);

  let dut <- mkFlashRequestInput;
  PortalCtrlMemSlave#(SlaveControlAddrWidth,SlaveDataBusWidth) ctrlPort <- mkPortalCtrlMemSlave(id, dut.portalIfc.intr);
  let memslave  <- mkMemMethodMuxIn(ctrlPort.memSlave,dut.portalIfc.requests);
  interface FlashRequestInputPipes pipes = dut.pipes;
  interface MemPortal portalIfc = (interface MemPortal;
      interface PhysMemSlave slave = memslave;
      interface ReadOnly interrupt = ctrlPort.interrupt;
      interface WriteOnly num_portals = ctrlPort.num_portals;
    endinterface);
endmodule

// exposed wrapper MemPortal implementation
module mkFlashRequestWrapper#(idType id, FlashRequest ifc)(FlashRequestWrapper)
   provisos (Bits#(idType, a__),
	     Add#(b__, a__, SlaveDataBusWidth));
  let dut <- mkFlashRequestWrapperMemPortalPipes(zeroExtend(pack(id)));
  mkConnection(dut.pipes, ifc);
  interface MemPortal portalIfc = dut.portalIfc;
endmodule

// exposed proxy interface
typedef PipePortal#(0, 8, SlaveDataBusWidth) FlashRequestPortalOutput;
interface FlashRequestOutput;
    interface FlashRequestPortalOutput portalIfc;
    interface FlashTop::FlashRequest ifc;
endinterface
interface FlashRequestProxy;
    interface StdPortal portalIfc;
    interface FlashTop::FlashRequest ifc;
endinterface

interface FlashRequestOutputPipeMethods;
    interface PipeIn#(ReadPage_Message) readPage;
    interface PipeIn#(WritePage_Message) writePage;
    interface PipeIn#(EraseBlock_Message) eraseBlock;
    interface PipeIn#(SetDmaReadRef_Message) setDmaReadRef;
    interface PipeIn#(SetDmaWriteRef_Message) setDmaWriteRef;
    interface PipeIn#(Start_Message) start;
    interface PipeIn#(DebugDumpReq_Message) debugDumpReq;
    interface PipeIn#(SetDebugVals_Message) setDebugVals;

endinterface

interface FlashRequestOutputPipes;
    interface FlashRequestOutputPipeMethods methods;
    interface FlashRequestPortalOutput portalIfc;
endinterface

function Bit#(16) getFlashRequestMessageSize(Bit#(16) methodNumber);
    case (methodNumber)
            0: return fromInteger(valueOf(SizeOf#(ReadPage_Message)));
            1: return fromInteger(valueOf(SizeOf#(WritePage_Message)));
            2: return fromInteger(valueOf(SizeOf#(EraseBlock_Message)));
            3: return fromInteger(valueOf(SizeOf#(SetDmaReadRef_Message)));
            4: return fromInteger(valueOf(SizeOf#(SetDmaWriteRef_Message)));
            5: return fromInteger(valueOf(SizeOf#(Start_Message)));
            6: return fromInteger(valueOf(SizeOf#(DebugDumpReq_Message)));
            7: return fromInteger(valueOf(SizeOf#(SetDebugVals_Message)));
    endcase
endfunction

(* synthesize *)
module mkFlashRequestOutputPipes(FlashRequestOutputPipes);
    Vector#(8, PipeOut#(Bit#(SlaveDataBusWidth))) indicationPipes;

    AdapterToBus#(SlaveDataBusWidth,ReadPage_Message) readPage_responseAdapter <- mkAdapterToBus();
    indicationPipes[0] = readPage_responseAdapter.out;

    AdapterToBus#(SlaveDataBusWidth,WritePage_Message) writePage_responseAdapter <- mkAdapterToBus();
    indicationPipes[1] = writePage_responseAdapter.out;

    AdapterToBus#(SlaveDataBusWidth,EraseBlock_Message) eraseBlock_responseAdapter <- mkAdapterToBus();
    indicationPipes[2] = eraseBlock_responseAdapter.out;

    AdapterToBus#(SlaveDataBusWidth,SetDmaReadRef_Message) setDmaReadRef_responseAdapter <- mkAdapterToBus();
    indicationPipes[3] = setDmaReadRef_responseAdapter.out;

    AdapterToBus#(SlaveDataBusWidth,SetDmaWriteRef_Message) setDmaWriteRef_responseAdapter <- mkAdapterToBus();
    indicationPipes[4] = setDmaWriteRef_responseAdapter.out;

    AdapterToBus#(SlaveDataBusWidth,Start_Message) start_responseAdapter <- mkAdapterToBus();
    indicationPipes[5] = start_responseAdapter.out;

    AdapterToBus#(SlaveDataBusWidth,DebugDumpReq_Message) debugDumpReq_responseAdapter <- mkAdapterToBus();
    indicationPipes[6] = debugDumpReq_responseAdapter.out;

    AdapterToBus#(SlaveDataBusWidth,SetDebugVals_Message) setDebugVals_responseAdapter <- mkAdapterToBus();
    indicationPipes[7] = setDebugVals_responseAdapter.out;

    PortalInterrupt#(SlaveDataBusWidth) intrInst <- mkPortalInterrupt(indicationPipes);
    interface FlashRequestOutputPipeMethods methods;
    interface readPage = readPage_responseAdapter.in;
    interface writePage = writePage_responseAdapter.in;
    interface eraseBlock = eraseBlock_responseAdapter.in;
    interface setDmaReadRef = setDmaReadRef_responseAdapter.in;
    interface setDmaWriteRef = setDmaWriteRef_responseAdapter.in;
    interface start = start_responseAdapter.in;
    interface debugDumpReq = debugDumpReq_responseAdapter.in;
    interface setDebugVals = setDebugVals_responseAdapter.in;

    endinterface
    interface PipePortal portalIfc;
        interface PortalSize messageSize;
            method size = getFlashRequestMessageSize;
        endinterface
        interface Vector requests = nil;
        interface Vector indications = indicationPipes;
        interface PortalInterrupt intr = intrInst;
    endinterface
endmodule

(* synthesize *)
module mkFlashRequestOutput(FlashRequestOutput);
    let indicationPipes <- mkFlashRequestOutputPipes;
    interface FlashTop::FlashRequest ifc;

    method Action readPage(Bit#(32) card, Bit#(32) bus, Bit#(32) chip, Bit#(32) block, Bit#(32) page, Bit#(32) tag);
        indicationPipes.methods.readPage.enq(ReadPage_Message {card: card, bus: bus, chip: chip, block: block, page: page, tag: tag});
        //$display("indicationMethod 'readPage' invoked");
    endmethod
    method Action writePage(Bit#(32) card, Bit#(32) bus, Bit#(32) chip, Bit#(32) block, Bit#(32) page, Bit#(32) tag);
        indicationPipes.methods.writePage.enq(WritePage_Message {card: card, bus: bus, chip: chip, block: block, page: page, tag: tag});
        //$display("indicationMethod 'writePage' invoked");
    endmethod
    method Action eraseBlock(Bit#(32) card, Bit#(32) bus, Bit#(32) chip, Bit#(32) block, Bit#(32) tag);
        indicationPipes.methods.eraseBlock.enq(EraseBlock_Message {card: card, bus: bus, chip: chip, block: block, tag: tag});
        //$display("indicationMethod 'eraseBlock' invoked");
    endmethod
    method Action setDmaReadRef(Bit#(32) sgId);
        indicationPipes.methods.setDmaReadRef.enq(SetDmaReadRef_Message {sgId: sgId});
        //$display("indicationMethod 'setDmaReadRef' invoked");
    endmethod
    method Action setDmaWriteRef(Bit#(32) sgId);
        indicationPipes.methods.setDmaWriteRef.enq(SetDmaWriteRef_Message {sgId: sgId});
        //$display("indicationMethod 'setDmaWriteRef' invoked");
    endmethod
    method Action start(Bit#(32) dummy);
        indicationPipes.methods.start.enq(Start_Message {dummy: dummy});
        //$display("indicationMethod 'start' invoked");
    endmethod
    method Action debugDumpReq(Bit#(32) card);
        indicationPipes.methods.debugDumpReq.enq(DebugDumpReq_Message {card: card});
        //$display("indicationMethod 'debugDumpReq' invoked");
    endmethod
    method Action setDebugVals(Bit#(32) flag, Bit#(32) debugDelay);
        indicationPipes.methods.setDebugVals.enq(SetDebugVals_Message {flag: flag, debugDelay: debugDelay});
        //$display("indicationMethod 'setDebugVals' invoked");
    endmethod
    endinterface
    interface PipePortal portalIfc = indicationPipes.portalIfc;
endmodule
instance PortalMessageSize#(FlashRequestOutput);
   function Bit#(16) portalMessageSize(FlashRequestOutput p, Bit#(16) methodNumber);
      return getFlashRequestMessageSize(methodNumber);
   endfunction
endinstance


interface FlashRequestInverse;
    method ActionValue#(ReadPage_Message) readPage;
    method ActionValue#(WritePage_Message) writePage;
    method ActionValue#(EraseBlock_Message) eraseBlock;
    method ActionValue#(SetDmaReadRef_Message) setDmaReadRef;
    method ActionValue#(SetDmaWriteRef_Message) setDmaWriteRef;
    method ActionValue#(Start_Message) start;
    method ActionValue#(DebugDumpReq_Message) debugDumpReq;
    method ActionValue#(SetDebugVals_Message) setDebugVals;

endinterface

interface FlashRequestInverter;
    interface FlashTop::FlashRequest ifc;
    interface FlashRequestInverse inverseIfc;
endinterface

instance Connectable#(FlashRequestInverse, FlashRequestOutputPipeMethods);
   module mkConnection#(FlashRequestInverse in, FlashRequestOutputPipeMethods out)(Empty);
    mkConnection(in.readPage, out.readPage);
    mkConnection(in.writePage, out.writePage);
    mkConnection(in.eraseBlock, out.eraseBlock);
    mkConnection(in.setDmaReadRef, out.setDmaReadRef);
    mkConnection(in.setDmaWriteRef, out.setDmaWriteRef);
    mkConnection(in.start, out.start);
    mkConnection(in.debugDumpReq, out.debugDumpReq);
    mkConnection(in.setDebugVals, out.setDebugVals);

   endmodule
endinstance

(* synthesize *)
module mkFlashRequestInverter(FlashRequestInverter);
    FIFOF#(ReadPage_Message) fifo_readPage <- mkFIFOF();
    FIFOF#(WritePage_Message) fifo_writePage <- mkFIFOF();
    FIFOF#(EraseBlock_Message) fifo_eraseBlock <- mkFIFOF();
    FIFOF#(SetDmaReadRef_Message) fifo_setDmaReadRef <- mkFIFOF();
    FIFOF#(SetDmaWriteRef_Message) fifo_setDmaWriteRef <- mkFIFOF();
    FIFOF#(Start_Message) fifo_start <- mkFIFOF();
    FIFOF#(DebugDumpReq_Message) fifo_debugDumpReq <- mkFIFOF();
    FIFOF#(SetDebugVals_Message) fifo_setDebugVals <- mkFIFOF();

    interface FlashTop::FlashRequest ifc;

    method Action readPage(Bit#(32) card, Bit#(32) bus, Bit#(32) chip, Bit#(32) block, Bit#(32) page, Bit#(32) tag);
        fifo_readPage.enq(ReadPage_Message {card: card, bus: bus, chip: chip, block: block, page: page, tag: tag});
    endmethod
    method Action writePage(Bit#(32) card, Bit#(32) bus, Bit#(32) chip, Bit#(32) block, Bit#(32) page, Bit#(32) tag);
        fifo_writePage.enq(WritePage_Message {card: card, bus: bus, chip: chip, block: block, page: page, tag: tag});
    endmethod
    method Action eraseBlock(Bit#(32) card, Bit#(32) bus, Bit#(32) chip, Bit#(32) block, Bit#(32) tag);
        fifo_eraseBlock.enq(EraseBlock_Message {card: card, bus: bus, chip: chip, block: block, tag: tag});
    endmethod
    method Action setDmaReadRef(Bit#(32) sgId);
        fifo_setDmaReadRef.enq(SetDmaReadRef_Message {sgId: sgId});
    endmethod
    method Action setDmaWriteRef(Bit#(32) sgId);
        fifo_setDmaWriteRef.enq(SetDmaWriteRef_Message {sgId: sgId});
    endmethod
    method Action start(Bit#(32) dummy);
        fifo_start.enq(Start_Message {dummy: dummy});
    endmethod
    method Action debugDumpReq(Bit#(32) card);
        fifo_debugDumpReq.enq(DebugDumpReq_Message {card: card});
    endmethod
    method Action setDebugVals(Bit#(32) flag, Bit#(32) debugDelay);
        fifo_setDebugVals.enq(SetDebugVals_Message {flag: flag, debugDelay: debugDelay});
    endmethod
    endinterface
    interface FlashRequestInverse inverseIfc;

    method ActionValue#(ReadPage_Message) readPage;
        fifo_readPage.deq;
        return fifo_readPage.first;
    endmethod
    method ActionValue#(WritePage_Message) writePage;
        fifo_writePage.deq;
        return fifo_writePage.first;
    endmethod
    method ActionValue#(EraseBlock_Message) eraseBlock;
        fifo_eraseBlock.deq;
        return fifo_eraseBlock.first;
    endmethod
    method ActionValue#(SetDmaReadRef_Message) setDmaReadRef;
        fifo_setDmaReadRef.deq;
        return fifo_setDmaReadRef.first;
    endmethod
    method ActionValue#(SetDmaWriteRef_Message) setDmaWriteRef;
        fifo_setDmaWriteRef.deq;
        return fifo_setDmaWriteRef.first;
    endmethod
    method ActionValue#(Start_Message) start;
        fifo_start.deq;
        return fifo_start.first;
    endmethod
    method ActionValue#(DebugDumpReq_Message) debugDumpReq;
        fifo_debugDumpReq.deq;
        return fifo_debugDumpReq.first;
    endmethod
    method ActionValue#(SetDebugVals_Message) setDebugVals;
        fifo_setDebugVals.deq;
        return fifo_setDebugVals.first;
    endmethod
    endinterface
endmodule

(* synthesize *)
module mkFlashRequestInverterV(FlashRequestInverter);
    PutInverter#(ReadPage_Message) inv_readPage <- mkPutInverter();
    PutInverter#(WritePage_Message) inv_writePage <- mkPutInverter();
    PutInverter#(EraseBlock_Message) inv_eraseBlock <- mkPutInverter();
    PutInverter#(SetDmaReadRef_Message) inv_setDmaReadRef <- mkPutInverter();
    PutInverter#(SetDmaWriteRef_Message) inv_setDmaWriteRef <- mkPutInverter();
    PutInverter#(Start_Message) inv_start <- mkPutInverter();
    PutInverter#(DebugDumpReq_Message) inv_debugDumpReq <- mkPutInverter();
    PutInverter#(SetDebugVals_Message) inv_setDebugVals <- mkPutInverter();

    interface FlashTop::FlashRequest ifc;

    method Action readPage(Bit#(32) card, Bit#(32) bus, Bit#(32) chip, Bit#(32) block, Bit#(32) page, Bit#(32) tag);
        inv_readPage.mod.put(ReadPage_Message {card: card, bus: bus, chip: chip, block: block, page: page, tag: tag});
    endmethod
    method Action writePage(Bit#(32) card, Bit#(32) bus, Bit#(32) chip, Bit#(32) block, Bit#(32) page, Bit#(32) tag);
        inv_writePage.mod.put(WritePage_Message {card: card, bus: bus, chip: chip, block: block, page: page, tag: tag});
    endmethod
    method Action eraseBlock(Bit#(32) card, Bit#(32) bus, Bit#(32) chip, Bit#(32) block, Bit#(32) tag);
        inv_eraseBlock.mod.put(EraseBlock_Message {card: card, bus: bus, chip: chip, block: block, tag: tag});
    endmethod
    method Action setDmaReadRef(Bit#(32) sgId);
        inv_setDmaReadRef.mod.put(SetDmaReadRef_Message {sgId: sgId});
    endmethod
    method Action setDmaWriteRef(Bit#(32) sgId);
        inv_setDmaWriteRef.mod.put(SetDmaWriteRef_Message {sgId: sgId});
    endmethod
    method Action start(Bit#(32) dummy);
        inv_start.mod.put(Start_Message {dummy: dummy});
    endmethod
    method Action debugDumpReq(Bit#(32) card);
        inv_debugDumpReq.mod.put(DebugDumpReq_Message {card: card});
    endmethod
    method Action setDebugVals(Bit#(32) flag, Bit#(32) debugDelay);
        inv_setDebugVals.mod.put(SetDebugVals_Message {flag: flag, debugDelay: debugDelay});
    endmethod
    endinterface
    interface FlashRequestInverse inverseIfc;

    method ActionValue#(ReadPage_Message) readPage;
        let v <- inv_readPage.inverse.get;
        return v;
    endmethod
    method ActionValue#(WritePage_Message) writePage;
        let v <- inv_writePage.inverse.get;
        return v;
    endmethod
    method ActionValue#(EraseBlock_Message) eraseBlock;
        let v <- inv_eraseBlock.inverse.get;
        return v;
    endmethod
    method ActionValue#(SetDmaReadRef_Message) setDmaReadRef;
        let v <- inv_setDmaReadRef.inverse.get;
        return v;
    endmethod
    method ActionValue#(SetDmaWriteRef_Message) setDmaWriteRef;
        let v <- inv_setDmaWriteRef.inverse.get;
        return v;
    endmethod
    method ActionValue#(Start_Message) start;
        let v <- inv_start.inverse.get;
        return v;
    endmethod
    method ActionValue#(DebugDumpReq_Message) debugDumpReq;
        let v <- inv_debugDumpReq.inverse.get;
        return v;
    endmethod
    method ActionValue#(SetDebugVals_Message) setDebugVals;
        let v <- inv_setDebugVals.inverse.get;
        return v;
    endmethod
    endinterface
endmodule

// synthesizeable proxy MemPortal
(* synthesize *)
module mkFlashRequestProxySynth#(Bit#(SlaveDataBusWidth) id)(FlashRequestProxy);
  let dut <- mkFlashRequestOutput();
  PortalCtrlMemSlave#(SlaveControlAddrWidth,SlaveDataBusWidth) ctrlPort <- mkPortalCtrlMemSlave(id, dut.portalIfc.intr);
  let memslave  <- mkMemMethodMuxOut(ctrlPort.memSlave,dut.portalIfc.indications);
  interface MemPortal portalIfc = (interface MemPortal;
      interface PhysMemSlave slave = memslave;
      interface ReadOnly interrupt = ctrlPort.interrupt;
      interface WriteOnly num_portals = ctrlPort.num_portals;
    endinterface);
  interface FlashTop::FlashRequest ifc = dut.ifc;
endmodule

// exposed proxy MemPortal
module mkFlashRequestProxy#(idType id)(FlashRequestProxy)
   provisos (Bits#(idType, a__),
	     Add#(b__, a__, SlaveDataBusWidth));
   let rv <- mkFlashRequestProxySynth(extend(pack(id)));
   return rv;
endmodule
endpackage: FlashRequest
