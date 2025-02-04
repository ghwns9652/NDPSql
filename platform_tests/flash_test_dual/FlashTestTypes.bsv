import ControllerTypes::*;
typedef TMul#(PagesPerBlock, 8192) BlockSize;

typedef TMul#(NUM_BUSES, TMul#(BlocksPerCE, ChipsPerBus)) NumBlocks;

typedef TMul#(NUM_BUSES, ChipsPerBus) NumChips;

typedef Bit#(TLog#(NumBlocks)) BlockIdxT;
typedef Bit#(TLog#(BlocksPerCE)) BlockT;

typedef TDiv#(NumBlocks, TMul#(ChipsPerBus, NUM_BUSES)) NumBlocksPerChip;



Integer lgBlkOffset = valueOf(TLog#(BlocksPerCE));
Integer lgWayOffset = valueOf(TLog#(ChipsPerBus));
Integer lgBusOffset = valueOf(TLog#(NUM_BUSES));


`ifdef BSIM

typedef TMul#(TMul#(BlockSize,NUM_BUSES),ChipsPerBus) TestSz;  // 8MB
`else
typedef TMul#(TMul#(TMul#(BlockSize,NUM_BUSES),ChipsPerBus),1)  TestSz;  // 1GB
`endif

Integer testSz = valueOf(TestSz);

Integer blockSz = valueOf(BlockSize);

typedef TDiv#(TestSz, BlockSize) TestNumBlocks;

typedef TDiv#(TestSz, 8192) TestNumPages;

Integer testNumBlocks = valueOf(TestNumBlocks);

Integer testNumPages = valueOf(TestNumPages);

Integer testNumWords = testNumPages * pageWords;

