#include "GeneratedTypes.h"
#ifdef PORTAL_JSON
#include "jsoncpp/json/json.h"

int FlashRequestJson_readPage ( struct PortalInternal *p, const uint32_t card, const uint32_t bus, const uint32_t chip, const uint32_t block, const uint32_t page, const uint32_t tag )
{
    Json::Value request;
    request.append(Json::Value("readPage"));
    request.append((Json::UInt64)card);
    request.append((Json::UInt64)bus);
    request.append((Json::UInt64)chip);
    request.append((Json::UInt64)block);
    request.append((Json::UInt64)page);
    request.append((Json::UInt64)tag);

    std::string requestjson = Json::FastWriter().write(request);;
    connectalJsonSend(p, requestjson.c_str(), (int)CHAN_NUM_FlashRequest_readPage);
    return 0;
};

int FlashRequestJson_writePage ( struct PortalInternal *p, const uint32_t card, const uint32_t bus, const uint32_t chip, const uint32_t block, const uint32_t page, const uint32_t tag )
{
    Json::Value request;
    request.append(Json::Value("writePage"));
    request.append((Json::UInt64)card);
    request.append((Json::UInt64)bus);
    request.append((Json::UInt64)chip);
    request.append((Json::UInt64)block);
    request.append((Json::UInt64)page);
    request.append((Json::UInt64)tag);

    std::string requestjson = Json::FastWriter().write(request);;
    connectalJsonSend(p, requestjson.c_str(), (int)CHAN_NUM_FlashRequest_writePage);
    return 0;
};

int FlashRequestJson_eraseBlock ( struct PortalInternal *p, const uint32_t card, const uint32_t bus, const uint32_t chip, const uint32_t block, const uint32_t tag )
{
    Json::Value request;
    request.append(Json::Value("eraseBlock"));
    request.append((Json::UInt64)card);
    request.append((Json::UInt64)bus);
    request.append((Json::UInt64)chip);
    request.append((Json::UInt64)block);
    request.append((Json::UInt64)tag);

    std::string requestjson = Json::FastWriter().write(request);;
    connectalJsonSend(p, requestjson.c_str(), (int)CHAN_NUM_FlashRequest_eraseBlock);
    return 0;
};

int FlashRequestJson_setDmaReadRef ( struct PortalInternal *p, const uint32_t sgId )
{
    Json::Value request;
    request.append(Json::Value("setDmaReadRef"));
    request.append((Json::UInt64)sgId);

    std::string requestjson = Json::FastWriter().write(request);;
    connectalJsonSend(p, requestjson.c_str(), (int)CHAN_NUM_FlashRequest_setDmaReadRef);
    return 0;
};

int FlashRequestJson_setDmaWriteRef ( struct PortalInternal *p, const uint32_t sgId )
{
    Json::Value request;
    request.append(Json::Value("setDmaWriteRef"));
    request.append((Json::UInt64)sgId);

    std::string requestjson = Json::FastWriter().write(request);;
    connectalJsonSend(p, requestjson.c_str(), (int)CHAN_NUM_FlashRequest_setDmaWriteRef);
    return 0;
};

int FlashRequestJson_start ( struct PortalInternal *p, const uint32_t dummy )
{
    Json::Value request;
    request.append(Json::Value("start"));
    request.append((Json::UInt64)dummy);

    std::string requestjson = Json::FastWriter().write(request);;
    connectalJsonSend(p, requestjson.c_str(), (int)CHAN_NUM_FlashRequest_start);
    return 0;
};

int FlashRequestJson_debugDumpReq ( struct PortalInternal *p, const uint32_t card )
{
    Json::Value request;
    request.append(Json::Value("debugDumpReq"));
    request.append((Json::UInt64)card);

    std::string requestjson = Json::FastWriter().write(request);;
    connectalJsonSend(p, requestjson.c_str(), (int)CHAN_NUM_FlashRequest_debugDumpReq);
    return 0;
};

int FlashRequestJson_setDebugVals ( struct PortalInternal *p, const uint32_t flag, const uint32_t debugDelay )
{
    Json::Value request;
    request.append(Json::Value("setDebugVals"));
    request.append((Json::UInt64)flag);
    request.append((Json::UInt64)debugDelay);

    std::string requestjson = Json::FastWriter().write(request);;
    connectalJsonSend(p, requestjson.c_str(), (int)CHAN_NUM_FlashRequest_setDebugVals);
    return 0;
};

FlashRequestCb FlashRequestJsonProxyReq = {
    portal_disconnect,
    FlashRequestJson_readPage,
    FlashRequestJson_writePage,
    FlashRequestJson_eraseBlock,
    FlashRequestJson_setDmaReadRef,
    FlashRequestJson_setDmaWriteRef,
    FlashRequestJson_start,
    FlashRequestJson_debugDumpReq,
    FlashRequestJson_setDebugVals,
};
FlashRequestCb *pFlashRequestJsonProxyReq = &FlashRequestJsonProxyReq;
const char * FlashRequestJson_methodSignatures()
{
    return "{\"setDebugVals\": [\"long\", \"long\"], \"writePage\": [\"long\", \"long\", \"long\", \"long\", \"long\", \"long\"], \"eraseBlock\": [\"long\", \"long\", \"long\", \"long\", \"long\"], \"debugDumpReq\": [\"long\"], \"setDmaWriteRef\": [\"long\"], \"start\": [\"long\"], \"setDmaReadRef\": [\"long\"], \"readPage\": [\"long\", \"long\", \"long\", \"long\", \"long\", \"long\"]}";
}

int FlashRequestJson_handleMessage(struct PortalInternal *p, unsigned int channel, int messageFd)
{
    static int runaway = 0;
    int   tmp __attribute__ ((unused));
    int tmpfd __attribute__ ((unused));
    FlashRequestData tempdata __attribute__ ((unused));
    memset(&tempdata, 0, sizeof(tempdata));
    Json::Value msg = Json::Value(connectalJsonReceive(p));
    switch (channel) {
    case CHAN_NUM_FlashRequest_readPage: {
        ((FlashRequestCb *)p->cb)->readPage(p, tempdata.readPage.card, tempdata.readPage.bus, tempdata.readPage.chip, tempdata.readPage.block, tempdata.readPage.page, tempdata.readPage.tag);
      } break;
    case CHAN_NUM_FlashRequest_writePage: {
        ((FlashRequestCb *)p->cb)->writePage(p, tempdata.writePage.card, tempdata.writePage.bus, tempdata.writePage.chip, tempdata.writePage.block, tempdata.writePage.page, tempdata.writePage.tag);
      } break;
    case CHAN_NUM_FlashRequest_eraseBlock: {
        ((FlashRequestCb *)p->cb)->eraseBlock(p, tempdata.eraseBlock.card, tempdata.eraseBlock.bus, tempdata.eraseBlock.chip, tempdata.eraseBlock.block, tempdata.eraseBlock.tag);
      } break;
    case CHAN_NUM_FlashRequest_setDmaReadRef: {
        ((FlashRequestCb *)p->cb)->setDmaReadRef(p, tempdata.setDmaReadRef.sgId);
      } break;
    case CHAN_NUM_FlashRequest_setDmaWriteRef: {
        ((FlashRequestCb *)p->cb)->setDmaWriteRef(p, tempdata.setDmaWriteRef.sgId);
      } break;
    case CHAN_NUM_FlashRequest_start: {
        ((FlashRequestCb *)p->cb)->start(p, tempdata.start.dummy);
      } break;
    case CHAN_NUM_FlashRequest_debugDumpReq: {
        ((FlashRequestCb *)p->cb)->debugDumpReq(p, tempdata.debugDumpReq.card);
      } break;
    case CHAN_NUM_FlashRequest_setDebugVals: {
        ((FlashRequestCb *)p->cb)->setDebugVals(p, tempdata.setDebugVals.flag, tempdata.setDebugVals.debugDelay);
      } break;
    default:
        PORTAL_PRINTF("FlashRequestJson_handleMessage: unknown channel 0x%x\n", channel);
        if (runaway++ > 10) {
            PORTAL_PRINTF("FlashRequestJson_handleMessage: too many bogus indications, exiting\n");
#ifndef __KERNEL__
            exit(-1);
#endif
        }
        return 0;
    }
    return 0;
}
#endif /* PORTAL_JSON */
