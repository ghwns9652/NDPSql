#include "GeneratedTypes.h"
#ifndef _FLASHREQUEST_H_
#define _FLASHREQUEST_H_
#include "portal.h"

class FlashRequestProxy : public Portal {
    FlashRequestCb *cb;
public:
    FlashRequestProxy(int id, int tile = DEFAULT_TILE, FlashRequestCb *cbarg = &FlashRequestProxyReq, int bufsize = FlashRequest_reqinfo, PortalPoller *poller = 0) :
        Portal(id, tile, bufsize, NULL, NULL, this, poller), cb(cbarg) {};
    FlashRequestProxy(int id, PortalTransportFunctions *transport, void *param, FlashRequestCb *cbarg = &FlashRequestProxyReq, int bufsize = FlashRequest_reqinfo, PortalPoller *poller = 0) :
        Portal(id, DEFAULT_TILE, bufsize, NULL, NULL, transport, param, this, poller), cb(cbarg) {};
    FlashRequestProxy(int id, PortalPoller *poller) :
        Portal(id, DEFAULT_TILE, FlashRequest_reqinfo, NULL, NULL, NULL, NULL, this, poller), cb(&FlashRequestProxyReq) {};
    int readPage ( const uint32_t card, const uint32_t bus, const uint32_t chip, const uint32_t block, const uint32_t page, const uint32_t tag ) { return cb->readPage (&pint, card, bus, chip, block, page, tag); };
    int writePage ( const uint32_t card, const uint32_t bus, const uint32_t chip, const uint32_t block, const uint32_t page, const uint32_t tag ) { return cb->writePage (&pint, card, bus, chip, block, page, tag); };
    int eraseBlock ( const uint32_t card, const uint32_t bus, const uint32_t chip, const uint32_t block, const uint32_t tag ) { return cb->eraseBlock (&pint, card, bus, chip, block, tag); };
    int setDmaReadRef ( const uint32_t sgId ) { return cb->setDmaReadRef (&pint, sgId); };
    int setDmaWriteRef ( const uint32_t sgId ) { return cb->setDmaWriteRef (&pint, sgId); };
    int start ( const uint32_t dummy ) { return cb->start (&pint, dummy); };
    int debugDumpReq ( const uint32_t card ) { return cb->debugDumpReq (&pint, card); };
    int setDebugVals ( const uint32_t flag, const uint32_t debugDelay ) { return cb->setDebugVals (&pint, flag, debugDelay); };
};

extern FlashRequestCb FlashRequest_cbTable;
class FlashRequestWrapper : public Portal {
public:
    FlashRequestWrapper(int id, int tile = DEFAULT_TILE, PORTAL_INDFUNC cba = FlashRequest_handleMessage, int bufsize = FlashRequest_reqinfo, PortalPoller *poller = 0) :
           Portal(id, tile, bufsize, cba, (void *)&FlashRequest_cbTable, this, poller) {
    };
    FlashRequestWrapper(int id, PortalTransportFunctions *transport, void *param, PORTAL_INDFUNC cba = FlashRequest_handleMessage, int bufsize = FlashRequest_reqinfo, PortalPoller *poller=0):
           Portal(id, DEFAULT_TILE, bufsize, cba, (void *)&FlashRequest_cbTable, transport, param, this, poller) {
    };
    FlashRequestWrapper(int id, PortalPoller *poller) :
           Portal(id, DEFAULT_TILE, FlashRequest_reqinfo, FlashRequest_handleMessage, (void *)&FlashRequest_cbTable, this, poller) {
    };
    FlashRequestWrapper(int id, PortalTransportFunctions *transport, void *param, PortalPoller *poller):
           Portal(id, DEFAULT_TILE, FlashRequest_reqinfo, FlashRequest_handleMessage, (void *)&FlashRequest_cbTable, transport, param, this, poller) {
    };
    virtual void disconnect(void) {
        printf("FlashRequestWrapper.disconnect called %d\n", pint.client_fd_number);
    };
    virtual void readPage ( const uint32_t card, const uint32_t bus, const uint32_t chip, const uint32_t block, const uint32_t page, const uint32_t tag ) = 0;
    virtual void writePage ( const uint32_t card, const uint32_t bus, const uint32_t chip, const uint32_t block, const uint32_t page, const uint32_t tag ) = 0;
    virtual void eraseBlock ( const uint32_t card, const uint32_t bus, const uint32_t chip, const uint32_t block, const uint32_t tag ) = 0;
    virtual void setDmaReadRef ( const uint32_t sgId ) = 0;
    virtual void setDmaWriteRef ( const uint32_t sgId ) = 0;
    virtual void start ( const uint32_t dummy ) = 0;
    virtual void debugDumpReq ( const uint32_t card ) = 0;
    virtual void setDebugVals ( const uint32_t flag, const uint32_t debugDelay ) = 0;
};
#endif // _FLASHREQUEST_H_
