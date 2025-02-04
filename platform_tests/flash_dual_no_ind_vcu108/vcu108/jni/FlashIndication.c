#include "GeneratedTypes.h"

int FlashIndication_readDone ( struct PortalInternal *p, const uint32_t tag, const uint64_t cycles )
{
    volatile unsigned int* temp_working_addr_start = p->transport->mapchannelReq(p, CHAN_NUM_FlashIndication_readDone, 4);
    volatile unsigned int* temp_working_addr = temp_working_addr_start;
    if (p->transport->busywait(p, CHAN_NUM_FlashIndication_readDone, "FlashIndication_readDone")) return 1;
    p->transport->write(p, &temp_working_addr, tag);
    p->transport->write(p, &temp_working_addr, (cycles>>32));
    p->transport->write(p, &temp_working_addr, cycles);
    p->transport->send(p, temp_working_addr_start, (CHAN_NUM_FlashIndication_readDone << 16) | 4, -1);
    return 0;
};

int FlashIndication_writeDone ( struct PortalInternal *p, const uint32_t tag, const uint64_t cycles )
{
    volatile unsigned int* temp_working_addr_start = p->transport->mapchannelReq(p, CHAN_NUM_FlashIndication_writeDone, 4);
    volatile unsigned int* temp_working_addr = temp_working_addr_start;
    if (p->transport->busywait(p, CHAN_NUM_FlashIndication_writeDone, "FlashIndication_writeDone")) return 1;
    p->transport->write(p, &temp_working_addr, tag);
    p->transport->write(p, &temp_working_addr, (cycles>>32));
    p->transport->write(p, &temp_working_addr, cycles);
    p->transport->send(p, temp_working_addr_start, (CHAN_NUM_FlashIndication_writeDone << 16) | 4, -1);
    return 0;
};

int FlashIndication_eraseDone ( struct PortalInternal *p, const uint32_t tag, const uint32_t status, const uint64_t cycles )
{
    volatile unsigned int* temp_working_addr_start = p->transport->mapchannelReq(p, CHAN_NUM_FlashIndication_eraseDone, 5);
    volatile unsigned int* temp_working_addr = temp_working_addr_start;
    if (p->transport->busywait(p, CHAN_NUM_FlashIndication_eraseDone, "FlashIndication_eraseDone")) return 1;
    p->transport->write(p, &temp_working_addr, tag);
    p->transport->write(p, &temp_working_addr, status);
    p->transport->write(p, &temp_working_addr, (cycles>>32));
    p->transport->write(p, &temp_working_addr, cycles);
    p->transport->send(p, temp_working_addr_start, (CHAN_NUM_FlashIndication_eraseDone << 16) | 5, -1);
    return 0;
};

int FlashIndication_debugDumpResp ( struct PortalInternal *p, const uint32_t card, const uint32_t debug0, const uint32_t debug1, const uint32_t debug2, const uint32_t debug3, const uint32_t debug4, const uint32_t debug5 )
{
    volatile unsigned int* temp_working_addr_start = p->transport->mapchannelReq(p, CHAN_NUM_FlashIndication_debugDumpResp, 8);
    volatile unsigned int* temp_working_addr = temp_working_addr_start;
    if (p->transport->busywait(p, CHAN_NUM_FlashIndication_debugDumpResp, "FlashIndication_debugDumpResp")) return 1;
    p->transport->write(p, &temp_working_addr, card);
    p->transport->write(p, &temp_working_addr, debug0);
    p->transport->write(p, &temp_working_addr, debug1);
    p->transport->write(p, &temp_working_addr, debug2);
    p->transport->write(p, &temp_working_addr, debug3);
    p->transport->write(p, &temp_working_addr, debug4);
    p->transport->write(p, &temp_working_addr, debug5);
    p->transport->send(p, temp_working_addr_start, (CHAN_NUM_FlashIndication_debugDumpResp << 16) | 8, -1);
    return 0;
};

FlashIndicationCb FlashIndicationProxyReq = {
    portal_disconnect,
    FlashIndication_readDone,
    FlashIndication_writeDone,
    FlashIndication_eraseDone,
    FlashIndication_debugDumpResp,
};
FlashIndicationCb *pFlashIndicationProxyReq = &FlashIndicationProxyReq;

const uint32_t FlashIndication_reqinfo = 0x40020;
const char * FlashIndication_methodSignatures()
{
    return "{\"writeDone\": [\"long\", \"long\"], \"debugDumpResp\": [\"long\", \"long\", \"long\", \"long\", \"long\", \"long\", \"long\"], \"readDone\": [\"long\", \"long\"], \"eraseDone\": [\"long\", \"long\", \"long\"]}";
}

int FlashIndication_handleMessage(struct PortalInternal *p, unsigned int channel, int messageFd)
{
    static int runaway = 0;
    int   tmp __attribute__ ((unused));
    int tmpfd __attribute__ ((unused));
    FlashIndicationData tempdata __attribute__ ((unused));
    memset(&tempdata, 0, sizeof(tempdata));
    volatile unsigned int* temp_working_addr = p->transport->mapchannelInd(p, channel);
    switch (channel) {
    case CHAN_NUM_FlashIndication_readDone: {
        p->transport->recv(p, temp_working_addr, 3, &tmpfd);
        tmp = p->transport->read(p, &temp_working_addr);
        tempdata.readDone.tag = (uint32_t)(((tmp)&0xfffffffful));
        tmp = p->transport->read(p, &temp_working_addr);
        tempdata.readDone.cycles = (uint64_t)(((uint64_t)(((tmp)&0xfffffffful))<<32));
        tmp = p->transport->read(p, &temp_working_addr);
        tempdata.readDone.cycles |= (uint64_t)(((tmp)&0xfffffffful));
        ((FlashIndicationCb *)p->cb)->readDone(p, tempdata.readDone.tag, tempdata.readDone.cycles);
      } break;
    case CHAN_NUM_FlashIndication_writeDone: {
        p->transport->recv(p, temp_working_addr, 3, &tmpfd);
        tmp = p->transport->read(p, &temp_working_addr);
        tempdata.writeDone.tag = (uint32_t)(((tmp)&0xfffffffful));
        tmp = p->transport->read(p, &temp_working_addr);
        tempdata.writeDone.cycles = (uint64_t)(((uint64_t)(((tmp)&0xfffffffful))<<32));
        tmp = p->transport->read(p, &temp_working_addr);
        tempdata.writeDone.cycles |= (uint64_t)(((tmp)&0xfffffffful));
        ((FlashIndicationCb *)p->cb)->writeDone(p, tempdata.writeDone.tag, tempdata.writeDone.cycles);
      } break;
    case CHAN_NUM_FlashIndication_eraseDone: {
        p->transport->recv(p, temp_working_addr, 4, &tmpfd);
        tmp = p->transport->read(p, &temp_working_addr);
        tempdata.eraseDone.tag = (uint32_t)(((tmp)&0xfffffffful));
        tmp = p->transport->read(p, &temp_working_addr);
        tempdata.eraseDone.status = (uint32_t)(((tmp)&0xfffffffful));
        tmp = p->transport->read(p, &temp_working_addr);
        tempdata.eraseDone.cycles = (uint64_t)(((uint64_t)(((tmp)&0xfffffffful))<<32));
        tmp = p->transport->read(p, &temp_working_addr);
        tempdata.eraseDone.cycles |= (uint64_t)(((tmp)&0xfffffffful));
        ((FlashIndicationCb *)p->cb)->eraseDone(p, tempdata.eraseDone.tag, tempdata.eraseDone.status, tempdata.eraseDone.cycles);
      } break;
    case CHAN_NUM_FlashIndication_debugDumpResp: {
        p->transport->recv(p, temp_working_addr, 7, &tmpfd);
        tmp = p->transport->read(p, &temp_working_addr);
        tempdata.debugDumpResp.card = (uint32_t)(((tmp)&0xfffffffful));
        tmp = p->transport->read(p, &temp_working_addr);
        tempdata.debugDumpResp.debug0 = (uint32_t)(((tmp)&0xfffffffful));
        tmp = p->transport->read(p, &temp_working_addr);
        tempdata.debugDumpResp.debug1 = (uint32_t)(((tmp)&0xfffffffful));
        tmp = p->transport->read(p, &temp_working_addr);
        tempdata.debugDumpResp.debug2 = (uint32_t)(((tmp)&0xfffffffful));
        tmp = p->transport->read(p, &temp_working_addr);
        tempdata.debugDumpResp.debug3 = (uint32_t)(((tmp)&0xfffffffful));
        tmp = p->transport->read(p, &temp_working_addr);
        tempdata.debugDumpResp.debug4 = (uint32_t)(((tmp)&0xfffffffful));
        tmp = p->transport->read(p, &temp_working_addr);
        tempdata.debugDumpResp.debug5 = (uint32_t)(((tmp)&0xfffffffful));
        ((FlashIndicationCb *)p->cb)->debugDumpResp(p, tempdata.debugDumpResp.card, tempdata.debugDumpResp.debug0, tempdata.debugDumpResp.debug1, tempdata.debugDumpResp.debug2, tempdata.debugDumpResp.debug3, tempdata.debugDumpResp.debug4, tempdata.debugDumpResp.debug5);
      } break;
    default:
        PORTAL_PRINTF("FlashIndication_handleMessage: unknown channel 0x%x\n", channel);
        if (runaway++ > 10) {
            PORTAL_PRINTF("FlashIndication_handleMessage: too many bogus indications, exiting\n");
#ifndef __KERNEL__
            exit(-1);
#endif
        }
        return 0;
    }
    return 0;
}
