/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0.  If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright 1997 - July 2008 CWI, August 2008 - 2017 MonetDB B.V.
 */

/* This file should not be included in any file outside of this directory */

// #ifndef LIBGDK
// #error this file should not be included outside its source directory
// #endif

#define DISABLE_PARENT_HASH 1
/* #define PERSISTENTHASH 1 */
#define PERSISTENTIDX 1

#include "gdk_system_private.h"

#ifdef __cplusplus
#define cpp_export extern "C"
#else
#define cpp_export 
#endif

enum heaptype {
	offheap,
	varheap,
	hashheap,
	imprintsheap,
	orderidxheap
};

__hidden cpp_export gdk_return ATOMheap(int id, Heap *hp, size_t cap)
	__attribute__((__warn_unused_result__))
	__attribute__((__visibility__("hidden")));
__hidden cpp_export int ATOMisdescendant(int id, int parentid)
	__attribute__((__visibility__("hidden")));
__hidden cpp_export int ATOMunknown_find(const char *nme)
	__attribute__((__warn_unused_result__))
	__attribute__((__visibility__("hidden")));
__hidden cpp_export str ATOMunknown_name(int a)
	__attribute__((__visibility__("hidden")));
__hidden cpp_export void ATOMunknown_clean(void)
	__attribute__((__visibility__("hidden")));
__hidden cpp_export int BATcheckhash(BAT *b)
	__attribute__((__visibility__("hidden")));
__hidden cpp_export int BATcheckimprints(BAT *b)
	__attribute__((__visibility__("hidden")));
__hidden cpp_export gdk_return BATcheckmodes(BAT *b, int persistent)
	__attribute__((__warn_unused_result__))
	__attribute__((__visibility__("hidden")));
__hidden cpp_export int BATcheckorderidx(BAT *b)
	__attribute__((__visibility__("hidden")));
__hidden cpp_export BAT *BATcreatedesc(oid hseq, int tt, int heapnames, int role)
	__attribute__((__visibility__("hidden")));
__hidden cpp_export void BATdelete(BAT *b)
	__attribute__((__visibility__("hidden")));
__hidden cpp_export void BATdestroy(BAT *b)
	__attribute__((__visibility__("hidden")));
__hidden cpp_export void BATfree(BAT *b)
	__attribute__((__visibility__("hidden")));
__hidden cpp_export gdk_return BATgroup_internal(BAT **groups, BAT **extents, BAT **histo, BAT *b, BAT *s, BAT *g, BAT *e, BAT *h, int subsorted)
	__attribute__((__warn_unused_result__))
	__attribute__((__visibility__("hidden")));
__hidden cpp_export void BATinit_idents(BAT *bn)
	__attribute__((__visibility__("hidden")));
__hidden cpp_export BAT *BATload_intern(bat bid, int lock)
	__attribute__((__visibility__("hidden")));
__hidden cpp_export gdk_return BATmaterialize(BAT *b)
	__attribute__((__warn_unused_result__))
	__attribute__((__visibility__("hidden")));
__hidden cpp_export gdk_return BATsave(BAT *b)
	__attribute__((__warn_unused_result__))
	__attribute__((__visibility__("hidden")));
__hidden cpp_export void BATsetdims(BAT *b)
	__attribute__((__visibility__("hidden")));
__hidden cpp_export size_t BATvmsize(BAT *b, int dirty)
	__attribute__((__visibility__("hidden")));
__hidden cpp_export gdk_return BBPcacheit(BAT *bn, int lock)
	__attribute__((__warn_unused_result__))
	__attribute__((__visibility__("hidden")));
void BBPdump(void);		/* never called: for debugging only */
__hidden cpp_export void BBPexit(void)
	__attribute__((__visibility__("hidden")));
__hidden cpp_export BAT *BBPgetdesc(bat i)
	__attribute__((__visibility__("hidden")));
__hidden cpp_export void BBPinit(void)
	__attribute__((__visibility__("hidden")));
__hidden cpp_export bat BBPinsert(BAT *bn)
	__attribute__((__warn_unused_result__))
	__attribute__((__visibility__("hidden")));
__hidden cpp_export int BBPselectfarm(int role, int type, enum heaptype hptype)
	__attribute__((__visibility__("hidden")));
__hidden cpp_export void BBPunshare(bat b)
	__attribute__((__visibility__("hidden")));
__hidden cpp_export BUN binsearch(const oid *restrict indir, oid offset, int type, const void *restrict vals, const char * restrict vars, int width, BUN lo, BUN hi, const void *restrict v, int ordering, int last)
	__attribute__((__visibility__("hidden")));
__hidden cpp_export BUN binsearch_bte(const oid *restrict indir, oid offset, const bte *restrict vals, BUN lo, BUN hi, bte v, int ordering, int last)
	__attribute__((__visibility__("hidden")));
__hidden cpp_export BUN binsearch_sht(const oid *restrict indir, oid offset, const sht *restrict vals, BUN lo, BUN hi, sht v, int ordering, int last)
	__attribute__((__visibility__("hidden")));
__hidden cpp_export BUN binsearch_int(const oid *restrict indir, oid offset, const int *restrict vals, BUN lo, BUN hi, int v, int ordering, int last)
	__attribute__((__visibility__("hidden")));
__hidden cpp_export BUN binsearch_lng(const oid *restrict indir, oid offset, const lng *restrict vals, BUN lo, BUN hi, lng v, int ordering, int last)
	__attribute__((__visibility__("hidden")));
#ifdef HAVE_HGE
__hidden cpp_export BUN binsearch_hge(const oid *restrict indir, oid offset, const hge *restrict vals, BUN lo, BUN hi, hge v, int ordering, int last)
	__attribute__((__visibility__("hidden")));
#endif
__hidden cpp_export BUN binsearch_flt(const oid *restrict indir, oid offset, const flt *restrict vals, BUN lo, BUN hi, flt v, int ordering, int last)
	__attribute__((__visibility__("hidden")));
__hidden cpp_export BUN binsearch_dbl(const oid *restrict indir, oid offset, const dbl *restrict vals, BUN lo, BUN hi, dbl v, int ordering, int last)
	__attribute__((__visibility__("hidden")));
__hidden cpp_export Heap *createOIDXheap(BAT *b, int stable)
	__attribute__((__visibility__("hidden")));
__hidden cpp_export gdk_return BUNreplace(BAT *b, oid left, const void *right, bit force)
	__attribute__((__warn_unused_result__))
	__attribute__((__visibility__("hidden")));
__hidden cpp_export gdk_return GDKextend(const char *fn, size_t size)
	__attribute__((__warn_unused_result__))
	__attribute__((__visibility__("hidden")));
__hidden cpp_export gdk_return GDKextendf(int fd, size_t size, const char *fn)
	__attribute__((__warn_unused_result__))
	__attribute__((__visibility__("hidden")));
__hidden cpp_export int GDKfdlocate(int farmid, const char *nme, const char *mode, const char *ext)
	__attribute__((__visibility__("hidden")));
__hidden cpp_export FILE *GDKfilelocate(int farmid, const char *nme, const char *mode, const char *ext)
	__attribute__((__visibility__("hidden")));
__hidden cpp_export FILE *GDKfileopen(int farmid, const char *dir, const char *name, const char *extension, const char *mode)
	__attribute__((__visibility__("hidden")));
__hidden cpp_export char *GDKload(int farmid, const char *nme, const char *ext, size_t size, size_t *maxsize, storage_t mode)
	__attribute__((__visibility__("hidden")));
__hidden cpp_export void GDKlog(_In_z_ _Printf_format_string_ FILE * fl, const char *format, ...)
	__attribute__((__format__(__printf__, 2, 3)))
	__attribute__((__visibility__("hidden")));
__hidden cpp_export gdk_return GDKmove(int farmid, const char *dir1, const char *nme1, const char *ext1, const char *dir2, const char *nme2, const char *ext2)
	__attribute__((__warn_unused_result__))
	__attribute__((__visibility__("hidden")));
__hidden cpp_export void *GDKmremap(const char *path, int mode, void *old_address, size_t old_size, size_t *new_size)
	__attribute__((__visibility__("hidden")));
__hidden cpp_export gdk_return GDKmunmap(void *addr, size_t len)
	__attribute__((__warn_unused_result__))
	__attribute__((__visibility__("hidden")));
__hidden cpp_export gdk_return GDKremovedir(int farmid, const char *nme)
	__attribute__((__warn_unused_result__))
	__attribute__((__visibility__("hidden")));
__hidden cpp_export gdk_return GDKsave(int farmid, const char *nme, const char *ext, void *buf, size_t size, storage_t mode, int dosync)
	__attribute__((__warn_unused_result__))
	__attribute__((__visibility__("hidden")));
__hidden cpp_export gdk_return GDKssort_rev(void *h, void *t, const void *base, size_t n, int hs, int ts, int tpe)
	__attribute__((__warn_unused_result__))
	__attribute__((__visibility__("hidden")));
__hidden cpp_export gdk_return GDKssort(void *h, void *t, const void *base, size_t n, int hs, int ts, int tpe)
	__attribute__((__warn_unused_result__))
	__attribute__((__visibility__("hidden")));
__hidden cpp_export gdk_return GDKunlink(int farmid, const char *dir, const char *nme, const char *extension)
	__attribute__((__visibility__("hidden")));
__hidden cpp_export void HASHfree(BAT *b)
	__attribute__((__visibility__("hidden")));
__hidden cpp_export int HASHgonebad(BAT *b, const void *v)
	__attribute__((__visibility__("hidden")));
__hidden cpp_export BUN HASHmask(BUN cnt)
	__attribute__((__visibility__("hidden")));
__hidden cpp_export Hash *HASHnew(Heap *hp, int tpe, BUN size, BUN mask, BUN count)
	__attribute__((__visibility__("hidden")));
__hidden cpp_export gdk_return HEAPalloc(Heap *h, size_t nitems, size_t itemsize)
	__attribute__((__warn_unused_result__))
	__attribute__((__visibility__("hidden")));
__hidden cpp_export gdk_return HEAPcopy(Heap *dst, Heap *src)
	__attribute__((__warn_unused_result__))
	__attribute__((__visibility__("hidden")));
__hidden cpp_export int HEAPdelete(Heap *h, const char *o, const char *ext)
	__attribute__((__visibility__("hidden")));
__hidden cpp_export void HEAPfree(Heap *h, int remove)
	__attribute__((__visibility__("hidden")));
__hidden cpp_export gdk_return HEAPload(Heap *h, const char *nme, const char *ext, int trunc)
	__attribute__((__warn_unused_result__))
	__attribute__((__visibility__("hidden")));
__hidden cpp_export gdk_return HEAPsave(Heap *h, const char *nme, const char *ext)
	__attribute__((__warn_unused_result__))
	__attribute__((__visibility__("hidden")));
__hidden cpp_export gdk_return HEAPshrink(Heap *h, size_t size)
	__attribute__((__warn_unused_result__))
	__attribute__((__visibility__("hidden")));
__hidden cpp_export int HEAPwarm(Heap *h)
	__attribute__((__visibility__("hidden")));
__hidden cpp_export void IMPSfree(BAT *b)
	__attribute__((__visibility__("hidden")));
__hidden cpp_export int IMPSgetbin(int tpe, bte bits, const char *restrict bins, const void *restrict v)
	__attribute__((__visibility__("hidden")));
#ifndef NDEBUG
__hidden cpp_export void IMPSprint(BAT *b)
	__attribute__((__visibility__("hidden")));
#endif
__hidden cpp_export void MT_init_posix(void)
	__attribute__((__visibility__("hidden")));
__hidden cpp_export void *MT_mremap(const char *path, int mode, void *old_address, size_t old_size, size_t *new_size)
	__attribute__((__visibility__("hidden")));
__hidden cpp_export int MT_msync(void *p, size_t len)
	__attribute__((__visibility__("hidden")));
__hidden cpp_export void OIDXfree(BAT *b)
	__attribute__((__visibility__("hidden")));
__hidden cpp_export void persistOIDX(BAT *b)
	__attribute__((__visibility__("hidden")));
__hidden cpp_export gdk_return rangejoin(BAT *r1, BAT *r2, BAT *l, BAT *rl, BAT *rh, BAT *sl, BAT *sr, int li, int hi, BUN maxsize)
	__attribute__((__warn_unused_result__))
	__attribute__((__visibility__("hidden")));
__hidden cpp_export void strCleanHash(Heap *hp, int rebuild)
	__attribute__((__visibility__("hidden")));
__hidden cpp_export int strCmpNoNil(const unsigned char *l, const unsigned char *r)
	__attribute__((__visibility__("hidden")));
__hidden cpp_export var_t strLocate(Heap *h, const char *v)
	__attribute__((__visibility__("hidden")));
__hidden cpp_export gdk_return unshare_string_heap(BAT *b)
	__attribute__((__warn_unused_result__))
	__attribute__((__visibility__("hidden")));
__hidden cpp_export void VIEWdestroy(BAT *b)
	__attribute__((__visibility__("hidden")));
__hidden cpp_export gdk_return VIEWreset(BAT *b)
	__attribute__((__warn_unused_result__))
	__attribute__((__visibility__("hidden")));
__hidden cpp_export BAT *virtualize(BAT *bn)
	__attribute__((__visibility__("hidden")));
__hidden cpp_export int binsearchcand(const oid *cand, BUN lo, BUN hi, oid v)
	__attribute__((__visibility__("hidden")));
__hidden cpp_export void gdk_bbp_reset(void)
	__attribute__((__visibility__("hidden")));
__hidden cpp_export void gdk_system_reset(void)
	__attribute__((__visibility__("hidden")));

#define BBP_BATMASK	511
#define BBP_THREADMASK	63

struct PROPrec {
	int id;
	ValRecord v;
	struct PROPrec *next;	/* simple chain of properties */
};

struct Imprints {
	bte bits;		/* how many bits in imprints */
	Heap *imprints;
	void *bins;		/* pointer into imprints heap (bins borders)  */
	BUN *stats;		/* pointer into imprints heap (stats per bin) */
	void *imps;		/* pointer into imprints heap (bit vectors)   */
	void *dict;		/* pointer into imprints heap (dictionary)    */
	BUN impcnt;		/* counter for imprints                       */
	BUN dictcnt;		/* counter for cache dictionary               */
};

typedef struct {
	MT_Lock swap;
	MT_Lock hash;
	MT_Lock imprints;
} batlock_t;

typedef struct {
	MT_Lock alloc;
	MT_Lock trim;
	bat free;
} bbplock_t;

typedef char long_str[IDLENGTH];	/* standard GDK static string */

#define MAXFARMS       32

extern struct BBPfarm_t {
	unsigned int roles;	/* bitmask of allowed roles */
	const char *dirname;	/* farm directory */
	FILE *lock_file;
} BBPfarms[MAXFARMS];

extern int BBP_dirty;	/* BBP table dirty? */
extern batlock_t GDKbatLock[BBP_BATMASK + 1];
extern bbplock_t GDKbbpLock[BBP_THREADMASK + 1];
extern size_t GDK_mmap_minsize_persistent; /* size after which we use memory mapped files for persistent heaps */
extern size_t GDK_mmap_minsize_transient; /* size after which we use memory mapped files for transient heaps */
extern size_t GDK_mmap_pagesize; /* mmap granularity */
extern MT_Lock GDKnameLock;
extern MT_Lock GDKthreadLock;
extern MT_Lock GDKtmLock;
extern MT_Lock MT_system_lock;

#define ATOMappendpriv(t, h) (ATOMstorage(t) != TYPE_str || GDK_ELIMDOUBLES(h))

#define BBPdirty(x)	(BBP_dirty=(x))

#define BATcheck(tst, msg, err)						\
	do {								\
		if ((tst) == NULL) {					\
			if (strchr((msg), ':'))				\
				GDKerror("%s.\n", (msg));		\
			else						\
				GDKerror("%s: BAT required.\n", (msg));	\
			return (err);					\
		}							\
	} while (0)
#define ERRORcheck(tst,	msg, err)		\
	do {					\
		if (tst) {			\
			GDKerror(msg);		\
			return (err);		\
		}				\
	} while (0)
#define BATcompatible(P1,P2,E,F)					\
	do {								\
		ERRORcheck((P1) == NULL, F ": BAT required\n", E);	\
		ERRORcheck((P2) == NULL, F ": BAT required\n", E);	\
		if (TYPEerror(BATttype(P1),BATttype(P2))) {		\
			GDKerror("Incompatible operands.\n");		\
			return (E);					\
		}							\
		if (BATttype(P1) != BATttype(P2) &&			\
		    ATOMtype((P1)->ttype) != ATOMtype((P2)->ttype)) {	\
			CHECKDEBUG fprintf(stderr,"#Interpreting %s as %s.\n", \
				ATOMname(BATttype(P2)), ATOMname(BATttype(P1))); \
		}							\
	} while (0)
#define TYPEerror(t1,t2)	(ATOMstorage(ATOMtype(t1)) != ATOMstorage(ATOMtype(t2)))

#define GDKswapLock(x)  GDKbatLock[(x)&BBP_BATMASK].swap
#define GDKhashLock(x)  GDKbatLock[(x)&BBP_BATMASK].hash
#define GDKimprintsLock(x)  GDKbatLock[(x)&BBP_BATMASK].imprints
#if SIZEOF_SIZE_T == 8
#define threadmask(y)	((int) ((mix_int((unsigned int) y) ^ mix_int((unsigned int) (y >> 32))) & BBP_THREADMASK))
#else
#define threadmask(y)	((int) (mix_int(y) & BBP_THREADMASK))
#endif
#define GDKtrimLock(y)	GDKbbpLock[y].trim
#define GDKcacheLock(y)	GDKbbpLock[y].alloc
#define BBP_free(y)	GDKbbpLock[y].free

/* extra space in front of strings in string heaps when hashash is set
 * if at least (2*SIZEOF_BUN), also store length (heaps are then
 * incompatible) */
#define EXTRALEN ((SIZEOF_BUN + GDK_VARALIGN - 1) & ~(GDK_VARALIGN - 1))

#if !defined(NDEBUG) && !defined(STATIC_CODE_ANALYSIS)
/* see comment in gdk.h */
#ifdef __GNUC__
#define GDKmunmap(p, l)							\
	({	void *_ptr = (p);					\
		size_t _len = (l);					\
		gdk_return _res = GDKmunmap(_ptr, _len);		\
		ALLOCDEBUG						\
			fprintf(stderr,					\
				"#GDKmunmap(" PTRFMT "," SZFMT ") -> %d" \
				" %s[%s:%d]\n",				\
				PTRFMTCAST _ptr, _len, _res,		\
				__func__, __FILE__, __LINE__);		\
		_res;							\
	})
#define GDKmremap(p, m, oa, os, ns)					\
	({								\
		const char *_path = (p);				\
		int _mode = (m);					\
		void *_oa = (oa);					\
		size_t _os = (os);					\
		size_t *_ns = (ns);					\
		size_t _ons = *_ns;					\
		void *_res = GDKmremap(_path, _mode, _oa, _os, _ns);	\
		ALLOCDEBUG						\
			fprintf(stderr,					\
				"#GDKmremap(%s,0x%x," PTRFMT "," SZFMT "," SZFMT " > " SZFMT ") -> " PTRFMT \
				" %s[%s:%d]\n",				\
				_path ? _path : "NULL", _mode,		\
				PTRFMTCAST _oa, _os, _ons, *_ns,	\
				PTRFMTCAST _res,			\
				__func__, __FILE__, __LINE__);		\
		_res;							\
	 })
#else
static inline gdk_return
GDKmunmap_debug(void *ptr, size_t len, const char *filename, int lineno)
{
	gdk_return res = GDKmunmap(ptr, len);
	ALLOCDEBUG fprintf(stderr,
			   "#GDKmunmap(" PTRFMT "," SZFMT ") -> %d [%s:%d]\n",
			   PTRFMTCAST ptr, len, (int) res, filename, lineno);
	return res;
}
#define GDKmunmap(p, l)		GDKmunmap_debug((p), (l), __FILE__, __LINE__)
static inline void *
GDKmremap_debug(const char *path, int mode, void *old_address, size_t old_size, size_t *new_size, const char *filename, int lineno)
{
	size_t orig_new_size = *new_size;
	void *res = GDKmremap(path, mode, old_address, old_size, new_size);
	ALLOCDEBUG
		fprintf(stderr,
			"#GDKmremap(%s,0x%x," PTRFMT "," SZFMT "," SZFMT " > " SZFMT ") -> " PTRFMT
			" [%s:%d]\n",
			path ? path : "NULL", mode,
			PTRFMTCAST old_address, old_size, orig_new_size, *new_size,
			PTRFMTCAST res,
			filename, lineno);
	return res;
}
#define GDKmremap(p, m, oa, os, ns)	GDKmremap_debug(p, m, oa, os, ns, __FILE__, __LINE__)

#endif
#endif
