#include <sys/stat.h>
#include <sys/statvfs.h>

#include "HsFFI.h"

#if defined(__MINGW32__)
typedef struct _stati64 struct_statvfs;  /* jww (2012-10-18): ??? */
#else
typedef struct statvfs struct_statvfs;
#endif

#if defined(__MINGW32__)
extern int __hscore_statvfs(wchar_t *path, struct_statvfs *buf);
#else
extern int __hscore_statvfs(char *path, struct_statvfs *buf);
#endif
