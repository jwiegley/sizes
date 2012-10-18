#include "HsStat.h"

#if defined(__MINGW32__)
inline int __hscore_statvfs(wchar_t *path, struct_statvfs *buf) {
    /* jww (2012-10-18): ??? */
    return _wstatvfsi64(path,buf);
}
#else
inline int __hscore_statvfs(char *path, struct_statvfs *buf) {
    return statvfs(path,buf);
}
#endif
