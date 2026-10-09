/* Standard input, output or error that is closed when ge starts is opened on
 * /dev/null, for reading only, before the runtime starts and opens
 * descriptors of its own. Those would get the lowest free numbers, 0, 1 or
 * 2, and then what ge reads or writes as standard input, output or error
 * would go to a queue or a pipe of the runtime: with output and error both
 * closed, ge waited for good. Now reading gives end of file, and writing
 * fails, which ge reports (or cannot, when error is closed too) and ends
 * with exit status 1. */

#include <errno.h>
#include <fcntl.h>
#include <unistd.h>

__attribute__((constructor)) static void ge_hold_standard_descriptors(void) {
  for (int fd = 0; fd <= 2; fd++) {
    if (fcntl(fd, F_GETFD) != -1 || errno != EBADF)
      continue;
    /* the lowest free number, which is fd */
    int opened = open("/dev/null", O_RDONLY);
    if (opened >= 0 && opened != fd) {
      dup2(opened, fd);
      close(opened);
    }
  }
}
