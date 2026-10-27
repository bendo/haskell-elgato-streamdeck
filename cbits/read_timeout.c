#include <errno.h>
#include <stddef.h>

/* Use the HIDAPI implementation linked by the hidapi Haskell dependency. */
struct hid_device_;
extern int hid_read_timeout(struct hid_device_ *, unsigned char *, size_t, int);

int streamdeck_read_timeout(struct hid_device_ *device, unsigned char *data,
                           size_t length, int milliseconds)
{
    /* A signal can interrupt poll(). Return to Haskell so pending asynchronous
       exceptions (such as Ctrl-C) can be delivered, rather than reporting an
       I/O failure. Clear errno first: HIDAPI can fail without setting it. */
    errno = 0;
    int result = hid_read_timeout(device, data, length, milliseconds);
    return result < 0 && errno == EINTR ? 0 : result;
}
