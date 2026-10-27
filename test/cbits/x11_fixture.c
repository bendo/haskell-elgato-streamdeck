#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/XKBlib.h>
#include <X11/extensions/XTest.h>
#include <errno.h>
#include <poll.h>
#include <stdlib.h>

/* Only used by opt-in tests against a private Xvfb server. */
struct fixture {
    Display *display;
    Window window;
};

void *streamdeck_test_x11_open(void)
{
    XInitThreads();
    struct fixture *fixture = calloc(1, sizeof(*fixture));
    if (!fixture)
        return NULL;
    fixture->display = XOpenDisplay(NULL);
    if (!fixture->display) {
        free(fixture);
        return NULL;
    }
    fixture->window = XCreateSimpleWindow(fixture->display,
        DefaultRootWindow(fixture->display), 0, 0, 200, 100, 0, 0, 0);
    XSelectInput(fixture->display, fixture->window, KeyPressMask | KeyReleaseMask);
    XMapRaised(fixture->display, fixture->window);
    XSync(fixture->display, False);
    XSetInputFocus(fixture->display, fixture->window, RevertToPointerRoot, CurrentTime);
    XSync(fixture->display, False);
    return fixture;
}

void streamdeck_test_x11_close(struct fixture *fixture)
{
    char keys[32];
    XQueryKeymap(fixture->display, keys);
    for (unsigned int code = 8; code < 256; ++code) {
        if ((unsigned char)keys[code / 8] & (1u << (code % 8)))
            XTestFakeKeyEvent(fixture->display, code, False, CurrentTime);
    }
    XkbLatchModifiers(fixture->display, XkbUseCoreKbd, 0xff, 0);
    XkbLockModifiers(fixture->display, XkbUseCoreKbd, 0xff, 0);
    XkbLatchGroup(fixture->display, XkbUseCoreKbd, 0);
    XkbLockGroup(fixture->display, XkbUseCoreKbd, 0);
    XSync(fixture->display, False);
    XDestroyWindow(fixture->display, fixture->window);
    XCloseDisplay(fixture->display);
    free(fixture);
}

void streamdeck_test_x11_drain(struct fixture *fixture)
{
    XSync(fixture->display, False);
    while (XPending(fixture->display)) {
        XEvent event;
        XNextEvent(fixture->display, &event);
        if (event.type == MappingNotify)
            XRefreshKeyboardMapping(&event.xmapping);
    }
}

int streamdeck_test_x11_next(struct fixture *fixture, unsigned int *pressed,
                            unsigned long *symbol, unsigned int *state, int timeout)
{
    while (1) {
        if (!XPending(fixture->display)) {
            struct pollfd fd = { ConnectionNumber(fixture->display), POLLIN, 0 };
            int status = poll(&fd, 1, timeout);
            if (status < 0 && errno == EINTR)
                continue;
            if (status <= 0)
                return status;
        }
        XEvent event;
        XNextEvent(fixture->display, &event);
        if (event.type == MappingNotify) {
            XRefreshKeyboardMapping(&event.xmapping);
        } else if (event.type == KeyPress || event.type == KeyRelease) {
            char text[16];
            KeySym keysym;
            XLookupString(&event.xkey, text, sizeof(text), &keysym, NULL);
            *pressed = event.type == KeyPress;
            *symbol = keysym;
            *state = event.xkey.state;
            return 1;
        }
    }
}

unsigned long streamdeck_test_x11_symbol(const char *name)
{
    return XStringToKeysym(name);
}

int streamdeck_test_x11_hold(struct fixture *fixture, const char *name, int down)
{
    KeyCode code = XKeysymToKeycode(fixture->display, XStringToKeysym(name));
    if (!code)
        return 0;
    int result = XTestFakeKeyEvent(fixture->display, code, down, CurrentTime);
    XSync(fixture->display, False);
    return result;
}

void streamdeck_test_x11_keys(struct fixture *fixture, char keys[32])
{
    XQueryKeymap(fixture->display, keys);
}

void streamdeck_test_x11_state(struct fixture *fixture, unsigned int values[5])
{
    XkbStateRec state;
    XkbGetState(fixture->display, XkbUseCoreKbd, &state);
    values[0] = state.locked_mods;
    values[1] = state.latched_mods;
    values[2] = state.locked_group;
    values[3] = state.latched_group;
    values[4] = state.group;
}

void streamdeck_test_x11_lock(struct fixture *fixture, unsigned int locked, unsigned int latched)
{
    XkbLockModifiers(fixture->display, XkbUseCoreKbd, 0xff, locked);
    XkbLatchModifiers(fixture->display, XkbUseCoreKbd, 0xff, latched);
    XSync(fixture->display, False);
}

void streamdeck_test_x11_group(struct fixture *fixture, unsigned int group)
{
    XkbLockGroup(fixture->display, XkbUseCoreKbd, group);
    XSync(fixture->display, False);
}

void streamdeck_test_x11_unmap(struct fixture *fixture, const char *name)
{
    KeySym target = XStringToKeysym(name);
    int minimum, maximum, width;
    XDisplayKeycodes(fixture->display, &minimum, &maximum);
    KeySym *mapping = XGetKeyboardMapping(fixture->display, minimum, maximum - minimum + 1, &width);
    for (int code = minimum; code <= maximum; ++code) {
        KeySym *symbols = mapping + (code - minimum) * width;
        for (int level = 0; level < width; ++level) {
            if (symbols[level] == target) {
                for (int item = 0; item < width; ++item)
                    symbols[item] = NoSymbol;
                XChangeKeyboardMapping(fixture->display, code, width, symbols, 1);
                break;
            }
        }
    }
    XFree(mapping);
    XSync(fixture->display, False);
}
