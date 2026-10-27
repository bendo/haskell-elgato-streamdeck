#include <X11/Xlib.h>
#include <X11/XKBlib.h>
#include <X11/keysym.h>
#include <X11/extensions/XTest.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>

/* Serialize complete chords, including snapshot/restoration, across callers.
   Each call uses its own display connection. No process-global X error handlers
   or keyboard mappings are changed. */
static pthread_once_t xlib_once = PTHREAD_ONCE_INIT;
static pthread_mutex_t chord_mutex = PTHREAD_MUTEX_INITIALIZER;
static int xlib_threaded;

static void initialize_xlib(void)
{
    xlib_threaded = XInitThreads();
}

static int key_down(const char keys[32], unsigned int code)
{
    return ((unsigned char)keys[code / 8] & (1u << (code % 8))) != 0;
}

/* Resolve the requested symbol in the active group, preferring the unshifted
   level. Explicit Shift still works with a base name, e.g. Shift+d. A shifted
   symbol such as exclam adds Shift automatically. */
static KeyCode resolve_key(Display *display, KeySym symbol, int group, int *shift)
{
    int minimum, maximum;
    XDisplayKeycodes(display, &minimum, &maximum);
    for (int level = 0; level < 2; ++level) {
        for (int code = minimum; code <= maximum; ++code) {
            if (XkbKeycodeToKeysym(display, (KeyCode)code, group, level) == symbol) {
                *shift = level;
                return (KeyCode)code;
            }
        }
    }
    return 0;
}

int streamdeck_send_keys(const char *display_name, const char *const *names,
                        size_t count, char *error, size_t error_size)
{
    Display *display = NULL;
    XModifierKeymap *modifiers = NULL;
    XkbStateRec original;
    char down[32];
    unsigned char is_modifier[256] = {0};
    unsigned char seen[256] = {0};
    KeyCode chord[6], injected[6], released[256];
    int chord_count = 0, injected_count = 0, released_count = 0;
    int mutated = 0, success = 0, shift_requested = 0;
    int event_base, error_base, major, minor;

#define FAIL(...) do { snprintf(error, error_size, __VA_ARGS__); goto cleanup; } while (0)

    pthread_once(&xlib_once, initialize_xlib);
    if (!xlib_threaded) {
        snprintf(error, error_size, "Xlib thread initialization failed");
        return 0;
    }
    pthread_mutex_lock(&chord_mutex);
    if (count < 1 || count > 5)
        FAIL("Expected up to four modifiers and one key");
    if (!display_name)
        display_name = getenv("DISPLAY");
    if (!display_name || !display_name[0])
        FAIL("DISPLAY is not set; SendKeys needs an X11 session");
    display = XOpenDisplay(display_name);
    if (!display)
        FAIL("Cannot open X11 display '%s'", display_name);
    if (!XTestQueryExtension(display, &event_base, &error_base, &major, &minor))
        FAIL("X11 display does not support the XTEST extension");
    if (XkbGetState(display, XkbUseCoreKbd, &original) != Success)
        FAIL("Cannot read X11 keyboard state");
    modifiers = XGetModifierMapping(display);
    if (!modifiers)
        FAIL("Cannot read X11 modifier mapping");
    for (int index = 0; index < 8 * modifiers->max_keypermod; ++index) {
        KeyCode code = modifiers->modifiermap[index];
        if (code)
            is_modifier[code] = 1;
    }
    XQueryKeymap(display, down);

    /* Validate and resolve the complete chord before generating any input. */
    for (size_t index = 0; index < count; ++index) {
        if (!names[index] || !names[index][0])
            FAIL("Empty X11 key name");
        KeySym symbol = XStringToKeysym(names[index]);
        if (symbol == NoSymbol)
            FAIL("Unknown X11 keysym '%s'", names[index]);
        KeyCode code;
        int shift = 0;
        if (index + 1 < count) {
            code = XKeysymToKeycode(display, symbol);
            if (!code || !is_modifier[code])
                FAIL("Modifier '%s' is not mapped on this X11 display", names[index]);
            if (XkbKeysymToModifiers(display, symbol) & ShiftMask)
                shift_requested = 1;
        } else {
            code = resolve_key(display, symbol, original.group, &shift);
            if (!code)
                FAIL("Keysym '%s' is not mapped at a base/Shift level in the active keyboard group", names[index]);
            if (key_down(down, code))
                FAIL("Target key '%s' is already held; release it before sending the chord", names[index]);
        }
        if (seen[code])
            FAIL("Chord contains the same physical key more than once");
        seen[code] = 1;
        if (shift && !shift_requested) {
            KeyCode shift_code = XKeysymToKeycode(display, XK_Shift_L);
            if (!shift_code || !is_modifier[shift_code] || seen[shift_code])
                FAIL("Cannot resolve Shift for '%s'", names[index]);
            chord[chord_count++] = shift_code;
            seen[shift_code] = 1;
        }
        chord[chord_count++] = code;
    }

    mutated = 1;
    for (unsigned int code = 8; code < 256; ++code) {
        if (is_modifier[code] && key_down(down, code)) {
            if (!XTestFakeKeyEvent(display, code, False, CurrentTime))
                FAIL("XTEST could not temporarily release a held modifier");
            released[released_count++] = (KeyCode)code;
        }
    }
    /* Clear sticky/locked modifiers too (Caps Lock, Num Lock, etc.). Preserve
       the active layout while modifier/group-shift keys are temporarily up. */
    if (!XkbLatchModifiers(display, XkbUseCoreKbd, 0xff, 0) ||
        !XkbLockModifiers(display, XkbUseCoreKbd, 0xff, 0) ||
        !XkbLatchGroup(display, XkbUseCoreKbd, 0) ||
        !XkbLockGroup(display, XkbUseCoreKbd, original.group))
        FAIL("Cannot prepare X11 modifiers for the chord");

    for (int index = 0; index < chord_count; ++index) {
        if (!XTestFakeKeyEvent(display, chord[index], True, CurrentTime))
            FAIL("XTEST could not press a chord key");
        injected[injected_count++] = chord[index];
    }
    success = 1;

cleanup:
    if (display && mutated) {
        /* This runs before returning to Haskell, including on partial failures.
           A pending Haskell async exception cannot bypass native restoration. */
        while (injected_count > 0) {
            if (!XTestFakeKeyEvent(display, injected[--injected_count], False, CurrentTime)) {
                snprintf(error, error_size, "XTEST could not release a chord key");
                success = 0;
            }
        }
        for (int index = 0; index < released_count; ++index) {
            if (!XTestFakeKeyEvent(display, released[index], True, CurrentTime)) {
                snprintf(error, error_size, "XTEST could not restore a held modifier");
                success = 0;
            }
        }
        /* Attempt every restoration, even if an earlier one fails. */
        int restored = XkbLatchModifiers(display, XkbUseCoreKbd, 0xff, original.latched_mods);
        restored = XkbLockModifiers(display, XkbUseCoreKbd, 0xff, original.locked_mods) && restored;
        restored = XkbLatchGroup(display, XkbUseCoreKbd, original.latched_group) && restored;
        restored = XkbLockGroup(display, XkbUseCoreKbd, original.locked_group) && restored;
        if (!restored) {
            snprintf(error, error_size, "Cannot restore X11 modifier state");
            success = 0;
        }
        XSync(display, False);
    }
    if (modifiers)
        XFreeModifiermap(modifiers);
    if (display)
        XCloseDisplay(display);
    pthread_mutex_unlock(&chord_mutex);
    return success;
#undef FAIL
}
