// Luiz Mugnaini configuration for dwm window manager
// patch dependencies: fullgaps, movestack
//
// external dependencies:
// - firefox;
// - wezterm;
// - slock;
// - dmenu;
// - Terminus font;
// - pamixer;

#include <X11/XF86keysym.h>

/* appearance */
static const unsigned int borderpx = 2; /* border pixel of windows */
static const unsigned int gappx = 5;    /* gaps between windows */
static const unsigned int snap = 10;    /* snap pixel (original: 32) */
static const int showbar = 1;           /* 0 means no bar */
static const int topbar = 1;            /* 0 means bottom bar */
static const char *fonts[] = {"Terminus:size=9"};
static const char dmenufont[] = "Terminus:size=9";
static const char col_gray1[] = "#222222";
static const char col_gray2[] = "#444444";
static const char col_gray3[] = "#bbbbbb";
static const char col_gray4[] = "#eeeeee";
static const char col_purple[] = "#0d0e1c";
static const char col_border[] = "#cd00cd";
static const char *colors[][3] = {
    /*               fg         bg          border   */
    [SchemeNorm] = {col_gray3, col_gray1, col_gray2},
    [SchemeSel] = {col_gray4, col_purple, col_border},
};

/* tagging */
static const char *tags[] = {"1", "2", "3", "4", "5", "6", "7", "8", "9"};

/* rules */
static const Rule rules[] = {
    /* xprop(1):
     *	WM_CLASS(STRING) = instance, class
     *	WM_NAME(STRING) = title
     */
    /* class      instance    title   tags mask  isfloating   monitor */
    {"Firefox", NULL, NULL, 1 << 8, 0, -1},
};

/* layout(s) */
static const float mfact = 0.55; /* factor of master area size [0.05..0.95] */
static const int nmaster = 1;    /* number of clients in master area */
static const int resizehints =
    1; /* 1 means respect size hints in tiled resizals */

static const Layout layouts[] = {
    /* symbol arrange function */
    {"[]=", tile},    /* first entry is default */
    {"[M]", monocle}, /* master layout */
    {"><>", NULL},    /* no layout function means floating behavior */
};

/* key modifiers */
/* I'm currently using Alt as my mod key. For sup: Mod4Mask*/
#define MODKEY Mod1Mask
#define TAGKEYS(KEY, TAG)                                                      \
  {MODKEY, KEY, view, {.ui = 1 << TAG}},                                       \
      {MODKEY | ControlMask, KEY, toggleview, {.ui = 1 << TAG}},               \
      {MODKEY | ShiftMask, KEY, tag, {.ui = 1 << TAG}},                        \
      {MODKEY | ControlMask | ShiftMask, KEY, toggletag, {.ui = 1 << TAG}},

/* helper for spawning shell commands in the pre dwm-5.0 fashion */
#define SHCMD(cmd)                                                             \
  {                                                                            \
    .v = (const char *[]) { "/bin/sh", "-c", cmd, NULL }                       \
  }

/* commands */

// dmenu
static char dmenumon[2] = "0";
static const char *dmenucmd[] = {
    "dmenu_run", "-m",  dmenumon,   "-fn", dmenufont, "-nb", col_gray1, "-nf",
    col_gray3,   "-sb", col_purple, "-sf", col_gray4, NULL};

// Dev commands
static const char *termcmd[] = {"wezterm", NULL};
static const char *emacscmd[] = {"emacs", NULL};

// Brwoser
static const char *browsercmd[] = {"firefox", NULL};

// System locking and suspend
static const char *lockcmd[] = {"slock", NULL};
static const char *suspendcmd[] = {"systemctl", "suspend", NULL};

// Audio
static const char *mutecmd[] = {"pamixer", "-t", NULL};
static const char *downvolcmd[] = {"pamixer", "-d", "5", NULL};
static const char *upvolcmd[] = {"pamixer", "-i", "5", NULL};

#include "movestack.c"
static Key keys[] = {
    /* modifier            key        function        argument */
    /* dmenu */
    {MODKEY, XK_p, spawn, {.v = dmenucmd}},

    /* terminal */
    {MODKEY, XK_Return, spawn, {.v = termcmd}},

    /* emacs */
    {MODKEY | ShiftMask, XK_e, spawn, {.v = emacscmd}},

    /* browser */
    {MODKEY, XK_bracketright, spawn, {.v = browsercmd}},

    /* audio */
    {0, XF86XK_AudioMute, spawn, {.v = mutecmd}},
    {0, XF86XK_AudioLowerVolume, spawn, {.v = downvolcmd}},
    {0, XF86XK_AudioRaiseVolume, spawn, {.v = upvolcmd}},

    /* Screen locker and suspension */
    {MODKEY | ShiftMask, XK_z, spawn, {.v = lockcmd}},
    {MODKEY | ShiftMask, XK_s, spawn, {.v = suspendcmd}},

    /* Misc */
    {MODKEY, XK_b, togglebar, {0}},
    {MODKEY, XK_j, focusstack, {.i = +1}},
    {MODKEY, XK_k, focusstack, {.i = -1}},
    {MODKEY, XK_i, incnmaster, {.i = +1}},
    {MODKEY, XK_d, incnmaster, {.i = -1}},
    {MODKEY, XK_h, setmfact, {.f = -0.05}},
    {MODKEY, XK_l, setmfact, {.f = +0.05}},
    {MODKEY | ShiftMask, XK_j, movestack, {.i = +1}},
    {MODKEY | ShiftMask, XK_k, movestack, {.i = -1}},
    {MODKEY, XK_Return, zoom, {0}},
    {MODKEY, XK_Tab, view, {0}},
    {MODKEY | ShiftMask, XK_c, killclient, {0}},
    {MODKEY, XK_t, setlayout, {.v = &layouts[0]}},
    {MODKEY, XK_m, setlayout, {.v = &layouts[1]}},
    {MODKEY, XK_f, setlayout, {.v = &layouts[2]}},
    {MODKEY, XK_space, setlayout, {0}},
    {MODKEY | ShiftMask, XK_space, togglefloating, {0}},
    {MODKEY, XK_0, view, {.ui = ~0}},
    {MODKEY | ShiftMask, XK_0, tag, {.ui = ~0}},
    {MODKEY, XK_comma, focusmon, {.i = -1}},
    {MODKEY, XK_period, focusmon, {.i = +1}},
    {MODKEY | ShiftMask, XK_comma, tagmon, {.i = -1}},
    {MODKEY | ShiftMask, XK_period, tagmon, {.i = +1}},
    {MODKEY, XK_minus, setgaps, {.i = -1}},
    {MODKEY, XK_equal, setgaps, {.i = +1}},
    {MODKEY | ShiftMask, XK_equal, setgaps, {.i = 0}},
    TAGKEYS(XK_1, 0) TAGKEYS(XK_2, 1) TAGKEYS(XK_3, 2) TAGKEYS(XK_4, 3)
        TAGKEYS(XK_5, 4) TAGKEYS(XK_6, 5) TAGKEYS(XK_7, 6) TAGKEYS(XK_8, 7)
            TAGKEYS(XK_9, 8){MODKEY | ShiftMask, XK_q, quit, {0}},
};

/* button definitions */
/* click can be ClkTagBar, ClkLtSymbol, ClkStatusText, ClkWinTitle,
 * ClkClientWin, or ClkRootWin */
static Button buttons[] = {
    /* click         event mask  button    function        argument */
    {ClkLtSymbol, 0, Button1, setlayout, {0}},
    {ClkLtSymbol, 0, Button3, setlayout, {.v = &layouts[1]}},
    {ClkWinTitle, 0, Button2, zoom, {0}},
    {ClkStatusText, 0, Button2, spawn, {.v = termcmd}},
    {ClkClientWin, MODKEY, Button1, movemouse, {0}},
    {ClkClientWin, MODKEY, Button2, togglefloating, {0}},
    {ClkClientWin, MODKEY, Button3, resizemouse, {0}},
    {ClkTagBar, 0, Button1, view, {0}},
    {ClkTagBar, 0, Button3, toggleview, {0}},
    {ClkTagBar, MODKEY, Button1, tag, {0}},
    {ClkTagBar, MODKEY, Button3, toggletag, {0}},
};
