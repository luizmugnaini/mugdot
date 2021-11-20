// Luiz Mugnaini configuration for dwm window manager
// patch dependencies: fullgaps, movestack
//
// external dependencies: firefox, kitty, scrot, slock

#include <X11/XF86keysym.h>

/* appearance */
static const unsigned int borderpx  = 1;        /* border pixel of windows */
static const unsigned int gappx     = 5;        /* gaps between windows */
static const unsigned int snap      = 32;       /* snap pixel */
static const int showbar            = 1;        /* 0 means no bar */
static const int topbar             = 1;        /* 0 means bottom bar */
static const char *fonts[]          = { "Hasklug Nerd Font:size=10" };
static const char dmenufont[]       = "Hasklug Nerd Font:size=10";
static const char col_gray1[]       = "#222222";
static const char col_gray2[]       = "#444444";
static const char col_gray3[]       = "#bbbbbb";
static const char col_gray4[]       = "#eeeeee";
static const char col_purple[]      = "#674ea7";
static const char *colors[][3]      = {
	/*               fg         bg         border   */
	[SchemeNorm] = { col_gray3, col_gray1, col_gray2 },
	[SchemeSel]  = { col_gray4, col_purple,  col_purple },
};

/* tagging */
static const char *tags[] = { "", "", "", "", "", "", "", "", "" };

/* rules */
static const Rule rules[] = {
	/* xprop(1):
	 *	WM_CLASS(STRING) = instance, class
	 *	WM_NAME(STRING) = title
	 */
	/* class      instance    title   tags mask  isfloating   monitor */
	{ "Gimp",     NULL,       NULL,   0,         1,           -1 },
	{ "Firefox",  NULL,       NULL,   1 << 8,    0,           -1 },
};

/* layout(s) */
static const float mfact     = 0.55; /* factor of master area size [0.05..0.95] */
static const int nmaster     = 1;    /* number of clients in master area */
static const int resizehints = 1;    /* 1 means respect size hints in tiled resizals */

static const Layout layouts[] = {
	/* symbol arrange function */
	{ "[]=",  tile    }, /* first entry is default */
	{ "><>",  NULL    }, /* no layout function means floating behavior */
	{ "[M]",  monocle },
};

/* key modifiers */
// I'm currently using Alt as my mod key
// for sup: Mod4Mask
#define MODKEY Mod1Mask
#define TAGKEYS(KEY,TAG) \
	{ MODKEY,                       KEY, view,       {.ui = 1 << TAG} }, \
	{ MODKEY|ControlMask,           KEY, toggleview, {.ui = 1 << TAG} }, \
	{ MODKEY|ShiftMask,             KEY, tag,        {.ui = 1 << TAG} }, \
	{ MODKEY|ControlMask|ShiftMask, KEY, toggletag,  {.ui = 1 << TAG} },

/* helper for spawning shell commands in the pre dwm-5.0 fashion */
#define SHCMD(cmd) { .v = (const char*[]){ "/bin/sh", "-c", cmd, NULL } }

/* commands */

// dmenu
// component of dmenucmd, manipulated in spawn()
static char dmenumon[2]         = "0";
static const char *dmenucmd[]   = {
  "dmenu_run", "-m", dmenumon, "-fn", dmenufont, "-nb", col_gray1,
  "-nf", col_gray3, "-sb", col_purple, "-sf", col_gray4, NULL
};

// rofi
static const char *roficmd[] = { "rofi", "-show", "run", NULL };

// terminal
static const char *termcmd[] = { "alacritty", NULL };

// browser
static const char *browsercmd[] = { "firefox", NULL };

// screenshot
static const char *printcmd[] = {
  "scrot -s ~/Pictures/screenshots/screenshot.png", NULL
};

// System locking and suspend
static const char *lockcmd[]    = { "slock", NULL };
static const char *suspendcmd[] = { "systemctl", "suspend", NULL };

// Keyboard
// "-option caps:swapescape" is not needed since we are using xmodmap
static const char *kbdus[]     = { "setxkbmap us -option caps:swapescape", NULL };
static const char *kbdbr[]     = { "setxkbmap br -option caps:swapescape", NULL };
static const char *kbdbrdead[] = { "setxkbmap br -variant nodeadkeys  -option caps:swapescape", NULL };

// Audio
static const char *mutecmd[]    = { "pamixer", "-t", NULL};
static const char *downvolcmd[] = { "pamixer", "-d", "5", NULL };
static const char *upvolcmd[]   = { "pamixer", "-i", "5", NULL};

#include "movestack.c"
static Key keys[] = {
	/* modifier            key        function        argument */
    // dmenu
    { MODKEY|ShiftMask,    XK_p,            spawn,          {.v = dmenucmd } },

    // rofi
    { MODKEY,              XK_p,            spawn,          {.v = roficmd } },

    // terminal
    { MODKEY,              XK_Return,       spawn,          {.v = termcmd } },

    // browser
    { MODKEY,              XK_bracketright, spawn,          {.v = browsercmd } },

    // screenshot
    { 0,                   XK_Print,        spawn,          {.v = printcmd } },

    // audio
    { 0,                   XF86XK_AudioMute,        spawn,  {.v = mutecmd } },
    { 0,                   XF86XK_AudioLowerVolume, spawn,  {.v = downvolcmd } },
    { 0,                   XF86XK_AudioRaiseVolume, spawn,  {.v = upvolcmd } },

    // keyboard
    { MODKEY|ControlMask,  XK_u,     spawn,          {.v = kbdus} },
    { MODKEY|ControlMask,  XK_i,     spawn,          {.v = kbdbr} },
    { MODKEY|ControlMask,  XK_o,     spawn,          {.v = kbdbrdead} },

    // Screen locker and suspension
    { MODKEY|ShiftMask,    XK_z,            spawn,          {.v = lockcmd } },
    { MODKEY|ShiftMask,    XK_s,            spawn,          {.v = suspendcmd } },

    // Misc
    { MODKEY,              XK_b,      togglebar,      {0} },
    { MODKEY,              XK_j,      focusstack,     {.i = +1 } },
    { MODKEY,              XK_k,      focusstack,     {.i = -1 } },
    { MODKEY,              XK_i,      incnmaster,     {.i = +1 } },
    { MODKEY,              XK_d,      incnmaster,     {.i = -1 } },
    { MODKEY,              XK_h,      setmfact,       {.f = -0.05} },
    { MODKEY,              XK_l,      setmfact,       {.f = +0.05} },
    { MODKEY|ShiftMask,    XK_j,      movestack,      {.i = +1 } },
    { MODKEY|ShiftMask,    XK_k,      movestack,      {.i = -1 } },
    { MODKEY,              XK_Return, zoom,           {0} },
    { MODKEY,              XK_Tab,    view,           {0} },
    { MODKEY|ShiftMask,    XK_c,      killclient,     {0} },
    { MODKEY,              XK_t,      setlayout,      {.v = &layouts[0]} },
    { MODKEY,              XK_f,      setlayout,      {.v = &layouts[1]} },
    { MODKEY,              XK_m,      setlayout,      {.v = &layouts[2]} },
    { MODKEY,              XK_space,  setlayout,      {0} },
    { MODKEY|ShiftMask,    XK_space,  togglefloating, {0} },
    { MODKEY,              XK_0,      view,           {.ui = ~0 } },
    { MODKEY|ShiftMask,    XK_0,      tag,            {.ui = ~0 } },
    { MODKEY,              XK_comma,  focusmon,       {.i = -1 } },
    { MODKEY,              XK_period, focusmon,       {.i = +1 } },
    { MODKEY|ShiftMask,    XK_comma,  tagmon,         {.i = -1 } },
    { MODKEY|ShiftMask,    XK_period, tagmon,         {.i = +1 } },
    { MODKEY,              XK_minus,  setgaps,        {.i = -1 } },
    { MODKEY,              XK_equal,  setgaps,        {.i = +1 } },
    { MODKEY|ShiftMask,    XK_equal,  setgaps,        {.i = 0  } },
    TAGKEYS(               XK_1,                      0)
    TAGKEYS(               XK_2,                      1)
    TAGKEYS(               XK_3,                      2)
    TAGKEYS(               XK_4,                      3)
    TAGKEYS(               XK_5,                      4)
    TAGKEYS(               XK_6,                      5)
    TAGKEYS(               XK_7,                      6)
    TAGKEYS(               XK_8,                      7)
    TAGKEYS(               XK_9,                      8)
    { MODKEY|ShiftMask,    XK_q,      quit,           {0} },
};

/* button definitions */
/* click can be ClkTagBar, ClkLtSymbol, ClkStatusText, ClkWinTitle, ClkClientWin, or ClkRootWin */
static Button buttons[] = {
	/* click         event mask  button    function        argument */
	{ ClkLtSymbol,   0,          Button1,  setlayout,      {0} },
	{ ClkLtSymbol,   0,          Button3,  setlayout,      {.v = &layouts[2]} },
	{ ClkWinTitle,   0,          Button2,  zoom,           {0} },
	{ ClkStatusText, 0,          Button2,  spawn,          {.v = termcmd } },
	{ ClkClientWin,  MODKEY,     Button1,  movemouse,      {0} },
	{ ClkClientWin,  MODKEY,     Button2,  togglefloating, {0} },
	{ ClkClientWin,  MODKEY,     Button3,  resizemouse,    {0} },
	{ ClkTagBar,     0,          Button1,  view,           {0} },
	{ ClkTagBar,     0,          Button3,  toggleview,     {0} },
	{ ClkTagBar,     MODKEY,     Button1,  tag,            {0} },
	{ ClkTagBar,     MODKEY,     Button3,  toggletag,      {0} },
};

