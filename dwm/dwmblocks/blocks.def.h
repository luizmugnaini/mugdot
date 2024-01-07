static const Block blocks[] = {
    /*Icon*/ /*Command*/ /*Update Interval*/ /*Update Signal*/
    {"", "~/Projects/mugdot/dwm/dwmblocks/scripts/wlan.sh", 5, 0},

    {"", "~/Projects/mugdot/dwm/dwmblocks/scripts/vol.sh", 5, 0},

    {"", "~/Projects/mugdot/dwm/dwmblocks/scripts/bat.sh", 30, 0},

    {"", "~/Projects/mugdot/dwm/dwmblocks/scripts/date.sh", 5, 0},
};

// sets delimeter between status commands. NULL character ('\0') means no delimeter.
static char delim[] = "";
static unsigned int delimLen = 5;
