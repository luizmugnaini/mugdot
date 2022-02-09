//Modify this file to change what commands output to your statusbar, and recompile using the make command.
static const Block blocks[] = {
  /*Icon*/	/*Command*/		/*Update Interval*/	/*Update Signal*/
  {"", "~/.mugdot/dwm/.config/dwmblocks/scripts/wlan.sh", 30, 0},

  {"", "~/.mugdot/dwm/.config/dwmblocks/scripts/vol.sh", 5, 0},

  {"", "~/.mugdot/dwm/.config/dwmblocks/scripts/mem.sh", 30, 0},

  {"", "~/.mugdot/dwm/.config/dwmblocks/scripts/bat.sh", 30, 0},

  {"", "~/.mugdot/dwm/.config/dwmblocks/scripts/date.sh", 5, 0},
};

//sets delimeter between status commands. NULL character ('\0') means no delimeter.
static char delim[] = "";
static unsigned int delimLen = 5;
