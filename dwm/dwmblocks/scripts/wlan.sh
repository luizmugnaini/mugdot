case "$(cat /sys/class/net/wlan0/operstate 2>/dev/null)" in
up) printf " 󰖩 Connected " ;;
down) printf " 󰖪  Disconnected " ;;
esac
