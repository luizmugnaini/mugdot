case "$(cat /sys/class/net/wlan0/operstate 2>/dev/null)" in
up) printf " 直 Connected " ;;
down) printf " 睊 Disconnected " ;;
esac
