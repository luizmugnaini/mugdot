case "$(cat /sys/class/net/wl*/operstate 2>/dev/null)" in
up) printf " 直 Connected " ;;
down) printf " 睊 Disconnected " ;;
esac
