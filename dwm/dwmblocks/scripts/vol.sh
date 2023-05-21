muted=$(pamixer --get-mute)
check="false"
if [ "$muted" = "$check" ]; then
    printf "   $(pamixer --get-volume)%% "
else
    printf "  $(pamixer --get-volume) "
fi
