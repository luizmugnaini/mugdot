muted=$(pamixer --get-mute)
check="false"
if [ "$muted" = "$check" ]; then
    printf " volume  $(pamixer --get-volume)%% "
else
    printf " volume  $(pamixer --get-volume) "
fi
