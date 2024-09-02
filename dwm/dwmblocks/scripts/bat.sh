is_charging="$(acpi | awk '{print $3}' | awk -F ',' '{print $1}')"
get_capacity="$(cat /sys/class/power_supply/BAT0/capacity)"
if [ $is_charging = "Charging" ]; then
    printf " Charging $get_capacity%% "
elif [ $is_charging = "Full" ]; then
    printf " Full  $get_capacity%% "
else
    printf " Battery $get_capacity%% "
fi
