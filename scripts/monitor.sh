#!/bin/bash

# HDMI support
# Put the newly recognizer monitor to the left of the laptop screen
xrandr --output HDMI1 --auto &
xrandr --output eDP-1 --auto --right-of HDMI-1 &
