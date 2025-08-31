#!/usr/bin/env bash

LAYOUT=$(hyprctl devices | rg -A2 '<keyboard-name>' | rg -m1 'keymap' | awk '{print $3}')
[[ "$LAYOUT" == "English" ]] && echo "EN" || echo "ES"
