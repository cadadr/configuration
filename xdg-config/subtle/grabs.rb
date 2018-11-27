# encoding: utf-8
# grabs.rb --- subtle window manager grabs
grab "W-1", :ViewSwitch1
grab "W-2", :ViewSwitch2
grab "W-3", :ViewSwitch3
grab "W-4", :ViewSwitch4

grab "W-space r", :SubtleReload
grab "W-space S-r", :SubtleRestart
grab "W-space S-q", :SubtleQuit

grab "W-B1", :WindowMove
grab "W-B3", :WindowResize
grab "W-space f", :WindowFloat
grab "W-space space", :WindowFull
grab "W-space s", :WindowStick      # visible on all views
grab "W-space equal", :WindowZaphod # span across all screens

grab "W-r", :WindowRaise
grab "W-l", :WindowLower

grab "W-b", :WindowLeft
grab "W-n", :WindowDown
grab "W-p", :WindowUp
grab "W-f", :WindowRight

grab "W-space S-k", :WindowKill

# Toggle gravities
grab "W-q", [ :top_left,     :top_left66,     :top_left33     ]
grab "W-w", [ :top,          :top66,          :top33          ]
grab "W-e", [ :top_right,    :top_right66,    :top_right33    ]
grab "W-a", [ :left,         :left66,         :left33         ]
grab "W-s", [ :center,       :center66,       :center33       ]
grab "W-d", [ :right,        :right66,        :right33        ]
grab "W-z", [ :bottom_left,  :bottom_left66,  :bottom_left33  ]
grab "W-x", [ :bottom,       :bottom66,       :bottom33       ]
grab "W-c", [ :bottom_right, :bottom_right66, :bottom_right33 ]

# Exec programs
grab "W-Return Return", "dmenu-desktop.pl"
grab "W-Return d", "dmenu_run"
grab "W-Return p", "passmenu.sh"
grab "W-Return t", "x-terminal-emulator"
