# encoding: utf-8
# tv.rb --- subtle window manager tags and views

### Tags:

# Simple tags
tag "emacs",   "emacs"
tag "browser", "firefox"

tag "fixed" do
  geometry [ 10, 10, 100, 100 ]
  stick    true
end

tag "gravity" do
  gravity :center
end

# Modes
tag "stick" do
  match "mplayer"
  float true
  stick true
end

tag "float" do
  match "display"
  float true
end

### Views:
view "home", "emacs|default"
view "www",  "browser"
