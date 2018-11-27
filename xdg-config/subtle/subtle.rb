# encoding: utf-8
# subtle.rb --- subtle window manager configuration

# Adapted from the default configuration at
#   /etc/xdg/subtle/subtle.rc
# Which is copyright as follows:
#   Author::  Christoph Kappel <unexist@subforge.org>
#   Version:: $Id: data/subtle.rb,v 3182 2012/02/04 16:39:33 unexist $
#   License:: GNU GPLv2

def load_part name
  eval File.read("#{ENV['HOME']}/.config/subtle/#{name}.rb")
end

### Options:
set :increase_step, 1
set :border_snap, 10
set :default_gravity, :center
set :urgent_dialogs, false
set :honor_size_hints, false
set :gravity_tiling, false
set :click_to_focus, false
set :skip_pointer_warp, false
set :skip_urgent_warp, false

### Parts:
load_part :style
load_part :grav
load_part :grabs
load_part :tv
