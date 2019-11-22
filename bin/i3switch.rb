#!/usr/bin/env ruby
# i3switch.rb --- switch windows within workspace with dmenu matching

require 'json'
require 'open3'

workspaces = JSON.parse `i3-msg -t get_workspaces`
layout_tree = JSON.parse `i3-msg -t get_tree`

focused_ws = (layout_tree["nodes"][1]["nodes"][1]["nodes"].select do |x|
  n = workspaces.select {|h| h["focused"]}
  x["type"] == "workspace" && x["name"] == n[0]["name"]
end)[0]

its_nodes = focused_ws["nodes"][0]["nodes"]

windows = {}

its_nodes.each do |n|
  window_name = "#{n["window_properties"]["class"]}: #{n["window_properties"]["title"]}"
  windows[window_name] = n["id"]
end

dmenu_input = windows.keys.sort.join "\n"
dmenu_prompt = "Type title to select window: "
dmenu_command = "dmenu"
dmenu_options = ["-i",          # case insensitive match
                 "-l", "5",     # list vertically
                 "-p", dmenu_prompt]

stdout, stderr, status = Open3.capture3(dmenu_command, *dmenu_options, :stdin_data => dmenu_input)

# Exit if dmenu fails
unless status.success?
  s = status.exitstatus
  puts "$0: error: dmenu exited with #{s}"
  unless stderr.empty?
    puts "dmenu error message:"
    puts stdout
    puts stderr
  end
  exit s
end

result = stdout.chomp
result_id = windows[result]

exec "i3-msg", "-t", "command", "[con_id=#{result_id}]", "focus"
