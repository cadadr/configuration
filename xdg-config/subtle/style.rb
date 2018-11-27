# encoding: utf-8
# style.rb --- subtle window manager style

# Style for all style elements
style :all do
  background  "#202020"
  icon        "#757575"
  border      "#303030", 0
  padding     0, 3
  font        "xft:sans-10"
end

# Style for the all views
style :views do
  foreground  "#757575"

  # Style for the active views
  style :focus do
    foreground  "#fecf35"
  end

  # Style for urgent window titles and views
  style :urgent do
    foreground  "#ff9800"
  end

  # Style for occupied views (views with clients)
  style :occupied do
    foreground  "#b8b8b8"
  end
end

# Style for sublets
style :sublets do
  foreground  "#757575"
end

# Style for separator
style :separator do
  foreground  "#757575"
  separator   "|"
end

# Style for focus window title
style :title do
  foreground  "#fecf35"
end

# Style for active/inactive windows
style :clients do
  active    "#303030", 2
  inactive  "#202020", 2
  margin    0
  width     50
end

# Style for subtle
style :subtle do
  margin      0, 0, 0, 0
  panel       "#202020"
  background  "#3d3d3d"
  stipple     "#757575"
end
