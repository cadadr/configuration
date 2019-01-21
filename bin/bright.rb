# coding: utf-8
# bright.rb --- set brightness

# bright.rb OPTIONS [FLOAT|INT]

# where FLOAT = 0 <= FLOAT <= 1.0
# set brightness to the given fraction

# where INT = 0 <= INT <= max brightness
# set brightness to that value

# TODO(2019-01-22): allow selecting from available backlight entries
# under ‘dir’.

require "pathname"

dir = Pathname.new "/sys/class/backlight/"

curfil = "actual_brightness"
maxfil = "max_brightness"
setfil = "brightness"

backlights = Dir.glob("*", base: dir)

backlights.each do |b|
  curlvl = File.new(dir.join(b).join curfil).read.to_i
  maxlvl = File.new(dir.join(b).join maxfil).read.to_i

  if ARGV.length == 0
    puts "#{b}: #{curlvl} (actual, max: #{maxlvl})"
  else
    arg = ARGV.shift
    newlvl = maxlvl * arg.to_f if arg.include?(".") or arg.to_i
    raise "out of range" unless 0 <= newlvl and newlvl <= maxlvl
    # .to_i.to_s in order make sure we get an integer b/c if arg was a
    # float newlvl will be a float too.
    File.open(dir.join(b).join(setfil), "w").write(newlvl.to_i.to_s)
  end
end
