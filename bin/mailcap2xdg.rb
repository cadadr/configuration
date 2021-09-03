#!/usr/bin/env ruby
# mailcap2xdg.rb --- shim to make mailcap use xdg-open

# How many mime handlers can you handle? I can only one.

require 'mime/types'
require 'tempfile'

infile = File.open ARGV[0]
mimeid = ARGV[1]

unless infile and mimeid
  puts "usage: mailcap2xdg.rb INFILE MIMETYPE"
  exit 2
end


mimetype = MIME::Types[mimeid].first

unless mimetype
  puts "bad mime type: #{mimeid} (file: #{infile})"
  exit 1
end

extension = mimetype.preferred_extension

# #create does not remove the file automatically after close,
# which is useful to us here.
outfile = Tempfile.create ['mailcap2xdg-', "." + extension]

outfile.write infile.read
outfile.fsync
outfile.close

exec "xdg-open", outfile.path
