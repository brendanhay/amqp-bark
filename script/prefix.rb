#!/usr/bin/env ruby

raise "No input file specified" if ARGV.length != 1

CATS = %w{
  EVENT
  CREATE
  UPDATE
  DELETE
}

SEVS = %w{
  INFO
  ERROR
  CRITICAL
  NOTICE
  WARNING
  ALERT
}

def prefix
  if [true, false].sample
    "[#{SEVS.sample}]"
  else
    "[#{SEVS.sample}][#{CATS.sample}]"
  end
end

input  = ARGV.shift
output = File.expand_path(File.dirname(__FILE__)) + "/prefix.log"
lines  = 0

File.open(output, "w") do |outp|
  while lines < 450000
    File.open(input, "r") do |inp|
      inp.each_line { |line| outp.puts prefix + line; lines += 1 }
    end
  end
end
