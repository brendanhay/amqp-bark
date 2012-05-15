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
    "[#{CATS.sample}]"
  else
    "[#{CATS.sample}][#{SEVS.sample}]"
  end
end

input = ARGV.shift

File.open("prefix.log", "w") do |outp|
  1000.times do
    File.open(input, "r") do |inp|
      inp.each_line { |line| outp.puts prefix + line }
    end
  end
end
