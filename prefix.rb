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
n = 0

File.open("prefix.log", "w") do |outp|
  while n < 1000000
    File.open(input, "r") do |inp|
      inp.each_line do |line|
        outp.puts prefix + line
        n += 1
      end
    end
  end
end
