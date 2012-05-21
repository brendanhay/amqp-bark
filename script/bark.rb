#!/usr/bin/env ruby

input = File.expand_path(File.dirname(__FILE__)) + "/prefix.log"

File.open(input, "r") do |inp|
  inp.each_line do |line|
    puts line
    sleep 0.5
  end
end
