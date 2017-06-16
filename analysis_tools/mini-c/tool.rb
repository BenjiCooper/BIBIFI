#!usr/bin/ruby
system("make clean")
system("make")
system("./minic.byte test.c > test.txt")
puts("")
system("cat test.txt")