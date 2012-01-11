# https://github.com/pry/pry/wiki/Pry-rc
# https://github.com/pry/pry/wiki/Customization-and-configuration

$stderr.print "loading pryrc..."

require 'bundler/setup'

# keep command history in current directory; share history with irb
Pry.config.history.file = "./.irb_history"

$stderr.puts "done."
