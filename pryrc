# https://github.com/pry/pry/wiki/Pry-rc
# https://github.com/pry/pry/wiki/Customization-and-configuration

$stderr.print "loading pryrc..."

# require 'bundler/setup' # this triggers errors about plugins not found

# keep command history in current directory; share history with irb

Pry.config.history.file = "./.irb_history"
Pry.config.auto_indent = false
Pry.config.color = true

$stderr.puts "done."
