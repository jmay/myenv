# -*- Mode: Ruby -*-
$stderr.puts "#{__FILE__} loading..."

begin
  # use Pry if it exists
  require 'pry'
  Pry.start || exit
rescue LoadError
end

begin
  # expanded auto-completion
  require 'bond'
  Bond.start
rescue LoadError
  # silent if bond is not installed
end

# http://feeds.feedburner.com/~r/RubyInside/~3/62868461/wirble-tab-completion-and-syntax-coloring-for-irb-336.html
# require "wirble"
# Wirble.init
# Wirble.colorize

# hirb (tabular console output for arrays, hashes, active record subclasses)
# e.g. `table MyArray`
# 101102 Hirb tries to interpret File objects and barfs with "Hirb Error: closed stream" or "Hirb Error: not opened for reading"
begin
  require 'hirb'
  # Hirb.enable
  extend Hirb::Console
rescue LoadError
  $stderr.puts "# Hirb not installed"
end

# require "pp"
# require 'utility_belt' # http://utilitybelt.rubyforge.org/usage.html

unless defined? Ripl
  ['rubygems', 'stringio'].each do |mod|
    IRB.conf[:LOAD_MODULES] << mod unless IRB.conf[:LOAD_MODULES].include?(mod)
  end

  #require 'irb/completion'
  require 'irb/ext/save-history'
  # ARGV.concat [ "--readline", "--prompt-mode", "simple" ]

  # IRB configuration.
  IRB.conf[:EVAL_HISTORY] = 100
  IRB.conf[:SAVE_HISTORY] = 1000
  # IRB.conf[:HISTORY_FILE] = File::expand_path("~/.irb_history")
  IRB.conf[:HISTORY_FILE] = ".irb_history" # separate command history in every directory where I run irb
  $stderr.puts "...logging commands to #{IRB.conf[:HISTORY_FILE]}"
  IRB.conf[:AUTO_INDENT] = true
  IRB.conf[:USE_READLINE] = true
  IRB.conf[:PROMPT_MODE] = :RVM #:DEFAULT

  IRB.conf[:LOAD_MODULES] ||= []

  IRB.conf[:PROMPT][:jmay] = {
    :PROMPT_N => "#{File.basename(Dir.getwd)} > ", # normal
    :PROMPT_I => "#{File.basename(Dir.getwd)} > ", # indent
    :PROMPT_S => "%l>> ", # continuing string
    :PROMPT_C => ">> ", # continuing statement
    :RETURN => "=> %s\n"
  }

  # Log to STDOUT if in Rails
  if ENV.include?('RAILS_ENV') && !Object.const_defined?('RAILS_DEFAULT_LOGGER')
    require 'logger'
    RAILS_DEFAULT_LOGGER = Logger.new(STDOUT)
    #IRB.conf[:USE_READLINE] = true

    # Display the RAILS ENV in the prompt
    # ie : [Development]>>
    IRB.conf[:PROMPT][:CUSTOM] = {
     :PROMPT_N => "[#{ENV["RAILS_ENV"].capitalize}]>> ",
     :PROMPT_I => "[#{ENV["RAILS_ENV"].capitalize}]>> ",
     :PROMPT_S => nil,
     :PROMPT_C => "?> ",
     :RETURN => "=> %s\n"
     }
    # Set default prompt
    IRB.conf[:PROMPT_MODE] = :CUSTOM
  end
end

# Handy functions

# Return a list of methods defined locally for a particular object.  Useful
# for seeing what it does whilst losing all the guff that's implemented
# by its parents (eg Object).
def local_methods(obj)
  obj.methods - obj.class.superclass.instance_methods
end

# List the various methods associated with an object.  Inspired by
# http://groups.google.com/group/comp.lang.ruby/browse_frm/thread/91d7c669214da8c3/1f220c54772b93fa?tvc=1&q=.irbrc#1f220c54772b93fa
def list_methods(obj)
  inspectee = obj.class == Class ? obj : obj.class
  c_list = (inspectee.methods - Object.methods).sort
  i_list = (inspectee.instance_methods - Object.instance_methods).sort
  a_list = inspectee.class.ancestors
  puts "Class Methods", "-"*13, c_list.inspect, '' unless c_list.empty?
  puts "Instance Methods", "-"*16, i_list.inspect, '' unless i_list.empty?
  puts "Ancestors", "-"*9, a_list.inspect, '' unless a_list.empty?
end

# MethodFinder, from http://www.nobugs.org/developer/ruby/method_finder.html
# which takes an object and an expected result, then finds methods which
# match it.  Useful for finding out if string concatenation is '+', ','
# or '.', for example...
class MethodFinder

  def initialize( obj, *args )
      @obj = obj
      @args = args
  end

  def ==( val )
      MethodFinder.show( @obj, val, *@args )
  end

  # Find all methods on [anObject] which, when called with [args] return [expectedResult]
  def self.find( anObject, expectedResult, *args )
    anObject.methods.select { |name| anObject.method(name).arity == args.size }.
                     select { |name| begin anObject.megaClone.method( name ).call(*args) == expectedResult;
                                     rescue; end }
  end

  # Pretty-prints the results of the previous method
  def self.show( anObject, expectedResult, *args )
    $old_stderr = $stderr; $stderr = StringIO.new
    methods =
      find( anObject, expectedResult, *args ).each { |name|
        print "#{anObject.inspect}.#{name}"
        print "(" + args.map { |o| o.inspect }.join(", ") + ")" unless args.empty?
        puts " == #{expectedResult.inspect}"
      }
    $stderr = $old_stderr
    methods
  end
end

class Object
  def what?(*a)
    MethodFinder.new(self, *a)
  end

  # Clone fails on numbers, but they're immutable anyway
  def megaClone
    begin self.clone; rescue; self; end
  end
end

# Add `ri` support in irb
def ri arg
  puts `ri #{arg}`
end

class Module
  def ri(meth=nil)
    if meth
      if instance_methods(false).include? meth.to_s
        puts `ri #{self}##{meth}`
      end
    else
      puts `ri #{self}`
    end
  end
end

if File.exists?(Dir.pwd + "/.irb")
  puts "...loading .irb"
  load ".irb"
end

class Array
  def nonuniq
    each_with_object(Hash.new(0)) { |x,memo| memo[x] += 1; memo }.find_all { |_,count| count > 1 }.sort_by { |x| -x[1] }
  end
end

$stderr.puts "...done."
