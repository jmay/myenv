$stderr.puts "# loading #{__FILE__}"

require "ap"
require "set"
require "csv"
require "facets" # for Hash#delete_values

class Array
  # convert an array of hashes to a hash of the same hashes
  # where the key values are picked from the hashes
  # the keys can be single fields, or an array, or a list
  # options:
  #   :multi (boolean, default false): if true, allow multiple values per key; store values as an array for each key
  #   :first (boolean, default false): if true, when finding multiple values per key, store only the first and ignore the rest
  #   :truncate (integer): see `Hash#key_for`
  #
  def key_on(*keyarray)
    raise "Key(s) required" if keyarray.empty?
    opts = keyarray.last.is_a?(Hash) ? keyarray.pop : {}
    keyarray = keyarray.flatten

    memo = opts[:multi] ? Hash.new {|h,k| h[k] = []} : Hash.new
    each do |hash|
      this_key = hash.key_for(keyarray, opts)
      raise "Missing value for #{keyarray} in record #{hash}" if this_key.nil?
      if opts[:multi]
        memo[this_key] << hash
      elsif opts[:first]
        # ignore this value if we already have one for this key
        if !memo.has_key?(this_key)
          memo[this_key] = hash
        end
      else
        raise "Found duplicate #{keyarray} in #{memo[this_key]} vs #{hash}" if memo.has_key?(this_key)
        memo[this_key] = hash
      end
      memo
    end
    memo.default = nil
    memo
  end

  # shorthand for `Array#select {|hash| hash[...] && hash[...] ...}`
  # find all the members of the array where all the specified criteria are true
  def where(conditions)
    case conditions
    when Hash
      select do |record|
        conditions.map do |k,v|
          case v
          when Regexp
            record[k] =~ v
          when TrueClass
            !record[k].nil?
          when FalseClass
            record[k].nil?
          else
            record[k] == v
          end
        end.reduce(:&) # all tests must pass
      end
    when String,Symbol
      # just check for presence & non-nil value of specified key
      select {|record| record[conditions]}
    end
  end

  # are all the values for `key` defined and unique?
  def unique?(*keyarray)
    raise "Key(s) required" if keyarray.empty?
    keyarray = keyarray.flatten
    keys = map {|hash| hash.key_for(keyarray)}
    return false if keys.any?(&:nil?)
    keys.uniq.count == self.count
  end

  def unique_values_for(*keyarray)
    raise "Key(s) required" if keyarray.empty?
    keyarray = keyarray.flatten
    map {|hash| hash.key_for(keyarray)}.to_set
  end

  # assign unique IDs to every hash in the array
  # argument is the name of the field to use for the generated sequential key
  def count_off!(key = :key, start = 0)
    raise "Values exist for [#{key}]" if any?{|h| h[key]}
    each_with_index do |hash, i|
      hash[key] = i + start
    end
    self
  end

  def redundant(*keyarray)
    key_on(keyarray, :multi => true).select {|k,v| v.count > 1}
  end

  # turns an array-of-arrays into an array-of-hashes
  # the headers are used as names for the fields
  def hashify(headers = shift)
    select {|row| row.any?}.map do |row|
      raise "Row count mismatch: #{row}" if row.count != headers.count
      hash = {}
      row.zip(headers) {|v,k| hash[k] = v}
      hash.delete_values(nil) # completely remove keys for nil values
      hash
    end
  end

  # ARRAY OF HASHES
  # combine a set of hashes into one
  # for each key, find all the distinct values from all the hashes
  # if there's one unique value, store the single value in key of the result
  # if there are multiple values, store them all as an array
  def coalesce
    allkeys = map {|h| h.keys}.flatten.uniq
    allkeys.reduce({}) do |memo,key|
      memo[key] = map {|h| h[key]}.compact.uniq
      memo[key] = memo[key].first if memo[key].count <= 1
      memo
    end
  end

  # ARRAY OF SCALARS
  # apply an operation (block) to every member of the array
  # return the list of unique results
  # if there is just one result, convert to a scalar value
  def resolve(&block)
    values = map {|v| block.call(v)}.uniq
    values.count <= 1 ? values.first : values
  end

  # ARRAY OF HASHES
  # apply the same resolution operation to every hash in the list
  def resolve_all(key, &block)
    map do |hash|
      hash = hash.dup
      hash[key] = hash[key].resolve(&block)
      hash
    end
  end

  def dumpme(filename)
    raise "#{filename} exists" if File.exists?(filename)
    File.open(filename, "w") {|f| f << Marshal.dump(self)}
  end
  def dumpme!(filename)
    File.unlink(filename)
    File.open(filename, "w") {|f| f << Marshal.dump(self)}
  end

  # ARRAY OF HASHES
  # What different keys appear in this collection of hashes?
  def keys
    map {|h| h.keys}.flatten.uniq
  end

  # ARRAY OF HASHES
  def metrics
    keys.reduce({}) do |m,k|
      values = self.map {|h| h[k]}
      m[k] = {
        :non_nil => values.compact.count,
        :nil => values.count - values.compact.count,
        :unique => values.uniq.count
      }
      if m[k][:unique] <= 10
        m[k][:values] = histogram(k)
      end
      m
    end
  end

  # ARRAY OF HASHES
  # For each record, output a subset of the values as an array (suitable for passing to `#to_csv`)
  # supports hierarchical subkeys (e.g. :master:id or "master:name")
  def project(args)
    defaults = args[:defaults] || {}
    map do |h|
      args[:keys].map do |k|
        (k.splitkey? && (deref = h[k.superkey]) && deref[k.subkey]) || h[k] || defaults[k] || args[:nilvalue]
      end
    end
  end

  def numify!(*keyarray)
    each {|h| h.numify!(*keyarray)}
  end

  def nilify!(keyvalue)
    each {|h| h.nilify!(keyvalue)}
  end

  # ARRAY OF HASHES
  # return histogram of value distribution for the specified key: hash of value/count pairs
  def histogram(*args, &block)
    reduce(Hash.new(0)) do |hist, h|
      if block_given?
        v = yield(h)
      else
        v = h[args.first]
      end
      hist[v] += 1
      hist
    end
  end
end


class Hash
  # construct a hash of changes needed to convert from an original hash to the new set of values
  # keys in the original that do not appear in the new hash should appear in the diff with nil values
  # EXCEPT that *symbol* keys from the original that *do not appear* (a nil value means it still appears) in the new hash should be ignored
  def diffs_from(orig)
    (self.keys | orig.keys).inject({}) do |diffs,key|
      if key.is_a?(Symbol) && !self.include?(key)
        # ignore this
      elsif orig[key] != self[key]
        diffs[key] = self[key]
      end
      diffs
    end
  end

  # construct a key field for the has based on the list of fields provided
  # options:
  #   :strip (true/false, default = true): remove leading & trailing whitespace from each value
  #   :truncate (integer): set maximum length for each value; truncate BEFORE stripping
  def key_for(keyarray, opts = {})
    opts[:strip] = true unless opts.has_key?(:strip)
    meth = lambda do |k|
      v = self[k]
      v = v[0,opts[:truncate]] if opts[:truncate]
      v = v.strip if opts[:strip] && v.is_a?(String)
      v
    end
    this_key = keyarray.map(&meth) #{|k| self[k].strip}
    return nil if this_key.all? {|v| v.nil?}
    return this_key.first if this_key.count == 1 # turn single-field keys into single values, not arrays
    if opts[:delim]
      this_key.join(opts[:delim])
    else
      this_key
    end
  end

  # for a Hash where all the values are Arrays
  # hash2 should also be a hash of key/array pairs
  # find all the cases where keys appear in both source hashes
  def pair_off(hash2)
    pairs = {}
    each do |k,ary|
      if hash2[k] && hash2[k].any?
        pairs[k] = [ary, hash2[k]]
      end
    end
    pairs
  end

  # same as `pair_off`, except that it chooses the partner key by calling a block
  # rather than doing a strict comparison
  def pair_off_by(hash2, &block)
    pairs = {}
    each do |k,ary|
      k2 = block.call(k)
      if hash2[k2] && hash2[k2].any?
        pairs[k] = [ary, hash2[k2]]
      end
    end
    pairs
  end

  # destructive version of `#pair_off` above.
  # when matching keys are found, the keys are removed from both source hashes.
  def pair_off!(hash2)
    pairs = {}
    each do |k,ary|
      if hash2[k].any?
        pairs[k] = [ary, hash2[k]]
        delete(k)
        hash2.delete(k)
      end
    end
    pairs
  end

  def pair_off_by!(hash2, &block)
    pairs = {}
    each do |k,ary|
      k2 = block.call(k)
      if hash2[k2] && hash2[k2].any?
        pairs[k] = [ary, hash2[k2]]
        delete(k)
        hash2.delete(k2)
      end
    end
    pairs
  end

  def dumpme(filename)
    raise "#{filename} exists" if File.exists?(filename)
    File.open(filename, "w") {|f| f << Marshal.dump(self)}
  end

  # HASH OF ARRAYS
  def append(hash2)
    (self.keys | hash2.keys).inject({}) {|h,k| h[k] = Array(self[k]) + Array(hash2[k]); h}
  end

  # HASH OF HASHES
  # compare to another hash-of-hashes (aka changes, deltas, diffs)
  # report the changes between a current state and a future state (hash2)
  # each of the four sections (new elements, lost elements, unchanged elements, changes) is another hash-of-hashes
  def compare(hash2)
    newkeys = hash2.keys - self.keys
    lostkeys = self.keys - hash2.keys
    commonkeys = self.keys & hash2.keys

    unchanged = []
    changes = {}
    commonkeys.each do |k|
      if (diffs = hash2[k].diff(self[k])).any?
        changes[k] = diffs
      else
        unchanged << k
      end
    end

    {
      :new => hash2.slice(*newkeys),
      :lost => self.slice(*lostkeys),
      :unchanged => self.slice(*unchanged),
      :changes => changes
    }
  end

  # convert specified fields to integers
  def numify!(*keyarray)
    keyarray.each do |k|
      self[k] = self[k].to_i if self[k]
    end
    self
  end

  # ARRAY OF HASHES
  #     correlated(:with => correlation-hash, :by => key-field)
  # pull subset that have mappings in the correlation hash
  def correlated?(args = {})
    with = args[:with]
    through = args[:through]
    onkey = args[:onkey]

    my_keys = keys
    correlation_keys = through.keys

    mismatches = select do |k,h|
      this_match = h[onkey]
      should_match = through[k] && with[through[k]]
      this_match != should_match
    end
    unmatched = correlation_keys - my_keys
    mismatches | unmatched
    # should be any empty array
    # select {|h| args[:with][h.key_for(args[:by], :delim => nil)]}
  end

  # apply correlations
  #     correlate!(:with => hash2, :through => mapping-hash, :onkey => attribute-to-record-mapping-in)
  # replaces any existing correlations (the `:on` field will be set to nil where the key does not appear in the correlation hash)
  def correlate!(args = {})
    with = args[:with]
    through = args[:through]
    onkey = args[:onkey]
    raise "Missing argument" if args[:onkey].nil?
    each do |k,h|
      this_match = through[k] && with[through[k]]
      h[onkey] = this_match
    end
  end

  def nilify!(nilvalue)
    each do |k,v|
      self.delete(k) if v == nilvalue
    end
  end
end

def reload!
  load "~/env/utilities.rb"
end

def unmarshal(file)
  Marshal.load(File.read(file))
end

class String
  # identifying keys (strings) that represent hierarchical structures, with format "superkey:subkey"
  def splitkey?
    self =~ /:/
  end
  # we always interpret the first part as a symbol
  def superkey
    split(/:/, 2).first.to_sym
  end
  # for STRINGS we always interpret the last part as a string ("resource:name" translates to :resource => name)
  def subkey
    split(/:/, 2).last
  end
end

class Symbol
  # identifying keys (strings) that represent hierarchical structures, with format :"superkey:subkey"
  def splitkey?
    to_s =~ /:/
  end
  # we always interpret the first part as a symbol
  def superkey
    to_s.split(/:/, 2).first.to_sym
  end
  # for SYMBOLS we always interpret the last part as a symbol (:"resource:id" translates to :resource => :id)
  def subkey
    to_s.split(/:/, 2).last.to_sym
  end
end

class Object
  def vconvert(rule)
    self && ConversionsLibrary.method(rule).call(self)
  end
end

module ConversionsLibrary
  def self.noop(value)
    value
  end

  # MSAD uses INT64 (8 bytes) for lastLogon, lastLogonTimestamp, accountExpires
  def self.msad_long_timestamp(value)
    case value.to_i
    when 0, 0x7FFFFFFFFFFFFFFF
      nil
    else
      DateTime.new(1601, 1, 1) + value.to_i/(60.0 * 10000000 * 1440)
    end
  end

  def self.readable_timestamp(value)
    DateTime.parse(value)
  end

  def self.first_ou(value)
    (ou = value.split(',').select{|s| s =~ /^OU=/}.first) && ou.split('=').last
  end

  def self.msad_active_account(value)
    value.to_i & 2 == 0
  end

  def self.datestr(value)
    value.strftime("%m/%d/%Y")
  end

  def self.max_datestr(values)
    (dt = values.compact.max) && dt.strftime("%m/%d/%Y")
  end
end

module Transformations
  # unraveling the hierarchical group membership structure in Microsoft Active Directory
  # expand the group information from MSAD "memberOf" fields
  # flatten the hierarchy, so each account records every group of which it is a member, even through sub-groups
  def self.expand_msad_groups(hashes)
    $stderr.puts "Analyzing #{hashes.size} Active Directory records"
    msad_accounts_by_dn = hashes.key_on('DN')
    $stderr.puts "Found #{msad_accounts_by_dn.size} distinct DN values"

    # expand the multi-valued memberOf field, and look up each group
    # WARNING: does not report any cases if the DN for the group does not appear in the hashes, will just leave a nil in the list
    hashes.each do |hash|
      hash[:memberof] = (hash['memberOf'] || '').split(';').map {|dn| msad_accounts_by_dn[dn]}
    end
    $stderr.puts "Expanded groups on #{hashes.select {|h| h[:memberof].any?}.size} records"

    membership_counts = hashes.map {|h| h[:memberof].size}.sum

    begin
      $stderr.puts "Found #{membership_counts} memberships, moving up membership hierarchy..."
      base_membership_counts = membership_counts
      hashes.each do |hash|
        hash[:memberof] |= hash[:memberof].map {|g| g[:memberof]}.flatten.uniq  
      end
      membership_counts = hashes.map {|h| h[:memberof].size}.sum
      # repeat until no further memberships are found
    end while membership_counts == base_membership_counts
  end

  def self.enhance(args)
    h = args[:hash]
    args[:rules].each do |rule|
      if rule[:input].is_a?(Array)
        h[rule[:output]] = h.values_at(rule[:input]).vconvert(rule[:rule])
      else
        h[rule[:output]] = h[rule[:input]].vconvert(rule[:rule])
      end
    end
    h
  end
end
