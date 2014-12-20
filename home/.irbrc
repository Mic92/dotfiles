gems = %w{
irb/completion
irb/ext/save-history
map_by_method
what_methods
pp
}
gems.each do |gem|
  begin
    require gem
  rescue LoadError
  end
end

IRB.conf[:AUTO_INDENT] = true
IRB.conf[:SAVE_HISTORY] = 100
IRB.conf[:HISTORY_FILE] = "#{ENV['HOME']}/.irb-save-history"
