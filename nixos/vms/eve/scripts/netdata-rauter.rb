require 'open-uri'
req = open('http://rauter.evenet.dn42:19999/api/v1/allmetrics?format=prometheus')
req.each_line do |l|
  l.gsub!(/#.*/,"")
  if l =~ /([^{]+){[^}]+}\s+(\d+)\s+\d+/
    metric, val = $1, $2
    puts "#{metric.gsub("_", ".")} #{val} #{Time.new.utc.to_i}"
  end
end
