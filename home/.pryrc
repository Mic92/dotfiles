Pry.config.color = true
Pry.config.pager = true
Pry.config.editor = "vim"

def pretty_json(json)
  JSON.pretty_generate(JSON.parse(json))
end

def preview_html(html, host="localhost:3000")
  file = Tempfile.new(['preview', '.html'])
  html.gsub!(/test\.host/, host)
  file.write(html)
  file.close
  link = "file://#{file.path}"
  if RbConfig::CONFIG['host_os'] =~ /mswin|mingw|cygwin/
    system "start #{link}"
  elsif RbConfig::CONFIG['host_os'] =~ /darwin/
    system "open #{link}"
  elsif RbConfig::CONFIG['host_os'] =~ /linux|bsd/
    system "xdg-open #{link}"
  end
end

def load_factory_girl
 require 'factory_girl'
 require Rails.root.join("spec", "factories.rb")
end
