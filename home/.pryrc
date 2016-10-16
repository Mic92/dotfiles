Pry.config.color = true
Pry.config.pager = true
Pry.config.editor = "vim"

def pretty_json(json)
  require 'json'
  JSON.pretty_generate(JSON.parse(json))
end

def preview_html(html, host="http://localhost:3000")
  file = Tempfile.new(['preview', '.html'])
  #html.gsub!(/test\.host/, host)
  html.gsub!(/\/assets/, host + "/assets")
  file.write(html)
  file.close
  link = file.path

  if RbConfig::CONFIG['host_os'] =~ /mswin|mingw|cygwin/
    system "start #{link}"
  elsif RbConfig::CONFIG['host_os'] =~ /darwin/
    system "open #{link}"
  else # assume linux, freebsd ...
    system "xdg-open #{link}"
  end
end

def write_to_file(content, path="/tmp/test")
  puts "Write to #{path}"
  f = File.open(path, "w+")
  f.puts(content)
  f.close
end

Pry.config.commands.command "remove-pry", "Remove current pry" do
  require 'pry/commands/edit/file_and_line_locator'
  file_name, remove_line =  Pry::Command::Edit::FileAndLineLocator.from_binding(_pry_.current_binding)
  temp_file = Tempfile.new('foo')
  i = 0
  File.foreach(file_name) do |line|
    i += 1
    if i == remove_line
      line.gsub!(/binding.pry(\s)?/, "")
      temp_file.write line unless line =~ /\A[[:space:]]*\z/
    else
      temp_file.write line
    end
  end
  temp_file.close
  FileUtils.cp(temp_file.path, file_name)
end

def load_factory_girl
  require 'factory_girl'
  require Rails.root.join("spec", "factories.rb")
end

def _clipboard_(content)
  case RbConfig::CONFIG["host_os"]
  when /darwin/
    IO.popen('pbcopy', 'r+') { |clip| clip.puts content }
  when /mswin|mingw|cygwin/
    IO.popen('putclip', 'r+') { |clip| clip.puts content }
  else # assume linux, freebsd ...
    IO.popen('xclip', 'r+') { |clip| clip.puts content }
  end
end

Pry.config.commands.command "copy", "Copy to clipboard" do |str|
  unless str
    str = "#{_pry_.input_array[-1]}#=> #{_pry_.last_result}\n"
  end
  _clipboard_ str
end

Pry.config.commands.command "lastcopy", "Last result copy to clipboard" do
  _clipboard_(Pry::Code(Pry.history.to_a).take_lines(-2, 1).raw)
end
