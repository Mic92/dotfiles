require "fileutils"

module Lxc
  module Utils
    def self.safe_write(path, content)
      dir = File.dirname(path)
      begin
        FileUtils.mkdir_p(dir)
      rescue Errno::EEXIST #don't care
      end
      temp_path = path.to_s + ".tmp"
      File.open(temp_path, 'w+') do |f|
        f.write(content)
      end

      FileUtils.mv(temp_path, path)
    end
    def self.sh(cmd, *args)
      puts "$ #{cmd} " + args.map {|a| "'#{a}'" }.join(" ")
      system(cmd, *args)
    end
  end
end
