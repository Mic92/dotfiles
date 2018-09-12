require "erb"

module Lxc
  class TemplateContext < OpenStruct
    def get_binding
      binding
    end
    def fqdn(v)
      v.to_s.gsub(/[^a-zA-Z0-9\-]/, "-")
    end
  end

  class Template
    def initialize(path, context: nil)
      @path = path
      @erb = ERB.new(File.read(path), nil, "-")
    end
    def render(params={})
      context = TemplateContext.new(params)
      @erb.result(context.get_binding)
    rescue => e
      raise StandardError.new("fail to render '#{@path}': #{e}")
    end

    def write(path, options={})
      Utils.safe_write(path, render(options))
    end
  end
end
