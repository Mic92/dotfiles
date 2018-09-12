require "json"
require "pathname"

module Lxc
  class Registry
    PATH = Pathname.new(File.expand_path("../../../../container.json", __FILE__))
    def initialize
      @data = JSON.load(File.open(Registry::PATH))
    end
    attr_accessor :data
    def save
      Utils.safe_write(Registry::PATH, JSON.pretty_generate(@data))
    end
  end
end
