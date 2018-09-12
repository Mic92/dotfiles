require_relative "lxc/registry"
require_relative "lxc/template"
require_relative "lxc/hetzner"
require_relative "lxc/rdns"
require_relative "lxc/utils"

module Lxc
  CONFIG_ROOT = Pathname.new(File.expand_path("../../..", __FILE__))
end
