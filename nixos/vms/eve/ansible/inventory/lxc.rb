#!/usr/bin/env ruby

require 'json'

json = JSON.load(File.open("/etc/nixos/lxc/container.json"))
containers = {"_meta" => {"hostvars" => {}}}
json["network"].each do |host, data|
  next if data["lxc"] == false
  role = data["group"] || "container"

  hostvars = { ansible_connection: "lxc", ansible_python_interpreter: "/usr/bin/python2" }
  hostvars.merge!(data["vars"]) if data["vars"]

  containers[role] ||= {}
  containers[role]["hosts"] ||= []
  containers[role]["hosts"] << host
  containers["_meta"]["hostvars"][host] = hostvars
end
puts JSON.pretty_generate(containers)
