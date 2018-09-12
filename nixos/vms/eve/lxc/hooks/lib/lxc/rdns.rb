require 'netaddr'

module Lxc
  class RdnsZone
    def initialize(data, addr_field, subnet)
      @data = data
      @addr_field = addr_field
      @subnet = NetAddr::CIDR.create(subnet)
    end

    attr_reader :data

    def [](key)
      (data["zone"] || {})[key]
    end

    def pointers(&blk)
      @data["network"].each do |name, host|
        ip = host[@addr_field]
        next unless ip
        arpa = NetAddr::CIDR.create(ip).arpa
        next unless arpa.end_with?(@subnet.arpa)
        host_part = arpa[0, arpa.size - @subnet.arpa.size - 1]
        # only allowed characters in FQDN
        name = name.gsub(/[^a-zA-Z0-9\-]/, "-")
        yield name, host_part
      end
    end

    def name
      @subnet.arpa.gsub(/\.$/, "")
    end

    def write_zone_file(path)
      zone_template = Template.new(CONFIG_ROOT.join("hooks/templates/rdns-zone.erb"))
      domain = data["zone"]["dn42-domain"]
      zone_template.write(path.join("zones", name),
                          zone: self,
                          data: data,
                          domain: domain)
    end
  end
end
