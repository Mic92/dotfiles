require "net/http"
require "json"

module Lxc
  class Hetzner
    BASE_URI = URI("https://robot-ws.your-server.de")
    def initialize(user, password)
      @user = user
      @password = password
    end

    def get(path)
      resp = perform_request(Net::HTTP::Get.new(uri_for(path)))
      JSON.parse(resp.body)
    end

    def post(path, params={})
      req = Net::HTTP::Post.new(uri_for(path))
      req.set_form_data(params)
      resp = perform_request(req)
      JSON.parse(resp.body)
    end

    def put(path, params={})
      req = Net::HTTP::Put.new(uri_for(path))
      req.set_form_data(params)
      resp = perform_request(req, allow_404)
      JSON.parse(resp.body)
    end

    def delete(path, allow_404: false)
      perform_request(Net::HTTP::Delete.new(uri_for(path)), allow_404: allow_404)
    end

    private
    def uri_for(path)
      u = BASE_URI.clone
      u.path = path
      u
    end

    def perform_request(req, allow_404: false)
      req.basic_auth(@user, @password)
      resp = Net::HTTP.start(BASE_URI.hostname, BASE_URI.port, use_ssl: true) do |http|
        http.request(req)
      end
      if resp.code.start_with?("2") || (allow_404 && resp.code == "404")
        return resp
      else
        raise StandardError.new("failed to perform request for '#{req.path}': #{resp.code} - #{resp.body}")
      end
    end
  end
end
