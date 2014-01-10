require 'puppet/face'
require 'puppet/forge'
require 'net/http'

class Puppet::Forge

  class Repository

    def make_http_post(request_path, body_location)
      body_file = File.open(body_location,"r")
      request = Net::HTTP::Post.new(URI.escape(@uri.path + request_path), {
          "User-Agent" => user_agent,
          "Content-Type" => "data/binary",
          "Content-Length" => File.size(body_location).to_s
      })
      request.body_stream = body_file
      if ! @uri.user.nil? && ! @uri.password.nil?
        request.basic_auth(@uri.user, @uri.password)
      end
      return read_response(request)
    end

  end

  def deploy(body_file_location)
    response = repository.make_http_post("/api/mf/deploy", body_file_location)
    unless response.code == "200"
      raise ResponseError.new(:uri => uri.to_s, :input => "#{body_file_location}", :message => "error", :response => response)
    end
  end
end


module Puppet::ModuleTool
  module Applications
    class Deployer < Application

      include Puppet::ModuleTool::Errors
      include Puppet::Forge::Errors

      def initialize(name, forge, options = {})
        super(options)
        @action              = :deploy
        @environment         = Puppet::Node::Environment.new(Puppet.settings[:environment])
        @name                = name
        @forge               = forge
      end

      def run
        results = {}
        begin
          @filename = File.expand_path(@name)
          raise MissingPackageError, :requested_package => @filename unless File.exist?(@filename) and is_module_package?(@filename)
          @forge.deploy(@filename)
        rescue ModuleToolError, ForgeError => err
          results[:error] = {
            :oneline   => err.message,
            :multiline => err.multiline,
          }
        else
          results[:result] = :success
        ensure
          results[:result] ||= :failure
        end
        results
      end

      def is_module_package?(name)
        filename = File.expand_path(name)
        filename =~ /.tar.gz$/
      end
    end
  end
end

Puppet::Face.define(:module, '1.0.0') do
  action(:deploy) do
    summary "Deploy a module to the Muppet Forge."
    description <<-EOT
      Deploy a module to the Muppet Forge.

      The specified module tarball will be deployed to the configured
      `module_repository`.
    EOT

    returns "Nothing."

    examples <<-'EOT'
        TODO.
    EOT

    arguments "<tarball>"

    when_invoked do |tarball, options|
      Puppet::ModuleTool.set_option_defaults options
      forge = Puppet::Forge.new("Muppetforge", self.version)
      Puppet.notice "Preparing to deploy #{tarball} into #{forge.uri} ..."
      deployer = Puppet::ModuleTool::Applications::Deployer.new(tarball, forge, options)
      
      deployer.run
    end

    when_rendering :console do |return_value, name, options|
      if return_value[:result] == :failure
        Puppet.err(return_value[:error][:multiline])
        exit 1
      else
        "Module succesfully deployed."
      end
    end

  end
end
