require 'test/unit'
require 'puppet/face'
require 'puppet/module_tool'
require 'socket'
require 'timeout'

class ModuleToolFacade

    def initialize
        @modulepath = "/tmp/modulepath"
        @vardir = "/tmp/vardir"
    end

    def setup_puppet
        Dir.mkdir @modulepath unless File.directory? @modulepath
        Dir.mkdir @vardir unless File.directory? @vardir
        Puppet.settings[:modulepath] = @modulepath
        Puppet.settings[:vardir] = @vardir
        Puppet.settings[:module_repository] = "http://forge:8080/"
    end

    def search(term)
        Puppet::Module::Tool::Applications::Searcher.run(term)
    end

    def install(name)
        Puppet::Module::Tool::Applications::Installer.run(name)
    end
end

class ApiIntegrationTest321 < Test::Unit::TestCase

    def setup
        @module_tool = ModuleToolFacade.new
        @module_tool.setup_puppet
        await_forge_boot()
    end

    def await_forge_boot
        Timeout::timeout(30) do
            begin
                s = TCPSocket.new("forge", 8080)
                s.close
                return
            rescue Errno::ECONNREFUSED, Errno::EHOSTUNREACH
                retry
            end
        end
    end

    def test_can_search
        answers = @module_tool.search("preloaded")
        expected = "muppetforge/integration_preloaded"
        got = answers[0]["full_name"]
        assert_equal(expected, got)
    end

    def test_can_install
        install_result = @module_tool.install("muppetforge/integration_preloaded")
        got = install_result[:installed_modules].size
        assert_equal(1, got)
    end

    def test_can_deploy
        #TODO: need the mf-integration module first!
    end

    def todo_can_upgrade
        #TODO
    end

end