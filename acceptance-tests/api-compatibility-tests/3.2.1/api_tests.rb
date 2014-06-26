require 'test/unit'
require 'puppet/face'
require 'puppet/module_tool'


# OUTSIDE: system `docker run --name fixture -d muppetforge-api-integration:fixture`
# OUTSIDE: run this container as: system `docker run -i -t --link fixture:forge <name>`
# docker returns the run exit status as exit, testunit returns 1 on failure

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
        Puppet::ModuleTool.set_option_defaults({})
        got = Puppet::ModuleTool::Applications::Searcher.new(term, Puppet::Forge.new("api integration test 3.2.1", SemVer.new("3.2.1")), {}).run
        raise "Remoting failure" if got[:result] != :success
        got[:answers]
    end

    def install(name)
        Puppet::ModuleTool.set_option_defaults({})
        forge = Puppet::Forge.new("api integration test 3.2.1", SemVer.new("3.2.1"))
        install_dir = Puppet::ModuleTool::InstallDirectory.new(Pathname.new(@modulepath))
        installer = Puppet::ModuleTool::Applications::Installer.new(name, forge, install_dir, {:target_dir => @modulepath })
        installer.run
    end


end

class ApiIntegrationTest321 < Test::Unit::TestCase

    def setup
        @module_tool = ModuleToolFacade.new
        @module_tool.setup_puppet
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