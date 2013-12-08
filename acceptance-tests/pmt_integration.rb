#!/usr/bin/env ruby
require 'test/unit'
require 'puppet'
require 'puppet/forge'
require '../muppetforge-integration/lib/puppet/face/module/deploy.rb'

class PmtIntegration < Test::Unit::TestCase

    def build_test_module(module_name)
        test_module_dir = Dir.pwd + "/resources/" + module_name
        module_root = Puppet::ModuleTool.find_module_root(test_module_dir)
        Puppet::ModuleTool::Applications::Builder.run(module_root, {})
    end

    def deploy_module(module_name)
        tarball_location = Dir.pwd + "/resources/" + module_name + "/pkg/" + module_name + "-0.1.0.tar.gz"
        options = { module_repository => "http://localhost:8080/mf"}
        Puppet::ModuleTool.set_option_defaults options
        forge = Puppet::Forge.new("Muppetforge-acceptance-tests", "0.0.1")
        deployer = Puppet::ModuleTool::Applications::Deployer.new(tarball_location, forge, options)
        deployer.run
    end

    def setup
        # note: 2 depends from 1
        build_test_module "muppetforge-acceptance1"
        deploy_module "muppetforge-acceptance1"
        build_test_module "muppetforge-acceptance2"
        deploy_module "muppetforge-acceptance2"
    end

    def test_can_install
        options = { module_repository => "http://localhost:8080/mf"}
        Puppet::ModuleTool.set_option_defaults options
        forge = Puppet::Forge.new("Muppetforge-acceptance-tests", "0.0.1")
        install_dir = Puppet::ModuleTool::InstallDirectory.new(Pathname.new(options[:target_dir]))
        installer = Puppet::ModuleTool::Applications::Installer.new("muppetforge-acceptance2", forge, install_dir, options)
        installer.run
    end

    def teardown
        # nothing to do here
    end
end