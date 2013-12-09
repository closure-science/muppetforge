#!/usr/bin/env ruby
require 'test/unit'

class PmtIntegration < Test::Unit::TestCase

    def build_module(module_dir)
        system "puppet module build #{module_dir}"
    end

    def deploy_module(tarball_location)
        system "puppet module deploy #{tarball_location} --module_repository=http://localhost:8080/mf"
    end

    def install_module(name)
        system "puppet module install #{name} --module_repository=http://localhost:8080/mf --force"
    end

    def setup
        # note: 2 depends from 1
        build_module "./resources/muppetforge-acceptance1"
        deploy_module "./resources/muppetforge-acceptance1/pkg/muppetforge-acceptance1-0.1.0.tar.gz"
        build_module "./resources/muppetforge-acceptance2"
        deploy_module "./resources/muppetforge-acceptance2/pkg/muppetforge-acceptance2-0.1.0.tar.gz"
        install_module "muppetforge/integration"
    end

    def test_can_install
        assert install_module "muppetforge/acceptance2"
    end

    def teardown
        # nothing to do here
    end
end
