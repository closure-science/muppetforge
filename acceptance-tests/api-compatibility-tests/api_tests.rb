require 'test/unit'
require 'socket'
require 'timeout'

class ApiIntegrationTest < Test::Unit::TestCase

    def setup
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
        out = `puppet module search preloaded | tail -n +3 | grep muppetforge-integration_preloaded`
        assert_equal(0, $?.to_i, out)
    end

    def test_can_install
        out = `puppet module install muppetforge/integration_preloaded`
        assert_equal(0, $?.to_i, out)
    end

    def test_can_deploy
        #TODO: need the mf-integration module first!
    end

    def todo_can_upgrade
        #TODO
    end

end