def module_exists(node, author, name)
    node.modules.select do |mod|
        (mod.name == name && mod.author == author)
    end.empty? == false
end

module Puppet::Parser::Functions
    newfunction(:require_module, :type => :statement, :arity => 3) do | args |
        author, modulename, version = args
        env_name = Puppet.settings[:environment]
        modulepath = Puppet.settings.value(:modulepath, env_name)
        options = {
            :version => version,
            :environment => env_name,
            :modulepath  => modulepath,
            :target_dir => File.expand_path(modulepath.split(File::PATH_SEPARATOR).first)
        }
        fn_version = SemVer.new "0.0.1" #whatever
        forge = Puppet::Forge.new("Muppetforge", fn_version)
        install_dir = Puppet::ModuleTool::InstallDirectory.new(Pathname.new(options[:target_dir]))
        env = Puppet::Node::Environment.current
        if module_exists(env, author, modulename) then 
            installer = Puppet::ModuleTool::Applications::Upgrader.new("#{author}/#{modulename}", forge, options)
        else
            installer = Puppet::ModuleTool::Applications::Installer.new("#{author}/#{modulename}", forge, install_dir, options)
        end
        installer.run
        env.instance_eval do ||
            @attr_expirations[:modules] = Time.now
        end
    end
end