require 'puppet/face'

# encoding: UTF-8
require 'puppet/forge'
require 'puppet/module_tool/install_directory'
require 'pathname'

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
      #Puppet::ModuleTool.set_option_defaults options
      #Puppet.notice "Preparing to install into #{options[:target_dir]} ..."
      #
      #forge = Puppet::Forge.new("Muppetforge", self.version)
      #install_dir = Puppet::ModuleTool::InstallDirectory.new(Pathname.new(options[:target_dir]))
      #installer = Puppet::ModuleTool::Applications::Installer.new(name, forge, install_dir, options)
      #
      #installer.run
    end

  end
end
