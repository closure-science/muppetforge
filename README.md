Overview
=======
The [Puppet Forge](http://forge.puppetlabs.com) is the central repository for Puppet modules.
No packages nor sources to install a private Puppet Forge are available and it lacks an endpoint to automatize deployment of modules.
This project is born from the necessity of a private Forge installation in an ecosystem knit for [CI](http://en.wikipedia.org/wiki/Continuous_integration) and/or [CD](http://en.wikipedia.org/wiki/Continuous_delivery).

[![Build Status](https://travis-ci.org/closure-science/muppetforge.png)](https://travis-ci.org/closure-science/muppetforge)

Installing muppetforge
=======
Muppetforge is available for install as a [Docker](http://www.docker.com/) image from the [Docker Hub](https://registry.hub.docker.com/u/closurescience/muppetforge/).

Using the Docker image
-------
To download the image and run it in a container choose a port on your host machine to expose the forge to, then run:
```bash
docker run -p <PORT_TO_PUBLISH>:8080 closurescience/muppetforge
```

If you want to customize the configuration before starting the container you'll need a custom `sys.config` file (read below) and a Dockerfile to build your configured image, like the following one:
```
FROM closurescience/muppetforge
ADD sys.config /opt/muppetforge/releases/1/sys.config
# if you configure the listen port to a different one than 8080,
# uncomment the following line filling the placeholder.
#EXPOSE <ANOTHER_PORT>
```
Then build your image and run it:
```bash
docker build -t my_configured_muppetforge .
docker run -p <PORT_TO_PUBLISH>:{8080|<ANOTHER_PORT>} my_configured_muppetforge
```

Configuring
=======
All configuration is set in the `muppetforge_node/releases/1/sys.config` file, that contains a list of per-module configurations.

Server connector
-------
HTTP
```erlang
{muppet_forge, [
    {protocol, http},
    {port, 8080} % port number to bind to
]}
```
HTTPS
```erlang
{muppet_forge, [
    {protocol, https},
    {cacertfile, "/tmp/cacert.crt"}, % path to CA certificate
    {certfile, "/tmp/server.crt"}, % path to server certificate
    {keyfile, "/tmp/server.key"}, % path to server key
    {port, 8084} % port number to bind to
]}
```
It is not currently possible to enable both the HTTP and the HTTPS connectors at the same time on the same node.

Authentication
-------
Free access (no auth)
```erlang
{muppet_auth, [
    {auth_modules, [
        {muppet_auth_always,[]}
    ]}    
]},
```

LDAP
```erlang
{muppet_auth, [
    {auth_modules, [
        {muppet_basic_auth_ldap, [
            {servers, ["ldap.example.com"]}, % hostname or IP of the LDAP server
            {dn_format, "uid=~s,ou=Bind,dc=example,dc=com"}, % LDAP bind format string. Username will be interpolated in place of '~s'
            {ldap_options, [
                {ssl, true}, % use SSL to connect to LDAP server
                {port, 636} % LDAP server port
            ]}
        ]}
    ]}    
]},
```

CIDR whitelisting
```erlang
{muppet_auth, [
    {auth_modules, [
        {muppet_auth_cidr, [
            {allowed_cidrs, [
                % list of allowed CIDRs. The network address is given as string, the bitmask size as a number (e.g. {"127.0.0.0",8} allows access from the network 127.0.0.0/8)
                {"127.0.0.0", 8},
                {"192.168.1.0", 24}
            ]}
        ]}
    ]}    
]},
```

It is possible to enable more than one auth module in cascade -  if the first auth module does not grant access, an authentication attempt is made with the next one.
```erlang
% e.g. allow passwordless authentication from the 127.0.0.0/8 network, a connection from other networks requires LDAP authentication.
{muppet_auth, [
    {auth_modules, [
        {muppet_auth_cidr, [
            {allowed_cidrs, [{"127.0.0.0", 8}]}
        ]},
        {muppet_basic_auth_ldap, [
            {servers, ["ldap.example.com"]},
            {dn_format, "uid=~s,ou=Bind,dc=example,dc=com"},
            {ldap_options, [{ssl, true}, {port, 636}]}
        ]}
    ]}    
]},
```

Building from sources
=======
Requirements
-------
* [Erlang](http://www.erlang.org/download.html) R16 or higher with HiPE enabled
* [git](http://git-scm.com/downloads)
* [make](http://www.gnu.org/software/make/)
* a c++ compiler for [Jiffy](https://github.com/davisp/jiffy)
* [rebar](https://github.com/rebar/rebar) present on path
* [puppet](http://puppetlabs.com/) 2.7 or higher

Building
-------
Clone the repository:
```bash
git clone https://github.com/closure-science/muppetforge.git
```
enter the project directory
```bash
cd muppetforge
```
run make
```bash
make
```

Results
-------
The folder `muppetforge/rel/muppetforge_node` will contain a self-contained environment suited to run the muppetforge.
To start the forge with a console, edit the configuration in `muppetforge/rel/muppetforge_node/releases/1/sys.config` to suit your needs, then run 
```bash
muppetforge/rel/muppetforge_node/bin/muppetforge_node console
```
You should be able to tarball the entire `muppetforge_node` folder, copy it to any system similar to the one where you compiled and run the muppetforge.
Only the same version of `libc` and a few system libraries are required for it to work (detailed list TBD).

The folder `muppetforge/muppetforge-integration/pkg/` will contain the tarballs for the `muppetforge/integration` puppet module. It will also be aveilable from the just-built muppetforge, so you can install it from there once started.

Integrating with Puppet
=======
The Muppet Forge exposes an API compatible with Puppet Forge, therefore it integrates naturally with the [Puppet module tool](http://docs.puppetlabs.com/puppet/3/reference/modules_installing.html).
Just point to your Muppet Forge installation in puppet.conf:
```ini
[main]
module_repository = http://localhost:8080
```
Or provide the `--module_repository` option  when invoking the Puppet module tool:
```bash
puppet module search --module_repository=http://localhost:8080
```

The muppetforge/integration Puppet module
-------
To aid integration with the new endpoints defined in Muppet Forge, a Puppet module is provided.
It contais the `deploy` action for the Puppet module tool, and the `require_module` function.

Configuring an upstream forge
=======
Muppet Forge can be configured to mirror and automatically keep in sync with another forge by adding a Forge URL in the "Upstream" panel.
Muppet Forge will periodically check for and mirror new releases. When Mirroring from another Muppet Forge instance, fast change notification can
be enabled to speed up synchronization of new releases. 

Blacklisting modules
-------
Unwanted packages can be excluded from mirroring from an upstream forge by configuring a blacklist entry matching the module.
A blacklist entry is made of source forge, author, module name and version.
To prevent a release from being mirrored, all of the specified conditions must match.
Already mirrored modules matching a blacklist entry are not removed.

Deploying a module
=======
To deploy a module, just POST the module tarball to the apposite endpoint.

With CURL
-------
```bash
caligin@theleth:~/projects/fmeden-sample$ puppet module build
Notice: Building /home/caligin/projects/fmeden-sample for release
Module built: /home/caligin/projects/fmeden-sample/pkg/fmeden-sample-0.1.0.tar.gz
caligin@theleth:~/projects/fmeden-sample$ curl -X POST --data-binary @pkg/fmeden-sample-0.1.0.tar.gz http://localhost:8080/api/mf/deploy
"ok"
```

With Puppet
-------
If you installed the `muppetforge/integration` puppet module, a new command will be available to the Puppet module tool:
```bash
fmeden@Eve:~/projects/fmeden-sample$ puppet module build
Notice: Building /home/fmeden/projects/fmeden-sample for release
Module built: /home/fmeden/projects/fmeden-sample/pkg/fmeden-sample-0.1.0.tar.gz
fmeden@Eve:~/projects/fmeden-sample$ puppet module deploy pkg/fmeden-sample-0.1.0.tar.gz --module_repository=http://localhost:8080
Notice: Preparing to deploy pkg/fmeden-sample-0.1.0.tar.gz into http://192.168.1.219:8080 ...
Module succesfully deployed.
```

The web interface
=======
Once started, the forge's web interface is available on the configured port.

Upstream
-------
The "Upstream" page allows configuration of forge mirroring. Just specify the upstream forge's base url and the replication mode.
* **Polling only** Muppetforge periodically queries the remote forge for the available modules, and schedules a retrieve if necessary. Available for all forges.
* **Fast change notification** Muppetforge registers itself on the remote forge to be notified immediately about new modules. Available only when the remote forge is a Muppetforge itself.

Blacklist
-------
When mirroring a remote forge it's possible to specify modules that are not to be retrieved by the means of blacklists.
A blacklist entry is made of four optional fields `upstream url`, `author`, `module`, `version`: it will prevent a module to be imported if its properties match all of the specified blacklist criterions.
A blacklist entry will not delete already retrieved modules.

Errors
-------
A log that reports all errors during modules mirroring.
Muppetforge will retry to retrieve a failed module periodically.

API documentation
=======
##### `/modules.json` `Puppet`
* **GET** Retrieves the list of modules served by the Forge. Supports the optional query parameter `q=<query terms>`

##### `/api/v1/releases.json` `Puppet`
* **GET** Retrieves the available releases for a module along with their details (tarball location, dependencies...). Requires the `module=<author>/<module name>` query parameter and supports the optional `version=<version constraints>`

##### `/{author}/{modulename}.json` `Puppet`
* **GET** Retrieves detail information for a single module.

##### `/api/mf/{author}/{modulename}/{version}`
* **DELETE** Deletes a release from the forge. If it is the last remaining release of a module, the module is deleted too.

##### `/api/mf/deploy`
* **POST** Deploys a Puppet module tarball into the Forge. The tarball must be given as the request body. No other parameters are required as metadata will be extracted directly from the tarball. Missing or invalid metadata will incur in an error.

##### `/api/mf/blacklist`
* **GET** Retrieves the blacklist rules.
* **PUT** Overwrites the blacklist rules with new ones.

##### `/api/mf/upstream`
* **GET** Retrieves the list of upstream Forges
* **PUT** Overwrites the list of upstream Forges

##### `/api/mf/errors`
* **GET** Retrieves the list of errors got during the mirroring job.

##### `/api/mf/info`
* **GET** Retrieves some informational counters about the Forge status.

##### `/api/mf/listen`
* **Websocket** Server will send a notification on the websocket everytime a new release is deployed/retrieved.
