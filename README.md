Overview
=======
The [Puppet Forge](http://forge.puppetlabs.com) is the central repository for Puppet modules.
No packages nor sources to install a private Puppet Forge are available, its API is undocumented and it lacks an endpoint to automatize deployment of modules.
This project is born from the necessity of a private Forge installation in an ecosystem knit for [CI](http://en.wikipedia.org/wiki/Continuous_integration) and/or [CD](http://en.wikipedia.org/wiki/Continuous_delivery).

[![Build Status](https://travis-ci.org/closure-science/muppetforge.png)](https://travis-ci.org/closure-science/muppetforge)

Installing muppetforge
=======
TBD

Configuring
=======
TBD

Building from sources
=======
TBD

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


API documentation
=======
##### `/modules.json` `Puppet`
* **GET** Retrieves the list of modules served by the Forge. Supports the optional query parameter `q=&lt;query terms&gt;`

##### `/api/v1/releases.json` `Puppet`
* **GET** Retrieves the available releases for a module along with their details (tarball location, dependencies...). Requires the `module=&lt;author&gt;/&lt;module name&gt;` query parameter and supports the optional `version=&lt;version constraints&gt;`

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
