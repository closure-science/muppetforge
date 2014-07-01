# Copyright 2012 Erlware, LLC. All Rights Reserved.
#
# This file is provided to you under the Apache License,
# Version 2.0 (the "License"); you may not use this file
# except in compliance with the License.  You may obtain
# a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied.  See the License for the
# specific language governing permissions and limitations
# under the License.
#

ERLFLAGS= -pa $(CURDIR)/.eunit -pa $(CURDIR)/apps/*/ebin -pa $(CURDIR)/deps/*/ebin
DEPS_PLT=$(CURDIR)/.deps_plt
DEPS=erts kernel stdlib ssh crypto public_key inets eldap

NODE=$(shell ls rel/ | grep node | sed -e 's/ //g' )
# =============================================================================
# Verify that the programs we need to run are installed on this system
# =============================================================================
ERL = $(shell which erl)

ifeq ($(ERL),)
$(error "Erlang not available on this system")
endif

REBAR=$(shell which rebar)

ifeq ($(REBAR),)
$(error "Rebar not available on this system")
endif

PUPPET=$(shell which puppet)

ifeq ($(PUPPET),)
$(error "Puppet not available on this system")
endif


.PHONY: all compile doc clean test dialyzer typer shell distclean pdf \
  update-deps clean-common-test-data rebuild release start console puppetmodule

all: deps compile dialyzer test puppetmodule release

# =============================================================================
# Rules to build the system
# =============================================================================

deps:
	$(REBAR) get-deps
	$(REBAR) compile

update-deps:
	$(REBAR) update-deps
	$(REBAR) compile

compile:
	$(REBAR) skip_deps=true compile

doc:
	$(REBAR) skip_deps=true doc

eunit: compile clean-common-test-data
	$(REBAR) skip_deps=true eunit

test: compile eunit

$(DEPS_PLT):
	@echo Building local plt at $(DEPS_PLT)
	@echo
	dialyzer --output_plt $(DEPS_PLT) --build_plt \
	   --apps $(DEPS) -r deps

dialyzer: $(DEPS_PLT)
	dialyzer --fullpath --plt $(DEPS_PLT) -Wrace_conditions -r apps/*/ebin

typer:
	typer --plt $(DEPS_PLT) -r apps/*/src

shell: deps compile
# You often want *rebuilt* rebar tests to be available to the
# shell you have to call eunit (to get the tests
# rebuilt). However, eunit runs the tests, which probably
# fails (thats probably why You want them in the shell). This
# runs eunit but tells make to ignore the result.
	- @$(REBAR) skip_deps=true eunit
	@$(ERL) $(ERLFLAGS)

pdf:
	pandoc README.md -o README.pdf

clean:
	- rm -rf $(CURDIR)/test/*.beam
	- rm -rf $(CURDIR)/logs
	- rm -rf $(CURDIR)/apps/*/ebin
	- rm -rf $(CURDIR)/apps/*/src/*.beam
	$(REBAR) skip_deps=true clean

distclean: clean
	- rm -rf $(DEPS_PLT)
	- rm -rvf $(CURDIR)/deps

rebuild: distclean deps compile escript dialyzer test puppetmodule


release:
	$(REBAR) generate	
tarball: release
	@cd rel; tar cvfz muppetforge.tar.gz muppetforge_node
start:
	rel/$(NODE)/bin/$(NODE) start
console:
	rel/${NODE}/bin/${NODE} console ${ARGS}

puppetmodule:
	puppet module build muppetforge-integration
	@mkdir -p apps/muppet_repository/priv/assets/
	@echo "Copying muppetforge-integration packages to repository assets dir..."
	@cp muppetforge-integration/pkg/muppetforge-integration-*.tar.gz apps/muppet_repository/priv/assets/

compatibility:
	$(MAKE) -C acceptance-tests/api-compatibility-tests/ all