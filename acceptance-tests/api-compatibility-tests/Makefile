VERSIONS = 2.7.12 2.7.25 3.2.1 3.6.1
TEMPDIR := target
PUPPET=$(shell which puppet)

ifeq ($(PUPPET),)
$(error "Puppet not available on this system")
endif

DOCKER=$(shell which docker)

ifeq ($(DOCKER),)
$(error "Docker not available on this system")
endif

.PHONY: clean fixture $(VERSIONS)

all: $(VERSIONS)

fixture:
	puppet module build test-fixture/muppetforge-integration_preloaded/
	docker build -t muppetforge-api-integration:fixture test-fixture/

#weird trick to match rule to folder name...
$(TEMPDIR)/%/./:
	mkdir -p $@

$(TEMPDIR)/%/api_tests.rb: api_tests.rb | $(TEMPDIR)/%/./
	cp api_tests.rb $@

$(TEMPDIR)/%/Dockerfile: Dockerfile.template | $(TEMPDIR)/%/./
	cat Dockerfile.template | sed s/@@VERSION@@/$*/g > $@

$(TEMPDIR)/%/image_built: $(TEMPDIR)/%/Dockerfile $(TEMPDIR)/%/api_tests.rb
	docker build -t muppetforge-api-integration:$* $(TEMPDIR)/$*/
	touch $(TEMPDIR)/$*/image_built

$(VERSIONS): %: $(TEMPDIR)/%/image_built fixture
	docker run --name fixture -d muppetforge-api-integration:fixture && \
	docker run --link fixture:forge --name $@ --rm muppetforge-api-integration:$@; \
	RETVAL=$$?; \
	$(MAKE) cleanup_fixture; \
	exit $$RETVAL

cleanup_fixture:
	docker kill fixture; \
	docker rm fixture

clean:
	- $(MAKE) cleanup_fixture
	rm -rf target
