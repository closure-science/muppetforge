PUPPET=$(shell which puppet)

ifeq ($(PUPPET),)
$(error "Puppet not available on this system")
endif

DOCKER=$(shell which docker)

ifeq ($(DOCKER),)
$(error "Docker not available on this system")
endif

.PHONY: all clean fixture 3.2.1

all: 2.7.12 3.2.1

fixture:
	puppet module build test-fixture/muppetforge-integration_preloaded/
	docker build -t muppetforge-api-integration:fixture test-fixture/

2.7.12: fixture
	docker build -t muppetforge-api-integration:2.7.12 2.7.12/
	docker run --name fixture -d muppetforge-api-integration:fixture
	docker run --link fixture:forge --name 2.7.12 --rm muppetforge-api-integration:2.7.12
	- docker kill fixture
	- docker rm fixture

3.2.1: fixture
	docker build -t muppetforge-api-integration:3.2.1 3.2.1/
	docker run --name fixture -d muppetforge-api-integration:fixture
	docker run --link fixture:forge --name 3.2.1 --rm muppetforge-api-integration:3.2.1
	- docker kill fixture
	- docker rm fixture

clean:
	- docker kill fixture
	- docker rm fixture
	- docker kill 3.2.1
	- docker rm 3.2.1
	@echo -n 