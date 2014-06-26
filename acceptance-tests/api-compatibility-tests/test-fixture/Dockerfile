FROM closurescience/muppetforge

RUN rm /opt/muppetforge/lib/muppet_repository-1/priv/assets/*
ADD muppetforge-integration_preloaded/pkg/ /tmp/
USER root
RUN mv /tmp/muppetforge-integration_preloaded-0.1.0.tar.gz /opt/muppetforge/lib/muppet_repository-1/priv/assets
RUN rm -rf /tmp/muppetforge-integration_preloaded-0.1.0
USER muppetforge
