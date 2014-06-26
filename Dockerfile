FROM ubuntu:trusty
MAINTAINER Cassibba Alessio <swapon+mf@gmail.com>, Fulvio Meden <caligin35+mf@gmail.com>, Roberto Ferranti <roberto.ferranti+mf@gmail.com> 

RUN adduser --home /opt/muppetforge --shell /usr/sbin/nologin --no-create-home --disabled-password --gecos none --disabled-login muppetforge
RUN mkdir -p /opt/muppetforge/

EXPOSE 8080
ENTRYPOINT ["/opt/muppetforge/bin/muppetforge_node"]
CMD ["foreground"]

ADD rel/muppetforge_node /opt/muppetforge
RUN chown -R muppetforge. /opt/muppetforge/

USER muppetforge