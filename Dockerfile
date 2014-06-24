FROM ubuntu:trusty
MAINTAINER Cassibba Alessio <swapon+mf@gmail.com>, Fulvio Meden <caligin35+mf@gmail.com>, Roberto Ferranti <roberto.ferranti+mf@gmail.com> 

RUN mkdir -p /opt/muppetforge/
ADD rel/muppetforge_node /opt/muppetforge

EXPOSE 8080
ENTRYPOINT ["/opt/muppetforge/bin/muppetforge_node", "console"]
