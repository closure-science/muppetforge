FROM centos:centos6
MAINTAINER Fulvio Meden <caligin35+mf@gmail.com>

RUN sed -i '/^\[centosplus\]$/,/^\[/ s/^enabled=0$/enabled=1/' /etc/yum.repos.d/CentOS-Base.repo
RUN rpm -ivh http://yum.puppetlabs.com/puppetlabs-release-el-6.noarch.rpm
RUN yum install -y puppet-2.7.12 tar
ADD api_tests.rb .

ENTRYPOINT ["/usr/bin/ruby"]
CMD ["api_tests.rb"]