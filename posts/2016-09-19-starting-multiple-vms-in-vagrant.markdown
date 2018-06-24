---
layout: post
title: "Running multiple VMs in vagrant"
subtitle: "Spin them up!"
categories: [hadoop, devops]
---

This post is the first in a series where I want to setup a [Hadoop][hadoop] cluster which can than be used for development and in testing.

So with out further ado let's start.

# Installing Vagrant and Virtualbox

The tools we are going to use are [Vagrant][vagrant] (to manage the boxes) and [Virtualbox][virtualbox] (as a hypervisor). 

If you are on a mac installing both is as easy as:

``` zsh
brew install vagrant virtualbox
```

Other operating system should be comparably easy.

# Setting up the boxes

I'd like to use Ubuntu 16.04 (LTS) for my boxes. Searching the vagrantcloud I found: [bento/ubuntu-16.04][bento]. 
To use it create a file called `Vagrantfile` with the following content:

``` ruby
Vagrant.configure("2") do |config|
  config.vm.define :server1 do |server1|
      server1.vm.box = "bento/ubuntu-16.04"
      server1.vm.network :private_network, ip: "10.11.1.100"
  end

  config.vm.define :server2 do |server2|
      server2.vm.box = "bento/ubuntu-16.04"
      server2.vm.network :private_network, ip: "10.11.1.101"
  end
end
```

# Spinning up the boxes

Starting both machines is as easy as:

``` zsh
vagrant up
```

Both servers are now up and running. You can ssh into server1 with:

``` zsh
vagrant ssh server1
```

To see if server2 to is also up and running, we can ping it:

``` zsh
vagrant ssh server1
vagrant@vagrant:~$ ping 10.11.1.101
PING 10.11.1.101 (10.11.1.101) 56(84) bytes of data.
64 bytes from 10.11.1.101: icmp_seq=1 ttl=64 time=0.562 ms
64 bytes from 10.11.1.101: icmp_seq=2 ttl=64 time=0.562 ms
64 bytes from 10.11.1.101: icmp_seq=3 ttl=64 time=0.537 ms
64 bytes from 10.11.1.101: icmp_seq=4 ttl=64 time=0.535 ms
...
```

[hadoop]:      http://hadoop.apache.org/
[vagrant]:     http://vagrantup.com/
[virtualbox]:  http://virtualbox.org/
[bento]:       https://atlas.hashicorp.com/bento/boxes/ubuntu-16.04
