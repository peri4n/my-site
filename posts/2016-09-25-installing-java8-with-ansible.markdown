---
layout: post
title: "Installing Java 8 with ansible"
subtitle: "Let here be light."
categories: [hadoop, devops]
---

This time I'd like to show you how you can easily install [Java 8][java8] on your virtual machines. In a [previous post][part1] I
described how to fire up several virtual machines using [Vagrant][vagrant] and [Virtualbox][virtualbox]. The final goal being to
boot an entire hadoop cluster on my MacBook Pro.

# Provisioning vagrant machines with ansible

To tell Vagrant that we want to use Ansible to provision the our virtual machines we have to adjust our `Vagrantfile`.
The follwing snippet has to be placed inside the config blog we introduced last time:

``` ruby
config.vm.provision :ansible do |ansible|
   ansible.playbook = "provision/playbook.yml"
   ansible.sudo = true
end
```

The first line inside the ansible block tells Vagrant the path to our playbook, which we will create soon. The second
line tells Ansible to execute every command inside the playbook as root user.

# Installing Java 8

Now we need to create the play we just referenced in our `Vagrantfile`. You'll see a lot of playbooks on the web that claim to
install Java 8 but I haven't found a single one which uses Ansible modules. Why is using Ansible modules so important, I here you
asking? Thats a topic for a different article but to give you an impresson, they introduce up-to-date checks to see if a task has 
already been done in a previous provisioning.

But let's get back on track. Please copy the content, I'll explain everything afterwards.


``` yaml
---
- hosts: all
  tasks:

  - name: Install java 8 preresequesits
    apt: name=python-software-properties

  - name: Add Java 8 repository
    apt_repository: repo='ppa:webupd8team/java'

  - name: Agree to oracle license
    debconf: name=oracle-java8-installer question=shared/accepted-oracle-license-v1-1 vtype=select value=true

  - name: Install Java 8
    apt: name=oracle-java8-installer force=yes
```

The first line is Ansible specific and tells it on which hosts the playbook should be run. When using Vagrant, this line should be 
ignore because Vagrant decides on which hosts the playbook is applied.

The following tasks install preresequesits, add the Oracle Java 8 repository, agree to their license and finally install Java 8.

If you now ask Vagrant to provision you servers:

``` zsh
vagrant provision
```

You can test your Java version with:

``` zsh
java -version
```

[part1]: {% post_url 2016-09-19-starting-multiple-vms-in-vagrant %}
[java8]: https://www.java.com/en/download/faq/java8.xml
[vagrant]:     http://vagrantup.com/
[virtualbox]:  http://virtualbox.org/
