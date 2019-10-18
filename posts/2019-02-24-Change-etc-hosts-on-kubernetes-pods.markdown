---
layout: post
title: Changing the /etc/hosts on every Kubernetes pod with a label
keywords: kubernetes,kubectl,saas,zsh,bash, shell
tags: kubernetes
---

I once was tasked to setup a SaaS application in [Kubernetes][1] (short K8S).
Everything went smoothly until I found out that some pod, spawned by the application, had to have their `/etc/hosts/` file adjusted.

# So this is what I did

I created a `hosts` file in my home directory containing the lines I wanted to add.
Than I run:

~~~~ {.bash .numberLines}
kubectl exec <podName> -- sh -c "echo '$(cat ~/hosts )' >> /etc/hosts"; 
~~~~

The command executes an `echo` on the pod and appends to output to `/etc/hosts`.
The tricky part is, what gets echoed.
Before executing the shell, on your computer, replaces `$(cat·~/hosts·)'`with the actual content of `~/hosts`.
This feature of a shell is called [Command Substitution][3] and is available in both [Bash][4] and [Zsh][5].

# Look ma no hands

If you want to run this on all pods labeled `foo=bar`, you can use shell for loops:

~~~~ {.bash .numberLines}
for podName in $(kubectl get pods -l foo=bar -o name | sed 's/pod\///'); do
  kubectl exec $podName -- sh -c "echo '$(cat ~/hosts )' >> /etc/hosts"; 
done
~~~~

# Disclaimer

Do not do these things in production.
This tip is useful while testing and debugging but setting up your production environment should be *completely automated*.

[1]: http://kubernetes.io
[2]: https://kubernetes.io/docs/concepts/workloads/pods/pod/
[3]: http://zsh.sourceforge.net/Intro/intro_7.html
[4]: https://www.gnu.org/software/bash/
[5]: http://zsh.sourceforge.net/
