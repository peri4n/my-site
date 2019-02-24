---
layout: post
title: "Default setup of my Jupyter notebooks"
---

~~~~ {.bash .numberLines}
kubectl exec <podName> -- sh -c "echo '$(cat ~/hosts )' >> /etc/hosts"; 
~~~~

~~~~ {.bash .numberLines}
for podName in $(kubectl get pods -l foo=bar -o name | sed 's/pod\///'); do
  kubectl exec $podName -- sh -c "echo '$(cat ~/hosts )' >> /etc/hosts"; 
done
~~~~
