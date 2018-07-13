#!/bin/sh

echo $(cat /var/run/secrets/kubernetes.io/serviceaccount/token)
K8S_HOST=kubernetes.default.svc K8S_TOKEN=$(cat /var/run/secrets/kubernetes.io/serviceaccount/token) exec "$@"
