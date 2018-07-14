#!/bin/sh

export K8S_HOST=http://kubernetes.default.svc
export K8S_TOKEN=$(cat /var/run/secrets/kubernetes.io/serviceaccount/token)

exec "$@"
