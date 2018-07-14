#!/bin/sh

export K8S_HOST="https://${KUBERNETES_SERVICE_HOST}"
export K8S_TOKEN=$(cat /var/run/secrets/kubernetes.io/serviceaccount/token)

exec "$@"
