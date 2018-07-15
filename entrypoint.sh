#!/bin/sh

export K8S_HOST="https://${KUBERNETES_SERVICE_HOST}"

exec "$@"
