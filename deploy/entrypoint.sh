#!/bin/sh

set -e

echo $GCLOUD_KEY_JSON | base64 -d > gcloud-service-key.json
gcloud auth activate-service-account --key-file gcloud-service-key.json

gcloud config set project $PROJECT_ID
gcloud config set compute/zone $COMPUTE_ZONE
gcloud container clusters get-credentials $CLUSTER_NAME

kubectl set image deployment/api-server -n $CLUSTER_NAMESPACE api-server=$IMAGE_NAME
kubectl set image deployment/deployment-runner -n $CLUSTER_NAMESPACE deployment-runner=$IMAGE_NAME
kubectl set image deployment/job-status-checker -n $CLUSTER_NAMESPACE job-status-checker=$IMAGE_NAME
kubectl set image deployment/slack-api-server -n $CLUSTER_NAMESPACE slack-api-server=$IMAGE_NAME
kubectl set image deployment/background-job-runner -n $CLUSTER_NAMESPACE background-job-runner=$IMAGE_NAME
