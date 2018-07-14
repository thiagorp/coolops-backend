FROM google/cloud-sdk:alpine

RUN gcloud components install kubectl

COPY entrypoint.sh .

ENTRYPOINT ["./entrypoint.sh"]
