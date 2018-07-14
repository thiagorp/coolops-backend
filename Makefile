PROJECT_ID=fluent-cosine-206514
RELEASE_DOCKER_IMG = gcr.io/${PROJECT_ID}/coolops-api:${VERSION}
BUILD_DOCKER_IMG = coolops-api-build:latest

build_base:
	docker build -t coolopsio/api-buildbase:latest -f Dockerfile.buildbase .
	docker push coolopsio/api-buildbase:latest

auth_container_registry:
	echo ${GCLOUD_KEY_JSON} | base64 --decode --ignore-garbage > ${HOME}/gcloud-service-key.json
	docker login -u _json_key --password-stdin https://gcr.io < ${HOME}/gcloud-service-key.json

build:
	stack install --local-bin-path application

release:
	docker build -t ${RELEASE_DOCKER_IMG} -f Dockerfile.release .

push: auth_container_registry
	docker push ${RELEASE_DOCKER_IMG}
	curl -d '{"name":"${VERSION}","params":{"IMAGE_NAME":"${RELEASE_DOCKER_IMG}"}}' -H "Authorization: Token MzLHStpKFawckrJq7ErJkVv66HKjm68HyTga" -H "Content-Type: application/json" -X POST -v https://api-staging.coolops.io/builds


live-backend:
	find . -type f \( -iname \*.hs -o -iname \*.yaml -o -iname \*.sql \) | entr -r make run

run:
	stack build
	stack exec api-exe
