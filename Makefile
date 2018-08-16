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
	docker build -t ${RELEASE_DOCKER_IMG} -f Dockerfile.release .

push: auth_container_registry
	docker push ${RELEASE_DOCKER_IMG}

notify_coolops:
	curl -d '{"name":"${VERSION}","params":{"IMAGE_NAME":"${RELEASE_DOCKER_IMG}"},"metadata":{"Job url":"${JOB_URL}"}}' -H "Authorization: Token ${COOLOPS_STAGING_API_TOKEN}" -H "Content-Type: application/json" -X POST -f https://api-staging.coolops.io/builds
	curl -d '{"name":"${VERSION}","params":{"IMAGE_NAME":"${RELEASE_DOCKER_IMG}"},"metadata":{"Job url":"${JOB_URL}"}}' -H "Authorization: Token ${COOLOPS_PRODUCTION_API_TOKEN}" -H "Content-Type: application/json" -X POST -f https://api.coolops.io/builds


dev-api:
	ghcid --command "stack ghci api:lib api:exe:api-exe"

dev-slack-api:
	ghcid --command "stack ghci api:lib api:exe:slack-api"


run-tests:
	ghcid --command "stack ghci api:lib api:test:api-test" --test "main"


live-backend:
	find . -path ./.stack-work -prune -o -type f \( -iname \*.hs -o -iname \*.yaml \) | entr -d -r make run

run:
	stack build --fast
	stack exec slack-api | sed -e 's/^/[Slack Api] /' & \
		stack exec deployment-runner | sed -e 's/^/[Deployment Runner] /' & \
		stack exec job-status-checker | sed -e 's/^/[Job Status Checker] /' & \
		stack exec background-job-runner | sed -e 's/^/[Background Jobs Runner] /' & \
		stack exec api-exe | sed -e 's/^/[Api] /' &
