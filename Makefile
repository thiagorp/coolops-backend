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
	hpack
	cat coolops-api.cabal
	stack install --local-bin-path application
	docker build -t ${RELEASE_DOCKER_IMG} -f Dockerfile.release .

push: auth_container_registry
	docker push ${RELEASE_DOCKER_IMG}

notify_coolops:
	curl -L https://github.com/coolopsio/coolops/releases/download/v0.1.0/install.sh | sh
	coolops build:new ${BUILD_NAME} -t ${COOLOPS_PRODUCTION_API_TOKEN} -p DOCKER_IMAGE=${RELEASE_DOCKER_IMG} -m "Job url"="${JOB_URL}"


dev:
	ghcid --command "stack ghci coolops-api:lib" --test "Executables.Api.run"


deployment-runner:
	ghcid --command "stack ghci coolops-api:lib" --test "Executables.DeploymentRunner.run"


run-tests:
	ghcid --command "stack ghci coolops-api:lib coolops-api:test:api-test" --test "main"


live-backend:
	find . -path ./.stack-work -prune -o -type f \( -iname \*.hs -o -iname \*.yaml \) | entr -d -r make run

run:
	stack build --fast
	stack exec deployment-runner | sed -e 's/^/[Deployment Runner] /' & \
		stack exec job-status-checker | sed -e 's/^/[Job Status Checker] /' & \
		stack exec background-job-runner | sed -e 's/^/[Background Jobs Runner] /' & \
		stack exec api-exe | sed -e 's/^/[Api] /' &
