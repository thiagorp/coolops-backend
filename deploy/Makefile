DOCKER_IMAGE=coolopsio/api-deployment:${VERSION}

publish: build push

build:
	docker build -t ${DOCKER_IMAGE} .

push:
	docker push ${DOCKER_IMAGE}
