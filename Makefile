RELEASE_DOCKER_IMG = coolops-api-release
BUILD_DOCKER_IMG = coolops-api-build:latest

build_base:
	docker build -t coolopsio/api-buildbase:latest -f Dockerfile.buildbase .
	docker push coolopsio/api-buildbase:latest

build:
	docker build -t ${BUILD_DOCKER_IMG} -f Dockerfile.build .

release:
	docker run -d \
		-v $$(pwd):/tmp \
		${BUILD_DOCKER_IMG} \
		cp -r executables /tmp
	docker build -t ${RELEASE_DOCKER_IMG} -f Dockerfile.release .

live-backend:
	find . -type f \( -iname \*.hs -o -iname \*.yaml -o -iname \*.sql \) | entr -r make run

run:
	stack build
	stack exec api-exe
