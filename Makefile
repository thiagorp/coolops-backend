RELEASE_DOCKER_IMG = kapis-release

build:
	docker build -t kapis-build -f Dockerfile.build .

release:
	docker run -d \
		-v $$(pwd):/tmp \
		kapis-build \
		cp /bin/server /tmp
	docker build -t ${RELEASE_DOCKER_IMG} -f Dockerfile.release .

heroku_login:
	heroku container:login

deploy_production: build release heroku_login
	docker tag kapis-release:latest registry.heroku.com/kapis-api/web
	docker push registry.heroku.com/kapis-api/web 


live-backend:
	find . -type f \( -iname \*.hs -o -iname \*.yaml -o -iname \*.sql \) | entr -r make run

run:
	stack build
	foreman start
