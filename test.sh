#!/bin/bash
set -o errexit
set -o pipefail
set -o nounset

readonly CWD="$(pwd)"
readonly TESTS_DIR="tests"

function git_clone() {
    local git_url="$1"
    git clone --quiet --depth 1 "$git_url"
}

function clone_repos() {
    rm -rf "$TESTS_DIR" && mkdir -p "$TESTS_DIR" && cd "$TESTS_DIR"
    # offical docker images
    git_clone https://github.com/docker-library/ruby.git &
    git_clone https://github.com/docker-library/mariadb.git &
    git_clone https://github.com/docker-library/ghost.git &
    git_clone https://github.com/docker-library/redis.git &
    git_clone https://github.com/docker-library/python.git &
    git_clone https://github.com/docker-library/buildpack-deps.git &
    git_clone https://github.com/docker-library/postgres.git &
    git_clone https://github.com/docker-library/rabbitmq.git &
    git_clone https://github.com/docker-library/redmine.git &
    git_clone https://github.com/docker-library/mysql.git &
    git_clone https://github.com/docker-library/golang.git &
    git_clone https://github.com/docker-library/drupal.git &
    git_clone https://github.com/docker-library/haproxy.git &
    git_clone https://github.com/docker-library/elasticsearch.git &
    git_clone https://github.com/docker-library/kibana.git &
    git_clone https://github.com/docker-library/php.git &
    git_clone https://github.com/docker-library/mongo.git &
    git_clone https://github.com/docker-library/gcc.git &
    git_clone https://github.com/docker-library/httpd.git &
    git_clone https://github.com/docker-library/java.git &
    git_clone https://github.com/docker-library/wordpress.git &
    git_clone https://github.com/docker-library/tomcat.git &
    git_clone https://github.com/docker-library/logstash.git &
    git_clone https://github.com/docker-library/julia.git &
    git_clone https://github.com/docker-library/busybox.git &
    git_clone https://github.com/docker-library/percona.git &
    git_clone https://github.com/docker-library/django.git &
    git_clone https://github.com/docker-library/memcached.git &
    git_clone https://github.com/docker-library/docker.git &
    git_clone https://github.com/docker-library/rails.git &
    git_clone https://github.com/docker-library/pypy.git &
    git_clone https://github.com/docker-library/hello-world.git &
    git_clone https://github.com/docker-library/celery.git &
    git_clone https://github.com/nodejs/docker-node.git &
    git_clone https://github.com/nginxinc/docker-nginx.git &

    # popular Dockerfile repos
    git_clone https://github.com/wking/dockerfile.git &
    git_clone https://github.com/eugeneware/docker-wordpress-nginx.git &
    git_clone https://github.com/CentOS/CentOS-Dockerfiles.git &
    git_clone https://github.com/octohost/octohost.git &
    git_clone https://github.com/komljen/dockerfile-examples.git &
    git_clone https://github.com/tianon/dockerfiles.git &
    git_clone https://github.com/Netflix-Skunkworks/zerotodocker.git &
    git_clone https://github.com/amplab/docker-scripts.git &
    git_clone https://github.com/oracle/docker-images.git &

    wait
    cd "$CWD"
}

function lint_dockerfiles() {
    cabal build
    local hadolint_bin="./dist/build/hadolint/hadolint"
    local dockerfiles=$(find . -name 'Dockerfile')
    for dockerfile in $dockerfiles; do
        echo "Lint $dockerfile"
        $hadolint_bin "$dockerfile"
    done
}

function main() {
    clone_repos
    lint_dockerfiles
}

main
