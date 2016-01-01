#!/bin/bash
set -o errexit
set -o pipefail
set -o nounset

readonly CWD="$(pwd)"
readonly TESTS_DIR="tests"

function clone_repos() {
    rm -rf "$TESTS_DIR" && mkdir -p "$TESTS_DIR" && cd "$TESTS_DIR"
    git clone https://github.com/docker-library/ruby.git
    git clone https://github.com/docker-library/mariadb.git
    git clone https://github.com/docker-library/ghost.git
    git clone https://github.com/docker-library/redis.git
    git clone https://github.com/docker-library/python.git
    git clone https://github.com/docker-library/buildpack-deps.git
    git clone https://github.com/docker-library/postgres.git
    git clone https://github.com/docker-library/rabbitmq.git
    git clone https://github.com/docker-library/redmine.git
    git clone https://github.com/docker-library/mysql.git
    git clone https://github.com/docker-library/golang.git
    git clone https://github.com/docker-library/drupal.git
    git clone https://github.com/docker-library/haproxy.git
    git clone https://github.com/docker-library/elasticsearch.git
    git clone https://github.com/docker-library/kibana.git
    git clone https://github.com/docker-library/php.git
    git clone https://github.com/docker-library/mongo.git
    git clone https://github.com/docker-library/gcc.git
    git clone https://github.com/docker-library/httpd.git
    git clone https://github.com/docker-library/java.git
    git clone https://github.com/docker-library/wordpress.git
    git clone https://github.com/docker-library/tomcat.git
    git clone https://github.com/docker-library/logstash.git
    git clone https://github.com/docker-library/julia.git
    git clone https://github.com/docker-library/busybox.git
    git clone https://github.com/docker-library/percona.git
    git clone https://github.com/docker-library/django.git
    git clone https://github.com/docker-library/memcached.git
    git clone https://github.com/docker-library/docker.git
    git clone https://github.com/docker-library/rails.git
    git clone https://github.com/docker-library/pypy.git
    git clone https://github.com/docker-library/hello-world.git
    git clone https://github.com/docker-library/celery.git
    git clone https://github.com/nodejs/docker-node.git
    git clone https://github.com/nginxinc/docker-nginx.git
    cd "$CWD"
}

function lint_dockerfiles() {
    local dockerfiles=$(find . -name 'Dockerfile')
    for dockerfile in $dockerfiles; do
        echo "Linting $dockerfile"
        cabal run hadolint "$dockerfile"
    done
}

function main() {
    clone_repos
    lint_dockerfiles
}

main
