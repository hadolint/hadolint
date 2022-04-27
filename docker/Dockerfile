FROM scratch as base
COPY hadolint /bin/hadolint 
RUN chmod 777 -R /bin/hadolint
CMD ["/bin/hadolint", "-"]

FROM debian:bullseye-slim as debian
COPY hadolint /bin/hadolint
RUN chmod 777 -R /bin/hadolint
CMD ["/bin/hadolint", "-"]

FROM alpine:3.15 as alpine
COPY hadolint /bin/hadolint
RUN chmod 777 -R /bin/hadolint
CMD ["/bin/hadolint", "-"]
