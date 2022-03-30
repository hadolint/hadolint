FROM scratch as base
COPY --chmod=777 hadolint /bin/hadolint
CMD ["/bin/hadolint", "-"]

FROM debian:bullseye-slim as debian
COPY --chmod=777 hadolint /hadolint
CMD ["/bin/hadolint", "-"]

FROM alpine:3.15 as alpine
COPY --chmod=777 hadolint /hadolint
CMD ["/bin/hadolint", "-"]