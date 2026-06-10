FROM scratch AS base
COPY --chmod=777 hadolint /bin/hadolint
LABEL org.opencontainers.image.source="https://github.com/hadolint/hadolint"
CMD ["/bin/hadolint", "-"]

FROM debian:trixie-slim AS debian
COPY --chmod=777 hadolint /bin/hadolint
LABEL org.opencontainers.image.source="https://github.com/hadolint/hadolint"
CMD ["/bin/hadolint", "-"]

FROM alpine:3.24 AS alpine
COPY --chmod=777 hadolint /bin/hadolint
LABEL org.opencontainers.image.source="https://github.com/hadolint/hadolint"
CMD ["/bin/hadolint", "-"]
