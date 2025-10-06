FROM scratch AS base
COPY --chmod=777 hadolint /bin/hadolint
LABEL org.opencontainers.image.source="https://github.com/hadolint/hadolint"
CMD ["/bin/hadolint", "-"]

FROM debian:bookworm-slim AS debian
COPY --chmod=777 hadolint /bin/hadolint
LABEL org.opencontainers.image.source="https://github.com/hadolint/hadolint"
CMD ["/bin/hadolint", "-"]

FROM alpine:3.22 AS alpine
COPY --chmod=777 hadolint /bin/hadolint
LABEL org.opencontainers.image.source="https://github.com/hadolint/hadolint"
CMD ["/bin/hadolint", "-"]
