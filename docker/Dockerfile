FROM scratch as base
COPY --chmod=777 hadolint /bin/hadolint
CMD ["/bin/hadolint", "-"]

FROM debian:bullseye-slim as debian
COPY --from=base --chmod=777 /bin/hadolint /hadolint
CMD ["/bin/hadolint", "-"]

FROM alpine:3.15 as alpine
COPY --from=base --chmod=777 /bin/hadolint /hadolint
CMD ["/bin/hadolint", "-"]