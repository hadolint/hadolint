# Release procedure

Only `master` branch is used for releases.

1.  Update _hadolint-docker_ entry version in the
    [.pre-commit-hooks.yaml](../.pre-commit-hooks.yaml) file.
    Use the new version you're going to release.
    And push to master.

1.  Create branch to update release number in `package.yaml` and
    [integration](INTEGRATION.md) guide and merge it when it pass CI.

1.  Write a **draft** of release where _tag version_ and _release title_ are
    the same as a version of `hadolint`, eg `v1.2.3`.
    ![draft](https://user-images.githubusercontent.com/18702153/32983073-f7477820-cc86-11e7-92c6-fabfc1223a25.png)

1.  Create an annotated tag with tag name as a message and push it back to
    `hadolint/hadolint` remote. Release notes should be in the draft not
    in a tag message.

    ```bash
    export TAG=v1.2.3 && git pull && git tag -a "$TAG" -m "$TAG" && git push origin master --tags && stack upload .
    ```

1.  Tag creation will trigger build in _Travis_ and _AppVeyor_ which will upload
    binaries to GitHub release draft with the same name as is the name
    of the tag (created in step 1).
    ![draft_binaries](https://user-images.githubusercontent.com/18702153/32983247-7692d528-cc89-11e7-9340-2af1434a6bdf.png)

1.  Edit release notes, mention all new features and important fixes and
    publish it.
