## Release checklist

- Check the Travis log from the latest build(s) and make sure nothing strange is
  happening, such as errors or warnings that did not result in a command exiting
  with nonzero status.
- Pick a version number according to [semantic versioning][semver].
- Update the `CHANGES` file.
- `grep` for the previous version number. Replace occurences with the new
  version number. This should include at least `version.ml` and `README.md`.
- Tag (`tag -a`) the release in the release branch. Include the changelog in
  the tag message, so it can be viewed with `git tag -n99`.
- Push the tag.
- Create a nicely-formatted GitHub release.
- Submit the release to OPAM from the release branch.
- Update GitHub Pages (this is on hold until self-instrumentation is restored).

[semver]: http://semver.org/
