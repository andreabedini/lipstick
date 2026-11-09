# Contributing

## Building and testing

```
cabal build all
cabal test all
cabal run lipstick-demo
```

The `>>>` examples in the Haddock documentation run with [doctest](https://hackage.haskell.org/package/doctest). CI builds doctest with GHC 9.12.2 and runs it as below; doctest must be built with the same GHC as the project.

```
cabal install doctest-0.25.0.2 --ignore-project --installdir="$HOME/.local/bin" --install-method=copy --overwrite-policy=always
cabal repl lipstick --with-compiler="$HOME/.local/bin/doctest" --repl-options=-XOverloadedStrings
```

The screenshots in the README are drawn from `examples/Screenshots.hs`, as SVG on the terminal's cell grid. CI regenerates them and fails if they differ from the committed ones, so a change in rendering must come with the new images:

```
cabal run lipstick-screenshots -- docs/images
```

The Unicode tables in `internal/Lipstick/Internal/UnicodeTable.hs` are generated from the Unicode Character Database 16.0.0; the library uses none of `base`'s Unicode data. To move to another version, download `EastAsianWidth.txt`, `auxiliary/GraphemeBreakProperty.txt`, `emoji/emoji-data.txt` and `DerivedCoreProperties.txt` from `https://www.unicode.org/Public/VERSION/ucd/` into one directory, without the subdirectories, and run

```
runghc scripts/GenUnicodeTable.hs VERSION DIRECTORY > internal/Lipstick/Internal/UnicodeTable.hs
```

Then copy `auxiliary/GraphemeBreakTest.txt` of the same version to `test/data/`, and update the expected version in `test/Main.hs` and in the `unicodeVersion` example in `src/Lipstick/Width.hs`.

## Commit messages

Commit messages follow [Conventional Commits](https://www.conventionalcommits.org/). For example:

```
fix: count negative widths, heights and crops as 0
feat(table): style cells by position
feat!: width and height mean the same for every thing
docs: explain the concepts and qualified import
```

The allowed types are `feat`, `fix`, `perf`, `docs`, `ci`, `test`, `refactor`, `build`, `chore`, `style` and `revert`. A scope in parentheses is optional. `!` before the colon, or a `BREAKING CHANGE:` footer, marks a breaking change. The `commits` CI job checks every commit of a pull request with [git-cliff](https://git-cliff.org/), so the allowed types are the ones that the commit parsers in `cliff.toml` know. There is no commit hook (jj would not run one), but the check can be run locally:

```
GIT_CLIFF__GIT__FILTER_UNCONVENTIONAL=false GIT_CLIFF__GIT__FILTER_COMMITS=false \
GIT_CLIFF__GIT__REQUIRE_CONVENTIONAL=true GIT_CLIFF__GIT__FAIL_ON_UNMATCHED_COMMIT=true \
git cliff origin/main..HEAD
```

The commit types choose the next version:

| Commits since the last release | Next version |
|---|---|
| At least one breaking change, of any type | `B` + 1 |
| At least one `feat` | `C` + 1 |
| At least one `fix` or `perf` | `D` + 1 |
| Only `docs`, `ci`, `test`, `refactor`, `build`, `chore`, `style`, `revert` | No release |

Pull requests are merged with rebase merge. A squash merge would leave only the PR title for the release task to read. The check rejects merge commits and GitHub's `Revert "…"` messages, which are not Conventional Commits: bring a pull request up to date by rebasing it on `main`, and write a revert as `revert: …`.

Only the pull request of the release task, from its branch `release-VERSION`, may have a `chore(release):` commit, as its last commit: merging one publishes a release.

## Versions

Versions follow the [Haskell PVP](https://pvp.haskell.org/) and have four components, `A.B.C.D`, for example `0.1.0.0`. `A.B` is the major version, `C` the minor version and `D` the patch version.

- git-cliff calculates the next version from the commit types, as in the table above. This is SemVer applied to `B.C.D`. There is no special rule for versions below 1.0: a breaking change increments `B` even when it is `0`.
- Incrementing a field sets all fields to its right to `0`.
- `A` changes only by hand, when the maintainer decides that a release deserves a new number. Like a change of `B`, it makes a major release.
- Release tags are `v` followed by the version, for example `v0.1.0.0`. The first release is `0.1.0.0`.
- Every package in `cabal.project` has the version of the release, and `lipstick-brick` depends on the `lipstick` of the same release (`lipstick ^>=VERSION`); the release task sets both.

The `version:` fields in the `.cabal` files are the only source of the version. Only the release task, and the maintainer in a release PR, change them and `CHANGELOG.md`.

## Making a release

The maintainer makes releases.

1. Make sure that `main` is green.
2. Run `mise run release-pr`. It reads the commits since the last release tag, sets the next version in every `.cabal` file, adds a section for the release to every package's `CHANGELOG.md` with [git-cliff](https://git-cliff.org/), and opens a pull request titled `chore(release): VERSION`. The `CHANGELOG.md` at the top lists every commit and gives the release notes; `lipstick-brick/CHANGELOG.md` lists the commits that change `lipstick-brick/`. A section for the release that is already in a `CHANGELOG.md`, written by hand, is kept with the date of the release. If there are no releasable commits, it says so and does nothing.
3. Review the pull request. Edit the new sections of `CHANGELOG.md` as you like; the next release keeps them. To increment `A`, change by hand the version in every `.cabal` file and `lipstick-brick`'s `lipstick ^>=` bound, and the `CHANGELOG.md` headings and the commit description to the same version; the branch keeps its name.
4. When `ci-ok` passes, merge the pull request with rebase merge. CI checks that the version is at least the one that the commits since the last release call for, on the pull request and again on `main`; if `main` gained a breaking change after the task ran, the check fails, and the task must run again.
5. CI's `release` job sees the release commit on `main`. It publishes a GitHub release with the source distributions of `lipstick` and `lipstick-brick` and their documentation for Hackage (`PACKAGE-VERSION-docs.tar.gz`), their `SHA256SUMS`, and the notes from `CHANGELOG.md`. Publishing creates the tag. Then the `pages` job publishes the documentation of the release to <https://andreabedini.github.io/lipstick/>.
6. Verify the release:

   ```
   gh release verify vVERSION
   gh attestation verify ASSET --repo andreabedini/lipstick
   ```

To publish a release on Hackage too, upload its source distribution and its documentation, which were built by CI, from the GitHub release:

```
gh release download vVERSION --repo andreabedini/lipstick --pattern 'PACKAGE-VERSION*.tar.gz'
cabal upload --publish PACKAGE-VERSION.tar.gz
cabal upload --publish --documentation PACKAGE-VERSION-docs.tar.gz
```

Releases are immutable: a published version cannot change. If a release is wrong, fix it and make a new release. If the `release` job fails before it publishes, rerun the failed jobs; the job continues the draft release.

### Release artifacts without a release

Every CI run, on a pull request or started by hand, uploads everything that a release would publish as one artifact, `dist`: the same files and `SHA256SUMS`, without attestations. To get them for any branch without releasing:

```
gh workflow run ci.yml --ref BRANCH
gh run list --workflow ci.yml --branch BRANCH --event workflow_dispatch --limit 1
gh run download RUN_ID --name dist
```

The `release` job runs only on a push to `main`, so a manual run never releases. The `github-pages` artifact of a run is the documentation site, as a tar file.
