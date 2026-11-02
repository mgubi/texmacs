# The `svn_sync` branch and its synchronisation

`svn_sync` is a git mirror of the SVN trunk of TeXmacs on Savannah
(`svn.savannah.gnu.org/texmacs/trunk`), made with `git svn`. One commit
corresponds to one SVN revision. The workflow
[`svn-sync.yml`](svn-sync.yml) keeps it up to date.

## What the workflow does

**When it runs:**

- every six hours, at :17 past the hour (UTC): `cron: '17 */6 * * *'`;
- by hand, from the Actions tab ("Sync svn_sync from Savannah" → Run
  workflow), or with
  ```bash
  gh workflow run svn-sync.yml
  ```

**What one run does:** a single job, in an `ubuntu:24.04` container.

1. Install `git`, `git-svn` and `ca-certificates`.
2. Check out `svn_sync` with its full history (`fetch-depth: 0`).
3. Mark the checkout as a safe directory. The checkout belongs to the
   runner's user and the container runs as root.
4. **Fetch:**
   ```bash
   git svn init --rewrite-root=svn+ssh://svn.sv.gnu.org/texmacs/trunk \
     https://svn.savannah.gnu.org/svn/texmacs/trunk
   git update-ref refs/remotes/git-svn HEAD
   git svn fetch
   ```
5. **Push:** if new revisions came in, and they extend `svn_sync`, it runs
   ```bash
   git push origin refs/remotes/git-svn:refs/heads/svn_sync
   ```
   This is a plain push, never `--force`.

A run with nothing new ends with "svn_sync is up to date" and pushes
nothing. A run that finds new revisions lists them in the log of the
Fetch step.

## Why the commits are identical to a developer's git-svn

The branch was first made from a developer checkout with SVN write access,
using the URL `svn+ssh://svn.sv.gnu.org/texmacs/trunk`. Each commit
message ends with a line such as
```
git-svn-id: svn+ssh://svn.sv.gnu.org/texmacs/trunk@15775 64cb5145-927a-446d-8aed-2fb7b4773692
```

The workflow reads the repository anonymously over https. Three points
keep its commits identical to those of a developer checkout:

- **`--rewrite-root`:** git-svn writes the `svn+ssh://…` URL into
  `git-svn-id` instead of the https URL it reads from. The UUID is that
  of the repository, so it does not change either.
- **Authors:** git-svn's default, `name <name@uuid>`, with no authors
  file. The branch was made this way too.
- **Dates:** git-svn takes the author and committer dates from the SVN
  revision, so they do not depend on when the sync runs.

As a result, the hashes do not depend on who fetches, or from which URL,
and the branch only ever fast-forwards. You can still update it by hand
from a developer checkout (see below), and the two never conflict.

## The rev map, rebuilt on each run

git-svn needs a map from SVN revisions to git commits, which it keeps in
`.git/svn/`. That directory is never pushed, so a fresh checkout does not
have it. The workflow points `refs/remotes/git-svn` at the tip of
`svn_sync`. `git svn fetch` then rebuilds the map from the `git-svn-id`
lines of the history ("Rebuilding … rev_map …" in the log). This is why
the checkout needs the full history. It takes a few seconds, after which
only the revisions after the tip are fetched.

## Why a container

On GitHub's `ubuntu-24.04` runner, git comes from a PPA, which is newer
than the git that Ubuntu's `git-svn` package depends on. So `git-svn`
cannot be installed there. A plain `ubuntu:24.04` container has git and
git-svn from the same archive.

## Safety

- **No force-push.** The Push step first checks
  `git merge-base --is-ancestor HEAD refs/remotes/git-svn`. If the SVN
  history ever stopped extending `svn_sync` (for example, a different
  URL, author mapping or rewritten SVN history), the run fails with
  "the SVN history does not extend svn_sync", and the branch is left
  untouched.
- **No overlapping runs.** `concurrency: svn-sync` (without cancellation)
  makes two runs wait for each other rather than race.
- **Backup.** `backup/svn_sync-r15774` holds the state of `svn_sync`
  before the workflow existed (r15774, `22c18b60`). To restore it:
  ```bash
  git push --force origin backup/svn_sync-r15774:svn_sync
  ```
- **No other workflows triggered.** Pushes made with `GITHUB_TOKEN` do
  not start other workflows, so this push does not start any CI. CI runs
  on `fixes_ci`.

## Requirements and limits

- A scheduled workflow runs only from the **default branch** of the
  repository (`wip_fixes`), so `svn-sync.yml` has to stay there.
- GitHub disables scheduled workflows after 60 days without activity in
  the repository. Re-enable it from the Actions tab if that happens.
- Only the trunk is mirrored, not branches or tags.
- If Savannah is unreachable, the run fails and the next one catches up.

## Updating by hand

**From a git-svn checkout with developer access:**
```bash
git svn rebase
git push origin HEAD:svn_sync
```

**From any clone, anonymously** (the same commands as the workflow):
```bash
git fetch origin svn_sync
git checkout -B svn_sync origin/svn_sync
git svn init --rewrite-root=svn+ssh://svn.sv.gnu.org/texmacs/trunk \
  https://svn.savannah.gnu.org/svn/texmacs/trunk
git update-ref refs/remotes/git-svn HEAD
git svn fetch
git push origin refs/remotes/git-svn:refs/heads/svn_sync
```
