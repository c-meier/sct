# sct_prepare

`scripts/sct_prepare.py` builds a student template from a main branch and
(optionally) publishes it to a separate starter repo. It's a single `uv`-runnable
script: per-repo settings (branch names, current phase, starter remote, ...)
live in a `.sct.toml` file, so the script itself can be shared unmodified
across courses — copied in, or run straight from this repo's URL.

## Requirements

- [uv](https://docs.astral.sh/uv/) (to run the script — it declares its own
  Python requirement, no venv setup needed)
- `git` and the `sct` binary on `PATH`

## Running it

From the root of a course repository (the one that gets split into a student
template), either copy the script in and run it locally:

```sh
uv run scripts/sct_prepare.py
```

or run it straight from this repo without copying anything:

```sh
uv run https://raw.githubusercontent.com/c-meier/sct/master/scripts/sct_prepare.py
```

### Flags

- `--yes` / `-y` — auto-confirm the student commit step (no interactive
  prompt). The starter-repo push step always prompts, regardless of this flag.
- `--no-starter` — skip the starter-repo push step even if `starter-remote`
  is configured.
- `--dry-run` — build the student template and show the diff, but commit and
  push nothing.
- `--phase PHASE` — override the configured `current-phase` for this run.
- `--starter-remote URL` — override the configured `starter-remote` for this
  run (see note below on why you'd want this).

### Environment variables

- `SCT_PHASE` — same effect as `--phase`, lower priority (an explicit flag
  still wins).
- `SCT_STARTER_REMOTE` — same effect as `--starter-remote`, lower priority.

Resolution order for both is: CLI flag, then environment variable, then
`.sct.toml`. These exist for values you don't want to type on every
invocation while repeatedly re-running this script (preparing a lab, say)
but also don't want baked into the committed config — set them once per
checkout with [direnv](https://direnv.net/), e.g. in that course repo's
`.envrc` (typically left untracked):

```sh
export SCT_PHASE=part2
export SCT_STARTER_REMOTE=git@github.com:my-org/course-2026-starter.git
```

## Configuration: `.sct.toml`

The script reads a `[prepare]` table from `.sct.toml` at the repository root
— the same config file the `sct` binary itself reads (for language specs and
other settings), so one file covers both. All keys are optional; if the file
or the table is missing, built-in defaults are used and a warning is printed.

`.sct.toml` is meant to be committed with the course repo — these are
project-level settings, shared across whoever runs this script. `starter-remote`
is the exception: if it's a different repo per year (or per class), don't
bake that into the committed file — either leave it unset there (the
starter step is simply skipped when it's absent) and use `--starter-remote`
or `SCT_STARTER_REMOTE` each time it's actually needed. Baking a one-off URL
into the tracked config just means editing and committing it again for
every new class/year, and it's easy to forget.

```toml
[prepare]
current-phase = "main"      # passed as sct's --context; omit to disable
main-branch = "master"      # the branch worktrees/main tracks; default: "master"
student-branch = "student"  # default: "student"
starter-remote = ""         # git URL of a starter repo; omit to skip that step
starter-remote-name = "starter"  # local name for that remote; default: "starter"
ignore-file = ".sctignore"  # default: ".sctignore"
binary-patterns = ["*.zip", "*.pdf", "*.png", "..."]  # copied as-is, not transformed; see below for the full default
```

`ignore-file` and `binary-patterns` both use plain `.gitignore` pattern
syntax (no shell brace expansion like `*.{a,b}` — write separate entries
instead), resolved by asking `git` itself, the same way for both.

`binary-patterns` defaults to a broad list of common archive, compiled,
document, image, font, and audio/video extensions (see
`DEFAULT_BINARY_PATTERNS` in the script for the exact list). It's
deliberately generous: a tracked file that *isn't* matched gets piped
through `sct` as if it were text, and `sct` reads its input assuming valid
UTF-8 — a genuinely binary file that falls through crashes the whole run
with a decoding error instead of just being copied wrong. Setting
`binary-patterns` in `.sct.toml` replaces the default list entirely (it
doesn't merge with it), so if you only want to add one extension, copy the
full default list and append to it.

## What it does

1. Ensures `worktrees/student` and `worktrees/main` exist (and
   `worktrees/starter` too, if `starter-remote` is set), adding them as git
   worktrees if missing. `worktrees/main` always has this fixed name,
   regardless of what `main-branch` is set to — the directory is just a
   label for "the worktree tracking the main branch", decoupled from the
   actual branch name it holds.
2. Resets `worktrees/student` to a clean state: checks out the
   `student-branch` (creating it as an orphan branch if it doesn't exist yet),
   then removes all tracked and untracked files — so files deleted from the
   main branch don't linger in old student checkouts.
3. Checks out `main-branch` in `worktrees/main` and runs `sct -so` over
   every tracked file (`git ls-files`), writing the transformed student
   version into `worktrees/student`. Which tracked files match
   `ignore-file` or `binary-patterns` is resolved by `git ls-files
   --ignored --exclude-from` — the same mechanism, just with two different
   pattern lists. Files matched by `ignore-file` are skipped entirely;
   files matched by `binary-patterns` are copied verbatim instead of being
   piped through `sct`.

   Note this only considers files tracked by git in `worktrees/main` at
   that commit — unlike a plain filesystem walk, a tracked file that also
   happens to match the repo's real `.gitignore` is still included (only
   `ignore-file` can skip it).
4. Stages the result in `worktrees/student`, shows `git diff --staged`, and
   (unless `--dry-run`) commits it with a message referencing the main-branch
   commit it was generated from.
5. If `starter-remote` is configured and `--no-starter` wasn't passed, offers
   to publish the student branch to that remote (added locally as
   `starter-remote-name`, a plain internal label — only worth changing if
   the repo already has an unrelated remote called `starter`) — this step
   always asks for confirmation, and a force-push additionally requires
   typing `force push` to confirm, since it can overwrite that remote's
   branch.

   If a remote named `starter-remote-name` already exists but points
   somewhere other than the configured `starter-remote` URL, the script
   stops with an error before touching anything, instead of resetting,
   checking out, or pushing against what would be the wrong repository.

## Error handling

The script validates its environment before doing anything destructive:

- Fails fast with a clear message if `git` or `sct` aren't on `PATH`.
- Fails fast if not run inside a git repository.
- Warns (but doesn't fail) if `.sct.toml` or its `[prepare]` table is
  missing, falling back to defaults.
- Reports malformed TOML or wrong-typed config values with the offending
  key and file path.
- Wraps every `git`/`sct` invocation so a failure prints the command
  and its stderr instead of a raw traceback.
