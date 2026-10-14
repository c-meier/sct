#!/usr/bin/env -S uv run
# /// script
# requires-python = ">=3.11"
# dependencies = []
# ///
"""sct_prepare: build the student template and (optionally) a starter repo.

See docs/sct-prepare.md in https://github.com/c-meier/sct for full usage.
Run with `uv run sct_prepare.py` (or `uv run <raw-github-url>`), from the
root of the course repository that contains a `.sct.toml` with a
`[prepare]` table.

Layout of this file, roughly in call order:
  * small utilities       - error/printing helpers, subprocess wrappers
  * configuration         - the `[prepare]` table in `.sct.toml`
  * pure helpers          - file partitioning (unit-testable, no
                            subprocess/filesystem access)
  * git plumbing          - thin wrappers around specific git invocations
  * workflow steps        - the actual worktree/transform/commit/push steps
  * CLI                   - argument parsing and orchestration (`main`)
"""

from __future__ import annotations

import argparse
import contextlib
import os
import shutil
import subprocess
import sys
import tempfile
from collections.abc import Generator
from dataclasses import dataclass
from pathlib import Path
from typing import Any, NoReturn, TypeVar

try:
    import tomllib
except ModuleNotFoundError:
    print(
        "sct_prepare: requires Python >= 3.11 (for tomllib). "
        "Run it with `uv run sct_prepare.py` instead of a bare `python`.",
        file=sys.stderr,
    )
    sys.exit(1)

# Without this, status messages printed just before a subprocess call (whose
# output is inherited, not captured) can appear *after* that subprocess's own
# output, because stdout is block-buffered when it isn't a tty.
sys.stdout.reconfigure(line_buffering=True)

DEFAULT_IGNORE_FILE = ".sctignore"
DEFAULT_BINARY_PATTERNS = [
    # archives
    "*.zip", "*.tar", "*.gz", "*.tgz", "*.7z", "*.rar",
    # compiled/data dumps
    "*.jar", "*.class", "*.dump", "*.db", "*.sqlite", "*.sqlite3",
    # documents
    "*.pdf", "*.doc", "*.docx", "*.xls", "*.xlsx", "*.ppt", "*.pptx",
    # images
    "*.png", "*.jpg", "*.jpeg", "*.gif", "*.bmp", "*.webp", "*.ico",
    # fonts
    "*.woff", "*.woff2", "*.ttf", "*.otf",
    # audio/video
    "*.mp3", "*.mp4", "*.wav", "*.mov", "*.ogg",
]
DEFAULT_MAIN_BRANCH = "master"
DEFAULT_STUDENT_BRANCH = "student"
DEFAULT_STARTER_REMOTE_NAME = "starter"

ENV_STARTER_REMOTE = "SCT_STARTER_REMOTE"
ENV_PHASE = "SCT_PHASE"

STUDENT_WT = "worktrees/student"
MAIN_WT = "worktrees/main"
STARTER_WT = "worktrees/starter"


# ---------------------------------------------------------------------------
# Small utilities
# ---------------------------------------------------------------------------


def step(msg: str) -> None:
    """Print a banner marking the start of a new phase.

    Without this, status prints, inherited git output, and diffs/prompts all
    run together with no visual boundary, making it hard to tell which step
    produced which line. One of these per phase in main() gives the output a
    scannable shape.
    """
    print(f"\n==> {msg}")


def warn(msg: str) -> None:
    print(f"sct_prepare: warning: {msg}", file=sys.stderr)


def die(msg: str, code: int = 1) -> NoReturn:
    print(f"sct_prepare: error: {msg}", file=sys.stderr)
    sys.exit(code)


@contextlib.contextmanager
def pushd(path: Path | str) -> Generator[None, None, None]:
    """Bash `pushd`/`popd`, as a context manager: chdir in, always chdir back out."""
    prev = Path.cwd()
    os.chdir(path)
    try:
        yield
    finally:
        os.chdir(prev)


def _run_checked(
    cmd: list[str], *, cwd: Path | str | None, text: bool, capture: bool
) -> subprocess.CompletedProcess[Any]:
    """Shared implementation behind `run`/`run_bytes`: run, or die with a readable message."""
    try:
        return subprocess.run(
            cmd,
            cwd=cwd,
            check=True,
            text=text,
            stdout=subprocess.PIPE if capture else None,
            stderr=subprocess.PIPE if capture else None,
        )
    except FileNotFoundError:
        die(f"command not found: {cmd[0]!r} (is it installed and on PATH?)")
    except subprocess.CalledProcessError as e:
        stderr = e.stderr if isinstance(e.stderr, str) else (e.stderr or b"").decode(errors="replace")
        detail = stderr.strip()
        die(f"command failed (exit {e.returncode}): {' '.join(cmd)}" + (f"\n{detail}" if detail else ""))


def run(cmd: list[str], *, cwd: Path | str | None = None, capture: bool = False) -> subprocess.CompletedProcess[str]:
    """Run a command, exiting the whole script with a readable message on failure.

    Used for steps whose output should either go straight to the user's
    terminal (capture=False) or be read back as text (capture=True).
    """
    return _run_checked(cmd, cwd=cwd, text=True, capture=capture)


def run_bytes(cmd: list[str], *, cwd: Path | str | None = None) -> subprocess.CompletedProcess[bytes]:
    """Like `run(..., capture=True)`, but without text decoding.

    Needed for `sct` output (which may legitimately contain non-UTF-8 bytes)
    and for NUL-separated `git ls-files -z` listings, so filenames with
    unusual characters can't corrupt the split.
    """
    return _run_checked(cmd, cwd=cwd, text=False, capture=True)


def require_tools(*names: str) -> None:
    missing = [n for n in names if shutil.which(n) is None]
    if missing:
        die(
            "required tool(s) not found on PATH: " + ", ".join(missing)
            + ". Install them (or enter the project's dev shell) before running this script."
        )


# ---------------------------------------------------------------------------
# Configuration: the `[prepare]` table in `.sct.toml`
# ---------------------------------------------------------------------------


@dataclass
class PrepareConfig:
    phase: str | None
    main_branch: str
    student_branch: str
    starter_remote: str | None
    starter_remote_name: str
    ignore_file: str
    binary_patterns: list[str]


class ConfigError(ValueError):
    """An invalid value was found in the `[prepare]` table."""


T = TypeVar("T")


def get_typed(table: dict[str, object], key: str, expected: type[T], default: T) -> T:
    """Read `table[key]`, type-checked against `expected`, falling back to `default`.

    TOML already distinguishes strings/bools/ints/etc., so this checks the
    type rather than casting - a stray `main-branch = true` should be a
    clear config error, not silently become the string `"True"`. Generic
    over `expected` so a future non-string `[prepare]` key (say a bool or
    an int) is just another call to this, no new helper needed.
    """
    value = table.get(key)
    if value is None:
        return default
    if not isinstance(value, expected):
        raise ConfigError(f"'prepare.{key}' must be a {expected.__name__}, got {value!r}")
    return value


def parse_prepare_config(table: dict[str, object]) -> PrepareConfig:
    """Turn an already-parsed `[prepare]` table into a `PrepareConfig`.

    Pure (no file or process I/O): raises `ConfigError` instead of exiting,
    so this part of config handling can be unit-tested with plain dicts,
    independent of `.sct.toml` actually existing on disk.
    """

    def get_opt_str(key: str) -> str | None:
        return get_typed(table, key, str, "") or None

    def get_str(key: str, default: str) -> str:
        return get_opt_str(key) or default

    def get_str_list(key: str, default: list[str]) -> list[str]:
        value = get_typed(table, key, list, default)
        if not all(isinstance(v, str) for v in value):
            raise ConfigError(f"'prepare.{key}' must be a list of strings, got {value!r}")
        return value

    return PrepareConfig(
        phase=get_opt_str("current-phase"),
        main_branch=get_str("main-branch", DEFAULT_MAIN_BRANCH),
        student_branch=get_str("student-branch", DEFAULT_STUDENT_BRANCH),
        starter_remote=get_opt_str("starter-remote"),
        starter_remote_name=get_str("starter-remote-name", DEFAULT_STARTER_REMOTE_NAME),
        ignore_file=get_str("ignore-file", DEFAULT_IGNORE_FILE),
        binary_patterns=get_str_list("binary-patterns", DEFAULT_BINARY_PATTERNS),
    )


def load_config(root: Path) -> PrepareConfig:
    """Find, read and parse `.sct.toml`, falling back to defaults with a warning.

    Missing file, missing `[prepare]` table, and malformed/mistyped values
    are all handled here (the I/O-facing half of config loading); the actual
    key-by-key validation lives in the pure `parse_prepare_config`.
    """
    path = root / ".sct.toml"
    table: dict[str, object] = {}
    if path.is_file():
        try:
            data = tomllib.loads(path.read_text())
        except tomllib.TOMLDecodeError as e:
            die(f"could not parse {path}: {e}")
        table = data.get("prepare", {})
        if not isinstance(table, dict):
            die(f"{path}: 'prepare' must be a table, e.g. [prepare]")
        if not table:
            warn(f"{path} has no [prepare] table; using built-in defaults")
    else:
        warn(f"no {path} found at repo root; using built-in defaults (see docs/sct-prepare.md)")

    try:
        return parse_prepare_config(table)
    except ConfigError as e:
        die(f"{path}: {e}")


# ---------------------------------------------------------------------------
# Pure helpers (no subprocess/filesystem access - easy to unit-test directly)
# ---------------------------------------------------------------------------


def partition_tracked_files(
    tracked: list[str], ignored: set[str], binary: set[str]
) -> tuple[list[str], list[str]]:
    """Split tracked files (relative paths) into (to_transform, to_copy_verbatim).

    Files in `ignored` are dropped entirely. Pure function: given the file
    lists as plain Python data (sets/lists computed from `git ls-files`
    elsewhere), no git/filesystem calls happen here, which is what makes it
    unit-testable without a real repository.
    """
    to_transform: list[str] = []
    to_copy: list[str] = []
    for rel in tracked:
        if rel in ignored:
            continue
        if rel in binary:
            to_copy.append(rel)
        else:
            to_transform.append(rel)
    return to_transform, to_copy


# ---------------------------------------------------------------------------
# Git plumbing
# ---------------------------------------------------------------------------


def find_repo_root() -> Path:
    res = subprocess.run(["git", "rev-parse", "--show-toplevel"], text=True, capture_output=True)
    if res.returncode != 0:
        die("not inside a git repository (or git is not installed). Run this script from within the course repository.")
    return Path(res.stdout.strip())


def branch_exists(branch: str) -> bool:
    if subprocess.run(["git", "show-ref", "--verify", "--quiet", f"refs/heads/{branch}"]).returncode == 0:
        return True
    refs = subprocess.run(
        ["git", "for-each-ref", "--format=%(refname)", "refs/remotes"],
        text=True,
        capture_output=True,
        check=True,
    ).stdout
    return any(line.rstrip("/").endswith(f"/{branch}") for line in refs.splitlines())


def ensure_worktrees(main_branch: str, need_starter: bool) -> None:
    existing = run(["git", "worktree", "list", "--porcelain"], capture=True).stdout
    existing_paths: set[Path] = {
        Path(line.split(" ", 1)[1]).resolve()
        for line in existing.splitlines()
        if line.startswith("worktree ")
    }

    def ensure(rel_path: str, args: list[str]) -> None:
        target = Path.cwd() / rel_path
        if target.resolve() in existing_paths:
            print(f"Worktree {rel_path} exists")
            return
        print(f"Adding worktree {rel_path}")
        run(["git", "worktree", "add", "-q", rel_path, *args])

    # worktrees/main is seeded with `heads/<branch>` rather than `<branch>`:
    # git then resolves it as a plain commit-ish and checks it out detached,
    # instead of recognizing it as "the `main_branch` branch" - which would
    # conflict with wherever the primary worktree already has that branch
    # checked out. It's re-detached to the latest tip every run (see main()),
    # so it never actually needs to own the branch.
    #
    # worktrees/student and worktrees/starter don't have that problem (their
    # branches aren't checked out anywhere else yet), so they're just seeded
    # detached with --detach as a placeholder; their real branch is checked
    # out right after, in prepare_student_worktree() / starter_flow().
    ensure(STUDENT_WT, ["--detach"])
    ensure(MAIN_WT, [f"heads/{main_branch}"])
    if need_starter:
        ensure(STARTER_WT, ["--detach"])


def tracked_files_matching(cwd: Path, patterns: str) -> set[str]:
    """Which tracked files in `cwd` match a gitignore-style pattern list.

    Delegates to git itself (`ls-files --ignored --exclude-from`) instead of
    hand-rolling gitignore-pattern matching - the same mechanism used for
    both the `.sctignore`-derived skip list and `binary-patterns`, so both
    kinds of file selection are configured and resolved identically.
    """
    with tempfile.NamedTemporaryFile("w", suffix=".patterns", delete=False) as tmp:
        tmp.write(patterns)
        patterns_file = tmp.name
    try:
        raw = run_bytes(
            ["git", "ls-files", "--cached", "--ignored", "--exclude-from", patterns_file, "-z"],
            cwd=cwd,
        ).stdout
    finally:
        os.unlink(patterns_file)
    return {os.fsdecode(p) for p in raw.split(b"\0") if p}


def git_rm_all(cwd: Path) -> None:
    res = subprocess.run(["git", "rm", "-rf", "--", "."], cwd=cwd, text=True, capture_output=True)
    # A fresh orphan branch has nothing tracked yet, so "nothing to remove"
    # is an expected outcome here, not an error.
    if res.returncode != 0 and "did not match any files" not in res.stderr:
        die(f"git rm failed in {cwd}:\n{res.stderr.strip()}")


# ---------------------------------------------------------------------------
# Workflow steps
# ---------------------------------------------------------------------------


def prepare_student_worktree(path: Path, student_branch: str) -> None:
    """Reset worktrees/student to a clean slate on `student_branch`.

    Clean + rm (rather than just clean) also drops files that are still
    tracked on `student_branch` from a previous run but no longer produced
    by the current main branch - otherwise they'd linger in the student
    template.
    """
    with pushd(path):
        # Refresh remote-tracking refs before deciding orphan-vs-checkout:
        # without this, a `student_branch` that already exists on a remote
        # but hasn't been fetched into this clone yet would look absent to
        # branch_exists() below, and get wrongly recreated as a fresh orphan
        # branch instead of continuing its real history. Best-effort: if
        # there's no network (or no remote at all), fall back to whatever
        # remote-tracking refs are already known locally.
        fetch = subprocess.run(["git", "fetch", "--all", "--prune"], capture_output=True, text=True)
        if fetch.returncode != 0:
            warn(f"git fetch failed, proceeding with possibly-stale remote refs:\n{fetch.stderr.strip()}")

        if branch_exists(student_branch):
            run(["git", "checkout", "-q", student_branch])
        else:
            run(["git", "checkout", "-q", "--orphan", student_branch])
        run(["git", "clean", "-dxf"])
        git_rm_all(Path.cwd())


def build_student_template(
    main_dir: Path,
    dest_dir: Path,
    phase: str | None,
    ignore_file: str,
    binary_patterns: list[str],
    sct_bin: str,
) -> None:
    """Write the sct-transformed (or copied) student version of every tracked file.

    `main_dir` must already be a clean checkout (so "tracked files" ==
    "files that should exist"); see the `git clean`/`checkout` calls in
    main() right before this is called.
    """
    sct_cmd = [sct_bin, "-so"] + ([f"--context={phase}"] if phase else [])

    ignore_src = main_dir / ignore_file
    if not ignore_src.is_file():
        die(f"ignore file not found: {ignore_src}")

    # The ignore file itself can contain sct phase-markup (e.g. a file is
    # only excluded in a later phase), so it has to be run through sct with
    # the current phase/context before it's usable as a plain gitignore-style
    # pattern list.
    ignore_content = run(sct_cmd + [ignore_file], cwd=main_dir, capture=True).stdout

    tracked_raw = run_bytes(["git", "ls-files", "--cached", "-z"], cwd=main_dir).stdout
    tracked = [os.fsdecode(p) for p in tracked_raw.split(b"\0") if p]

    ignored = tracked_files_matching(main_dir, ignore_content)
    binary = tracked_files_matching(main_dir, "\n".join(binary_patterns))

    to_transform, to_copy = partition_tracked_files(tracked, ignored, binary)

    for rel in to_transform:
        dest = dest_dir / rel
        dest.parent.mkdir(parents=True, exist_ok=True)
        dest.write_bytes(run_bytes(sct_cmd + [rel], cwd=main_dir).stdout)

    for rel in to_copy:
        dest = dest_dir / rel
        dest.parent.mkdir(parents=True, exist_ok=True)
        shutil.copy2(main_dir / rel, dest)


def prompt_choice(question: str, choices: list[str]) -> str:
    print(question)
    for i, choice in enumerate(choices, 1):
        print(f"{i}) {choice}")
    while True:
        reply = input("#? ").strip()
        if reply.isdigit() and 1 <= int(reply) <= len(choices):
            return choices[int(reply) - 1]
        print("Invalid choice.")


def commit_student_changes(path: Path, auto_yes: bool) -> None:
    with pushd(path):
        run(["git", "add", "-A"])
        if subprocess.run(["git", "diff", "--staged", "--quiet"]).returncode == 0:
            print("No changes to commit.")
            return
        subprocess.run(["git", "diff", "--staged"])

        # `worktrees/main/HEAD` is a git pseudo-ref: "the HEAD of the linked
        # worktree whose directory is named main", resolvable from any
        # worktree sharing the same .git. Not a filesystem path lookup.
        reference = run(
            ["git", "log", "-n", "1", "--pretty=reference", f"{MAIN_WT}/HEAD"], capture=True
        ).stdout.strip()
        message = f"sct transformed from {reference}"

        choice = "Yes" if auto_yes else prompt_choice("Do you wish to commit the changes (student)?", ["Yes", "Amend", "No"])

        if choice == "Yes":
            run(["git", "commit", "-m", message])
        elif choice == "Amend":
            run(["git", "commit", "-m", message, "--amend"])
        else:
            print("Not committed.")


def starter_flow(starter_remote: str, remote_name: str) -> None:
    """Publish the student branch to a separate starter repo, always interactively.

    Unlike the student commit step, this is never auto-confirmed by --yes:
    it mutates a remote (and can force-push), so it always asks - a
    force-push additionally requires typing a confirmation phrase, since it
    can overwrite the remote `starter_student` branch.

    `remote_name` is purely an internal label this script invents and fully
    controls (via `git remote add`) - unlike `main-branch`/`student-branch`,
    it doesn't need to match anything pre-existing. It's still configurable
    (`starter-remote-name` in `.sct.toml`) in case a repo already has an
    unrelated remote called `starter`.
    """
    with pushd(STARTER_WT):
        choice = prompt_choice("Do you wish to push to starter?", ["Yes", "No"])
        if choice == "No":
            print("Skipping starter push.")
            return

        remotes = run(["git", "remote"], capture=True).stdout.split()
        if remote_name in remotes:
            # A remote with this name existing is not enough: if it's an
            # unrelated remote that just happens to share the name, reusing
            # it below would reset/checkout/pull against the wrong repo and
            # could eventually push to it. Check the URL matches before
            # doing anything destructive.
            existing_url = run(["git", "remote", "get-url", remote_name], capture=True).stdout.strip()
            if existing_url != starter_remote:
                die(
                    f"remote '{remote_name}' already exists in {Path.cwd()} but points to "
                    f"'{existing_url}', not the configured starter-remote "
                    f"'{starter_remote}'. This looks like an unrelated remote - rename it, "
                    "or set a different starter-remote-name in .sct.toml, before re-running."
                )
            print(f"Remote {remote_name} exists")
            run(["git", "reset", "-q", "--hard", "HEAD"])
            run(["git", "clean", "-dxf"])
            run(["git", "checkout", "-q", "starter_student"])
            run(["git", "pull"])
            git_rm_all(Path.cwd())
            run(["git", "checkout", "student", "--", "."])

            subprocess.run(["git", "status"])
            subprocess.run(["git", "diff", "--staged"])

            choice = prompt_choice("Do you wish to commit the changes (starter)?", ["Yes", "Amend", "No"])
            if choice == "Yes":
                run(["git", "commit"])
                run(["git", "push", remote_name, "starter_student:main"])
            elif choice == "Amend":
                run(["git", "commit", "--amend"])
                confirm = input(
                    f"This will force-push and overwrite the '{remote_name}' remote's branch. "
                    "Type 'force push' to confirm: "
                )
                if confirm.strip() == "force push":
                    run(["git", "push", remote_name, "starter_student:main", "--force"])
                else:
                    print("Not pushed.")
            else:
                print("Not committed.")
        else:
            print(f"Adding remote {remote_name}")
            run(["git", "remote", "add", remote_name, starter_remote])
            run(["git", "fetch", remote_name])

            print("Creating base branch with starter commit")
            run(["git", "checkout", "-q", "-b", "starter_student", f"{remote_name}/main"])
            run(["git", "merge", "--squash", "--allow-unrelated-histories", "student"])
            run(["git", "commit", "-m", "Starter content"])
            run(["git", "push", "--set-upstream", remote_name, "starter_student:main"])


# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------


def main() -> None:
    parser = argparse.ArgumentParser(description="Build the student template from the configured main branch.")
    parser.add_argument("--yes", "-y", action="store_true", help="auto-confirm the student commit (no prompt)")
    parser.add_argument("--no-starter", action="store_true", help="skip the starter-repo push step")
    parser.add_argument("--dry-run", action="store_true", help="build and show the diff, but commit/push nothing")
    parser.add_argument(
        "--phase",
        help="override the configured current-phase (--context passed to sct); "
        f"the {ENV_PHASE} environment variable works the same way, e.g. via direnv",
    )
    parser.add_argument(
        "--starter-remote",
        help="override the configured starter-remote URL (useful when it changes "
        f"per year/class and shouldn't be baked into a committed .sct.toml; the "
        f"{ENV_STARTER_REMOTE} environment variable works the same way, e.g. via direnv)",
    )
    args = parser.parse_args()

    require_tools("git", "sct")

    root = find_repo_root()
    os.chdir(root)
    config = load_config(root)
    phase: str | None = args.phase or os.environ.get(ENV_PHASE) or config.phase
    starter_remote: str | None = args.starter_remote or os.environ.get(ENV_STARTER_REMOTE) or config.starter_remote
    need_starter = bool(starter_remote) and not args.no_starter

    step("Ensuring worktrees exist")
    ensure_worktrees(config.main_branch, need_starter)

    step("Resetting student worktree")
    prepare_student_worktree(Path(STUDENT_WT), config.student_branch)

    step("Building student template from main branch")
    with pushd(MAIN_WT):
        # Re-detach to the branch's latest tip (see the comment in
        # ensure_worktrees() on why `heads/<branch>` rather than `<branch>`).
        run(["git", "checkout", "-q", f"heads/{config.main_branch}"])
        run(["git", "clean", "-dxf"])
        build_student_template(Path.cwd(), (root / STUDENT_WT), phase, config.ignore_file, config.binary_patterns, "sct")

    if args.dry_run:
        step("Showing uncommitted changes (dry run)")
        with pushd(STUDENT_WT):
            run(["git", "add", "-A"])
            subprocess.run(["git", "diff", "--staged"])
        print("Dry run: nothing committed or pushed.")
        return

    step("Committing student changes")
    commit_student_changes(Path(STUDENT_WT), args.yes)

    if need_starter and starter_remote is not None:
        step("Publishing to starter")
        starter_flow(starter_remote, config.starter_remote_name)


if __name__ == "__main__":
    try:
        main()
    except KeyboardInterrupt:
        print()
        die("interrupted", code=130)
