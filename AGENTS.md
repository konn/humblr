# Repository Guidelines

## Project Structure & Module Organization

This is a multi-package Haskell/Cabal repository. `humblr-core/` contains shared types and Markdown support; `humblr-frontend/` contains the Miso frontend and static files under `data/`; `humblr-workers/` implements Cloudflare Worker services and keeps templates, migrations, and Wrangler examples under `data/`. `tumblr-to-sqlite/` is the migration CLI and currently owns the only test suite. `shake-humblr/` orchestrates WASM and Worker builds. Shared build configuration lives in `cabal-common.project`; native, WASM, and Shake entry points are `cabal.project`, `cabal-wasm.project`, and `cabal-shake.project`.

Place library modules beneath `<package>/src/` using matching paths, for example `Humblr.Worker.Router` in `src/Humblr/Worker/Router.hs`. Executable entry points belong in `<package>/app/`.

## Build, Test, and Development Commands

- `cabal build <package>` builds one native package, such as `cabal build humblr-core`.
- `cabal test tumblr-to-sqlite-test` runs the current test suite.
- `cabal --project-file=cabal-shake.project run shake-humblr` produces frontend and Worker artifacts in `_build/`.
- `bash scripts/dev.sh` starts the generated Workers locally with Wrangler; build artifacts must already exist.
- `fourmolu --mode inplace path/to/File.hs` formats Haskell source.
- `cabal-gild --io path/to/package.cabal` formats Cabal files. The pre-commit hook applies this repository-wide.

Use Cabal nix-style builds; do not invoke `ghc` directly. If a `package.yaml` is introduced or changed, run `hpack` to regenerate its Cabal file.

## Coding Style & Naming Conventions

The project uses GHC2021, `-Wall`, and two-space indentation configured in `fourmolu.yaml`. Use `UpperCamelCase` for modules and types, `lowerCamelCase` for functions and values, and explicit module export lists. Prefer `(<>)` over `(++)`, including for lists and strings.

## Testing Guidelines

Add tests under the owning package’s `test/` directory and declare a named `test-suite` in its `.cabal` file. The existing test harness is only a placeholder, so new behavior should include focused regression tests. Before opening a PR, run the affected package tests and the relevant native or WASM build.

## Agent Tooling

For all Haskell tasks, use applicable skills and hooks from [konn/haskell-claude-marketplace](https://github.com/konn/haskell-claude-marketplace). Treat each `SKILL.md` as authoritative and run its formatter or validation hooks at the documented points. This requirement is agent-agnostic: use equivalent mechanisms in Claude, Codex, or another compatible coding agent.

## Commit & Pull Request Guidelines

History favors short, imperative subjects such as `Update lockfile`; keep commits focused and omit internal session metadata. PRs should explain the change, identify affected packages, link issues when applicable, and include screenshots for frontend changes. Ensure Fourmolu, `cabal check`, and the WASM build pass in GitHub Actions.
