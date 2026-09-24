# Release Command

1. Fetch latest tags from remote and analyze commits since the last release:
   - `git fetch --tags`
   - `git log --oneline $(git describe --tags --abbrev=0)..HEAD`
   - `git diff --stat $(git describe --tags --abbrev=0)..HEAD`

2. Determine the version bump following [Semantic Versioning 2.0.0](https://semver.org/). "Breaking" means a change to the public API: user-facing commands, customizable variables, key bindings, MCP tool names and parameters.
   - While the version is 0.x.y (initial development, SemVer §4):
     - MINOR (0.x.0): Breaking changes or new features
     - PATCH (0.x.y): Backward-compatible bug fixes, documentation updates
   - From 1.0.0 on:
     - MAJOR (x.0.0): Breaking changes
     - MINOR (x.y.0): Backward-compatible new features
     - PATCH (x.y.z): Backward-compatible bug fixes
   - Never go to 1.0.0 on your own; that is the maintainer's explicit declaration that the public API is stable (SemVer §5).

3. Update version numbers in the project files:
   - Run: `./scripts/update-version.sh X.Y.Z --auto-commit`
   - Push the commit and tag: `git push && git push --tags`
   - **IMPORTANT**: Always push before creating the release to ensure --verify-tag works

4. Create a comprehensive release notes file that includes:
   - A clear summary of what changed and why it matters
   - Categorized changes (Features, Bug Fixes, Documentation, etc.)
   - Any breaking changes or migration notes
   - Credits to contributors if applicable
   - Save the release notes to a temporary file

5. Execute the release script with the determined version and release notes:
   - For draft release: `./scripts/create-release.sh <version> --notes-file <temp-file>`
   - For immediate publish: `./scripts/create-release.sh <version> --publish --notes-file <temp-file>`

6. Open the created release in the browser for review:
   - For draft releases: `gh release view <version> --web`
   - This allows you to review and edit the release before publishing
