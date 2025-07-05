# Release Command

1. Fetch latest tags from remote and analyze commits since the last release:
   - `git fetch --tags`
   - `git log --oneline $(git describe --tags --abbrev=0)..HEAD`
   - `git diff --stat $(git describe --tags --abbrev=0)..HEAD`

2. Determine the appropriate version bump following semantic versioning:
   - MAJOR (x.0.0): Breaking changes or major feature overhauls
   - MINOR (0.x.0): New features, significant improvements, or minor breaking changes to internal APIs
   - PATCH (0.0.x): Bug fixes, documentation updates, small improvements

3. Update version numbers in the project files:
   - Run: `./scripts/update-version.sh X.Y.Z --auto-commit`
   - This will:
     - Update `claude-code-emacs.el` header: `;; Version: X.Y.Z`
     - Update `mcp-server/package.json`: `"version": "X.Y.Z"`
     - Update `mcp-server/package-lock.json`: Updated by npm
     - Automatically commit changes with message: `chore: bump version to X.Y.Z`
     - Create git tag: `vX.Y.Z`
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

6. Clean up the temporary release notes file after the release is created

7. The script will:
   - Verify that the tag exists in the remote repository (--verify-tag)
   - Use the provided release notes file
   - Create a GitHub release (draft by default)
   - Trigger automation workflow when published
   - If the tag doesn't exist remotely, the script will fail with an error

8. Open the created release in the browser for review:
   - For draft releases: `gh release view <version> --web`
   - This allows you to review and edit the release before publishing
