# Release Command

1. Analyze commits since the last release tag using:
   - `git log --oneline $(git describe --tags --abbrev=0)..HEAD`
   - `git diff --stat $(git describe --tags --abbrev=0)..HEAD`

2. Determine the appropriate version bump following semantic versioning:
   - MAJOR (x.0.0): Breaking changes or major feature overhauls
   - MINOR (0.x.0): New features, significant improvements, or minor breaking changes to internal APIs
   - PATCH (0.0.x): Bug fixes, documentation updates, small improvements

3. Create a comprehensive release notes file that includes:
   - A clear summary of what changed and why it matters
   - Categorized changes (Features, Bug Fixes, Documentation, etc.)
   - Any breaking changes or migration notes
   - Credits to contributors if applicable
   - Save the release notes to a temporary file

4. Execute the release script with the determined version and release notes:
   - For draft release: `./scripts/create-release.sh <version> --notes-file <temp-file>`
   - For immediate publish: `./scripts/create-release.sh <version> --publish --notes-file <temp-file>`

5. Clean up the temporary release notes file after the release is created

6. The script will:
   - Use the provided release notes file
   - Create a GitHub release (draft by default)
   - Trigger automation workflow when published

7. Open the created release in the browser for review:
   - For draft releases: `gh release view <version> --web`
   - This allows you to review and edit the release before publishing
