# Release Command

1. Analyze commits since the last release tag using:
   - `git log --oneline $(git describe --tags --abbrev=0)..HEAD`
   - `git diff --stat $(git describe --tags --abbrev=0)..HEAD`

2. Determine the appropriate version bump following semantic versioning:
   - MAJOR (x.0.0): Breaking changes or major feature overhauls
   - MINOR (0.x.0): New features, significant improvements, or minor breaking changes to internal APIs
   - PATCH (0.0.x): Bug fixes, documentation updates, small improvements

3. Execute the release script with the determined version:
   - For draft release: `./scripts/create-release.sh <version>`
   - For immediate publish: `./scripts/create-release.sh <version> --publish`

4. The script will:
   - Generate categorized release notes from commit messages
   - Create a GitHub release (draft by default)
   - Trigger automation workflow when published