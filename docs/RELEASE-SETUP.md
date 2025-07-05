# Release Setup Guide

This guide explains how to set up the automated release process.

## Prerequisites

1. **GitHub CLI**: Install from [cli.github.com](https://cli.github.com/)
2. **NPM Account**: Create an account at [npmjs.com](https://www.npmjs.com/)
3. **NPM Token**: Generate an automation token for publishing

## Setting up NPM Token

1. Log in to npmjs.com
2. Go to Access Tokens: https://www.npmjs.com/settings/[username]/tokens
3. Click "Generate New Token"
4. Select "Automation" type
5. Copy the generated token

## GitHub Repository Setup

### Required Permissions

GitHub Actions needs the following permissions:
- `contents: write` - To create releases and tags
- `contents: read` - To read repository content for npm publish
- `id-token: write` - For npm provenance (proves package was published from this repo)

These are already configured in the workflow files.

### NPM Token Setup

1. Go to your repository settings
2. Navigate to Secrets and variables → Actions
3. Click "New repository secret"
4. Add the following secret:
   - Name: `NPM_TOKEN`
   - Value: Your npm automation token

## Using GitHub CLI for Releases

The `gh` CLI provides powerful release management features:

```bash
# Authenticate with GitHub (first time only)
gh auth login

# Create a draft release with auto-generated notes
./scripts/create-release.sh 0.2.0

# Create and publish immediately
./scripts/create-release.sh 0.2.0 --publish

# View existing releases
gh release list

# Edit a release
gh release edit v0.2.0 --notes "Updated release notes"

# Delete a release (be careful!)
gh release delete v0.2.0
```

## Testing the Release Process

1. Create a test release draft:
   ```bash
   ./scripts/create-release.sh 0.1.1
   ```

2. Review the draft release:
   ```bash
   gh release view v0.1.1 --web
   ```

3. When ready for production:
   - Edit the release notes if needed
   - Click "Publish release" on GitHub
   - Monitor the Actions tab for the automated process

## Troubleshooting

### NPM Publish Fails

- Ensure the package name is unique
- Check that NPM_TOKEN is correctly set
- Verify npm account has publish permissions
- **Version already published**: If you see "You cannot publish over the previously published versions", ensure:
  - The version bump commit is pushed before creating the release
  - The tag points to the correct commit with updated version
  - Use `./scripts/update-version.sh X.Y.Z --auto-commit` followed by `git push && git push --tags`

### Version Update Fails

- Ensure the main branch is not protected
- Check GitHub Actions has write permissions

### Release Creation Fails with "tag not found"

- This happens when using `--verify-tag` and the tag doesn't exist remotely
- Solution: Always push tags before creating release: `git push --tags`
- The new workflow ensures this by requiring tag push before release creation

### Manual Recovery

If automation fails, you can manually:

```bash
# Update versions locally
./scripts/update-version.sh 0.2.0

# Commit and push
git add -A
git commit -m "chore: bump version to 0.2.0"
git push

# Create tag manually
git tag v0.2.0
git push --tags

# Or use gh to create release
gh release create v0.2.0 --generate-notes

# Publish to npm manually
cd mcp-server
npm publish
```