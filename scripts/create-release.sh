#!/bin/bash

# Create a release draft using gh CLI
# Usage: ./scripts/create-release.sh <version> [--publish]

set -e

if [ -z "$1" ]; then
    echo "Usage: $0 <version> [--publish]"
    echo "Examples:"
    echo "  $0 0.2.0           # Create draft release"
    echo "  $0 0.2.0 --publish # Create and publish release"
    exit 1
fi

VERSION=$1
TAG_NAME="v$VERSION"
PUBLISH_FLAG=""

# Check if --publish flag is provided
if [ "$2" = "--publish" ]; then
    PUBLISH_FLAG=""
else
    PUBLISH_FLAG="--draft"
fi

# Check if gh CLI is installed
if ! command -v gh &> /dev/null; then
    echo "Error: GitHub CLI (gh) is not installed."
    echo "Install it from: https://cli.github.com/"
    exit 1
fi

# Check if we're in a git repository
if ! git rev-parse --git-dir > /dev/null 2>&1; then
    echo "Error: Not in a git repository"
    exit 1
fi

# Fetch tags to ensure we have the latest
echo "Fetching tags from remote..."
git fetch --tags

# Get the previous tag for release notes generation
PREV_TAG=$(git describe --tags --abbrev=0 2>/dev/null || echo "")

echo "Creating release $TAG_NAME..."

# Create the release
if [ -n "$PREV_TAG" ]; then
    echo "Generating notes from $PREV_TAG to HEAD..."
    gh release create "$TAG_NAME" \
        $PUBLISH_FLAG \
        --generate-notes \
        --title "Release $VERSION" \
        --notes-start-tag "$PREV_TAG"
else
    echo "No previous tag found, generating notes for all commits..."
    gh release create "$TAG_NAME" \
        $PUBLISH_FLAG \
        --generate-notes \
        --title "Release $VERSION"
fi

if [ -z "$PUBLISH_FLAG" ]; then
    echo ""
    echo "✅ Release published: $TAG_NAME"
    echo ""
    echo "The automated workflow will now:"
    echo "  1. Update version in claude-code-emacs.el"
    echo "  2. Update version in mcp-server/package.json"
    echo "  3. Publish to npm"
else
    echo ""
    echo "✅ Draft release created: $TAG_NAME"
    echo ""
    echo "Next steps:"
    echo "  1. Review the release notes at: https://github.com/$(gh repo view --json nameWithOwner -q .nameWithOwner)/releases"
    echo "  2. Edit if needed"
    echo "  3. Click 'Publish release' to trigger automation"
fi
