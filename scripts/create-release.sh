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

# Generate release notes from commits
if [ -n "$PREV_TAG" ]; then
    echo "Generating notes from $PREV_TAG to HEAD..."
    
    # Create release notes
    RELEASE_NOTES="## What's Changed\n\n"
    
    # Get features
    FEATURES=$(git log --pretty=format:"- %s" "$PREV_TAG"..HEAD | grep "^- feat:" || true)
    if [ -n "$FEATURES" ]; then
        RELEASE_NOTES+="### ✨ Features\n$FEATURES\n\n"
    fi
    
    # Get fixes
    FIXES=$(git log --pretty=format:"- %s" "$PREV_TAG"..HEAD | grep "^- fix:" || true)
    if [ -n "$FIXES" ]; then
        RELEASE_NOTES+="### 🐛 Bug Fixes\n$FIXES\n\n"
    fi
    
    # Get docs
    DOCS=$(git log --pretty=format:"- %s" "$PREV_TAG"..HEAD | grep "^- docs:" || true)
    if [ -n "$DOCS" ]; then
        RELEASE_NOTES+="### 📚 Documentation\n$DOCS\n\n"
    fi
    
    # Get other changes (excluding version bumps)
    OTHERS=$(git log --pretty=format:"- %s" "$PREV_TAG"..HEAD | grep -v "^- feat:\|^- fix:\|^- docs:\|^- chore: bump version" || true)
    if [ -n "$OTHERS" ]; then
        RELEASE_NOTES+="### 🔧 Other Changes\n$OTHERS\n\n"
    fi
    
    RELEASE_NOTES+="**Full Changelog**: https://github.com/$(gh repo view --json nameWithOwner -q .nameWithOwner)/compare/$PREV_TAG...$TAG_NAME"
    
    # Create release with custom notes
    gh release create "$TAG_NAME" \
        $PUBLISH_FLAG \
        --title "Release $VERSION" \
        --notes "$(echo -e "$RELEASE_NOTES")"
else
    echo "No previous tag found, generating notes for all commits..."
    
    # Create simple release notes for first release
    RELEASE_NOTES="## Initial Release\n\n"
    RELEASE_NOTES+="$(git log --pretty=format:"- %s" | grep -v "^- chore: bump version" || true)\n"
    
    gh release create "$TAG_NAME" \
        $PUBLISH_FLAG \
        --title "Release $VERSION" \
        --notes "$(echo -e "$RELEASE_NOTES")"
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
