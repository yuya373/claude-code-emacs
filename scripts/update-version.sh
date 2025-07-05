#!/bin/bash

# Update version in multiple files
# Usage: ./scripts/update-version.sh 0.2.0

set -e

if [ -z "$1" ]; then
    echo "Usage: $0 <version> [--auto-commit]"
    echo "Example: $0 0.2.0"
    echo "         $0 0.2.0 --auto-commit"
    exit 1
fi

VERSION=$1
AUTO_COMMIT=false
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"

# Check for --auto-commit flag
if [[ "$2" == "--auto-commit" ]]; then
    AUTO_COMMIT=true
fi

echo "Updating version to $VERSION..."

# Update claude-code-emacs.el
echo "Updating claude-code-emacs.el..."
# Use sed -i with OS detection for compatibility
if [[ "$OSTYPE" == "darwin"* ]]; then
    sed -i '' "s/^;; Version: .*/;; Version: $VERSION/" "$PROJECT_ROOT/claude-code-emacs.el"
else
    sed -i "s/^;; Version: .*/;; Version: $VERSION/" "$PROJECT_ROOT/claude-code-emacs.el"
fi

# Update mcp-server/package.json
echo "Updating mcp-server/package.json..."
cd "$PROJECT_ROOT/mcp-server"
npm version "$VERSION" --no-git-tag-version

echo "Version updated to $VERSION"
echo ""
echo "Files updated:"
echo "  - claude-code-emacs.el"
echo "  - mcp-server/package.json"
echo "  - mcp-server/package-lock.json"

if [ "$AUTO_COMMIT" = true ]; then
    echo ""
    echo "Creating commit and tag..."
    cd "$PROJECT_ROOT"
    git add -A
    git commit -m "chore: bump version to $VERSION"
    git tag "v$VERSION"
    echo ""
    echo "✅ Committed and tagged as v$VERSION"
    echo ""
    echo "Next steps:"
    echo "1. Push changes: git push && git push --tags"
    echo "2. Create release: ./scripts/create-release.sh $VERSION"
else
    echo ""
    echo "Next steps:"
    echo "1. Review the changes: git diff"
    echo "2. Commit and tag: git add -A && git commit -m 'chore: bump version to $VERSION' && git tag v$VERSION"
    echo "3. Push with tags: git push && git push --tags"
    echo "4. Create release: ./scripts/create-release.sh $VERSION"
fi
