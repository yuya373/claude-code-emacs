#!/bin/bash

# Update version in multiple files
# Usage: ./scripts/update-version.sh 0.2.0

set -e

if [ -z "$1" ]; then
    echo "Usage: $0 <version>"
    echo "Example: $0 0.2.0"
    exit 1
fi

VERSION=$1
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"

echo "Updating version to $VERSION..."

# Update claude-code-emacs.el
echo "Updating claude-code-emacs.el..."
sed -i.bak "s/^;; Version: .*/;; Version: $VERSION/" "$PROJECT_ROOT/claude-code-emacs.el"
rm -f "$PROJECT_ROOT/claude-code-emacs.el.bak"

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
echo ""
echo "Next steps:"
echo "1. Review the changes: git diff"
echo "2. Commit the changes: git add -A && git commit -m 'chore: bump version to $VERSION'"
echo "3. Push to GitHub: git push"
echo "4. Create a release draft using GitHub Actions"