#!/usr/bin/env bash
set -euo pipefail

# Set working directory for git checkout
export GIT_WORK_TREE=/var/lib/gitea/builds/higgsboson.tk

# Set deployment directory (nginx serves from here)
export DEPLOY_DIR="/var/www/higgsboson.tk"

# Ensure work tree exists
mkdir -p "$GIT_WORK_TREE"

# Checkout the latest code
git checkout -f

# Enter directory
cd "$GIT_WORK_TREE"

# Build the website using nix
echo "Building website with Nix..."
BUILD_RESULT=$(nix build --no-link --print-out-paths)

# Deploy to nginx directory
echo "Deploying website to $DEPLOY_DIR..."
mkdir -p "$DEPLOY_DIR"
# Use rsync without preserving timestamps to ensure proper cache busting
rsync -rlpgoDv --delete "$BUILD_RESULT"/ "$DEPLOY_DIR/"

echo "Deployment complete!"
