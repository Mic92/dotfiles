#!/usr/bin/env bash
# Wait for a Buildkite build to finish, polling every 30s.
# Usage: bk-wait <build-url>
# Example: bk-wait https://buildkite.com/anthropic/monorepo-tests/builds/1494425

if [[ $# -ne 1 ]]; then
  echo "Usage: bk-wait <buildkite-build-url>" >&2
  exit 1
fi

url="$1"

# Extract org, pipeline, build number from URL
if [[ $url =~ buildkite\.com/([^/]+)/([^/]+)/builds/([0-9]+) ]]; then
  org="${BASH_REMATCH[1]}"
  pipeline="${BASH_REMATCH[2]}"
  build="${BASH_REMATCH[3]}"
else
  echo "Error: Could not parse Buildkite URL: $url" >&2
  echo "Expected format: https://buildkite.com/ORG/PIPELINE/builds/NUMBER" >&2
  exit 1
fi

if [[ -z ${BUILDKITE_TOKEN:-} ]]; then
  echo "Error: BUILDKITE_TOKEN not set" >&2
  exit 1
fi

api_url="https://api.buildkite.com/v2/organizations/${org}/pipelines/${pipeline}/builds/${build}"

echo "Waiting for build #${build} (${org}/${pipeline})..."

while true; do
  state=$(curl -sf -H "Authorization: Bearer $BUILDKITE_TOKEN" "$api_url" |
    python3 -c "import sys,json; print(json.load(sys.stdin)['state'])")
  echo "$(date '+%H:%M:%S') state: $state"
  case "$state" in
  passed)
    echo "✅ Build passed"
    exit 0
    ;;
  failed | canceled | not_run | skipped)
    echo "❌ Build finished: $state"
    exit 1
    ;;
  esac
  sleep 30
done
