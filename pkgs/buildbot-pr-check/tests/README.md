# Tests for buildbot-pr-check

This directory contains integration tests using VCR.py to record and replay HTTP
interactions.

## Running Tests

```bash
# Run tests with recorded cassettes
pytest

# Record new cassettes (makes real HTTP requests)
python tests/record_cassettes.py
```

## Test Coverage

The tests cover two real-world pull requests:

1. **GitHub PR**: https://github.com/TUM-DSE/doctor-cluster-config/pull/459
   - Tests handling of canceled builds
   - Expects exit code 1

2. **Gitea PR**: https://git.clan.lol/clan/clan-core/pulls/4210
   - Tests successful builds
   - Expects exit code 0

## VCR Cassettes

The cassettes are stored in `tests/cassettes/` and contain recorded HTTP
responses from:

- GitHub API
- Gitea API
- Buildbot API

To update the cassettes, run `python tests/record_cassettes.py`.
