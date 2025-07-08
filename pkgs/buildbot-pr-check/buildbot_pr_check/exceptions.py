#!/usr/bin/env python3
"""Custom exceptions for buildbot-pr-check."""


class BuildbotCheckError(Exception):
    """Base exception for buildbot-pr-check errors"""

    pass


class InvalidPRURLError(BuildbotCheckError):
    """Raised when PR URL is invalid or unsupported"""

    pass


class APIError(BuildbotCheckError):
    """Raised when API calls fail"""

    pass


class BuildbotAPIError(APIError):
    """Raised when Buildbot API calls fail"""

    pass


class GitHubAPIError(APIError):
    """Raised when GitHub API calls fail"""

    pass


class GiteaAPIError(APIError):
    """Raised when Gitea API calls fail"""

    pass
