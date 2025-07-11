"""Paperless-ngx API client."""

import json
import logging
import mimetypes
import urllib.error
import urllib.parse
import urllib.request
import uuid
from pathlib import Path
from typing import Any, cast

logger = logging.getLogger(__name__)


class PaperlessAPIError(Exception):
    """Exception raised for Paperless API errors."""


class PaperlessClient:
    """Client for interacting with Paperless-ngx API."""

    def __init__(self, url: str, token: str) -> None:
        self.url = url.rstrip("/")
        self.token = token

        # Validate base URL
        parsed = urllib.parse.urlparse(self.url)
        assert parsed.scheme in ("http", "https"), f"Invalid URL scheme: {parsed.scheme}"
        assert parsed.netloc, "URL must have a valid netloc"

    def _request(
        self,
        method: str,
        endpoint: str,
        data: dict[str, Any] | None = None,
        params: dict[str, Any] | None = None,
    ) -> dict[str, Any]:
        """Make an API request."""
        url = urllib.parse.urljoin(self.url, endpoint)

        # Validate URL scheme for security
        parsed_url = urllib.parse.urlparse(url)
        assert parsed_url.scheme in ("http", "https"), f"Invalid URL scheme: {parsed_url.scheme}"
        assert parsed_url.netloc, "URL must have a valid netloc"

        if params:
            query_string = urllib.parse.urlencode(params)
            url = f"{url}?{query_string}"

        headers = {
            "Authorization": f"Token {self.token}",
            "Content-Type": "application/json",
        }

        body = None
        if data is not None:
            body = json.dumps(data).encode("utf-8")

        # Log HTTP request
        logger.debug(f"HTTP Request: {method} {url}")
        logger.debug(f"Headers: {headers}")
        if data:
            logger.debug(f"Body: {json.dumps(data, indent=2)}")

        request = urllib.request.Request(url, data=body, headers=headers, method=method)  # noqa: S310

        try:
            with urllib.request.urlopen(request) as response:  # noqa: S310
                response_body = None
                if response.status == 204:  # No content
                    logger.debug(f"HTTP Response: {response.status} No Content")
                    return {}

                response_text = response.read().decode("utf-8")
                response_body = json.loads(response_text)
                logger.debug(f"HTTP Response: {response.status}")
                logger.debug(f"Response body: {json.dumps(response_body, indent=2)}")
                return cast("dict[str, Any]", response_body)
        except urllib.error.HTTPError as e:
            error_body = e.read().decode("utf-8")
            logger.debug(f"HTTP Error: {e.code}")
            logger.debug(f"Error body: {error_body}")
            error_msg = f"HTTP {e.code}: {error_body}"
            raise PaperlessAPIError(error_msg) from e

    def get_mail_accounts(self) -> list[dict[str, Any]]:
        """Get all mail accounts."""
        response = self._request("GET", "/api/mail_accounts/")
        return cast("list[dict[str, Any]]", response["results"])

    def get_mail_rules(self) -> list[dict[str, Any]]:
        """Get all mail rules."""
        response = self._request("GET", "/api/mail_rules/")
        return cast("list[dict[str, Any]]", response["results"])

    def get_mail_rule(self, rule_id: int) -> dict[str, Any]:
        """Get a specific mail rule."""
        return self._request("GET", f"/api/mail_rules/{rule_id}/")

    def create_mail_rule(self, data: dict[str, Any]) -> dict[str, Any]:
        """Create a new mail rule."""
        return self._request("POST", "/api/mail_rules/", data=data)

    def update_mail_rule(self, rule_id: int, data: dict[str, Any]) -> dict[str, Any]:
        """Update an existing mail rule."""
        return self._request("PUT", f"/api/mail_rules/{rule_id}/", data=data)

    def delete_mail_rule(self, rule_id: int) -> None:
        """Delete a mail rule."""
        self._request("DELETE", f"/api/mail_rules/{rule_id}/")

    def get_tags(self) -> list[dict[str, Any]]:
        """Get all tags."""
        response = self._request("GET", "/api/tags/")
        return cast("list[dict[str, Any]]", response["results"])

    def create_tag(self, data: dict[str, Any]) -> dict[str, Any]:
        """Create a new tag."""
        return self._request("POST", "/api/tags/", data=data)

    def delete_tag(self, tag_id: int) -> None:
        """Delete a tag."""
        self._request("DELETE", f"/api/tags/{tag_id}/")

    def get_correspondents(self) -> list[dict[str, Any]]:
        """Get all correspondents."""
        response = self._request("GET", "/api/correspondents/")
        return cast("list[dict[str, Any]]", response["results"])

    def get_document_types(self) -> list[dict[str, Any]]:
        """Get all document types."""
        response = self._request("GET", "/api/document_types/")
        return cast("list[dict[str, Any]]", response["results"])

    def search_documents(
        self, query: str | None = None, page: int = 1, page_size: int = 25
    ) -> dict[str, Any]:
        """Search documents."""
        params: dict[str, Any] = {"page": page, "page_size": page_size}
        if query:
            params["query"] = query
        return self._request("GET", "/api/documents/", params=params)

    def get_document(self, document_id: int) -> dict[str, Any]:
        """Get a specific document."""
        return self._request("GET", f"/api/documents/{document_id}/")

    def get_document_metadata(self, document_id: int) -> dict[str, Any]:
        """Get document metadata."""
        return self._request("GET", f"/api/documents/{document_id}/metadata/")

    def download_document(self, document_id: int, original: bool = False) -> bytes:
        """Download a document."""
        endpoint = f"/api/documents/{document_id}/download/"
        params = {"original": "true"} if original else None
        url = urllib.parse.urljoin(self.url, endpoint)
        if params:
            url = f"{url}?{urllib.parse.urlencode(params)}"

        headers = {"Authorization": f"Token {self.token}"}
        request = urllib.request.Request(url, headers=headers)  # noqa: S310

        with urllib.request.urlopen(request) as response:  # noqa: S310
            return cast("bytes", response.read())

    def upload_document(
        self, file_path: str, title: str | None = None, tags: list[int] | None = None
    ) -> dict[str, Any]:
        """Upload a document."""
        # Read file
        with Path(file_path).open("rb") as f:
            file_data = f.read()

        # Detect MIME type
        mime_type, _ = mimetypes.guess_type(file_path)
        if not mime_type:
            mime_type = "application/octet-stream"

        # Create multipart form data
        boundary = f"----WebKitFormBoundary{uuid.uuid4().hex[:16]}"
        body_parts = []

        # Add file field
        body_parts.append(f"------{boundary}")
        body_parts.append(
            f'Content-Disposition: form-data; name="document"; filename="{Path(file_path).name}"'
        )
        body_parts.append(f"Content-Type: {mime_type}")
        body_parts.append("")

        # Join text parts and add file data
        text_part = "\r\n".join(body_parts) + "\r\n"
        body = text_part.encode() + file_data + b"\r\n"

        # Add title if provided
        if title:
            body += f"------{boundary}\r\n".encode()
            body += b'Content-Disposition: form-data; name="title"\r\n\r\n'
            body += title.encode() + b"\r\n"

        # Add tags if provided
        if tags:
            for tag_id in tags:
                body += f"------{boundary}\r\n".encode()
                body += b'Content-Disposition: form-data; name="tags"\r\n\r\n'
                body += str(tag_id).encode() + b"\r\n"

        body += f"------{boundary}--\r\n".encode()

        headers = {
            "Authorization": f"Token {self.token}",
            "Content-Type": f"multipart/form-data; boundary=----{boundary}",
        }

        url = urllib.parse.urljoin(self.url, "/api/documents/post_document/")
        request = urllib.request.Request(url, data=body, headers=headers, method="POST")  # noqa: S310

        try:
            with urllib.request.urlopen(request) as response:  # noqa: S310
                response_text = response.read().decode("utf-8")
                if response_text:
                    # The API returns just the task_id as a string
                    task_id = response_text.strip().strip('"')
                    return {"task_id": task_id}
                return {"status": "success"}
        except urllib.error.HTTPError as e:
            error_body = e.read().decode("utf-8")
            logger.debug(f"HTTP Error: {e.code}")
            logger.debug(f"Error body: {error_body}")
            error_msg = f"HTTP {e.code}: {error_body}"
            raise PaperlessAPIError(error_msg) from e

    def delete_document(self, document_id: int) -> None:
        """Delete a document."""
        self._request("DELETE", f"/api/documents/{document_id}/")

    def get_task_status(self, task_id: str) -> dict[str, Any] | None:
        """Get task status by task_id."""
        response = self._request("GET", "/api/tasks/", params={"task_id": task_id})
        # The tasks endpoint returns an array, not a paginated response
        tasks = cast("list[dict[str, Any]]", response)
        return tasks[0] if tasks else None
