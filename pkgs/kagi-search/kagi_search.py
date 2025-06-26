#!/usr/bin/env python3
"""
Kagi search script using session token authentication.
Scrapes search results from Kagi web interface.
"""

import json
import subprocess
import sys
import os
from pathlib import Path
from typing import List, Dict, Optional, Any
from urllib.request import Request, HTTPCookieProcessor, build_opener
from urllib.parse import urlencode
from urllib.error import URLError, HTTPError
from http.cookiejar import CookieJar
import gzip
from bs4 import BeautifulSoup
import time
from dataclasses import dataclass
import argparse
import logging

# Module-level logger
logger = logging.getLogger(__name__)


def colorize(text: str, color: str = "", bold: bool = False, dim: bool = False) -> str:
    """
    Colorize text if colors are enabled (TTY detected and NO_COLOR not set).

    Args:
        text: The text to colorize
        color: Color name (red, green, yellow, blue, magenta, cyan, white)
        bold: Whether to make text bold
        dim: Whether to make text dim/faint

    Returns:
        Colorized text or plain text based on environment
    """
    # Check if we should use colors
    if not sys.stdout.isatty() or os.environ.get("NO_COLOR"):
        return text

    # Color codes
    colors = {
        "red": "\033[91m",
        "green": "\033[92m",
        "yellow": "\033[93m",
        "blue": "\033[94m",
        "magenta": "\033[95m",
        "cyan": "\033[96m",
        "white": "\033[97m",
    }

    # Start with empty escape sequence
    escape = ""

    # Add color if specified
    if color and color.lower() in colors:
        escape = colors[color.lower()]

    # Add bold if requested
    if bold:
        escape = "\033[1m" + escape

    # Add dim if requested
    if dim:
        escape = "\033[2m" + escape

    # Return plain text if no formatting requested
    if not escape:
        return text

    # Return formatted text
    return f"{escape}{text}\033[0m"


def hyperlink(url: str, text: str = "") -> str:
    """
    Create a terminal hyperlink if supported (TTY detected and NO_COLOR not set).

    Args:
        url: The URL to link to
        text: The text to display (defaults to URL if not provided)

    Returns:
        Hyperlinked text or plain text based on environment
    """
    # Check if we should use hyperlinks (same conditions as colors)
    if not sys.stdout.isatty() or os.environ.get("NO_COLOR"):
        return text or url

    # Use the URL as text if no text provided
    display_text = text or url

    # OSC 8 hyperlink format: ESC]8;;URL ESC\TEXT ESC]8;; ESC\
    return f"\033]8;;{url}\033\\{display_text}\033]8;;\033\\"


@dataclass
class SearchResult:
    """Represents a single search result from Kagi."""

    title: str
    url: str
    snippet: str


@dataclass
class QuickAnswer:
    """Represents a Kagi Quick Answer response."""

    html: str
    markdown: str
    raw_text: str
    references: List[Dict[str, Any]]


class KagiSearch:
    """Kagi search client using session token authentication."""

    def __init__(self, session_token: Optional[str] = None, config_path: Optional[str] = None):
        """
        Initialize Kagi search client.

        Args:
            session_token: Kagi session token (if not provided, will load from config)
            config_path: Path to config file (defaults to ~/.config/kagi/config.json)
        """
        self.base_url = "https://kagi.com"
        self.user_agent = "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36"

        # Load config and get session token
        if not session_token:
            config = self._load_config(config_path)
            session_token = self._get_session_token(config)

        self.session_token = session_token

        # Set up cookie handling
        self.cookie_jar = CookieJar()
        self.opener = build_opener(HTTPCookieProcessor(self.cookie_jar))

        # Authenticate with token
        self._authenticate()

    def _load_config(self, config_path: Optional[str] = None) -> Dict[str, Any]:
        """Load configuration from file."""
        if not config_path:
            config_file = Path.home() / ".config" / "kagi" / "config.json"
        else:
            config_file = Path(config_path)

        if not config_file.exists():
            # Create default config
            config_file.parent.mkdir(parents=True, exist_ok=True)
            default_config = {
                "password_command": "rbw get kagi-session-link",
                "timeout": 30,
                "max_retries": 5,
            }
            with open(config_file, "w") as f:
                json.dump(default_config, f, indent=2)
            msg = colorize(f"Created default config at {config_file}", color="green")
            print(msg, file=sys.stderr)
            return default_config

        with open(config_file, "r") as f:
            data: Dict[str, Any] = json.load(f)
            return data

    def _get_session_token(self, config: Dict[str, Any]) -> str:
        """Get session token using password command from config."""
        password_command = config.get("password_command", "rbw get kagi-session-link")

        try:
            # Execute the password command
            result = subprocess.run(
                password_command.split(), capture_output=True, text=True, check=True
            )
            session_link = result.stdout.strip()

            # Extract token from session link
            if "token=" in session_link:
                return session_link.split("token=")[1].split("&")[0]
            else:
                # Assume the entire output is the token
                return session_link

        except subprocess.CalledProcessError as e:
            error_msg = colorize(f"Error executing password command: {e}", color="red", bold=True)
            print(error_msg, file=sys.stderr)
            if e.stderr:
                stderr_msg = colorize(f"stderr: {e.stderr}", color="red")
                print(stderr_msg, file=sys.stderr)
            sys.exit(1)
        except Exception as e:
            error_msg = colorize(f"Error getting session token: {e}", color="red", bold=True)
            print(error_msg, file=sys.stderr)
            sys.exit(1)

    def _authenticate(self) -> None:
        """Authenticate with Kagi using session token."""
        token_url = f"{self.base_url}/html/search?token={self.session_token}"
        request = Request(token_url)
        request.add_header("User-Agent", self.user_agent)

        try:
            response = self.opener.open(request, timeout=30)
            final_url = response.geturl()

            # Check if we were redirected to home page or html search (successful auth)
            if final_url in [f"{self.base_url}/", self.base_url, f"{self.base_url}/html/search"]:
                return  # Success
            elif "/signin" in final_url or "/welcome" in final_url:
                raise Exception(f"Authentication failed - redirected to {final_url}")
        except Exception as e:
            raise Exception(f"Failed to authenticate with token: {e}")

    def search(self, query: str, limit: int = 10) -> List[SearchResult]:
        """
        Search Kagi and return results.

        Args:
            query: Search query
            limit: Maximum number of results to return

        Returns:
            List of SearchResult objects
        """
        # Construct search URL for HTML version (no token needed, we use cookies)
        params = {"q": query}
        search_url = f"{self.base_url}/html/search?{urlencode(params)}"

        max_retries = 5
        retry_delay = 1.0

        for attempt in range(max_retries):
            try:
                # Create request with headers
                request = Request(search_url)
                request.add_header("User-Agent", self.user_agent)
                request.add_header(
                    "Accept",
                    "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,*/*;q=0.8",
                )
                request.add_header("Accept-Language", "en-US,en;q=0.5")
                request.add_header("DNT", "1")
                request.add_header("Connection", "keep-alive")
                request.add_header("Upgrade-Insecure-Requests", "1")

                # Make request using opener (with cookies)
                response = self.opener.open(request, timeout=30)

                # Check if we're redirected to sign in
                final_url = response.geturl()
                if "/signin" in final_url or "/welcome" in final_url:
                    raise Exception(f"Authentication failed - redirected to {final_url}")

                # Read response
                content = response.read()

                # Handle gzip encoding if present
                if response.headers.get("Content-Encoding") == "gzip":
                    content = gzip.decompress(content)

                html_content = content.decode("utf-8")

                # Parse results
                soup = BeautifulSoup(html_content, "html.parser")

                # Find results container
                results_box = soup.find(class_="results-box")
                if not results_box:
                    if attempt < max_retries - 1:
                        time.sleep(retry_delay)
                        continue
                    raise Exception("No results box found on page")

                # Extract search results
                results = []
                search_results = results_box.find_all(class_="search-result")  # type: ignore

                for result in search_results[:limit]:
                    # Extract title
                    title_elem = result.find(class_="__sri-title")
                    title = ""
                    if title_elem:
                        # Get only the text content, not the UI elements
                        # Remove "More results from this site", "Remove results from this site", etc.
                        title_text = title_elem.get_text(separator=" ", strip=True)
                        # Split by common Kagi UI elements and take the first part
                        for separator in [
                            "More results from",
                            "Remove results from",
                            "Open page in",
                        ]:
                            if separator in title_text:
                                title_text = title_text.split(separator)[0]
                        title = title_text.strip()

                    # Extract URL
                    url_box = result.find(class_="__sri-url-box")
                    url = ""
                    if url_box:
                        link = url_box.find("a", href=True)
                        if link:
                            url = link["href"]

                    # Extract snippet
                    desc_elem = result.find(class_="__sri-desc")
                    snippet = desc_elem.get_text(strip=True) if desc_elem else ""

                    if title and url:
                        results.append(SearchResult(title=title, url=url, snippet=snippet))

                if results:
                    return results
                elif attempt < max_retries - 1:
                    time.sleep(retry_delay)
                    continue

            except (URLError, HTTPError) as e:
                if attempt < max_retries - 1:
                    time.sleep(retry_delay)
                    continue
                raise Exception(f"Request failed: {e}")

        return []

    def get_quick_answer(self, query: str) -> Optional[QuickAnswer]:
        """
        Get Kagi Quick Answer for a query.

        Args:
            query: Search query

        Returns:
            QuickAnswer object or None if no answer available
        """
        # Construct Quick Answer URL
        params = {"q": query, "stream": "1"}
        quick_answer_url = f"{self.base_url}/mother/context?{urlencode(params)}"
        logger.debug(f"Quick Answer URL: {quick_answer_url}")
        logger.info("Fetching Quick Answer...")

        try:
            # Create request with headers
            request = Request(quick_answer_url)
            request.add_header("User-Agent", self.user_agent)
            request.add_header("Accept", "application/vnd.kagi.stream")
            request.add_header("Accept-Language", "en-US,en;q=0.5")
            request.add_header("Accept-Encoding", "gzip, deflate, br, zstd")
            request.add_header("Referer", f"{self.base_url}/search?q={query}")
            request.add_header("DNT", "1")
            request.add_header("Connection", "keep-alive")

            # Make request using opener (with cookies)
            logger.debug("Making Quick Answer request...")
            response = self.opener.open(request, timeout=30)
            logger.debug(f"Response status: {response.getcode()}")

            # Check if we're redirected to sign in
            final_url = response.geturl()
            logger.debug(f"Final URL: {final_url}")
            if "/signin" in final_url or "/welcome" in final_url:
                raise Exception(f"Authentication failed - redirected to {final_url}")

            # Read response
            content = response.read()
            logger.debug(f"Response content length: {len(content)} bytes")

            # Handle gzip encoding if present
            if response.headers.get("Content-Encoding") == "gzip":
                logger.debug("Decompressing gzip content")
                content = gzip.decompress(content)

            # Parse streaming response - split by null bytes
            messages = content.decode("utf-8").split("\x00")
            logger.debug(f"Response has {len(messages)} messages")
            final_data = None

            for message in messages:
                if message.strip():
                    logger.debug(f"Processing message: {message[:100]}...")  # Log first 100 chars
                    # Each message starts with "update:" or "final:"
                    if message.startswith("final:"):
                        json_str = message[6:]  # Remove "final:" prefix
                        logger.debug(f"Parsing final JSON: {json_str[:200]}...")
                        try:
                            final_data = json.loads(json_str)
                            logger.debug("Successfully parsed final data")
                        except json.JSONDecodeError as e:
                            logger.error(f"Failed to parse final JSON: {e}")
                            logger.debug(f"Full message: {message}")
                        break

            if not final_data:
                logger.debug("No final data found in response")
                return None

            if "output_data" not in final_data:
                logger.debug(f"No output_data in final response: {final_data.keys()}")
                return None

            output_data = final_data["output_data"]

            # Extract data with defaults
            html = final_data.get("output_text", "")
            markdown = output_data.get("markdown", "")
            raw_text = output_data.get("raw_text", "")
            references = output_data.get("references", [])

            # If no content, return None
            if not html and not markdown and not raw_text:
                logger.debug("No content found in Quick Answer")
                return None

            logger.debug(f"Quick Answer found with {len(references)} references")

            return QuickAnswer(
                html=html, markdown=markdown, raw_text=raw_text, references=references
            )

        except Exception as e:
            # Quick Answer might not be available for all queries
            logger.debug(f"Quick Answer error: {type(e).__name__}: {e}")
            return None


def main():
    """Main entry point for command line usage."""
    parser = argparse.ArgumentParser(description="Search Kagi using session token")
    parser.add_argument("query", nargs="?", help="Search query")
    parser.add_argument(
        "-n", "--num-results", type=int, default=10, help="Number of results (default: 10)"
    )
    parser.add_argument("-t", "--token", help="Session token (overrides config)")
    parser.add_argument("-c", "--config", help="Config file path")
    parser.add_argument("-j", "--json", action="store_true", help="Output as JSON")
    parser.add_argument("-d", "--debug", action="store_true", help="Enable debug logging to stderr")

    args = parser.parse_args()

    # Configure logging - always to stderr
    log_level = logging.DEBUG if args.debug else logging.WARNING
    logging.basicConfig(
        level=log_level,
        format="%(asctime)s - %(name)s - %(levelname)s - %(message)s",
        stream=sys.stderr,
    )

    # If no query provided, read from stdin
    if not args.query:
        if sys.stdin.isatty():
            parser.error("No query provided")
        args.query = sys.stdin.read().strip()

    # Initialize client
    try:
        client = KagiSearch(session_token=args.token, config_path=args.config)
    except Exception as e:
        error_msg = colorize(f"Error initializing Kagi client: {e}", color="red", bold=True)
        print(error_msg, file=sys.stderr)
        sys.exit(1)

    # Perform search
    try:
        results = client.search(args.query, limit=args.num_results)
        quick_answer = client.get_quick_answer(args.query)
    except Exception as e:
        error_msg = colorize(f"Search failed: {e}", color="red", bold=True)
        print(error_msg, file=sys.stderr)
        sys.exit(1)

    # Output results
    if args.json:
        output = {
            "results": [{"title": r.title, "url": r.url, "snippet": r.snippet} for r in results]
        }
        if quick_answer:
            output["quick_answer"] = {
                "markdown": quick_answer.markdown,
                "raw_text": quick_answer.raw_text,
                "references": quick_answer.references,
            }
        print(json.dumps(output, indent=2))
    else:
        # Display Quick Answer if available
        if quick_answer:
            qa_title = colorize("Quick Answer", color="cyan", bold=True)
            print(f"\n{qa_title}")
            print(colorize("─" * 80, color="cyan", dim=True))

            # Display raw text for terminal (cleaner than HTML)
            if quick_answer.raw_text:
                print(quick_answer.raw_text)
            elif quick_answer.markdown:
                # Fallback to markdown if no raw text
                print(quick_answer.markdown)

            # Display references
            if quick_answer.references:
                print()
                refs_title = colorize("References:", color="cyan", dim=True)
                print(refs_title)
                for i, ref in enumerate(quick_answer.references[:5], 1):  # Limit to 5 refs
                    ref_num = colorize(f"[{i}]", color="cyan", dim=True)
                    ref_title = ref.get("title", "")
                    ref_url = ref.get("url", "")
                    if ref_title and ref_url:
                        ref_link = hyperlink(ref_url, colorize(ref_title, color="blue"))
                        print(f"  {ref_num} {ref_link}")

            print(colorize("─" * 80, color="cyan", dim=True))
            print()  # Extra newline before search results
        for i, result in enumerate(results, 1):
            # Result number in yellow/bold
            number = colorize(f"{i}.", color="yellow", bold=True)
            # Title in blue/bold with hyperlink
            title = colorize(result.title, color="blue", bold=True)
            title_with_link = hyperlink(result.url, title)
            print(f"\n{number} {title_with_link}")

            # URL in green with hyperlink
            url_text = colorize(result.url, color="green")
            url_with_link = hyperlink(result.url, url_text)
            print(f"   {url_with_link}")

            # Snippet in default color but dim
            if result.snippet:
                snippet = colorize(result.snippet, dim=True)
                print(f"   {snippet}")

    if not results and not args.json:
        error_msg = colorize("No results found", color="red")
        print(error_msg, file=sys.stderr)


if __name__ == "__main__":
    main()
