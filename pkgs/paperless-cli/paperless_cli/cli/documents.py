"""Document management commands for Paperless-ngx."""

from dataclasses import dataclass
from pathlib import Path
import time

from paperless_cli.api import PaperlessAPIError, PaperlessClient
from paperless_cli.cli.formatter import print_table


@dataclass
class DocumentsSearchCommand:
    """Documents search command."""

    query: str | None = None
    page: int = 1
    page_size: int = 25


@dataclass
class DocumentsGetCommand:
    """Documents get command."""

    document_id: int
    download: bool = False
    original: bool = False
    output: str | None = None
    metadata: bool = False


@dataclass
class DocumentsUploadCommand:
    """Documents upload command."""

    file_path: str
    title: str | None = None
    tags: str | None = None


@dataclass
class DocumentsDeleteCommand:
    """Documents delete command."""

    document_id: int
    force: bool = False


def search_documents(client: PaperlessClient, cmd: DocumentsSearchCommand) -> None:
    """Search for documents."""
    result = client.search_documents(cmd.query, cmd.page, cmd.page_size)

    documents = result.get("results", [])
    if not documents:
        print("No documents found.")
        return

    headers = ["ID", "Title", "Correspondent", "Created", "Tags"]
    rows = []

    for doc in documents:
        # Get tag names
        tags = doc.get("tags", [])
        tag_names = []
        if tags:
            all_tags = client.get_tags()
            tag_dict = {t["id"]: t["name"] for t in all_tags}
            tag_names = [tag_dict.get(tag_id, str(tag_id)) for tag_id in tags]

        # Get correspondent name
        correspondent = ""
        if doc.get("correspondent"):
            correspondents = client.get_correspondents()
            corr = next((c for c in correspondents if c["id"] == doc["correspondent"]), None)
            if corr:
                correspondent = corr["name"]

        rows.append(
            [
                doc["id"],
                doc.get("title", "Untitled"),
                correspondent or "-",
                doc.get("created", "-")[:10],  # Just date part
                ", ".join(tag_names) if tag_names else "-",
            ]
        )

    print_table(headers, rows)

    # Print pagination info
    count = result.get("count", 0)
    if count > cmd.page_size:
        print(f"\nShowing page {cmd.page} of {(count + cmd.page_size - 1) // cmd.page_size}")
        print(f"Total documents: {count}")


def get_document(client: PaperlessClient, cmd: DocumentsGetCommand) -> None:
    """Get document details or download it."""
    if cmd.download:
        # Download the document
        content = client.download_document(cmd.document_id, cmd.original)

        if cmd.output:
            output_path = cmd.output
        else:
            # Get document info to determine filename
            doc = client.get_document(cmd.document_id)
            filename = doc.get("original_file_name", f"document_{cmd.document_id}.pdf")
            output_path = filename

        with Path(output_path).open("wb") as f:
            f.write(content)
        print(f"Downloaded document to: {output_path}")

    elif cmd.metadata:
        # Show metadata
        metadata = client.get_document_metadata(cmd.document_id)
        print(f"\nDocument Metadata (ID: {cmd.document_id})")
        print("=" * 50)
        for key, value in metadata.items():
            print(f"{key}: {value}")

    else:
        # Show document details
        doc = client.get_document(cmd.document_id)

        print(f"\nDocument Details (ID: {doc['id']})")
        print("=" * 50)
        print(f"Title: {doc.get('title', 'Untitled')}")
        print(f"ASN: {doc.get('archive_serial_number', '-')}")
        print(f"Created: {doc.get('created', '-')}")
        print(f"Added: {doc.get('added', '-')}")
        print(f"Modified: {doc.get('modified', '-')}")
        print(f"Original filename: {doc.get('original_file_name', '-')}")

        # Correspondent
        if doc.get("correspondent"):
            correspondents = client.get_correspondents()
            corr = next((c for c in correspondents if c["id"] == doc["correspondent"]), None)
            if corr:
                print(f"Correspondent: {corr['name']}")

        # Document type
        if doc.get("document_type"):
            doc_types = client.get_document_types()
            doc_type = next((dt for dt in doc_types if dt["id"] == doc["document_type"]), None)
            if doc_type:
                print(f"Document type: {doc_type['name']}")

        # Tags
        if doc.get("tags"):
            tags = client.get_tags()
            tag_names = [tag["name"] for tag in tags if tag["id"] in doc["tags"]]
            print(f"Tags: {', '.join(tag_names)}")

        # Content
        if doc.get("content"):
            print("\nContent preview:")
            print("-" * 20)
            # Show first 500 characters
            content_str = str(doc["content"])[:500]
            if len(str(doc["content"])) > 500:
                content_str += "..."
            print(content_str)


def upload_document(client: PaperlessClient, cmd: DocumentsUploadCommand) -> None:
    """Upload a document."""
    # Check if file exists
    if not Path(cmd.file_path).exists():
        print(f"Error: File not found: {cmd.file_path}")
        return

    # Parse tags if provided
    tag_ids = None
    if cmd.tags:
        tag_ids = [int(tag_id.strip()) for tag_id in cmd.tags.split(",")]

    try:
        print(f"Uploading '{Path(cmd.file_path).name}'...")
        result = client.upload_document(cmd.file_path, cmd.title, tag_ids)

        # Get task_id from response
        task_id = result.get("task_id")
        if not task_id:
            print("Document uploaded but no task ID returned")
            return

        print("Upload successful. Waiting for processing...")

        # Poll for task completion
        start_time = time.time()
        dots = 0

        while True:
            task = client.get_task_status(task_id)
            if not task:
                print("\nError: Could not find task status")
                return

            status = task.get("status", "UNKNOWN")

            if status not in ["PENDING", "STARTED"]:
                # Task completed (success or failure)
                processing_time = time.time() - start_time
                print()  # New line after dots

                if status == "SUCCESS":
                    print("✓ Document processed successfully!")

                    # Extract document ID from result message
                    result_msg = task.get("result", "")
                    print(f"  Result: {result_msg}")
                    print(f"  Processing time: {processing_time:.1f} seconds")

                    # Try to extract document ID from result
                    doc_id = task.get("related_document")
                    if doc_id:
                        # Construct URL based on the known pattern
                        base_url = client.url.replace("-api", "")  # Remove -api from URL
                        print(f"  View at: {base_url}/documents/{doc_id}")
                else:
                    print("✗ Document processing failed!")
                    result_msg = task.get("result", "Unknown error")
                    print(f"  Result: {result_msg}")
                    print(f"  Processing time: {processing_time:.1f} seconds")

                return

            # Still processing, show progress
            print(".", end="", flush=True)
            dots += 1
            time.sleep(2)

    except PaperlessAPIError as e:
        print(f"Error uploading document: {e}")


def delete_document(client: PaperlessClient, document_id: int, force: bool) -> None:
    """Delete a document."""
    if not force:
        doc = client.get_document(document_id)
        confirm = input(
            f"Are you sure you want to delete document '{doc.get('title', 'Untitled')}' (ID: {document_id})? [y/N]: "
        )
        if confirm.lower() != "y":
            print("Cancelled.")
            return

    client.delete_document(document_id)
    print(f"Deleted document with ID {document_id}")
