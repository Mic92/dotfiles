"""Database schema and utilities for afew spam filter."""

import os
import sqlite3
from pathlib import Path


class SpamDatabase:
    """Database handler for spam scores."""

    def __init__(self) -> None:
        """Initialize database path."""
        xdg_data_home = os.environ.get(
            "XDG_DATA_HOME", Path.home() / ".local" / "share"
        )
        self.db_dir = Path(xdg_data_home) / "afew"
        self.db_dir.mkdir(parents=True, exist_ok=True)
        self.db_path = self.db_dir / "spam_scores.sqlite"

    def init_database(self) -> None:
        """Initialize the SQLite database schema."""
        with sqlite3.connect(self.db_path) as conn:
            # Spam scores table
            conn.execute("""
                CREATE TABLE IF NOT EXISTS spam_scores (
                    email TEXT PRIMARY KEY,
                    score REAL NOT NULL,
                    last_seen TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                    total_messages INTEGER DEFAULT 1,
                    spam_count INTEGER DEFAULT 0,
                    ham_count INTEGER DEFAULT 0
                )
            """)
            conn.execute("""
                CREATE INDEX IF NOT EXISTS idx_score ON spam_scores(score)
            """)

            conn.commit()

    def get_connection(self) -> sqlite3.Connection:
        """Get a database connection."""
        return sqlite3.connect(self.db_path)

    def extract_email_address(self, from_header: str) -> str:
        """Extract email address from From header."""
        if "<" in from_header and ">" in from_header:
            return from_header.split("<")[1].split(">")[0].lower()
        return from_header.lower().strip()

    def get_spam_score(self, email_address: str) -> float | None:
        """Get spam score for an email address."""
        email = self.extract_email_address(email_address)
        with self.get_connection() as conn:
            cursor = conn.execute(
                "SELECT score FROM spam_scores WHERE email = ?", (email,)
            )
            row = cursor.fetchone()
            return row[0] if row else None

    def update_spam_score(
        self, email_address: str, is_spam: bool, confidence: float
    ) -> None:
        """Update spam score for an email address."""
        email = self.extract_email_address(email_address)

        # Calculate score adjustment based on confidence
        # Spam: positive score, Ham: negative score
        score_delta = (confidence / 100.0) * (1 if is_spam else -1)

        with self.get_connection() as conn:
            # Check if email exists
            cursor = conn.execute(
                "SELECT score, total_messages, spam_count, ham_count FROM spam_scores WHERE email = ?",
                (email,),
            )
            row = cursor.fetchone()

            if row:
                # Update existing record
                current_score, total_messages, spam_count, ham_count = row
                new_score = current_score + score_delta
                new_total = total_messages + 1
                new_spam_count = spam_count + (1 if is_spam else 0)
                new_ham_count = ham_count + (0 if is_spam else 1)

                conn.execute(
                    """UPDATE spam_scores
                       SET score = ?, total_messages = ?, spam_count = ?,
                           ham_count = ?, last_seen = CURRENT_TIMESTAMP
                       WHERE email = ?""",
                    (new_score, new_total, new_spam_count, new_ham_count, email),
                )
            else:
                # Insert new record
                conn.execute(
                    """INSERT INTO spam_scores (email, score, spam_count, ham_count)
                       VALUES (?, ?, ?, ?)""",
                    (email, score_delta, 1 if is_spam else 0, 0 if is_spam else 1),
                )
            conn.commit()
