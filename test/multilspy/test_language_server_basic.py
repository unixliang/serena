"""
Basic integration tests for the language server functionality.

These tests validate the functionality of the language server APIs
like request_references using the test repository.
"""

from pathlib import Path

import pytest

from multilspy.language_server import SyncLanguageServer
from multilspy.multilspy_config import Language, MultilspyConfig
from multilspy.multilspy_logger import MultilspyLogger


@pytest.fixture(scope="session")
def language_server(repo_path: Path):
    """Create a SyncLanguageServer instance configured to use the test repository."""
    config = MultilspyConfig(code_language=Language.PYTHON)
    logger = MultilspyLogger()

    # Create a language server instance
    server = SyncLanguageServer.create(config, logger, str(repo_path))

    # Start the server
    server.start()

    try:
        yield server
    finally:
        # Ensure server is shut down
        server.stop()


class TestLanguageServerBasics:
    """Test basic functionality of the language server."""

    def test_request_references_user_class(self, language_server: SyncLanguageServer, repo_path: Path):
        """Test request_references on the User class."""
        # Get references to the User class in models.py
        file_path = str(repo_path / "test_repo" / "models.py")
        # Line 31 contains the User class definition
        references = language_server.request_references(file_path, 31, 6)

        # User class should be referenced in multiple files
        assert len(references) > 0

        # At least two references should be found (one for the class definition itself)
        assert len(references) > 1

    def test_request_references_item_class(self, language_server: SyncLanguageServer, repo_path: Path):
        """Test request_references on the Item class."""
        # Get references to the Item class in models.py
        file_path = str(repo_path / "test_repo" / "models.py")
        # Line 56 contains the Item class definition
        references = language_server.request_references(file_path, 56, 6)

        # Item class should be referenced in multiple places
        assert len(references) > 0

        # At least one reference should be in services.py (ItemService class)
        services_references = [ref for ref in references if "services.py" in ref["uri"]]
        assert len(services_references) > 0

    def test_request_references_function_parameter(self, language_server: SyncLanguageServer, repo_path: Path):
        """Test request_references on a function parameter."""
        # Get references to the id parameter in get_user method
        file_path = str(repo_path / "test_repo" / "services.py")
        # Line 24 contains the get_user method with id parameter
        references = language_server.request_references(file_path, 24, 16)

        # id parameter should be referenced within the method
        assert len(references) > 0

    def test_request_references_create_user_method(self, language_server: SyncLanguageServer, repo_path: Path):
        # Get references to the create_user method in UserService
        file_path = str(repo_path / "test_repo" / "services.py")
        # Line 15 contains the create_user method definition
        references = language_server.request_references(file_path, 15, 9)

        # Verify that we get valid references
        assert len(references) > 1
