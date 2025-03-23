from pathlib import Path

import pytest


@pytest.fixture(scope="session")
def resources_dir() -> Path:
    """Path to the test resources directory."""
    current_dir = Path(__file__).parent
    return current_dir / "resources"


@pytest.fixture(scope="session")
def repo_path() -> Path:
    return Path(__file__).parent / "resources" / "test_repo"
