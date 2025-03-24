"""
The Serena Model Context Protocol (MCP) Server
"""

import os
import sys
from abc import abstractmethod
from collections.abc import AsyncIterator
from contextlib import asynccontextmanager
from dataclasses import dataclass
from typing import cast

import yaml
from mcp.server.fastmcp.server import Context, FastMCP, Settings
from sensai.util import logging

from multilspy import SyncLanguageServer
from multilspy.multilspy_config import Language, MultilspyConfig
from multilspy.multilspy_logger import MultilspyLogger

log = logging.getLogger(__name__)


@dataclass
class SerenaMCPRequestContext:
    language_server: SyncLanguageServer


@asynccontextmanager
async def server_lifespan(mcp_server: FastMCP) -> AsyncIterator[SerenaMCPRequestContext]:
    """Manage server startup and shutdown lifecycle."""
    argv = sys.argv[1:]
    if len(argv) != 1:
        print("\nUsage: mcp_server <.yml project file>", file=sys.stderr)
        sys.exit(1)

    project_file = argv[0]
    if not os.path.exists(project_file):
        print(f"Project file not found: {project_file}", file=sys.stderr)
        sys.exit(1)

    # read project configuration
    with open(project_file, encoding="utf-8") as f:
        project = yaml.safe_load(f)
    language = Language(project["language"])
    project_root = project["project_root"]

    # create and start the language server instance
    config = MultilspyConfig(code_language=language)
    logger = MultilspyLogger()
    language_server = SyncLanguageServer.create(config, logger, project_root)
    language_server.start()
    try:
        yield SerenaMCPRequestContext(language_server=language_server)
    finally:
        language_server.stop()


mcp_settings = Settings(lifespan=server_lifespan)
mcp = FastMCP(**mcp_settings.model_dump())


class Tool:
    def __init__(self, ctx: Context):
        lifespan_context = cast(SerenaMCPRequestContext, ctx.request_context.lifespan_context)
        self.langsrv = lifespan_context.language_server

    def execute(self) -> str:
        try:
            return self._execute()
        except Exception as e:
            log.error(f"Error executing tool: {e}")
            return f"Error executing tool: {e}"

    @abstractmethod
    def _execute(self):
        pass


@mcp.tool()
def read_file(ctx: Context, relative_path: str) -> str:
    log.info(f"read_file: {relative_path=}")

    class ReadFileTool(Tool):
        def _execute(self):
            with self.langsrv.open_file(relative_path):
                return self.langsrv.get_open_file_text(relative_path)

    return ReadFileTool(ctx).execute()


if __name__ == "__main__":
    logging.basicConfig(
        level=logging.DEBUG, stream=sys.stderr, format="%(levelname)-5s %(asctime)-15s %(name)s:%(funcName)s:%(lineno)d - %(message)s"
    )
    mcp.run()
