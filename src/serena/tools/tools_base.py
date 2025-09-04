import inspect
import os
from abc import ABC
from collections.abc import Iterable
from dataclasses import dataclass
from types import TracebackType
from typing import TYPE_CHECKING, Any, Protocol, Self, TypeVar

from mcp.server.fastmcp.utilities.func_metadata import FuncMetadata, func_metadata
from sensai.util import logging
from sensai.util.string import dict_string

from serena.project import Project
from serena.prompt_factory import PromptFactory
from serena.symbol import LanguageServerSymbolRetriever
from serena.util.class_decorators import singleton
from serena.util.inspection import iter_subclasses
from solidlsp.ls_exceptions import SolidLSPException

if TYPE_CHECKING:
    from serena.agent import LinesRead, MemoriesManager, SerenaAgent
    from serena.code_editor import CodeEditor

log = logging.getLogger(__name__)
T = TypeVar("T")
SUCCESS_RESULT = "OK"


class Component(ABC):
    def __init__(self, agent: "SerenaAgent"):
        self.agent = agent

    def get_project_root(self) -> str:
        """
        :return: the root directory of the active project, raises a ValueError if no active project configuration is set
        """
        return self.agent.get_project_root()

    @property
    def prompt_factory(self) -> PromptFactory:
        return self.agent.prompt_factory

    @property
    def memories_manager(self) -> "MemoriesManager":
        assert self.agent.memories_manager is not None
        return self.agent.memories_manager

    def create_language_server_symbol_retriever(self) -> LanguageServerSymbolRetriever:
        if not self.agent.is_using_language_server():
            raise Exception("Cannot create LanguageServerSymbolRetriever; agent is not in language server mode.")
        language_server = self.agent.language_server
        assert language_server is not None
        return LanguageServerSymbolRetriever(language_server, agent=self.agent)

    @property
    def project(self) -> Project:
        return self.agent.get_active_project_or_raise()

    def create_code_editor(self) -> "CodeEditor":
        from ..code_editor import JetBrainsCodeEditor, LanguageServerCodeEditor

        if self.agent.is_using_language_server():
            return LanguageServerCodeEditor(self.create_language_server_symbol_retriever(), agent=self.agent)
        else:
            return JetBrainsCodeEditor(project=self.project, agent=self.agent)

    @property
    def lines_read(self) -> "LinesRead":
        assert self.agent.lines_read is not None
        return self.agent.lines_read


class ToolMarker:
    """
    Base class for tool markers.
    """


class ToolMarkerCanEdit(ToolMarker):
    """
    Marker class for all tools that can perform editing operations on files.
    """


class ToolMarkerDoesNotRequireActiveProject(ToolMarker):
    pass


class ToolMarkerOptional(ToolMarker):
    """
    Marker class for optional tools that are disabled by default.
    """


class ToolMarkerSymbolicRead(ToolMarker):
    """
    Marker class for tools that perform symbol read operations.
    """


class ToolMarkerSymbolicEdit(ToolMarkerCanEdit):
    """
    Marker class for tools that perform symbolic edit operations.
    """


class ApplyMethodProtocol(Protocol):
    """Callable protocol for the apply method of a tool."""

    def __call__(self, *args: Any, **kwargs: Any) -> str:
        pass


class Tool(Component):
    # NOTE: each tool should implement the apply method, which is then used in
    # the central method of the Tool class `apply_ex`.
    # Failure to do so will result in a RuntimeError at tool execution time.
    # The apply method is not declared as part of the base Tool interface since we cannot
    # know the signature of the (input parameters of the) method in advance.
    #
    # The docstring and types of the apply method are used to generate the tool description
    # (which is use by the LLM, so a good description is important)
    # and to validate the tool call arguments.

    @classmethod
    def get_name_from_cls(cls) -> str:
        name = cls.__name__
        if name.endswith("Tool"):
            name = name[:-4]
        # convert to snake_case
        name = "".join(["_" + c.lower() if c.isupper() else c for c in name]).lstrip("_")
        return name

    def get_name(self) -> str:
        return self.get_name_from_cls()

    def get_apply_fn(self) -> ApplyMethodProtocol:
        apply_fn = getattr(self, "apply")
        if apply_fn is None:
            raise RuntimeError(f"apply not defined in {self}. Did you forget to implement it?")
        return apply_fn

    @classmethod
    def can_edit(cls) -> bool:
        """
        Returns whether this tool can perform editing operations on code.

        :return: True if the tool can edit code, False otherwise
        """
        return issubclass(cls, ToolMarkerCanEdit)

    @classmethod
    def get_tool_description(cls) -> str:
        docstring = cls.__doc__
        if docstring is None:
            return ""
        return docstring.strip()

    @classmethod
    def get_apply_docstring_from_cls(cls) -> str:
        """Get the docstring for the apply method from the class (static metadata).
        Needed for creating MCP tools in a separate process without running into serialization issues.
        """
        # First try to get from __dict__ to handle dynamic docstring changes
        if "apply" in cls.__dict__:
            apply_fn = cls.__dict__["apply"]
        else:
            # Fall back to getattr for inherited methods
            apply_fn = getattr(cls, "apply", None)
            if apply_fn is None:
                raise AttributeError(f"apply method not defined in {cls}. Did you forget to implement it?")

        docstring = apply_fn.__doc__
        if not docstring:
            raise AttributeError(f"apply method has no (or empty) docstring in {cls}. Did you forget to implement it?")
        return docstring.strip()

    def get_apply_docstring(self) -> str:
        """Gets the docstring for the tool application, used by the MCP server."""
        return self.get_apply_docstring_from_cls()

    def get_apply_fn_metadata(self) -> FuncMetadata:
        """Gets the metadata for the tool application function, used by the MCP server."""
        return self.get_apply_fn_metadata_from_cls()

    @classmethod
    def get_apply_fn_metadata_from_cls(cls) -> FuncMetadata:
        """Get the metadata for the apply method from the class (static metadata).
        Needed for creating MCP tools in a separate process without running into serialization issues.
        """
        # First try to get from __dict__ to handle dynamic docstring changes
        if "apply" in cls.__dict__:
            apply_fn = cls.__dict__["apply"]
        else:
            # Fall back to getattr for inherited methods
            apply_fn = getattr(cls, "apply", None)
            if apply_fn is None:
                raise AttributeError(f"apply method not defined in {cls}. Did you forget to implement it?")

        return func_metadata(apply_fn, skip_names=["self", "cls"])

    def _log_tool_application(self, frame: Any) -> None:
        params = {}
        ignored_params = {"self", "log_call", "catch_exceptions", "args", "apply_fn"}
        for param, value in frame.f_locals.items():
            if param in ignored_params:
                continue
            if param == "kwargs":
                params.update(value)
            else:
                params[param] = value
        log.info(f"{self.get_name_from_cls()}: {dict_string(params)}")

    def _limit_length(self, result: str, max_answer_chars: int) -> str:
        if max_answer_chars == -1:
            max_answer_chars = self.agent.serena_config.default_max_tool_answer_chars
        if max_answer_chars <= 0:
            raise ValueError(f"Must be positive or the default (-1), got: {max_answer_chars=}")
        if (n_chars := len(result)) > max_answer_chars:
            result = (
                f"The answer is too long ({n_chars} characters). "
                + "Please try a more specific tool query or raise the max_answer_chars parameter."
            )
        return result

    def is_active(self) -> bool:
        return self.agent.tool_is_active(self.__class__)

    def apply_ex(self, log_call: bool = True, catch_exceptions: bool = True, **kwargs) -> str:  # type: ignore
        """
        Applies the tool with logging and exception handling, using the given keyword arguments
        """

        def task() -> str:
            apply_fn = self.get_apply_fn()

            try:
                if not self.is_active():
                    return f"Error: Tool '{self.get_name_from_cls()}' is not active. Active tools: {self.agent.get_active_tool_names()}"
            except Exception as e:
                return f"RuntimeError while checking if tool {self.get_name_from_cls()} is active: {e}"

            if log_call:
                self._log_tool_application(inspect.currentframe())
            try:
                # check whether the tool requires an active project and language server
                if not isinstance(self, ToolMarkerDoesNotRequireActiveProject):
                    if self.agent._active_project is None:
                        return (
                            "Error: No active project. Ask to user to select a project from this list: "
                            + f"{self.agent.serena_config.project_names}"
                        )
                    if self.agent.is_using_language_server() and not self.agent.is_language_server_running():
                        log.info("Language server is not running. Starting it ...")
                        self.agent.reset_language_server()

                # apply the actual tool
                try:
                    result = apply_fn(**kwargs)
                except SolidLSPException as e:
                    if e.is_language_server_terminated():
                        log.error(f"Language server terminated while executing tool ({e}). Restarting the language server and retrying ...")
                        self.agent.reset_language_server()
                        result = apply_fn(**kwargs)
                    else:
                        raise

                # record tool usage
                self.agent.record_tool_usage_if_enabled(kwargs, result, self)

            except Exception as e:
                if not catch_exceptions:
                    raise
                msg = f"Error executing tool: {e}"
                log.error(f"Error executing tool: {e}", exc_info=e)
                result = msg

            if log_call:
                log.info(f"Result: {result}")

            try:
                if self.agent.language_server is not None:
                    self.agent.language_server.save_cache()
            except Exception as e:
                log.error(f"Error saving language server cache: {e}")

            return result

        future = self.agent.issue_task(task, name=self.__class__.__name__)
        return future.result(timeout=self.agent.serena_config.tool_timeout)


class EditedFileContext:
    """
    Context manager for file editing.

    Create the context, then use `set_updated_content` to set the new content, the original content
    being provided in `original_content`.
    When exiting the context without an exception, the updated content will be written back to the file.
    """

    def __init__(self, relative_path: str, agent: "SerenaAgent"):
        self._project = agent.get_active_project()
        assert self._project is not None
        self._abs_path = os.path.join(self._project.project_root, relative_path)
        if not os.path.isfile(self._abs_path):
            raise FileNotFoundError(f"File {self._abs_path} does not exist.")
        with open(self._abs_path, encoding=self._project.project_config.encoding) as f:
            self._original_content = f.read()
        self._updated_content: str | None = None

    def __enter__(self) -> Self:
        return self

    def get_original_content(self) -> str:
        """
        :return: the original content of the file before any modifications.
        """
        return self._original_content

    def set_updated_content(self, content: str) -> None:
        """
        Sets the updated content of the file, which will be written back to the file
        when the context is exited without an exception.

        :param content: the updated content of the file
        """
        self._updated_content = content

    def __exit__(self, exc_type: type[BaseException] | None, exc_value: BaseException | None, traceback: TracebackType | None) -> None:
        if self._updated_content is not None and exc_type is None:
            assert self._project is not None
            with open(self._abs_path, "w", encoding=self._project.project_config.encoding) as f:
                f.write(self._updated_content)
            log.info(f"Updated content written to {self._abs_path}")
            # Language servers should automatically detect the change and update its state accordingly.
            # If they do not, we may have to add a call to notify it.


@dataclass(kw_only=True)
class RegisteredTool:
    tool_class: type[Tool]
    is_optional: bool
    tool_name: str


@singleton
class ToolRegistry:
    def __init__(self) -> None:
        self._tool_dict: dict[str, RegisteredTool] = {}
        for cls in iter_subclasses(Tool):
            if not cls.__module__.startswith("serena.tools"):
                continue
            is_optional = issubclass(cls, ToolMarkerOptional)
            name = cls.get_name_from_cls()
            if name in self._tool_dict:
                raise ValueError(f"Duplicate tool name found: {name}. Tool classes must have unique names.")
            self._tool_dict[name] = RegisteredTool(tool_class=cls, is_optional=is_optional, tool_name=name)

    def get_tool_class_by_name(self, tool_name: str) -> type[Tool]:
        return self._tool_dict[tool_name].tool_class

    def get_all_tool_classes(self) -> list[type[Tool]]:
        return list(t.tool_class for t in self._tool_dict.values())

    def get_tool_classes_default_enabled(self) -> list[type[Tool]]:
        """
        :return: the list of tool classes that are enabled by default (i.e. non-optional tools).
        """
        return [t.tool_class for t in self._tool_dict.values() if not t.is_optional]

    def get_tool_classes_optional(self) -> list[type[Tool]]:
        """
        :return: the list of tool classes that are optional (i.e. disabled by default).
        """
        return [t.tool_class for t in self._tool_dict.values() if t.is_optional]

    def get_tool_names_default_enabled(self) -> list[str]:
        """
        :return: the list of tool names that are enabled by default (i.e. non-optional tools).
        """
        return [t.tool_name for t in self._tool_dict.values() if not t.is_optional]

    def get_tool_names_optional(self) -> list[str]:
        """
        :return: the list of tool names that are optional (i.e. disabled by default).
        """
        return [t.tool_name for t in self._tool_dict.values() if t.is_optional]

    def get_tool_names(self) -> list[str]:
        """
        :return: the list of all tool names.
        """
        return list(self._tool_dict.keys())

    def print_tool_overview(
        self, tools: Iterable[type[Tool] | Tool] | None = None, include_optional: bool = False, only_optional: bool = False
    ) -> None:
        """
        Print a summary of the tools. If no tools are passed, a summary of the selection of tools (all, default or only optional) is printed.
        """
        if tools is None:
            if only_optional:
                tools = self.get_tool_classes_optional()
            elif include_optional:
                tools = self.get_all_tool_classes()
            else:
                tools = self.get_tool_classes_default_enabled()

        tool_dict: dict[str, type[Tool] | Tool] = {}
        for tool_class in tools:
            tool_dict[tool_class.get_name_from_cls()] = tool_class
        for tool_name in sorted(tool_dict.keys()):
            tool_class = tool_dict[tool_name]
            print(f" * `{tool_name}`: {tool_class.get_tool_description().strip()}")

    def is_valid_tool_name(self, tool_name: str) -> bool:
        return tool_name in self._tool_dict
