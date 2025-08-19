"""
The Serena Model Context Protocol (MCP) Server
"""

import os
import shutil
from collections.abc import Iterable
from copy import deepcopy
from dataclasses import dataclass, field
from datetime import datetime
from functools import cached_property
from pathlib import Path
from typing import TYPE_CHECKING, Any, Optional, Self, TypeVar

import yaml
from ruamel.yaml.comments import CommentedMap
from sensai.util import logging
from sensai.util.logging import LogTime, datetime_tag
from sensai.util.string import ToStringMixin

from serena.constants import (
    DEFAULT_ENCODING,
    PROJECT_TEMPLATE_FILE,
    REPO_ROOT,
    SERENA_CONFIG_TEMPLATE_FILE,
    SERENA_MANAGED_DIR_IN_HOME,
    SERENA_MANAGED_DIR_NAME,
)
from serena.util.general import load_yaml, save_yaml
from serena.util.inspection import determine_programming_language_composition
from solidlsp.ls_config import Language

from ..analytics import RegisteredTokenCountEstimator
from ..util.class_decorators import singleton

if TYPE_CHECKING:
    from ..project import Project

log = logging.getLogger(__name__)
T = TypeVar("T")
DEFAULT_TOOL_TIMEOUT: float = 240


@singleton
class SerenaPaths:
    """
    Provides paths to various Serena-related directories and files.
    """

    def __init__(self) -> None:
        self.user_config_dir: str = SERENA_MANAGED_DIR_IN_HOME
        """
        the path to the user's Serena configuration directory, which is typically ~/.serena
        """

    def get_next_log_file_path(self, prefix: str) -> str:
        """
        :param prefix: the filename prefix indicating the type of the log file
        :return: the full path to the log file to use
        """
        log_dir = os.path.join(self.user_config_dir, "logs", datetime.now().strftime("%Y-%m-%d"))
        os.makedirs(log_dir, exist_ok=True)
        return os.path.join(log_dir, prefix + "_" + datetime_tag() + ".txt")

    # TODO: Paths from constants.py should be moved here


class ToolSet:
    def __init__(self, tool_names: set[str]) -> None:
        self._tool_names = tool_names

    @classmethod
    def default(cls) -> "ToolSet":
        """
        :return: the default tool set, which contains all tools that are enabled by default
        """
        from serena.tools import ToolRegistry

        return cls(set(ToolRegistry().get_tool_names_default_enabled()))

    def apply(self, *tool_inclusion_definitions: "ToolInclusionDefinition") -> "ToolSet":
        """
        :param tool_inclusion_definitions: the definitions to apply
        :return: a new tool set with the definitions applied
        """
        from serena.tools import ToolRegistry

        registry = ToolRegistry()
        tool_names = set(self._tool_names)
        for definition in tool_inclusion_definitions:
            included_tools = []
            excluded_tools = []
            for included_tool in definition.included_optional_tools:
                if not registry.is_valid_tool_name(included_tool):
                    raise ValueError(f"Invalid tool name '{included_tool}' provided for inclusion")
                if included_tool not in tool_names:
                    tool_names.add(included_tool)
                    included_tools.append(included_tool)
            for excluded_tool in definition.excluded_tools:
                if not registry.is_valid_tool_name(excluded_tool):
                    raise ValueError(f"Invalid tool name '{excluded_tool}' provided for exclusion")
                if excluded_tool in self._tool_names:
                    tool_names.remove(excluded_tool)
                    excluded_tools.append(excluded_tool)
            if included_tools:
                log.info(f"{definition} included {len(included_tools)} tools: {', '.join(included_tools)}")
            if excluded_tools:
                log.info(f"{definition} excluded {len(excluded_tools)} tools: {', '.join(excluded_tools)}")
        return ToolSet(tool_names)

    def without_editing_tools(self) -> "ToolSet":
        """
        :return: a new tool set that excludes all tools that can edit
        """
        from serena.tools import ToolRegistry

        registry = ToolRegistry()
        tool_names = set(self._tool_names)
        for tool_name in self._tool_names:
            if registry.get_tool_class_by_name(tool_name).can_edit():
                tool_names.remove(tool_name)
        return ToolSet(tool_names)

    def get_tool_names(self) -> set[str]:
        """
        Returns the names of the tools that are currently included in the tool set.
        """
        return self._tool_names

    def includes_name(self, tool_name: str) -> bool:
        return tool_name in self._tool_names


@dataclass
class ToolInclusionDefinition:
    excluded_tools: Iterable[str] = ()
    included_optional_tools: Iterable[str] = ()


class SerenaConfigError(Exception):
    pass


def get_serena_managed_in_project_dir(project_root: str | Path) -> str:
    return os.path.join(project_root, SERENA_MANAGED_DIR_NAME)


def is_running_in_docker() -> bool:
    """Check if we're running inside a Docker container."""
    # Check for Docker-specific files
    if os.path.exists("/.dockerenv"):
        return True
    # Check cgroup for docker references
    try:
        with open("/proc/self/cgroup") as f:
            return "docker" in f.read()
    except FileNotFoundError:
        return False


@dataclass(kw_only=True)
class ProjectConfig(ToolInclusionDefinition, ToStringMixin):
    project_name: str
    language: Language
    ignored_paths: list[str] = field(default_factory=list)
    read_only: bool = False
    ignore_all_files_in_gitignore: bool = True
    initial_prompt: str = ""
    encoding: str = DEFAULT_ENCODING

    SERENA_DEFAULT_PROJECT_FILE = "project.yml"

    def _tostring_includes(self) -> list[str]:
        return ["project_name"]

    @classmethod
    def autogenerate(
        cls, project_root: str | Path, project_name: str | None = None, project_language: Language | None = None, save_to_disk: bool = True
    ) -> Self:
        """
        Autogenerate a project configuration for a given project root.

        :param project_root: the path to the project root
        :param project_name: the name of the project; if None, the name of the project will be the name of the directory
            containing the project
        :param project_language: the programming language of the project; if None, it will be determined automatically
        :param save_to_disk: whether to save the project configuration to disk
        :return: the project configuration
        """
        project_root = Path(project_root).resolve()
        if not project_root.exists():
            raise FileNotFoundError(f"Project root not found: {project_root}")
        with LogTime("Project configuration auto-generation", logger=log):
            project_name = project_name or project_root.name
            if project_language is None:
                language_composition = determine_programming_language_composition(str(project_root))
                if len(language_composition) == 0:
                    raise ValueError(
                        f"No source files found in {project_root}\n\n"
                        f"To use Serena with this project, you need to either:\n"
                        f"1. Add source files in one of the supported languages (Python, JavaScript/TypeScript, Java, C#, Rust, Go, Ruby, C++, PHP)\n"
                        f"2. Create a project configuration file manually at:\n"
                        f"   {os.path.join(project_root, cls.rel_path_to_project_yml())}\n\n"
                        f"Example project.yml:\n"
                        f"  project_name: {project_name}\n"
                        f"  language: python  # or typescript, java, csharp, rust, go, ruby, cpp, php\n"
                    )
                # find the language with the highest percentage
                dominant_language = max(language_composition.keys(), key=lambda lang: language_composition[lang])
            else:
                dominant_language = project_language.value
            config_with_comments = load_yaml(PROJECT_TEMPLATE_FILE, preserve_comments=True)
            config_with_comments["project_name"] = project_name
            config_with_comments["language"] = dominant_language
            if save_to_disk:
                save_yaml(str(project_root / cls.rel_path_to_project_yml()), config_with_comments, preserve_comments=True)
            return cls._from_dict(config_with_comments)

    @classmethod
    def rel_path_to_project_yml(cls) -> str:
        return os.path.join(SERENA_MANAGED_DIR_NAME, cls.SERENA_DEFAULT_PROJECT_FILE)

    @classmethod
    def _from_dict(cls, data: dict[str, Any]) -> Self:
        """
        Create a ProjectConfig instance from a configuration dictionary
        """
        language_str = data["language"].lower()
        project_name = data["project_name"]
        # backwards compatibility
        if language_str == "javascript":
            log.warning(f"Found deprecated project language `javascript` in project {project_name}, please change to `typescript`")
            language_str = "typescript"
        try:
            language = Language(language_str)
        except ValueError as e:
            raise ValueError(f"Invalid language: {data['language']}.\nValid languages are: {[l.value for l in Language]}") from e
        return cls(
            project_name=project_name,
            language=language,
            ignored_paths=data.get("ignored_paths", []),
            excluded_tools=data.get("excluded_tools", []),
            included_optional_tools=data.get("included_optional_tools", []),
            read_only=data.get("read_only", False),
            ignore_all_files_in_gitignore=data.get("ignore_all_files_in_gitignore", True),
            initial_prompt=data.get("initial_prompt", ""),
            encoding=data.get("encoding", DEFAULT_ENCODING),
        )

    @classmethod
    def load(cls, project_root: Path | str, autogenerate: bool = False) -> Self:
        """
        Load a ProjectConfig instance from the path to the project root.
        """
        project_root = Path(project_root)
        yaml_path = project_root / cls.rel_path_to_project_yml()
        if not yaml_path.exists():
            if autogenerate:
                return cls.autogenerate(project_root)
            else:
                raise FileNotFoundError(f"Project configuration file not found: {yaml_path}")
        with open(yaml_path, encoding="utf-8") as f:
            yaml_data = yaml.safe_load(f)
        if "project_name" not in yaml_data:
            yaml_data["project_name"] = project_root.name
        return cls._from_dict(yaml_data)


class RegisteredProject(ToStringMixin):
    def __init__(self, project_root: str, project_config: "ProjectConfig", project_instance: Optional["Project"] = None) -> None:
        """
        Represents a registered project in the Serena configuration.

        :param project_root: the root directory of the project
        :param project_config: the configuration of the project
        """
        self.project_root = Path(project_root).resolve()
        self.project_config = project_config
        self._project_instance = project_instance

    def _tostring_exclude_private(self) -> bool:
        return True

    @property
    def project_name(self) -> str:
        return self.project_config.project_name

    @classmethod
    def from_project_instance(cls, project_instance: "Project") -> "RegisteredProject":
        return RegisteredProject(
            project_root=project_instance.project_root,
            project_config=project_instance.project_config,
            project_instance=project_instance,
        )

    def matches_root_path(self, path: str | Path) -> bool:
        """
        Check if the given path matches the project root path.

        :param path: the path to check
        :return: True if the path matches the project root, False otherwise
        """
        return self.project_root == Path(path).resolve()

    def get_project_instance(self) -> "Project":
        """
        Returns the project instance for this registered project, loading it if necessary.
        """
        if self._project_instance is None:
            from ..project import Project

            with LogTime(f"Loading project instance for {self}", logger=log):
                self._project_instance = Project(project_root=str(self.project_root), project_config=self.project_config)
        return self._project_instance


@dataclass(kw_only=True)
class SerenaConfig(ToolInclusionDefinition, ToStringMixin):
    """
    Holds the Serena agent configuration, which is typically loaded from a YAML configuration file
    (when instantiated via :method:`from_config_file`), which is updated when projects are added or removed.
    For testing purposes, it can also be instantiated directly with the desired parameters.
    """

    projects: list[RegisteredProject] = field(default_factory=list)
    gui_log_window_enabled: bool = False
    log_level: int = logging.INFO
    trace_lsp_communication: bool = False
    web_dashboard: bool = True
    web_dashboard_open_on_launch: bool = True
    tool_timeout: float = DEFAULT_TOOL_TIMEOUT
    loaded_commented_yaml: CommentedMap | None = None
    config_file_path: str | None = None
    """
    the path to the configuration file to which updates of the configuration shall be saved;
    if None, the configuration is not saved to disk
    """
    jetbrains: bool = False
    """
    whether to apply JetBrains mode
    """
    record_tool_usage_stats: bool = False
    """Whether to record tool usage statistics, they will be shown in the web dashboard if recording is active. 
    """
    token_count_estimator: str = RegisteredTokenCountEstimator.TIKTOKEN_GPT4O.name
    """Only relevant if `record_tool_usage` is True; the name of the token count estimator to use for tool usage statistics.
    See the `RegisteredTokenCountEstimator` enum for available options.
    
    Note: some token estimators (like tiktoken) may require downloading data files
    on the first run, which can take some time and require internet access. Others, like the Anthropic ones, may require an API key
    and rate limits may apply.
    """

    CONFIG_FILE = "serena_config.yml"
    CONFIG_FILE_DOCKER = "serena_config.docker.yml"  # Docker-specific config file; auto-generated if missing, mounted via docker-compose for user customization

    def _tostring_includes(self) -> list[str]:
        return ["config_file_path"]

    @classmethod
    def generate_config_file(cls, config_file_path: str) -> None:
        """
        Generates a Serena configuration file at the specified path from the template file.

        :param config_file_path: the path where the configuration file should be generated
        """
        log.info(f"Auto-generating Serena configuration file in {config_file_path}")
        loaded_commented_yaml = load_yaml(SERENA_CONFIG_TEMPLATE_FILE, preserve_comments=True)
        save_yaml(config_file_path, loaded_commented_yaml, preserve_comments=True)

    @classmethod
    def _determine_config_file_path(cls) -> str:
        """
        :return: the location where the Serena configuration file is stored/should be stored
        """
        if is_running_in_docker():
            return os.path.join(REPO_ROOT, cls.CONFIG_FILE_DOCKER)
        else:
            config_path = os.path.join(SERENA_MANAGED_DIR_IN_HOME, cls.CONFIG_FILE)

            # if the config file does not exist, check if we can migrate it from the old location
            if not os.path.exists(config_path):
                old_config_path = os.path.join(REPO_ROOT, cls.CONFIG_FILE)
                if os.path.exists(old_config_path):
                    log.info(f"Moving Serena configuration file from {old_config_path} to {config_path}")
                    os.makedirs(os.path.dirname(config_path), exist_ok=True)
                    shutil.move(old_config_path, config_path)

            return config_path

    @classmethod
    def from_config_file(cls, generate_if_missing: bool = True) -> "SerenaConfig":
        """
        Static constructor to create SerenaConfig from the configuration file
        """
        config_file_path = cls._determine_config_file_path()

        # create the configuration file from the template if necessary
        if not os.path.exists(config_file_path):
            if not generate_if_missing:
                raise FileNotFoundError(f"Serena configuration file not found: {config_file_path}")
            log.info(f"Serena configuration file not found at {config_file_path}, autogenerating...")
            cls.generate_config_file(config_file_path)

        # load the configuration
        log.info(f"Loading Serena configuration from {config_file_path}")
        try:
            loaded_commented_yaml = load_yaml(config_file_path, preserve_comments=True)
        except Exception as e:
            raise ValueError(f"Error loading Serena configuration from {config_file_path}: {e}") from e

        # create the configuration instance
        instance = cls(loaded_commented_yaml=loaded_commented_yaml, config_file_path=config_file_path)

        # read projects
        if "projects" not in loaded_commented_yaml:
            raise SerenaConfigError("`projects` key not found in Serena configuration. Please update your `serena_config.yml` file.")

        # load list of known projects
        instance.projects = []
        num_project_migrations = 0
        for path in loaded_commented_yaml["projects"]:
            path = Path(path).resolve()
            if not path.exists() or (path.is_dir() and not (path / ProjectConfig.rel_path_to_project_yml()).exists()):
                log.warning(f"Project path {path} does not exist or does not contain a project configuration file, skipping.")
                continue
            if path.is_file():
                path = cls._migrate_out_of_project_config_file(path)
                if path is None:
                    continue
                num_project_migrations += 1
            project_config = ProjectConfig.load(path)
            project = RegisteredProject(
                project_root=str(path),
                project_config=project_config,
            )
            instance.projects.append(project)

        # set other configuration parameters
        if is_running_in_docker():
            instance.gui_log_window_enabled = False  # not supported in Docker
        else:
            instance.gui_log_window_enabled = loaded_commented_yaml.get("gui_log_window", False)
        instance.log_level = loaded_commented_yaml.get("log_level", loaded_commented_yaml.get("gui_log_level", logging.INFO))
        instance.web_dashboard = loaded_commented_yaml.get("web_dashboard", True)
        instance.web_dashboard_open_on_launch = loaded_commented_yaml.get("web_dashboard_open_on_launch", True)
        instance.tool_timeout = loaded_commented_yaml.get("tool_timeout", DEFAULT_TOOL_TIMEOUT)
        instance.trace_lsp_communication = loaded_commented_yaml.get("trace_lsp_communication", False)
        instance.excluded_tools = loaded_commented_yaml.get("excluded_tools", [])
        instance.included_optional_tools = loaded_commented_yaml.get("included_optional_tools", [])
        instance.jetbrains = loaded_commented_yaml.get("jetbrains", False)
        instance.record_tool_usage_stats = loaded_commented_yaml.get("record_tool_usage_stats", False)
        instance.token_count_estimator = loaded_commented_yaml.get(
            "token_count_estimator", RegisteredTokenCountEstimator.TIKTOKEN_GPT4O.name
        )

        # re-save the configuration file if any migrations were performed
        if num_project_migrations > 0:
            log.info(
                f"Migrated {num_project_migrations} project configurations from legacy format to in-project configuration; re-saving configuration"
            )
            instance.save()

        return instance

    @classmethod
    def _migrate_out_of_project_config_file(cls, path: Path) -> Path | None:
        """
        Migrates a legacy project configuration file (which is a YAML file containing the project root) to the
        in-project configuration file (project.yml) inside the project root directory.

        :param path: the path to the legacy project configuration file
        :return: the project root path if the migration was successful, None otherwise.
        """
        log.info(f"Found legacy project configuration file {path}, migrating to in-project configuration.")
        try:
            with open(path, encoding="utf-8", errors="ignore") as f:
                project_config_data = yaml.safe_load(f)
            if "project_name" not in project_config_data:
                project_name = path.stem
                with open(path, "a", encoding="utf-8", errors="ignore") as f:
                    f.write(f"\nproject_name: {project_name}")
            project_root = project_config_data["project_root"]
            shutil.move(str(path), str(Path(project_root) / ProjectConfig.rel_path_to_project_yml()))
            return Path(project_root).resolve()
        except Exception as e:
            log.error(f"Error migrating configuration file: {e}")
            return None

    @cached_property
    def project_paths(self) -> list[str]:
        return sorted(str(project.project_root) for project in self.projects)

    @cached_property
    def project_names(self) -> list[str]:
        return sorted(project.project_config.project_name for project in self.projects)

    def get_project(self, project_root_or_name: str) -> Optional["Project"]:
        # look for project by name
        project_candidates = []
        for project in self.projects:
            if project.project_config.project_name == project_root_or_name:
                project_candidates.append(project)
        if len(project_candidates) == 1:
            return project_candidates[0].get_project_instance()
        elif len(project_candidates) > 1:
            raise ValueError(
                f"Multiple projects found with name '{project_root_or_name}'. Please activate it by location instead. "
                f"Locations: {[p.project_root for p in project_candidates]}"
            )
        # no project found by name; check if it's a path
        if os.path.isdir(project_root_or_name):
            for project in self.projects:
                if project.matches_root_path(project_root_or_name):
                    return project.get_project_instance()
        return None

    def add_project_from_path(self, project_root: Path | str) -> "Project":
        """
        Add a project to the Serena configuration from a given path. Will raise a FileExistsError if a
        project already exists at the path.

        :param project_root: the path to the project to add
        :return: the project that was added
        """
        from ..project import Project

        project_root = Path(project_root).resolve()
        if not project_root.exists():
            raise FileNotFoundError(f"Error: Path does not exist: {project_root}")
        if not project_root.is_dir():
            raise FileNotFoundError(f"Error: Path is not a directory: {project_root}")

        for already_registered_project in self.projects:
            if str(already_registered_project.project_root) == str(project_root):
                raise FileExistsError(
                    f"Project with path {project_root} was already added with name '{already_registered_project.project_name}'."
                )

        project_config = ProjectConfig.load(project_root, autogenerate=True)

        new_project = Project(project_root=str(project_root), project_config=project_config, is_newly_created=True)
        self.projects.append(RegisteredProject.from_project_instance(new_project))
        self.save()

        return new_project

    def remove_project(self, project_name: str) -> None:
        # find the index of the project with the desired name and remove it
        for i, project in enumerate(list(self.projects)):
            if project.project_name == project_name:
                del self.projects[i]
                break
        else:
            raise ValueError(f"Project '{project_name}' not found in Serena configuration; valid project names: {self.project_names}")
        self.save()

    def save(self) -> None:
        """
        Saves the configuration to the file from which it was loaded (if any)
        """
        if self.config_file_path is None:
            return
        assert self.loaded_commented_yaml is not None, "Cannot save configuration without loaded YAML"
        loaded_original_yaml = deepcopy(self.loaded_commented_yaml)
        # projects are unique absolute paths
        # we also canonicalize them before saving
        loaded_original_yaml["projects"] = sorted({str(project.project_root) for project in self.projects})
        save_yaml(self.config_file_path, loaded_original_yaml, preserve_comments=True)
