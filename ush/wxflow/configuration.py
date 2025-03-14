import glob
import os
import random
import shutil
import subprocess
from pathlib import Path
from pprint import pprint
from typing import Any, Dict, List, Union

from .attrdict import AttrDict
from .timetools import to_datetime

__all__ = ['Configuration', 'cast_as_dtype', 'cast_strdict_as_dtypedict']


class ShellScriptException(Exception):
    def __init__(self, scripts, errors):
        self.scripts = scripts
        self.errors = errors
        super(ShellScriptException, self).__init__(
            str(errors) +
            ': error processing' +
            (' '.join(scripts)))


class UnknownConfigError(Exception):
    pass


class Configuration:
    """
    Configuration parser for the global-workflow
    (or generally for sourcing a shell script into a python dictionary)
    """

    def __init__(self, config_dir: Union[str, Path]):
        """
        Given a directory containing config files (config.XYZ),
        return a list of config_files minus the ones ending with ".default"
        """

        self.config_dir = config_dir
        self.config_files = self._get_configs

    @property
    def _get_configs(self) -> List[str]:
        """
        Given a directory containing config files (config.XYZ),
        return a list of config_files minus the ones ending with ".default"
        """
        result = list()
        for config in glob.glob(f'{self.config_dir}/config.*'):
            if not config.endswith('.default'):
                result.append(config)

        return result

    def find_config(self, config_name: str) -> str:
        """
            Given a config file name, find the full path of the config file
        """

        for config in self.config_files:
            if config_name == os.path.basename(config):
                return config

        raise UnknownConfigError(
            f'{config_name} does not exist (known: {repr(config_name)}), ABORT!')

    def parse_config(self, files: Union[str, bytes, list], **envvars) -> Dict[str, Any]:
        """
        Given the name of config file(s), key-value pair of all variables in the config file(s)
        are returned as a dictionary

        :param files: config file or list of config files
        :type files: list or str or unicode

        :param envvars: environment variables to be set prior to sourcing config files
        :type envvars: Any

        :return: Key value pairs representing the environment variables defined
                 in the script.
        :rtype: dict
        """
        if isinstance(files, (str, bytes)):
            files = [files]
        files = [self.find_config(file) for file in files]

        return cast_strdict_as_dtypedict(self._get_script_env(files, **envvars))

    def print_config(self, files: Union[str, bytes, list]) -> None:
        """
        Given the name of config file(s), key-value pair of all variables in the config file(s) are printed
        Same signature as parse_config
        :param files: config file or list of config files
        :type files: list or str or unicode
        :return: None
        """
        config = self.parse_config(files)
        pprint(config, width=4)

    @classmethod
    def _get_script_env(cls, scripts: List, **envvars) -> Dict[str, Any]:
        default_env = cls._get_shell_env([])
        and_script_env = cls._get_shell_env(scripts, **envvars)
        vars_just_in_script = set(and_script_env) - set(default_env)
        union_env = dict(default_env)
        union_env.update(and_script_env)
        return dict([(v, union_env[v]) for v in vars_just_in_script])

    @staticmethod
    def _get_shell_env(scripts: List, **envvars) -> Dict[str, Any]:
        varbls = dict()
        runme = ''
        if envvars:
            runme += '; '.join([f'export {key}={value}' for key, value in envvars.items()])
            runme += '; '
        runme += ''.join([f'source {s}; ' for s in scripts])
        magic = f'--- ENVIRONMENT BEGIN {random.randint(0,64**5)} ---'
        runme += f'/bin/echo -n "{magic}" ; /usr/bin/env -0'
        bash_path = shutil.which('bash')
        with open('/dev/null', 'w') as null:
            env = subprocess.Popen(runme, shell=True, executable=bash_path, stdin=null.fileno(),
                                   stdout=subprocess.PIPE)
            (out, err) = env.communicate()
        out = out.decode()
        begin = out.find(magic)
        if begin < 0:
            raise ShellScriptException(scripts, 'Cannot find magic string; '
                                       'at least one script failed: ' + repr(out))
        for entry in out[begin + len(magic):].split('\x00'):
            iequal = entry.find('=')
            varbls[entry[0:iequal]] = entry[iequal + 1:]
        return varbls


def cast_strdict_as_dtypedict(ctx: Dict[str, str]) -> Dict[str, Any]:
    """
    Environment variables are typically stored as str
    This method attempts to translate those into datatypes
    Parameters
    ----------
    ctx : dict
          dictionary with values as str
    Returns
    -------
    varbles : dict
              dictionary with values as datatypes
    """
    varbles = AttrDict()
    for key, value in ctx.items():
        varbles[key] = cast_as_dtype(value)
    return varbles


def cast_as_dtype(string: str) -> Union[str, int, float, bool, Any]:
    """
    Cast a value into known datatype

    Parameters
    ----------
    string: str

    Returns
    -------
    value : str, int, float, bool or datetime; or List of these
            default: str
    """
    TRUTHS = ['y', 'yes', 't', 'true', '.t.', '.true.']
    BOOLS = ['n', 'no', 'f', 'false', '.f.', '.false.'] + TRUTHS
    BOOLS = [x.upper() for x in BOOLS] + BOOLS + ['Yes', 'No', 'True', 'False']

    if ',' in string:
        # Convert comma-separated list to python list
        return [cast_as_dtype(elem.strip()) for elem in string.split(',') if elem.strip() != '']

    def _cast_or_not(to_type: Any, string: str):
        try:
            return to_type(string)
        except ValueError:
            return string

    def _true_or_not(string: str):
        try:
            return string.lower() in TRUTHS
        except AttributeError:
            return string

    try:
        return to_datetime(string)  # Try as a datetime
    except Exception:
        if string in BOOLS:  # Likely a boolean, convert to True/False
            return _true_or_not(string)
        elif '.' in string:  # Likely a number and that too a float
            return _cast_or_not(float, string)
        else:  # Still could be a number, may be an integer
            return _cast_or_not(int, string)
