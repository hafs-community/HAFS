import os
import sys
from functools import reduce
from pathlib import Path
from typing import Dict, List, Union

import jinja2
from markupsafe import Markup

from .timetools import (add_to_datetime, strftime, to_fv3time, to_isotime,
                        to_julian, to_timedelta, to_YMD, to_YMDH)

__all__ = ['Jinja']


@jinja2.pass_eval_context
class SilentUndefined(jinja2.Undefined):
    """
    Description
    -----------
    A Jinja2 undefined that does not raise an error when it is used in a
    template. Instead, it returns the template back when the variable is not found
    This class is not to be used outside of this file
    Its purpose is to return the template instead of an empty string
    Presently, it also does not return the filter applied to the variable.
    This will be added later when a use case for it presents itself.
    """
    def __str__(self):
        return "{{ " + self._undefined_name + " }}"

    def __add__(self, other):
        return str(self) + other

    def __radd__(self, other):
        return other + str(self)

    def __mod__(self, other):
        return str(self) % other

    def __call__(self, *args, **kwargs):
        return Markup("{{ " + self._undefined_name + " }}")


class Jinja:
    """
    Description
    -----------
    A wrapper around jinja2 to render templates
    """

    def __init__(self, template_path_or_string: str,
                 data: Dict,
                 allow_missing: bool = True,
                 searchpath: Union[str, List] = '/') -> None:
        """
        Description
        -----------
        Given a path to a (jinja2) template and a data object, substitute the
        template file with data.
        Allow for retaining missing or undefined variables.
        Also provide additional search paths for templates that may be included in the main template
        Parameters
        ----------
        template_path_or_string : str
            Path to the template file or a templated string
        data : dict
            Data to be substituted into the template
            TODO: make "data" optional so that the user can render the same template with different data
        allow_missing : bool
            If True, allow for missing or undefined variables
        searchpath: str | list
            Additional search paths for templates (default '/')
        """

        self.jinja2_version = jinja2.__version__

        self.data = data
        self.undefined = SilentUndefined if allow_missing else jinja2.StrictUndefined
        self.template_searchpath = searchpath if isinstance(searchpath, list) else [searchpath]
        # Add a default search path if the user has not provided one
        if '/' not in self.template_searchpath:
            self.template_searchpath.insert(0, '/')

        if os.path.isfile(template_path_or_string):
            self.template_type = 'file'
            template_path = Path(template_path_or_string)
            template_dir = template_path.parent
            self.template_file = str(template_path.relative_to(template_dir))
            self.template_searchpath.append(str(template_dir))
        else:
            self.template_type = 'stream'
            self.template_stream = template_path_or_string

    def get_set_env(self, loader: jinja2.BaseLoader, filters: Dict[str, callable] = None) -> jinja2.Environment:
        """
        Description
        -----------
        Define the environment for the jinja2 template
        Any number of filters can be added here.
        Optionally, a dictionary of external filters can be passed in

        Currently, the following filters are defined:
        strftime: convert a datetime object to a string with a user defined format
        to_isotime: convert a datetime object to an ISO 8601 string
        to_fv3time: convert a datetime object to a FV3 time string
        to_YMDH: convert a datetime object to a YYYYMMDDHH string
        to_YMD: convert a datetime object to a YYYYMMDD string
        to_julian: convert a datetime object to a julian day
        to_f90bool: convert a boolean to a fortran boolean
        relpath: convert a full path to a relative path based on an input root_path
        getenv: read variable from environment if defined, else UNDEFINED
        to_timedelta: convert a string to a timedelta object
        add_to_datetime: add time to a datetime, return new datetime object
        replace_tmpl: replace substrings of an input string with replacements specified by an input dictionary

        The Expression Statement extension "jinja2.ext.do", which enables
            {% do ... %} statements.  These are useful for appending to lists.
            e.g. {{ bar.append(foo) }} would print "None" to the parsed jinja
            template, but {% do bar.append(foo) %} would not.

        Parameters
        ----------
        loader: jinja2.BaseLoader
            An instance of class jinja2.BaseLoader
        filters: Dict[str, callable] (optional)
            A dictionary of filters to be added to the environment

        Returns
        -------
        env: jinja2.Environment
        """

        env = jinja2.Environment(loader=loader, undefined=self.undefined)

        env.add_extension("jinja2.ext.do")

        env.filters["strftime"] = lambda dt, fmt: strftime(dt, fmt)
        env.filters["to_isotime"] = lambda dt: to_isotime(dt) if not isinstance(dt, SilentUndefined) else dt
        env.filters["to_fv3time"] = lambda dt: to_fv3time(dt) if not isinstance(dt, SilentUndefined) else dt
        env.filters["to_YMDH"] = lambda dt: to_YMDH(dt) if not isinstance(dt, SilentUndefined) else dt
        env.filters["to_YMD"] = lambda dt: to_YMD(dt) if not isinstance(dt, SilentUndefined) else dt
        env.filters["to_julian"] = lambda dt: to_julian(dt) if not isinstance(dt, SilentUndefined) else dt
        env.filters["to_f90bool"] = lambda bool: ".true." if bool else ".false."
        env.filters['getenv'] = lambda name, default='UNDEFINED': os.environ.get(name, default)
        env.filters["relpath"] = lambda pathname, start=os.curdir: os.path.relpath(pathname, start)
        env.filters["add_to_datetime"] = (
                lambda dt, delta: add_to_datetime(dt, delta)
                if not (isinstance(dt, SilentUndefined) or isinstance(delta, SilentUndefined))
                else dt if isinstance(dt, SilentUndefined) else delta)
        env.filters["to_timedelta"] = lambda delta_str: to_timedelta(delta_str) if not isinstance(delta_str, SilentUndefined) else delta_str
        env.filters["replace_tmpl"] = lambda string, tmpl_dict: reduce(lambda ss, kk: ss.replace(kk, tmpl_dict[kk]), tmpl_dict, string)

        # Add any additional filters
        if filters is not None:
            for filter_name, filter_func in filters.items():
                env = self.add_filter_to_env(env, filter_name, filter_func)

        return env

    @staticmethod
    def add_filter_to_env(env: jinja2.Environment, filter_name: str, filter_func: callable) -> jinja2.Environment:
        """
        Description
        -----------
        Add a custom filter to the jinja2 environment
        Parameters
        ----------
        env: jinja2.Environment
            Active jinja2 environment
        filter_name: str
            name of the filter
        filter_func: callable
            function that will be called
        Returns
        -------
        env: jinja2.Environment
            Active jinja2 environment with the new filter added
        """

        env.filters[filter_name] = filter_func

        return env

    @property
    def render(self) -> str:
        """
        Description
        -----------
        Render the Jinja2 template with the data
        Parameters
        ----------
        None
        Returns
        -------
        rendered: str
        Rendered template into text
        """

        render_map = {'stream': self._render_stream,
                      'file': self._render_file}
        return render_map[self.template_type]()

    def _render_stream(self) -> str:
        loader = jinja2.BaseLoader()
        env = self.get_set_env(loader)
        template = env.from_string(self.template_stream)
        return self._render_template(template)

    def _render_file(self) -> str:
        loader = jinja2.FileSystemLoader(self.template_searchpath)
        env = self.get_set_env(loader)
        template = env.get_template(self.template_file)
        return self._render_template(template)

    def _render_template(self, template: jinja2.Template) -> str:
        """
        Description
        -----------
        Render a jinja2 template object
        Parameters
        ----------
        template: jinja2.Template

        Returns
        -------
        rendered: str
        """
        try:
            rendered = template.render(**self.data)
        except jinja2.UndefinedError as ee:
            raise NameError(f"Undefined variable in Jinja2 template\n{ee}")

        return rendered

    def save(self, output_file: str) -> None:
        """
        Description
        -----------
        Render and save the output to a file
        Parameters
        ----------
        output_file: str
        Path to the output file
        Returns
        -------
        None
        """
        with open(output_file, 'wb') as fh:
            fh.write(self.render.encode("utf-8"))

    @property
    def dump(self) -> None:
        """
        Description
        -----------
        Render and dump the output to stdout
        Returns
        -------
        None
        """
        sys.stdout.write(self.render)
