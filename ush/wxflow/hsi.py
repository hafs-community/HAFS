from typing import List, Union

from .executable import which

__all__ = ['Hsi']


class Hsi:
    """
    Class offering an interface to HPSS via the hsi utility.

    Examples:
    --------

    >>> from wxflow import Hsi
    >>> hsi = Hsi()  # Generates an Executable object of "hsi"
    >>> output = hsi.put("some_local_file", "/HPSS/path/to/some_file") # Put a file onto HPSS
    >>> output = hsi.ls("/HPSS/path/to/some_file") # List the file
    >>> output = hsi.chgrp("rstprod", "/HPSS/pth/to/some_file") # Change the group to rstprod
    """

    def __init__(self, quiet: bool = True, echo_commands: bool = True, opts: Union[str, List] = []):
        """Instantiate the hsi command

        Parameters:
        -----------
        quiet : bool
                Run hsi in quiet mode (suppress login information, transfer info, etc)
        echo_commands : bool
                Echo each command.  Some commands will still not echo (e.g. chmod).
        opts : str | list
                Additional arguments to send to each hsi command.
        """

        self.exe = which("hsi", required=True)

        hsi_args = []

        if quiet:
            hsi_args.append("-q")

        if echo_commands:
            hsi_args.append("-e")

        hsi_args.extend(Hsi._split_opts(hsi_args))

        for arg in hsi_args:
            self.exe.add_default_arg(arg)

    def _hsi(self, arg_list: list, silent: bool = False, ignore_errors: list = []) -> str:
        """Direct command builder function for hsi based on the input arguments.

        Parameters:
        arg_list : list
                A list of arguments to sent to hsi

        silent : bool
                Whether the output of the hsi command should be written to stdout

        ignore_errors : list
                List of error numbers to ignore.  For example, hsi returns error
                number 64 if a target file does not exist on HPSS.

        Returns
        -------
        output : str
                Concatenated output and error of the hsi command.

        Example:
        --------
            >>> hsi = Hsi()
            >>> # Execute `hsi get some_local_file : /some/hpss/file`
            >>> hsi._hsi(["get","some_local_file : /some/hpss/file"])
        """

        # Remove any empty arguments which can cause issues for hsi
        arg_list = [arg for arg in arg_list if arg != ""]

        if silent:
            output = self.exe(*arg_list, output=str, error=str,
                              ignore_errors=ignore_errors)
        else:
            output = self.exe(*arg_list, output=str.split, error=str.split,
                              ignore_errors=ignore_errors)

        return output

    def get(self, source: str, target=None, opts: Union[List, str] = []) -> str:
        """ Method to get a file from HPSS via hsi

        Parameters
        ----------
        source : str
                Full path location on HPSS of the file

        target : str
                Location on the local machine to place the file.  If not specified,
                then the file will be placed in the current directory.

        opts : str | list
                List or string of additional options to send to hsi command.

        Returns
        -------
        output : str
                Concatenated output and error of the hsi get command.
        """
        arg_list = []

        # Convert to str to handle Path objects
        source = str(source)
        target = str(target) if target is not None else None

        # Parse any hsi options
        arg_list.extend(Hsi._split_opts(opts))

        arg_list.append("get")
        args_list.append(source) if target is None else arg_list.append(f"{target} : {source}")

        output = self._hsi(arg_list)

        return output

    def put(self, source: str, target: str, opts: Union[List, str] = [],
            listing_file: str = None) -> str:
        """ Method to put a file onto HPSS via hsi

        Parameters
        ----------
        source : str
                Location on the local machine of the source file to send to HPSS.

        target : str
                Full path of the target location of the file on HPSS.

        opts : str | List
                List or string of additional options to send to hsi.

        Returns
        -------
        output : str
                Concatenated output and error of the hsi put command.
        """
        arg_list = []

        # Convert to str to handle Path objects
        target = str(target)
        source = str(source)

        # Parse any hsi options
        arg_list.extend(Hsi._split_opts(opts))

        arg_list.append("put")
        arg_list.append(source + " : " + target)
        output = self._hsi(arg_list)

        return output

    def chmod(self, mod: str, target: str, hsi_opts: Union[List, str] = "",
              chmod_opts: Union[List, str] = "") -> str:
        """ Method to change the permissions of a file or directory on HPSS

        Parameters
        ----------
        mod : str
                Permissions to set for the file or directory,
                e.g. "640", "o+r", etc.

        target : str
                Full path of the target location of the file on HPSS.

        hsi_opts : list | str
                Options to send to hsi.

        chmod_opts : list | str
                Options to send to chmod. See "hsi chmod -?" for more details.

        Return
        ------
        output : str
                Concatenated output and error of the hsi chmod command.
        """

        arg_list = []

        # Parse any hsi options
        arg_list.extend(Hsi._split_opts(hsi_opts))

        arg_list.append("chmod")

        # Parse any chmod options
        arg_list.extend(Hsi._split_opts(chmod_opts))

        arg_list.append(mod)
        arg_list.append(target)
        output = self._hsi(arg_list)

        return output

    def chgrp(self, group_name: str, target: str, hsi_opts: str = "",
              chgrp_opts: str = "") -> str:
        """ Method to change the group of a file or directory on HPSS

        Parameters
        ----------
        group_name : str
                The group to which ownership of the file/directory is to be set.

        target : str
                Full path of the target location of the file on HPSS.

        hsi_opts : str
                String of options to send to hsi.

        chgrp_opts : str
                Options to send to chgrp.  See "hsi chgrp -?" for more details.

        Returns
        -------
        output : str
                Concatenated output and error of the hsi chgrp command.
        """

        arg_list = []

        # Parse any hsi options
        arg_list.extend(Hsi._split_opts(hsi_opts))

        arg_list.append("chgrp")

        # Parse any chgrp options
        arg_list.extend(Hsi._split_opts(chgrp_opts))

        arg_list.append(group_name)
        arg_list.append(target)
        output = self._hsi(arg_list)

        return output

    def rm(self, target: str, recursive: bool = False, hsi_opts: str = "", rm_opts: str = "") -> str:
        """ Method to delete a file or directory on HPSS via hsi

        Parameters
        ----------
        target : str
                Full path of the target location of the file on HPSS.

        hsi_opts : str
                String of options to send to hsi.

        rm_opts : str
                Options to send to rm.  See "hsi rm -?" for more details.

        recursive : bool
                Flag to indicate a call to rmdir.

        Returns
        -------
        output : str
                Concatenated output and error of the hsi rm command.
        """

        # Call rmdir if recursive is set
        # NOTE this will ONLY remove empty directories
        if recursive:
            output = self.rmdir(target, hsi_opts, rmdir_opts)
            return output

        arg_list = []

        # Parse any hsi options
        arg_list.extend(Hsi._split_opts(hsi_opts))

        arg_list.append("rm")

        # Parse any rm options
        arg_list.extend(Hsi._split_opts(rm_opts))

        arg_list.append(target)

        # Ignore missing files
        output = self._hsi(arg_list, ignore_errors=[72])

        return output

    def rmdir(self, target: str, hsi_opts: str = "", rmdir_opts: str = "") -> str:
        """ Method to delete an empty directory on HPSS via hsi

        Parameters
        ----------
        target : str
                Full path of the target location of the file on HPSS.

        hsi_opts : str
                String of options to send to hsi.

        rmdir_opts : str
                Options to send to rmdir.  See "hsi rmdir -?" for more details.

        Returns
        -------
        output : str
                Concatenated output and error of the hsi rmdir command.
        """

        arg_list = []

        # Parse any hsi options
        arg_list.extend(Hsi._split_opts(hsi_opts))

        arg_list.append("rmdir")

        # Parse any rmdir options
        arg_list.extend(Hsi._split_opts(rmdir_opts))

        arg_list.append(target)
        output = self._hsi(arg_list)

        return output

    def mkdir(self, target: str, hsi_opts: str = "", mkdir_opts: str = "") -> str:
        """ Method to delete a file or directory on HPSS via hsi

        Parameters
        ----------
        target : str
                Full path of the target location of the file on HPSS.

        hsi_opts : str
                String of options to send to hsi.

        Returns
        -------
        output : str
                Concatenated output and error of the hsi mkdir command.
        """

        arg_list = []

        # Parse any hsi options
        arg_list.extend(Hsi._split_opts(hsi_opts))

        # The only flag available for mkdir is -p, which we will use.
        arg_list.extend(["mkdir", "-p"])

        # Parse any mkdir options
        arg_list.extend(Hsi._split_opts(mkdir_opts))

        arg_list.append(target)
        output = self._hsi(arg_list)

        return output

    def ls(self, target: str, hsi_opts: str = "", ls_opts: str = "",
           ignore_missing: bool = False) -> str:
        """ Method to list files/directories on HPSS via hsi

        Parameters
        ----------
        target : str
                Full path of the target location on HPSS.

        hsi_opts : str
                String of options to send to hsi.

        ls_opts : str
                Options to send to ls.  See "hsi ls -?" for more details.

        ignore_missing : bool
                Flag to ignore missing files

        Returns
        -------
        output : str
                Concatenated output and error of the hsi ls command.
        """

        arg_list = []

        if ignore_missing:
            ignore_errors = [64]
        else:
            ignore_errors = []

        # Parse any hsi options
        arg_list.extend(Hsi._split_opts(hsi_opts))

        arg_list.append("ls")

        # Parse any ls options
        arg_list.extend(Hsi._split_opts(ls_opts))

        arg_list.append(target)
        output = self._hsi(arg_list, ignore_errors=ignore_errors)

        return output

    def exists(self, target: str) -> bool:
        """ Method to test the existence of a file/directory/glob on HPSS

        Parameters
        ----------
        target : str
                Full path of the target location on HPSS.

        Returns
        -------
        pattern_exists : bool
                True if the target exists on HPSS.
        """

        arg_list = ["-q", "ls", target]

        # Do not exit if the file is not found; do not pipe output to stdout
        output = self._hsi(arg_list, silent=True, ignore_errors=[64])

        if "HPSS_ENOENT" in output:
            pattern_exists = False
        # Catch wildcards
        elif f"Warning: No matching names located for '{target}'" in output:
            pattern_exists = False
        else:
            pattern_exists = True

        return pattern_exists

    @staticmethod
    def _split_opts(opts: Union[List, str] = "") -> list:
        """ Method to split input list or string of hsi options

        Parameters
        ----------
        opts : list | str
                Input list or string of options to send to hsi or subcommand

        Returns
        -------
        split_opts : list
                List of options to send to hsi or the hsi subcommand
        """

        split_opts = opts.split(" ") if isinstance(opts, str) else opts

        return split_opts
