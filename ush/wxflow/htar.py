from typing import List, Union

from .executable import which

__all__ = ['Htar']


class Htar:
    """
    Class offering an interface to HPSS via the htar utility.

    Examples:
    --------

    >>> from wxflow import Htar
    >>> htar = Htar()  # Generates an Executable object of "htar"
    >>> output = htar.cvf("/HPSS/path/to/archive.tar", "file1 file2") # Create an HPSS archive from two local files
    >>> output = htar.tell("/HPSS/path/to/archive.tar") # List the contents of an archive
    """
    def __init__(self) -> None:
        self.exe = which("htar")

    def _htar(self, arg_list: list, silent: bool = False) -> str:
        """
        Direct command builder function for htar based on the input arguments.

        Parameters:
        -----------
        arg_list : list
                List of string arguments to send to htar

        silent : bool
                Flag to suppress output to stdout

        Returns
        -------
        output : str
                Concatenated output and error from the htar command

        Examples:
        ---------
        >>> htar = Htar()
        >>> # Run `htar -cvPf /path/to/hpss/archive.tar file1 file2 file-*
        >>> htar._htar("-cvPf", "/path/to/hpss/archive.tar", "file1 file2 file-*")
        """

        # Remove any empty arguments which can cause issues for htar
        arg_list = [arg for arg in arg_list if arg != ""]

        if silent:
            output = self.exe(*arg_list, output=str, error=str)
        else:
            output = self.exe(*arg_list, output=str.split, error=str.split)

        return output

    def create(self, tarball: str, fileset: Union[List, str],
               dereference: bool = False, opts: Union[List, str] = "-P") -> str:
        """ Method to write an archive to HPSS

        Parameters
        ----------
        opts : str | list
                Options to send to htar.  By default, "-P" (create parent directories).

        tarball : str
                Full path location on HPSS to create the archive.

        fileset : list | str
                List containing filenames, patterns, or directories to archive

        dereference : bool
                Whether to dereference symbolic links (archive the pointed-to files instead).

        Returns
        -------
        output : str
                Concatenated output and error of the htar command.
        """
        arg_list = ["-c"]

        if dereference:
            arg_list.append("-h")

        # Parse any htar options
        arg_list.extend(Htar._split_opts(opts))

        if len(fileset) == 0:
            raise ValueError("Input fileset is empty, cannot create archive")

        arg_list.extend(["-f", tarball])

        # Convert filenames in fileset to strings to handle Path objects
        arg_list.extend([str(filename) for filename in Htar._split_opts(fileset)])

        output = self._htar(arg_list)

        return output

    def cvf(self, tarball: str, fileset: Union[List, str], dereference: bool = False) -> str:
        """ Method to write an archive to HPSS verbosely (without options).

        Parameters
        ----------
        tarball : str
                Full path location on HPSS to create the archive.

        fileset : list | str
                List containing filenames, patterns, or directories to archive

        dereference : bool
                Whether to dereference symbolic links (archive the pointed-to files instead).

        Returns
        -------
        output : str
                Concatenated output and error from the htar command
        """
        output = self.create(tarball, fileset, dereference=dereference, opts="-v -P")

        return output

    def extract(self, tarball: str, fileset: Union[List, str] = [], opts: Union[List, str] = "") -> str:
        """ Method to extract an archive from HPSS via htar

        Parameters
        ----------
        opts : str
                String of options to send to htar.

        tarball : str
                Full path location of an archive on HPSS to extract from.

        fileset : list | str
                Filenames, patterns, or directories to extract from
                the archive.  If empty, then all files will be extracted.

        Returns
        -------
        output : str
                Concatenated output and error from the htar command
        """
        arg_list = ["-x"]

        # Parse any htar options
        arg_list.extend(Htar._split_opts(opts))

        arg_list += ["-f", tarball]

        # Convert filename(s) to str to handle Path objects
        arg_list.extend([str(filename) for filename in Htar._split_opts(fileset)])

        output = self._htar(arg_list)

        return output

    def xvf(self, tarball: str = "", fileset: list = []) -> str:
        """ Method to extract an archive from HPSS verbosely (without options).

        Parameters
        ----------
        tarball : str
                Full path location of an archive on HPSS to extract from.

        fileset : list
                List containing filenames, patterns, or directories to extract from
                the archive.  If empty, then all files will be extracted.

        Returns
        -------
        output : str
                Concatenated output and error from the htar command
        """
        output = self.extract(tarball, fileset, opts="-v")

        return output

    def tell(self, tarball: str, opts: Union[List, str] = "", fileset: Union[List, str] = []) -> str:
        """ Method to list the contents of an archive on HPSS

        Parameters
        ----------
        opts : str
                String of options to send to htar.

        tarball : str
                Full path location on HPSS to list the contents of.

        fileset : list | str
                Filenames, patterns, or directories to list from
                the archive.  If empty, then all files will be listed.

        Returns
        -------
        output : str
                Concatenated output and error from the htar command
        """
        print("enter")
        arg_list = ["-t"]

        # Parse any htar options
        arg_list.extend(Htar._split_opts(opts))

        arg_list.extend(["-f", tarball])

        # Convert filename(s) to str to handle Path objects
        arg_list.extend([str(filename) for filename in Htar._split_opts(fileset)])

        output = self._htar(arg_list)

        return output

    @staticmethod
    def _split_opts(opts: Union[List, str] = "") -> list:
        """ Method to split input list or string of htar options

        Parameters
        ----------
        opts : list | str
                Input list or string of options to send to htar

        Returns
        -------
        split_opts : list
                List of options to send to htar
        """

        split_opts = opts.split(" ") if isinstance(opts, str) else opts

        return split_opts
