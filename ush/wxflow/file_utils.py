import os
from logging import getLogger

from .fsutils import cp, mkdir

__all__ = ['FileHandler']

logger = getLogger(__name__.split('.')[-1])


class FileHandler:
    """Class to manipulate files in bulk for a given configuration

    Parameters
    ----------
    config : dict
          A dictionary containing the "action" and the "act" in the form of a list

    NOTE
    ----
    "action" can be one of "mkdir", "copy", "copy_req", "copy_opt", etc.
    Corresponding "act" would be ['dir1', 'dir2'], [['src1', 'dest1'], ['src2', 'dest2']]
    "copy_req" will raise an error if the source file does not exist
    "copy_opt" will not raise an error if the source file does not exist but will present a warning

    Attributes
    ----------
    config : dict
            Dictionary of files to manipulate

    NOTE
    ----
    `copy` will be deprecated in the future in favor of `copy_req` and `copy_opt`
    Users are encouraged to use `copy_req` and `copy_opt` instead of `copy`
    """

    def __init__(self, config):

        self.config = config

    def sync(self):
        """
        Method to execute bulk actions on files described in the configuration
        """
        sync_factory = {
            'copy': self.copy_req,
            'copy_req': self.copy_req,
            'copy_opt': self.copy_opt,
            'mkdir': self._make_dirs,
        }
        # loop through the configuration keys
        for action, files in self.config.items():
            sync_factory[action](files)

    @staticmethod
    def copy_req(filelist):
        FileHandler._copy_files(filelist, required=True)

    @staticmethod
    def copy_opt(filelist):
        FileHandler._copy_files(filelist, required=False)

    @staticmethod
    def _copy_files(filelist, required=True):
        """Function to copy all files specified in the list

        `filelist` should be in the form:
        - [src, dest]

        Parameters
        ----------
        filelist : list
                List of lists of [src, dest]
        required : bool, optional
                Flag to indicate if the src file is required to exist. Default is True
        """
        for sublist in filelist:
            if len(sublist) != 2:
                raise IndexError(
                    f"List must be of the form ['src', 'dest'], not {sublist}")
            src = sublist[0]
            dest = sublist[1]
            if os.path.exists(src):
                try:
                    cp(src, dest)
                    logger.info(f'Copied {src} to {dest}')
                except Exception as ee:
                    logger.exception(f"Error copying {src} to {dest}")
                    raise ee
            else:
                if required:
                    logger.exception(f"Source file '{src}' does not exist and is required, ABORT!")
                    raise FileNotFoundError(f"Source file '{src}' does not exist")
                else:
                    logger.warning(f"Source file '{src}' does not exist, skipping!")

    @staticmethod
    def _make_dirs(dirlist):
        """Function to make all directories specified in the list

        Parameters
        ----------
        dirlist : list
                List of directories to create
        """
        for dd in dirlist:
            try:
                mkdir(dd)
                logger.info(f'Created {dd}')
            except Exception as ee:
                logger.exception(f"Error creating directory {dd}")
                raise ee
