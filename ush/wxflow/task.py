import logging
from typing import Dict

from .attrdict import AttrDict
from .timetools import add_to_datetime, to_timedelta

logger = logging.getLogger(__name__.split('.')[-1])


class Task:
    """
    Base class for all tasks
    """

    def __init__(self, config: Dict, *args, **kwargs):
        """
        Every task needs a config.
        Additional arguments (or key-value arguments) can be provided.

        Parameters
        ----------
        config : Dict
                 dictionary object containing task configuration

        *args : tuple
                Additional arguments to `Task`

        **kwargs : dict, optional
                   Extra keyword arguments to `Task`
        """

        # Store the config and arguments as attributes of the object
        self._config = AttrDict(config)

        for arg in args:
            setattr(self, str(arg), arg)

        for key, value in kwargs.items():
            setattr(self, key, value)

        # Create task_config with everything that is inside _config and whatever the user chooses to
        # extend it with when initializing a child subclass of Task. Only task_config should be referenced
        # in any application, not _config.
        self.task_config = self._config.deepcopy()

        # Any other composite runtime variables that may be needed for the duration of the task
        # can be constructed here

        # Construct the current cycle datetime object
        self.task_config['current_cycle'] = add_to_datetime(self._config['PDY'], to_timedelta(f"{self._config.cyc}H"))
        logger.debug(f"current cycle: {self.task_config['current_cycle']}")

        # Construct the previous cycle datetime object
        self.task_config['previous_cycle'] = add_to_datetime(self.task_config.current_cycle, -to_timedelta(f"{self._config['assim_freq']}H"))
        logger.debug(f"previous cycle: {self.task_config['previous_cycle']}")

        pass

    def initialize(self):
        """
        Initialize methods for a task
        """
        pass

    def configure(self):
        """
        Configuration methods for a task in preparation for execution
        """
        pass

    def execute(self):
        """
        Execute methods for a task
        """
        pass

    def finalize(self):
        """
        Methods for after the execution that produces output task
        """
        pass

    def clean(self):
        """
        Methods to clean after execution and finalization prior to closing out a task
        """
        pass
