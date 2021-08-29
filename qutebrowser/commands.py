# commands.py --- custom command definitions

### Commentary:

# These are custom commands defined using the internal (but with the
# planned Extensions API possibly to be external some time in the
# future) APIs of Qutebrowser.  Caution is needed when writing these
# given these could break stuff at the run time.


### Code:

from qutebrowser.api import cmdutils


# The general format of a command definition is as follows:

@cmdutils.register()
def test_I_can_register_commands(some_flag: bool = False):
    "Just an example command."
    pass

