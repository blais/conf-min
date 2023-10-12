# Link from ~/.ipython/profile_default/startup/00-startup.py

from IPython.core import interactiveshell
import builtins
import pandas
import pprint

# Provide some builtins.
builtins.pp = pprint.pprint
builtins.pf = pprint.pformat

# Expand the maximum number of columns (we have large tables).
pandas.set_option("display.max_columns", 500)

# Expand the maximum number of columns (we have large tables).
pandas.set_option("display.max_rows", 5000)

# cell_display: str, optional, Specifies what code a shell should output.
#   Options are: ["all", "last", "last_expr", "none", "last_expr_or_assign"]
interactiveshell.InteractiveShell.ast_node_interactivity = "all"
