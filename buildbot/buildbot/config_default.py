# The extensions to allow for uploaded code archives.
UPLOAD_EXTENSIONS = ['zip']

# The executable name for the Seashell compiler (seac).
SEASHELL_COMPILER = 'fuse'

# A prefix command to use *before* the invocations of Xilinx tools. For
# example, if your deployment needs to execute the Xilinx tools on a different
# machine or a different Docker container, a deployment could use this prefix
# to send these commands there.
HLS_COMMAND_PREFIX = []

# Spawn threads inside the server process for the workers (instead of
# using a separate worker process). The default (None) means True in the
# development environment and False in production.
WORKER_THREADS = None

# Filename extensions to send as plain text for job file viewing.
TEXT_EXTENSIONS = [
    'ss',
    'sea'
    'c',
    'cpp',
    's',
    'log',
    'jou',
    'tcl',
    'd',
    'dat',
]

# Configuration options allowed during job creation.
CONFIG_OPTIONS = [
    'skipseashell',
    'estimate',
    'make',
]

# The number of (recent) lines of the log to show on job pages.
LOG_PREVIEW_LINES = 5
