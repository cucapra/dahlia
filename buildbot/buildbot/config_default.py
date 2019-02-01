# The extensions to allow for uploaded code archives.
UPLOAD_EXTENSIONS = ['zip']

# The executable name for the Seashell compiler (seac).
SEASHELL_COMPILER = 'fuse'

# A prefix command to use *before* the invocations of Xilinx tools.
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
]
