# The extensions to allow for uploaded code archives.
UPLOAD_EXTENSIONS = ['zip']

# The executable name for the Seashell compiler (seac).
SEASHELL_COMPILER = 'seac'

# A prefix command to use *before* the invocations of Xilinx tools.
HLS_COMMAND_PREFIX = []

# Spawn threads inside the server process for the workers (instead of
# using a separate worker process). The default (None) means True in the
# development environment and False in production.
WORKER_THREADS = None

# Try to automatically launch a worker process if one is not already
# running. We detect a running worker process using the presence of the
# Unix domain socket. (This has no effect if WORKER_THREADS is True.)
WORKER_PROCESS = True

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
