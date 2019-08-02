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

# The number of jobs to process in parallel in the "make" stage (which is the
# expensive, long-running one).
PARALLELISM_MAKE = 1

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
    'rpt',
    'est',
]

# Configuration options allowed during job creation. Each option has a
# conversion function (i.e., type) used to translate the request value.
CONFIG_OPTIONS = {
    'skipseashell': bool,
    'estimate': bool,
    'skipexec': bool,
    'directives': str,
    'make': bool,
    'hwname': str,
    'sdsflags': str,
    'platform': str,
    'mode': str,
}

# The name to use for compiled executables.
EXECUTABLE_NAME = 'sdsoc'

# The number of (recent) lines of the log to show on job pages.
LOG_PREVIEW_LINES = 32

# The timeouts for running the initial compilation step and for running
# the synthesis step (or running an opaque Makefile), the latter of
# which has to be really long because synthesis is so slow.
COMPILE_TIMEOUT = 120
SYNTHESIS_TIMEOUT = 9000

# The Buildbot currently supports two backend toolchains: Xilinx's SDSoC
# (for Zynq processors) and SDAccel (for AWS F1). Set this to "f1" for
# deployment on F1; leave it as anything else for the SDSoC workflow.
TOOLCHAIN = 'f1'

# Options for SDSoC/Zynq.
DEFAULT_PLATFORM = 'zed'  # Use the "platform" job config option to override.

# Options for SDAccel/F1.
DEFAULT_F1_MODE = 'sw_emu'  # Use the "mode" job config option to override.
S3_BUCKET = 'test-bucket-1025132741'
S3_DCP = 'DCPs'  # dcp-folder-name
S3_LOG = 'SDAccel_log'  # logs-folder-name

#sleep time between each AFI status check
AFI_CHECK_INTERVAL = 300 