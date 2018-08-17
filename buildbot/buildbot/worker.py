import threading
import subprocess
import os
from .db import ARCHIVE_NAME, CODE_DIR, log, chdir
from contextlib import contextmanager
import traceback
import shlex

SEASHELL_EXT = '.sea'
C_EXT = '.cpp'
OBJ_EXT = '.o'
SDS_PLATFORM = 'zed'
C_MAIN = 'main.cpp'  # Currently, the host code *must* be named this.
HOST_O = 'main.o'  # The .o file for host code.
EXECUTABLE = 'sdsoc'


class WorkError(Exception):
    """An error that occurs in a worker that needs to be displayed in
    the log.
    """
    def __init__(self, message):
        self.message = message


@contextmanager
def work(db, old_state, temp_state, done_state):
    """A context manager for acquiring a job temporarily in an
    exclusive way to work on it.

    While inside the context, the working directory is changed to the
    job's private work directory.
    """
    job = db.acquire(old_state, temp_state)
    job_dir = db.job_dir(job['name'])
    try:
        with chdir(job_dir):
            yield job
    except WorkError as exc:
        log(job, exc.message)
        db.set_state(job, 'failed')
    except Exception:
        log(job, traceback.format_exc())
        db.set_state(job, 'failed')
    else:
        db.set_state(job, done_state)


def _stream_text(*args):
    """Given some bytes objects, return a string listing all the
    non-empty ones, delimited by a separator and starting with a
    newline (if any part is nonempty).
    """
    out = '\n---\n'.join(b.decode('utf8', 'ignore')
                         for b in args if b)
    if out:
        return '\n' + out
    else:
        return ''


def _cmd_str(cmd):
    """Given a list of command-line arguments, return a human-readable
    string for logging.
    """
    return ' '.join(shlex.quote(p) for p in cmd)


def run(cmd, **kwargs):
    """Run a command, like `subprocess.run`, while capturing output. Log an
    appropriate error if the command fails.

    `cmd` must be a list of arguments.
    """
    try:
        return subprocess.run(
            cmd,
            check=True,
            capture_output=True,
            **kwargs
        )
    except subprocess.CalledProcessError as exc:
        raise WorkError('$ {}\ncommand failed ({}){}'.format(
            _cmd_str(cmd),
            exc.returncode,
            _stream_text(exc.stdout, exc.stderr),
        ))
    except FileNotFoundError as exc:
        raise WorkError('$ {}\ncommand {} not found'.format(
            _cmd_str(cmd),
            exc.filename,
        ))
    except subprocess.TimeoutExpired as exc:
        raise WorkError('$ {}\ntimeout after {} seconds{}'.format(
            _cmd_str(cmd),
            exc.timeout,
            _stream_text(exc.stdout, exc.stderr),
        ))


def proc_log(job, cmd, proc, stdout=True, stderr=True):
    """Log the output of a command run on behalf of a job.

    Provide the job, the command (a list of arguments), and the process
    (e.g., returned from `run`. Specify whether to include the stdout
    and stderr streams.
    """
    out = '$ ' + _cmd_str(cmd)
    streams = []
    if stdout:
        streams.append(proc.stdout)
    if stderr:
        streams.append(proc.stderr)
    out += _stream_text(*streams)
    log(job, out)


def runl(job, cmd, log_stdout=True, timeout=60, **kwargs):
    """Run a command and log its output.

    Return an exited process object, with output captured (in `stdout`
    and `stderr`). The `log_stdout` flag determines whether the stdout
    stream is included in the log; set this to False if the point of
    running the command is to collect data from stdout. Additional
    arguments are forwarded to `subprocess.run`.
    """
    kwargs['timeout'] = timeout
    proc = run(cmd, **kwargs)
    proc_log(job, cmd, proc, stdout=log_stdout)
    return proc


class WorkThread(threading.Thread):
    """A base class for all our worker threads, which run indefinitely
    to process tasks in an appropriate state.

    The thread takes the database and configuration dictionaries as well
    as a function to which these will be passed. When the thread runs,
    the function is invoked repeatedly, indefinitely.
    """

    def __init__(self, db, config, func):
        self.db = db
        self.config = config
        self.func = func
        super(WorkThread, self).__init__(daemon=True)

    def run(self):
        while True:
            self.func(self.db, self.config)


def stage_unpack(db, config):
    """Work stage: unpack source code.
    """
    with work(db, 'uploaded', 'unpacking', 'unpacked') as job:
        # Unzip the archive into the code directory.
        runl(job, ["unzip", "-d", CODE_DIR, "{}.zip".format(ARCHIVE_NAME)])

        # Check for single-directory zip files: if the code directory
        # only contains one subdirectory now, "collapse" it.
        code_contents = os.listdir(CODE_DIR)
        if len(code_contents) == 1:
            path = os.path.join(CODE_DIR, code_contents[0])
            if os.path.isdir(path):
                print(path, os.listdir(path))
                for fn in os.listdir(path):
                    os.rename(os.path.join(path, fn),
                              os.path.join(CODE_DIR, fn))
                log(job, 'collapsed directory {}'.format(code_contents[0]))


def stage_seashell(db, config):
    """Work stage: compile Seashell code to HLS C.
    """
    compiler = config["SEASHELL_COMPILER"]
    with work(db, 'unpacked', 'seashelling', 'seashelled') as job:
        # Look for the Seashell source code.
        code_dir = os.path.join(db.job_dir(job['name']), CODE_DIR)
        for name in os.listdir(code_dir):
            _, ext = os.path.splitext(name)
            if ext == SEASHELL_EXT:
                source_name = name
                break
        else:
            raise WorkError('no source file found')
        job['seashell_main'] = name

        # Read the source code.
        with open(os.path.join(code_dir, source_name), 'rb') as f:
            code = f.read()

        # Run the Seashell compiler.
        hls_code = runl(job, [compiler], log_stdout=False, input=code).stdout

        # A filename for the translated C code.
        base, _ = os.path.splitext(source_name)
        c_name = base + C_EXT
        job['hw_basename'] = base

        # Write the C code.
        with open(os.path.join(code_dir, c_name), 'wb') as f:
            f.write(hls_code)


def _sds_cmd(prefix, func_hw, c_hw):
    """Make a sds++ command with all our standard arguments.
    """
    return prefix + [
        'sds++',
        '-sds-pf', SDS_PLATFORM,
        '-sds-hw', func_hw, c_hw, '-sds-end',
        '-clkid', '3',
        '-poll-mode', '1',
        '-verbose', '-Wall', '-O3',
    ]


def _hw_filenames(job):
    """For a given job, get its hardware source file's basename, C
    filename, and object filename.
    """
    hw_basename = job['hw_basename']
    return hw_basename, hw_basename + C_EXT, hw_basename + OBJ_EXT


def stage_hls(db, config):
    """Work stage: compile C code to O files with HLS toolchain.
    """
    prefix = config["HLS_COMMAND_PREFIX"]
    with work(db, 'seashelled', 'hlsing', 'hlsed') as job:
        hw_basename, hw_c, hw_o = _hw_filenames(job)

        # Run Xilinx SDSoC compiler for hardware functions.
        runl(
            job,
            _sds_cmd(prefix, hw_basename, hw_c) + [
                '-c', '-MMD', '-MP', '-MF"vsadd.d"', hw_c,
                '-o', hw_o,
            ],
            timeout=120,
            cwd=CODE_DIR
        )

        # Run the Xilinx SDSoC compiler for host function.
        runl(
            job,
            _sds_cmd(prefix, hw_basename, hw_c) + [
                '-c', '-MMD', '-MP', '-MF"main.d"', C_MAIN,
                '-o', HOST_O,
            ],
            cwd=CODE_DIR
        )


def stage_timeout(db, config):
    with work(db, 'hlsed', 'pondering', 'timedout') as job:
        runl(
            job,
            ['sleep','45'],
            cwd=CODE_DIR,
        )

def stage_synth(db, config):
    """Work stage: compile O files to bitstream with HLS toolchain.
    """
    prefix = config["HLS_COMMAND_PREFIX"]
    with work(db, 'timedout', 'synthing', 'synthed') as job:
        hw_basename, hw_c, hw_o = _hw_filenames(job)

        # Run Xilinx SDSoC compiler for created objects.
        runl(
            job,
            _sds_cmd(prefix, hw_basename, hw_c) + [
                hw_o, HOST_O, '-o', EXECUTABLE,
            ],
            timeout=1800,
            cwd=CODE_DIR,
        )


def work_threads(db, config):
    """Get a list of (unstarted) Thread objects for processing tasks.
    """
    out = []
    for stage in (stage_unpack, stage_seashell, stage_hls, stage_timeout, stage_synth):
        out.append(WorkThread(db, config, stage))
    return out
