import threading
import subprocess
import os
from .db import ARCHIVE_NAME, CODE_DIR, log
from contextlib import contextmanager
import traceback
import shlex

SEASHELL_EXT = '.ss'
C_EXT = '.c'


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
    """
    job = db.acquire(old_state, temp_state)
    try:
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
        raise WorkError('command failed ({}): {}{}'.format(
            exc.returncode,
            _cmd_str(cmd),
            _stream_text(exc.stdout, exc.stderr),
        ))
    except FileNotFoundError as exc:
        raise WorkError('command {} not found: {}'.format(
            exc.filename,
            _cmd_str(cmd),
        ))


def _log_cmd(job, cmd, proc, stdout=True, stderr=True):
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
        proc = subprocess.run(
            ["unzip", "-d", CODE_DIR, "{}.zip".format(ARCHIVE_NAME)],
            cwd=db.job_dir(job['name']),
            check=True,
            capture_output=True,
        )
        log(job, proc.stdout.decode('utf8', 'ignore'))


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
        proc = run([compiler], input=code)
        _log_cmd(job, [compiler], proc, stdout=False)
        hls_code = proc.stdout

        # A filename for the translated C code.
        base, _ = os.path.splitext(source_name)
        c_name = base + C_EXT
        job['c_main'] = c_name

        # Write the C code.
        with open(os.path.join(code_dir, c_name), 'wb') as f:
            f.write(hls_code)


def stage_hls(db, config):
    """Work stage: compile C code with HLS toolchain.
    """
    with work(db, 'seashelled', 'hlsing', 'hlsed') as job:
        code_dir = os.path.join(db.job_dir(job['name']), CODE_DIR)
        c_main = job['c_main']

        # Run the Xilinx SDSoC compiler.
        cmd = ['sds++', '-c', c_main]
        proc = run(cmd, cwd=code_dir)
        _log_cmd(job, cmd, proc)


def work_threads(db, config):
    """Get a list of (unstarted) Thread objects for processing tasks.
    """
    out = []
    for stage in (stage_unpack, stage_seashell, stage_hls):
        out.append(WorkThread(db, config, stage))
    return out
