import threading
import subprocess
import os
from .db import ARCHIVE_NAME, CODE_DIR, log
from contextlib import contextmanager
import traceback
import shlex
import time
from . import state

SEASHELL_EXT = '.fuse'
C_EXT = '.cpp'
OBJ_EXT = '.o'
SDS_PLATFORM = 'zed'
C_MAIN = 'main.cpp'  # Currently, the host code *must* be named this.
HOST_O = 'main.o'  # The .o file for host code.
EXECUTABLE = 'sdsoc'

# For executing on a Xilinx Zynq board.
ZYNQ_SSH_PREFIX = ['sshpass', '-p', 'root']  # Provide Zynq SSH password.
ZYNQ_HOST = 'zb1'
ZYNQ_REBOOT_DELAY = 40


class WorkError(Exception):
    """An error that occurs in a worker that needs to be displayed in
    the log.
    """
    def __init__(self, message):
        self.message = message


class JobTask:
    """A temporary acquisition of a job used to do a single unit of work
    on its behalf. Also, a container for lots of convenience methods for
    doing stuff with the job.

    Subscripting a task accesses the underlying job dict.
    """
    def __init__(self, db, job):
        self.db = db
        self.job = job

    def __getitem__(self, key):
        return self.job[key]

    def __setitem__(self, key, value):
        self.job[key] = value

    @property
    def dir(self):
        """The path the directory containing the job's files.
        """
        return self.db.job_dir(self.job['name'])

    @property
    def code_dir(self):
        """The path the job's directory for code and compilation.
        """
        return os.path.join(self.dir, CODE_DIR)

    def log(self, message):
        """Add an entry to the job's log.
        """
        log(self.job, message)

    def set_state(self, state):
        """Set the job's state.
        """
        self.db.set_state(self.job, state)

    def proc_log(self, cmd, proc, stdout=True, stderr=True):
        """Log the output of a command run on behalf of a job.

        Provide the command (a list of arguments), and the process
        (e.g., returned from `run`. Specify whether to include the
        stdout and stderr streams.
        """
        out = '$ ' + _cmd_str(cmd)
        streams = []
        if stdout:
            streams.append(proc.stdout)
        if stderr:
            streams.append(proc.stderr)
        out += _stream_text(*streams)
        self.log(out)

    def run(self, cmd, log_stdout=True, timeout=60, cwd='', **kwargs):
        """Run a command and log its output.

        Return an exited process object, with output captured (in `stdout`
        and `stderr`). The `log_stdout` flag determines whether the stdout
        stream is included in the log; set this to False if the point of
        running the command is to collect data from stdout. Additional
        arguments are forwarded to `subprocess.run`.
        """
        kwargs['timeout'] = timeout
        kwargs['cwd'] = os.path.normpath(os.path.join(self.dir, cwd))
        proc = run(cmd, **kwargs)
        self.proc_log(cmd, proc, stdout=log_stdout)
        return proc


@contextmanager
def work(db, old_state, temp_state, done_state):
    """A context manager for acquiring a job temporarily in an
    exclusive way to work on it. Produce a `JobTask`.
    Done state can either be a valid state string or a function that
    accepts a Task object and returns a valid state string.
    """
    job = db.acquire(old_state, temp_state)
    task = JobTask(db, job)
    if isinstance(done_state, str):
        done_state = lambda _: done_state
    try:
        yield task
    except WorkError as exc:
        task.log(exc.message)
        task.set_state(state.FAIL)
    except Exception:
        task.log(traceback.format_exc())
        task.set_state(state.FAIL)
    else:
        task.set_state(done_state(task))

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
    """Run a command, like `subprocess.run`, while capturing output.
    Raise an appropriate `WorkError` if the command fails.

    `cmd` must be a list of arguments.
    """
    try:
        return subprocess.run(
            cmd,
            check=True,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
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


def should_make(task):
    """Run stage_make if make was specified in the task config.
    """
    if task['config'].get('make'):
        return state.MAKE
    else:
        return state.UNPACK_FINISH

def stage_unpack(db, config):
    """Work stage: unpack source code.
    """
    with work(db, state.UPLOAD, state.UNPACK, should_make) as task:
        # Unzip the archive into the code directory.
        os.mkdir(task.code_dir)
        task.run(["unzip", "-d", task.code_dir, "{}.zip".format(ARCHIVE_NAME)])

        # Check for single-directory zip files: if the code directory
        # only contains one subdirectory now, "collapse" it.
        code_contents = os.listdir(task.code_dir)
        if len(code_contents) == 1:
            path = os.path.join(task.code_dir, code_contents[0])
            if os.path.isdir(path):
                for fn in os.listdir(path):
                    os.rename(os.path.join(path, fn),
                              os.path.join(task.code_dir, fn))
                task.log('collapsed directory {}'.format(code_contents[0]))

def stage_make(db, config):
    """Work stage: run make command. Assumes that at the end of the make command,
    work equivalent to the stage_hls is done, i.e., either estimation data has
    been generated or a bitstream has been generated.
    """
    prefix = config["HLS_COMMAND_PREFIX"]
    with work(db, state.MAKE, state.MAKE_PROGRESS, state.HLS_FINISH) as task:
        sdsflags = ''
        # If estimation is requested, pass in estimation flag
        if task['config'].get('estimate'):
            sdsflags += '-perf-est-hw-only'

        # NOTE(rachit): The timeout here is really high because synthesis takes
        # a real long time.
        task.run(prefix + ['make', 'SDSFLAGS={}'.format(sdsflags)],
                 timeout=3600,
                 cwd=CODE_DIR)

def stage_seashell(db, config):
    """Work stage: compile Seashell code to HLS C.
    """
    compiler = config["SEASHELL_COMPILER"]
    with work(db, state.UNPACK_FINISH, state.COMPILE, state.COMPILE_FINISH) as task:
        if task['config'].get('skipseashell'):
            # Skip the Seashell stage. Instead, just try to guess which
            # file contains the hardware function. For now, this
            # guessing is very unintelligent: it just looks for some
            # *.cpp file not named "main".
            for name in os.listdir(task.code_dir):
                base, ext = os.path.splitext(name)
                if ext == C_EXT and base != 'main':
                    c_name = name
                    break
            else:
                raise WorkError(
                    'No hardware source file found.'
                    ' Expected a file with extension {} and basename not `main`.'.format(C_EXT)
                )

            task.log('skipping Fuse compilation stage')
            task['hw_basename'] = base
            return

        # Look for the Seashell source code.
        for name in os.listdir(task.code_dir):
            _, ext = os.path.splitext(name)
            if ext == SEASHELL_EXT:
                source_name = name
                break
        else:
            raise WorkError('No Fuse source file found.'
                            ' Expected a file with extension {}'.format(SEASHELL_EXT))
        task['seashell_main'] = name

        # Run the Seashell compiler.
        source_path = os.path.join(task.code_dir, source_name)
        hls_code = task.run([compiler, source_path], log_stdout=False).stdout

        # A filename for the translated C code.
        base, _ = os.path.splitext(source_name)
        c_name = base + C_EXT
        task['hw_basename'] = base

        # Write the C code.
        with open(os.path.join(task.code_dir, c_name), 'wb') as f:
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


def _hw_filenames(task):
    """For a given task, get its hardware source file's basename, C
    filename, and object filename.
    """
    hw_basename = task['hw_basename']
    return hw_basename, hw_basename + C_EXT, hw_basename + OBJ_EXT


def stage_hls(db, config):
    """Work stage: compile C code to object files and then to an FPGA
    bitstream with HLS toolchain.
    """
    prefix = config["HLS_COMMAND_PREFIX"]
    with work(db, state.COMPILE_FINISH, state.HLS, state.HLS_FINISH) as task:
        hw_basename, hw_c, hw_o = _hw_filenames(task)
        xflags = ''

        # Run Xilinx SDSoC compiler for hardware functions.
        task.run(
            _sds_cmd(prefix, hw_basename, hw_c) + [
                '-c',
                hw_c, '-o', hw_o,
            ],
            timeout=120,
            cwd=CODE_DIR,
        )

        # Run the Xilinx SDSoC compiler for host function.
        task.run(
            _sds_cmd(prefix, hw_basename, hw_c) + [
                '-c',
                C_MAIN, '-o', HOST_O,
            ],
            cwd=CODE_DIR,
        )

        if task['config'].get('estimate'):
            xflags = '-perf-est-hw-only'

        # Run Xilinx SDSoC compiler for created objects.
        task.run(
            _sds_cmd(prefix, hw_basename, hw_c) + [
                xflags, hw_o, HOST_O, '-o', EXECUTABLE,
            ],
            timeout=3000,
            cwd=CODE_DIR,
        )


def stage_fpga_execute(db, config):
    """Work stage: upload bitstream to the FPGA controller, run the
    program, and output the results.

    This stage currently assumes we want to execute on a Xilinx Zynq
    board, which is accessible via SSH. We require `sshpass` to provide
    the password for the board (because the OS that comes with ZedBoards
    hard-codes the root password as root---not terribly secure, so the
    board should clearly not be on a public network).
    """
    with work(db, state.HLS_FINISH, state.RUN, state.DONE) as task:
        # Do nothing in this stage if we're just running estimation.
        if task['config'].get('estimate'):
            task.log('skipping FPGA execution stage')
            return

        # Copy the compiled code (CPU binary + FPGA bitstream) to the
        # Zynq board.
        bin_dir = os.path.join(task.code_dir, 'sd_card')
        bin_files = [os.path.join(bin_dir, f) for f in os.listdir(bin_dir)]
        dest = ZYNQ_HOST + ':/mnt'
        task.run(
            ZYNQ_SSH_PREFIX + ['scp', '-r'] + bin_files + [dest],
            timeout=1200
        )

        # Restart the FPGA and wait for it to come back up.
        task.run(
            ZYNQ_SSH_PREFIX + ['ssh', ZYNQ_HOST, '/sbin/reboot'],
        )
        task.log('waiting {} seconds for reboot'.format(ZYNQ_REBOOT_DELAY))
        time.sleep(ZYNQ_REBOOT_DELAY)

        # Run the FPGA program and collect results
        task.run(
            ZYNQ_SSH_PREFIX + ['ssh', ZYNQ_HOST, '/mnt/' + EXECUTABLE],
            timeout=120
        )


def work_threads(db, config):
    """Get a list of (unstarted) Thread objects for processing tasks.
    """
    out = []
    for stage in (stage_unpack, stage_make, stage_seashell, stage_hls, stage_fpga_execute):
        out.append(WorkThread(db, config, stage))
    return out
