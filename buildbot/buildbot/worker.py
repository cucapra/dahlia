import threading
import subprocess
import os
from .db import ARCHIVE_NAME, CODE_DIR, log, chdir
from contextlib import contextmanager
import traceback
import shlex
import glob

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
    """
    job = db.acquire(old_state, temp_state)
    task = JobTask(db, job)
    try:
        with chdir(task.dir):
            yield task
    except WorkError as exc:
        task.log(exc.message)
        task.set_state('failed')
    except Exception:
        task.log(traceback.format_exc())
        task.set_state('failed')
    else:
        task.set_state(done_state)


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
    with work(db, 'uploaded', 'unpacking', 'unpacked') as task:
        # Unzip the archive into the code directory.
        task.run(["unzip", "-d", CODE_DIR, "{}.zip".format(ARCHIVE_NAME)])

        # Check for single-directory zip files: if the code directory
        # only contains one subdirectory now, "collapse" it.
        code_contents = os.listdir(CODE_DIR)
        if len(code_contents) == 1:
            path = os.path.join(CODE_DIR, code_contents[0])
            if os.path.isdir(path):
                for fn in os.listdir(path):
                    os.rename(os.path.join(path, fn),
                              os.path.join(CODE_DIR, fn))
                task.log('collapsed directory {}'.format(code_contents[0]))


def stage_seashell(db, config):
    """Work stage: compile Seashell code to HLS C.
    """
    compiler = config["SEASHELL_COMPILER"]
    with work(db, 'unpacked', 'seashelling', 'seashelled') as task:
        if task['config'].get('skipseashell'):
            # Skip the Seashell stage. Instead, just try to guess which
            # file contains the hardware function. For now, this
            # guessing is very unintelligent: it just looks for some
            # *.cpp file not named "main".
            for name in os.listdir(CODE_DIR):
                base, ext = os.path.splitext(name)
                if ext == C_EXT and base != 'main':
                    c_name = name
                    break
            else:
                raise WorkError('no C source file found')

            task.log('skipping Seashell compilation stage')
            task['hw_basename'] = base
            return

        # Look for the Seashell source code.
        for name in os.listdir(CODE_DIR):
            _, ext = os.path.splitext(name)
            if ext == SEASHELL_EXT:
                source_name = name
                break
        else:
            raise WorkError('no source file found')
        task['seashell_main'] = name

        # Run the Seashell compiler.
        source_path = os.path.join(CODE_DIR, source_name)
        hls_code = task.run([compiler, source_path], log_stdout=False).stdout

        # A filename for the translated C code.
        base, _ = os.path.splitext(source_name)
        c_name = base + C_EXT
        task['hw_basename'] = base

        # Write the C code.
        with open(os.path.join(CODE_DIR, c_name), 'wb') as f:
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
    with work(db, 'seashelled', 'hlsing', 'hlsed') as task:
        hw_basename, hw_c, hw_o = _hw_filenames(task)
        xflags = ''

        # Run Xilinx SDSoC compiler for hardware functions.
        task.run(
            _sds_cmd(prefix, hw_basename, hw_c) + [
                '-c', '-MMD', '-MP', '-MF"vsadd.d"', hw_c,
                '-o', hw_o,
            ],
            timeout=120,
            cwd=CODE_DIR,
        )

        # Run the Xilinx SDSoC compiler for host function.
        task.run(
            _sds_cmd(prefix, hw_basename, hw_c) + [
                '-c', '-MMD', '-MP', '-MF"main.d"', C_MAIN,
                '-o', HOST_O,
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
            timeout=1800,
            cwd=CODE_DIR,
        )


def _copy_file(task, path, mode):
    if mode == 'scp':
        task.run(
            ['sshpass', '-p', 'root', 'scp', '-r', path, 'zb1:/mnt'],
            timeout=1200
        )
    else:
        task.run(['cp', '-r', path, 'sd_card/'])


def _copy_directory(task, path, mode):
    for i in glob.glob(os.path.join(path, '*')):
        _copy_file(task, i, mode)


def stage_cheat(db, config):
    """Work stage: Skip hls stage to test Areesh.
    """
    with work(db, 'seashelled', 'cheating', 'hlsed') as task:
        # Make sd card directory
        task.run(['mkdir', 'sd_card'])

        # Copy folder to current directory
        _copy_directory(task, '/home/opam/seashell/buildbot/instance/'
                              'jobs/n3EOVfKLock/code/sd_card', 'copy')


def stage_areesh(db, config):
    """Work stage: upload bitstream to the FPGA controller, run the
    program, and output the result gathered.
    """
    with work(db, 'hlsed', 'areeshing', 'done') as task:
        if task['config'].get('estimate'):
            # Skip the Areesh stage. Bit files not generated in
            # estimation stage.
            task.log('skipping run on FPGA stage')
            return

        # Upload bit stream to FPGA
        _copy_directory(task, 'sd_card', 'scp')

        # Restart the FPGA
        task.run(
            ['sshpass', '-p', 'root', 'ssh', 'zb1', '/sbin/reboot'],
            timeout=120
        )

        # Wait for restart
        task.run(
            ['sleep', '120'],
            timeout=1200
        )

        # Run the FPGA program and collect results
        task.run(
            ['sshpass', '-p', 'root', 'ssh', 'zb1', '/mnt/sdsoc'],
            timeout=120
        )


def work_threads(db, config):
    """Get a list of (unstarted) Thread objects for processing tasks.
    """
    out = []
    for stage in (stage_unpack, stage_seashell, stage_hls, stage_areesh):
        out.append(WorkThread(db, config, stage))
    return out
