import threading
import subprocess
import os
from .db import ARCHIVE_NAME, CODE_DIR
from contextlib import contextmanager
import traceback
import shlex
import time
from . import state
import json
import glob

SEASHELL_EXT = '.fuse'
C_EXT = '.cpp'
OBJ_EXT = '.o'
C_MAIN = 'main.cpp'  # Currently, the host code *must* be named this.
HOST_O = 'main.o'  # The .o file for host code.

# For executing on a Xilinx Zynq board.
ZYNQ_SSH_PREFIX = ['sshpass', '-p', 'root']  # Provide Zynq SSH password.
ZYNQ_HOST = 'zb1'
ZYNQ_DEST_DIR = '/mnt'
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
        self.db.log(self.job['name'], message)

    def set_state(self, state):
        """Set the job's state.
        """
        self.db.set_state(self.job, state)

    def run(self, cmd, capture=False, timeout=60, cwd='', **kwargs):
        """Run a command and log its output.

        Return an exited process object. If `capture`, then  the
        standard output is *not* logged and is instead available as the return
        value's `stdout` field. Additional arguments are forwarded to
        `subprocess.run`.

        Raise an appropriate `WorkError` if the command fails.
        """
        full_cwd = os.path.normpath(os.path.join(self.dir, cwd))
        self.log('$ {}'.format(_cmd_str(cmd)))

        log_filename = self.db._log_path(self.job['name'])
        with open(log_filename, 'ab') as f:
            try:
                return subprocess.run(
                    cmd,
                    check=True,
                    stdout=subprocess.PIPE if capture else f,
                    stderr=f,
                    timeout=timeout,
                    cwd=full_cwd,
                    **kwargs,
                )
            except subprocess.CalledProcessError as exc:
                raise WorkError('command failed ({})'.format(
                    exc.returncode,
                ))
            except FileNotFoundError as exc:
                raise WorkError('command {} not found'.format(
                    exc.filename,
                ))
            except subprocess.TimeoutExpired as exc:
                raise WorkError('timeout after {} seconds{}'.format(
                    exc.timeout,
                ))


@contextmanager
def work(db, old_state, temp_state, done_state_or_func):
    """A context manager for acquiring a job temporarily in an
    exclusive way to work on it. Produce a `JobTask`.
    Done state can either be a valid state string or a function that
    accepts a Task object and returns a valid state string.
    """
    done_func = None
    if isinstance(done_state_or_func, str):
        done_func = lambda _: done_state_or_func  # noqa
    else:
        done_func = done_state_or_func

    job = db.acquire(old_state, temp_state)
    task = JobTask(db, job)
    try:
        yield task
    except WorkError as exc:
        task.log(exc.message)
        task.set_state(state.FAIL)
    except Exception:
        task.log(traceback.format_exc())
        task.set_state(state.FAIL)
    else:
        task.set_state(done_func(task))


def _cmd_str(cmd):
    """Given a list of command-line arguments, return a human-readable
    string for logging.
    """
    return ' '.join(shlex.quote(p) for p in cmd)


def _task_config(task, config):
    """Interpret some configuration options on a task, and assign the
    task's `sdsflags` and `platform` fields so they can be used
    directly.
    """
    flags = task['config'].get('sdsflags') or ''
    est = 0
    if task['config'].get('estimate'):
        flags += ' -perf-est-hw-only'
        est = 1
    task['sdsflags'] = flags
    task['estimate'] = est

    task['platform'] = task['config'].get('platform') or \
        config['DEFAULT_PLATFORM']
    task['target'] = task['config'].get('target') or \
        config['DEFAULT_F1_TARGET']


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
    """Work stage: run make command. Assumes that at the end of the make
    command, work equivalent to the stage_hls is done, i.e., either
    estimation data has been generated or a bitstream has been
    generated.
    """
    # After this stage, transfer either to F1-style execution (i.e., AFI
    # generation) or ordinary execution (for the Zynq toolchain).
    def stage_after_make(task):
        if config['TOOLCHAIN'] == 'f1' and task['target'] == 'hw':
            return state.AFI_START
        else:
            return state.HLS_FINISH

    prefix = config["HLS_COMMAND_PREFIX"]
    with work(db, state.MAKE, state.MAKE_PROGRESS, stage_after_make) as task:
        _task_config(task, config)

        if task['sdsflags']:
            task.log('WARNING: make stage is ignoring sdsflags={}'.format(
                task['sdsflags']
            ))

        if config['TOOLCHAIN'] == 'f1':
            # Get the AWS platform ID for F1 builds.
            platform_script = (
                'cd $AWS_FPGA_REPO_DIR ; '
                'source ./sdaccel_setup.sh > /dev/null ; '
                'echo $AWS_PLATFORM'
            )
            proc = task.run([platform_script], capture=True, shell = True)
            aws_platform = proc.stdout.decode('utf8').strip()

            make = [
                'make',
                'TARGET={}'.format(task['target']),
                'DEVICE={}'.format(aws_platform),
            ]

        else:
            # Simple make invocation for SDSoC.
            make = [
                'make',
                'ESTIMATE={}'.format(task['estimate']),
                'PLATFORM={}'.format(task['platform']),
                'TARGET={}'.format(config['EXECUTABLE_NAME']),
            ]

        make_cmd = prefix + make
        if task['config']['directives']:
            make_cmd.append(
                'DIRECTIVES={}'.format(task['config']['directives'])
            )

        task.run(
            make_cmd,
            timeout=config["SYNTHESIS_TIMEOUT"],
            cwd=CODE_DIR,
        )


def stage_seashell(db, config):
    """Work stage: compile Seashell code to HLS C.
    """
    compiler = config["SEASHELL_COMPILER"]
    with work(db, state.UNPACK_FINISH, state.COMPILE,
              state.COMPILE_FINISH) as task:
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
                    'No hardware source file found. Expected a file with '
                    'extension {} and basename not `main`.'.format(C_EXT)
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
            raise WorkError('No Fuse source file found. Expected a file '
                            'with extension {}'.format(SEASHELL_EXT))
        task['seashell_main'] = name

        # Run the Seashell compiler.
        source_path = os.path.join(task.code_dir, source_name)
        hls_code = task.run([compiler, source_path], capture=True).stdout

        # A filename for the translated C code.
        base, _ = os.path.splitext(source_name)
        c_name = base + C_EXT
        task['hw_basename'] = base

        # Write the C code.
        with open(os.path.join(task.code_dir, c_name), 'wb') as f:
            f.write(hls_code)


def _sds_cmd(prefix, task):
    """Make a sds++ command with all our standard arguments.
    """
    hw_basename, hw_c, hw_o = _hw_filenames(task)
    sds_cmd_head = prefix + [
        'sds++',
        '-sds-pf', task['platform'],
    ]
    sds_cmd_tail = [
        '-clkid', '3',
        '-poll-mode', '1',
        '-verbose', '-Wall', '-O3',
    ]
    if task['config']['directives']:
        return sds_cmd_head + [
            '-sds-hw', hw_basename, hw_c, '-hls-tcl',
            task['config']['directives'], '-sds-end',
        ] + sds_cmd_tail
    else:
        return sds_cmd_head + [
            '-sds-hw', hw_basename, hw_c, '-sds-end',
        ] + sds_cmd_tail


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
        _task_config(task, config)
        flags = shlex.split(task['sdsflags'])
        sds_cmd = _sds_cmd(prefix, task)
        hw_basename, hw_c, hw_o = _hw_filenames(task)

        # Run Xilinx SDSoC compiler for hardware functions.
        task.run(
            sds_cmd + flags + [
                '-c',
                hw_c, '-o', hw_o,
            ],
            timeout=config["COMPILE_TIMEOUT"],
            cwd=CODE_DIR,
        )

        # Run the Xilinx SDSoC compiler for host function.
        task.run(
            sds_cmd + flags + [
                '-c',
                C_MAIN, '-o', HOST_O,
            ],
            cwd=CODE_DIR,
        )

        # Run Xilinx SDSoC compiler for created objects.
        task.run(
            sds_cmd + flags + [
                hw_o, HOST_O, '-o', config['EXECUTABLE_NAME'],
            ],
            timeout=config["SYNTHESIS_TIMEOUT"],
            cwd=CODE_DIR,
        )

        # Copy datafiles to the executable directory.
        data_files = [
            os.path.join(task.code_dir, f)
            for f in os.listdir(task.code_dir)
            if f.endswith('.data')
        ]
        if data_files:
            dest = os.path.join(task.code_dir, 'sd_card')
            task.run(
                ['cp'] + data_files + [dest]
            )


def stage_afi(db, config):
    """Work stage: create the AWS FPGA binary and AFI from the *.xclbin
    (Xilinx FPGA binary file).
    """
    with work(db, state.AFI_START, state.AFI, state.HLS_FINISH) as task:
        # Clean up any generated files from previous runs.
        task.run(
            ['rm -rf to_aws *afi_id.txt \
                *.tar *agfi_id.txt manifest.txt'],
            cwd=os.path.join(CODE_DIR, 'xclbin'),
            shell = True
        )

        # Find *.xclbin file from hardware synthesis.
        xcl_dir = os.path.join(task.dir, 'code', 'xclbin')
        xclbin_file_path = glob.glob(
            os.path.join(xcl_dir, '*hw.*.xclbin'))
        xclbin_file = os.path.basename(xclbin_file_path[0])

        # Generate the AFI and AWS binary.
        afi_script = (
            'cur=`pwd` ; '
            'cd $AWS_FPGA_REPO_DIR ; '
            'source ./sdaccel_setup.sh > /dev/null ; '
            'cd $cur/xclbin ; '
            '$SDACCEL_DIR/tools/create_sdaccel_afi.sh '
            '-xclbin={} '
            '-s3_bucket={} '
            '-s3_dcp_key={} '
            '-s3_logs_key={}'.format(
                xclbin_file,
                config['S3_BUCKET'],
                config['S3_DCP'],
                config['S3_LOG'],
            )
        )
        task.run([afi_script], cwd=CODE_DIR, shell= True)

        # Every 5 minutes, check if the AFI is ready.
        while True:
            time.sleep(300)

            # Get the AFI ID.
            afi_id_files = glob.glob(os.path.join(xcl_dir, '*afi_id.txt'))
            with open(afi_id_files[0]) as f:
                afi_id = json.loads(f.read())['FpgaImageId']

            # Check the status of the AFI.
            status_string = task.run(
                ['aws', 'ec2', 'describe-fpga-images',
                 '--fpga-image-ids', afi_id],
                cwd=CODE_DIR,
                capture=True
            )
            status_json = json.loads(status_string.stdout)

            # When the AFI becomes available, exit the loop and enter
            # execution stage.
            status = status_json['FpgaImages'][0]['State']['Code']
            task.log('AFI status: {}'.format(status))
            if status == 'available':
                break


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
        if task['config'].get('estimate') or task['config'].get('skipexec'):
            task.log('skipping FPGA execution stage')
            return

        if config['TOOLCHAIN'] == 'f1':
            # On F1, use the run either the real hardware-augmented
            # binary or the emulation executable.
            if task['target'] == 'hw':
                exe_cmd = ['sudo', 'sh', '-c',
                           'source /opt/xilinx/xrt/setup.sh ; ./host']
            else:
                exe_cmd = ['cur=`pwd`; cd $AWS_FPGA_REPO_DIR ;\
                source ./sdaccel_setup.sh > /dev/null; \
                cd $cur; XCL_EMULATION_MODE={} ./host'.format(task['target'])]
            task.run(
                exe_cmd,
                cwd=CODE_DIR,
                shell = True
            )

        else:
            # Copy the compiled code (CPU binary + FPGA bitstream) to the
            # Zynq board.
            bin_dir = os.path.join(task.code_dir, 'sd_card')
            bin_files = [os.path.join(bin_dir, f) for f in os.listdir(bin_dir)]
            dest = '{}:{}'.format(ZYNQ_HOST, ZYNQ_DEST_DIR)
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
                ZYNQ_SSH_PREFIX + [
                    'ssh', ZYNQ_HOST,
                    'cd {}; ./{}'.format(ZYNQ_DEST_DIR,
                                         config['EXECUTABLE_NAME']),
                ],
                timeout=120
            )


STAGES = (stage_unpack, stage_make, stage_seashell, stage_hls,
          stage_fpga_execute)


def work_threads(db, config):
    """Get a list of (unstarted) Thread objects for processing tasks.
    """
    stages = list(STAGES) + \
        [stage_make for i in range(config['PARALLELISM_MAKE'] - 1)]
    if config['TOOLCHAIN'] == 'f1':
        stages.append(stage_afi)
    return [WorkThread(db, config, stage) for stage in stages]
