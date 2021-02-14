import os
import sys

from e3.fs import sync_tree
from e3.testsuite.driver.diff import DiffTestDriver
from e3.testsuite.result import TestStatus

class AdaMainDriver(DiffTestDriver):
    """
    Test driver to compile and run a "main.adb" Ada source file.
    """

    # This is a workaround for Windows, where attempting to use rlimit by e3-core
    # causes permission errors. TODO: remove once e3-core has a proper solution.
    @property
    def default_process_timeout(self):
        return None

    @property
    def output_file(self):
        return os.path.join(self.working_dir(), self.test_name + '.result')

    @property
    def baseline_file(self):
        return ("expected.out", False)

    def run(self):
        env = dict(os.environ)

        gprfile = os.path.join(self.test_env['working_dir'], "prj.gpr")

        dir_path = os.path.dirname(os.path.realpath(__file__))
        support_dir = os.path.join(dir_path, '..', 'support')

        with open(gprfile, 'w') as f:
            f.write('with "usb_embedded";\n')
            f.write('project Prj is\n')
            f.write('  for Source_Dirs use (".", "%s");\n' % support_dir)
            f.write('  for Main use ("main.adb");\n')
            f.write('   package Binder is');
            f.write('      for Switches ("Ada") use ("-Es", "-g", "-static");');
            f.write('   end Binder;');
            f.write('end Prj;\n')

        self.shell(['gprbuild'],
                   env=env,
                   analyze_output=False,
                   cwd=self.test_env['working_dir'])

        self.shell(['./main'],
                   env=env,
                   cwd=self.test_env['working_dir'])
