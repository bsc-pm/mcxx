import os
import lit.formats
import tempfile
import shutil

class BetsTest(lit.formats.FileBasedTest):
    """Bets tests
    """
    def __init__(self, execute_external=False):
        self.execute_external = execute_external

    def execute(self, test, litConfig):
        sourcePath = test.getSourcePath()
        tempFile = tempfile.NamedTemporaryFile(mode="w+t")
        cmd = [os.path.join(test.config.test_exec_root, "config", "bets"), \
                "-nocolor", "-o", tempFile.name, sourcePath]

        # This is for Fortran so we can run tests in parallel without
        # clobbering module files.
        tempdir = tempfile.mkdtemp()
        test.config.environment["TEMPORARY_MODULE_PATH"] = \
                "-J {}".format(tempdir)

        result_log = ""
        try:
            out, err, exitCode = lit.util.executeCommand(
                cmd, env=test.config.environment,
                timeout=litConfig.maxIndividualTestTime)
            tempFile.seek(0)
            for line in tempFile:
                result_log += "[BETS LOG] {}".format(line)
        except lit.util.ExecuteCommandTimeoutException:
            return (lit.Test.TIMEOUT,
                    'Reached timeout of {} seconds'.format(
                        litConfig.maxIndividualTestTime)
                   )
        finally:
            shutil.rmtree(tempdir)
            tempFile.close()

        if exitCode:
            return lit.Test.FAIL, out + err + result_log

        return lit.Test.PASS,''

        # Remove this
        return lit.Test.UNRESOLVED, "Code unreachable"
