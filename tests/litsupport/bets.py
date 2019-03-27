import os
import lit.formats
import tempfile
import shutil

class BetsTest(lit.formats.FileBasedTest):
    """Bets tests
    """
    def __init__(self, execute_external=False):
        self.execute_external = execute_external

    def getTestsInDirectory(self, testSuite, path_in_suite,
                            litConfig, localConfig):
        source_path = testSuite.getSourcePath(path_in_suite)
        for filename in os.listdir(source_path):
            # Ignore dot files and excluded tests.
            if (filename.startswith('.') or
                filename in localConfig.excludes):
                continue

            filepath = os.path.join(source_path, filename)
            if not os.path.isdir(filepath):
                if any(filename.endswith(suffix) for suffix in localConfig.suffixes):
                    yield lit.Test.Test(testSuite, path_in_suite + (filename,),
                                        localConfig)

    def execute(self, test, litConfig):
        sourcePath = test.getSourcePath()

        # Create a temporary directory and change to it
        saveCwd = os.getcwd()
        tempDir = tempfile.mkdtemp()
        os.chdir(tempDir)

        tempFile = tempfile.NamedTemporaryFile(mode="w+t")
        cmd = [os.path.join(test.config.test_exec_root, "config", "bets"), \
                "-nocolor", "-o", tempFile.name, \
                "-generator-root-dir", test.config.test_exec_root, \
                sourcePath]

        # This is for Fortran so we can run tests in parallel without
        # clobbering module files.
        test.config.environment["TEMPORARY_MODULE_PATH"] = \
                "-J {}".format(tempDir)

        result_log = "[BETS COMMAND] {}\n".format(" ".join(cmd))
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
            # Change back to the original directory
            os.chdir(saveCwd)
            shutil.rmtree(tempDir)
            tempFile.close()

        litTestResult = lit.Test.PASS
        if exitCode:
            litTestResult = lit.Test.FAIL

        return litTestResult, out + err + result_log

        # Remove this
        return lit.Test.UNRESOLVED, "Code unreachable"
