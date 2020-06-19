/*
<testinfo>
test_generator=config/mercurium-ompss-2
test_nolink=yes
</testinfo>
*/

// These tasks behave like host tasks but cuda flag is passed to nanos6

// Outline
#pragma oss task device(cuda)
void outlineDeviceTask() {}

int main() {

        // Inline
        #pragma oss task device(cuda)
        {}

        outlineDeviceTask();

        #pragma oss taskwait

        return 0;
}
