#include <tl-compilerphase.hpp>
#include <tl-compilerpipeline.hpp>
#include <fstream>

using namespace TL;

class MultifileTest : public CompilerPhase
{
    public:
        virtual void run(DTO& dto);
};

void MultifileTest::run(DTO& dto)
{
    std::cerr << "Running multifile test" << std::endl;

    const std::string hello_file = "hello_world.c";

    std::ofstream hello_world("hello_world.c");

    if (hello_world.bad())
    {
        std::cerr << "Descriptor is bad. Bailing out" << std::endl;
        return;
    }

    hello_world 
        << "#include <stdio.h>"

        << "void do_hello_world(void)"
        << "{"
        <<    "printf(\"Hello world in a world where pointers are of size %zd bytes\n\", sizeof(void*));"
        << "}"
        ;
        
    hello_world.close();

    CompilationProcess::add_file(hello_file, "secondcc");
}

EXPORT_PHASE(MultifileTest);
