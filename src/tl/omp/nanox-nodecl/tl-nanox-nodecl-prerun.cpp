#include "tl-nanox-nodecl.hpp"
#include "tl-source.hpp"
#include "tl-nodecl.hpp"

#include "tl-nanos.hpp"

#include "cxx-driver-utils.h"
#include "cxx-utils.h"

#include <string>
#include <fstream>
#include <iomanip>

namespace TL { namespace Nanox {

void Lowering::pre_run(DTO& dto)
{
    std::cerr << "Nanos++ prerun" << std::endl;

    if (!IS_FORTRAN_LANGUAGE)
        return;

    Nodecl::NodeclBase top_level = dto["nodecl"];

    const char** old_preprocessor_options = CURRENT_CONFIGURATION->preprocessor_options;

    int num_orig_args = count_null_ended_array((void**)old_preprocessor_options);
    int num_args = num_orig_args;

    // -x c
    num_args += 2;

    // NULL ended
    num_args += 1;

    const char** preprocessor_options = new const char*[num_args];

    for (int i = 0;  i < num_orig_args; i++)
    {
        preprocessor_options[i] = old_preprocessor_options[i];
    }

    // We add -x c since we want /dev/null be preprocessed as an empty C file
    preprocessor_options[num_args - 3] = "-x";
    preprocessor_options[num_args - 2] = "c";
    preprocessor_options[num_args - 1] = NULL;

    CURRENT_CONFIGURATION->preprocessor_options = preprocessor_options;

    const char* output_filename = preprocess_file("/dev/null");

    delete[] preprocessor_options;

    // Restore old flags
    CURRENT_CONFIGURATION->preprocessor_options = old_preprocessor_options;

    TL::Source src;

    std::ifstream preproc_file(output_filename);

    if (preproc_file.is_open())
    {
        std::string str;

        while (preproc_file.good())
        {
            std::getline(preproc_file, str);
            src << str << "\n";
        }
        preproc_file.close();
    }
    else
    {
        running_error("Could not open Nanos++ include", 0);
    }

    Source::source_language = SourceLanguage::C;

    Nodecl::NodeclBase new_tree = src.parse_global(top_level);

    // Run Nanos::Interface on this header to get
    // the pragma information
    Nanos::Interface interface;
    interface.walk(top_level);

    Source::source_language = SourceLanguage::Current;
}

} }
