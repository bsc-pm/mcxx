#ifndef CXX_MULTIFILE_H
#define CXX_MULTIFILE_H

#include "cxx-macros.h"

MCXX_BEGIN_DECLS

#define MULTIFILE_DIRECTORY "./.mercurium"
#define MULTIFILE_SECTION ".mercurium"
#define MULTIFILE_TAR_FILE "multifile.tar"

// Directory handling
char multifile_dir_exists(void);
void multifile_wipe_dir(void);

// Extended info handling
char multifile_object_has_extended_info(const char* filename);
void multifile_extract_extended_info(const char* filename);

void multifile_get_extracted_profiles(const char*** multifile_profiles, int *num_multifile_profiles);

void multifile_get_profile_file_list(const char* profile_name,
        const char*** multifile_file_list,
        int *num_multifile_files);

MCXX_END_DECLS

#endif // CXX_MULTIFILE_H
