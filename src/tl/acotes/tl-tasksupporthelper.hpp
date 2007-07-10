#ifndef TLTASKSUPPORTHELPER_HPP_
#define TLTASKSUPPORTHELPER_HPP_

#include "tl-pragmasupport.hpp"

namespace TL
{
    
class TaskInfo;

class TaskSupportHelper
{
public:
    static void add_clauses(TaskInfo* info, PragmaCustomConstruct 
            pragma_custom_construct);
    
private:	TaskSupportHelper();
	virtual ~TaskSupportHelper();
};

}

#endif /*TLTASKSUPPORTHELPER_HPP_*/
