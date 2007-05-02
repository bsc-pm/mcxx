#ifndef TLMINTAKATRANSFORMHELPER_HPP_
#define TLMINTAKATRANSFORMHELPER_HPP_

#include <string>

namespace TL
{

class TaskInfo;
class TaskgroupInfo;

class MintakaTransformHelper
{
public:
	static std::string declare_initialized_condition(void);
	static std::string initialized_condition_name(void);
	static std::string initialize_task(TaskInfo*);
	static std::string initialize_taskgroup(TaskgroupInfo*);
	static std::string finalize_task(TaskInfo*);
	static std::string finalize_taskgroup(TaskgroupInfo*);

private:
	MintakaTransformHelper();
	
	static int s_task_count;
};

}

#endif /*TLMINTAKATRANSFORMHELPER_HPP_*/
