#ifndef TL_HANDLER_HPP
#define TL_HANDLER_HPP

#include <vector>

/*
 * Simplified signal/slot library
 */
namespace TL
{

// 6 parameters
template <class Param1 = void, class Param2 = void, class Param3 = void, class Param4 = void, class Param5 = void, class Param6 = void>
class BaseHandler
{
    public:
        virtual void execute(Param1, Param2, Param3, Param4, Param5, Param6) = 0;

        virtual ~BaseHandler()
        {
        }
};

// 5 parameters
template <class Param1, class Param2, class Param3, class Param4, class Param5>
class BaseHandler<Param1, Param2, Param3, Param4, Param5>
{
    public:
        virtual void execute(Param1, Param2, Param3, Param4, Param5) = 0;

        virtual ~BaseHandler()
        {
        }
};

// 4 parameters
template <class Param1, class Param2, class Param3, class Param4>
class BaseHandler<Param1, Param2, Param3, Param4>
{
    public:
        virtual void execute(Param1, Param2, Param3, Param4) = 0;

        virtual ~BaseHandler()
        {
        }
};

// 3 parameters
template <class Param1, class Param2, class Param3>
class BaseHandler<Param1, Param2, Param3>
{
    public:
        virtual void execute(Param1, Param2, Param3) = 0;

        virtual ~BaseHandler()
        {
        }
};

// 2 parameters
template <class Param1, class Param2>
class BaseHandler<Param1, Param2>
{
    public:
        virtual void execute(Param1, Param2) = 0;

        virtual ~BaseHandler()
        {
        }
};

// 1 parameter
template <class Param1>
class BaseHandler<Param1>
{
    public:
        virtual void execute(Param1) = 0;

        virtual ~BaseHandler()
        {
        }
};

// 0 parameters
template<>
class BaseHandler<>
{
    public:
        virtual void execute() = 0;

        virtual ~BaseHandler()
        {
        }
};

// 6 parameters fun_ptr
template <class Param1, class Param2, class Param3, class Param4, class Param5, class Param6>
class FunctionHandler6 : public BaseHandler<Param1, Param2, Param3, Param4, Param5, Param6>
{
    private:
        void (*_pf)(Param1, Param2, Param3, Param4, Param5, Param6);
    public:
        FunctionHandler6(void (*pf)(Param1, Param2, Param3, Param4, Param5, Param6))
            : _pf(pf)
        {
        }
        
        virtual void execute(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6)
        {
            return (this->_pf)(p1, p2, p3, p4, p5, p6);
        }

        virtual ~FunctionHandler6()
        {
        }
};

// 5 parameters fun_ptr
template <class Param1, class Param2, class Param3, class Param4, class Param5>
class FunctionHandler5 : public BaseHandler<Param1, Param2, Param3, Param4, Param5>
{
    private:
        void (*_pf)(Param1, Param2, Param3, Param4, Param5);
    public:
        FunctionHandler5(void (*pf)(Param1, Param2, Param3, Param4, Param5))
            : _pf(pf)
        {
        }
        
        virtual void execute(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5)
        {
            return (this->_pf)(p1, p2, p3, p4, p5);
        }

        virtual ~FunctionHandler5()
        {
        }
};

// 4 parameters fun_ptr
template <class Param1, class Param2, class Param3, class Param4>
class FunctionHandler4 : public BaseHandler<Param1, Param2, Param3, Param4>
{
    private:
        void (*_pf)(Param1, Param2, Param3, Param4);
    public:
        FunctionHandler4(void (*pf)(Param1, Param2, Param3, Param4))
            : _pf(pf)
        {
        }
        
        virtual void execute(Param1 p1, Param2 p2, Param3 p3, Param4 p4)
        {
            return (this->_pf)(p1, p2, p3, p4);
        }

        virtual ~FunctionHandler4()
        {
        }
};

// 3 parameters fun_ptr
template <class Param1, class Param2, class Param3>
class FunctionHandler3 : public BaseHandler<Param1, Param2, Param3>
{
    private:
        void (*_pf)(Param1, Param2, Param3);
    public:
        FunctionHandler3(void (*pf)(Param1, Param2, Param3))
            : _pf(pf)
        {
        }
        
        virtual void execute(Param1 p1, Param2 p2, Param3 p3)
        {
            return (this->_pf)(p1, p2, p3);
        }

        virtual ~FunctionHandler3()
        {
        }
};

// 2 parameters fun_ptr
template <class Param1, class Param2>
class FunctionHandler2 : public BaseHandler<Param1, Param2>
{
    private:
        void (*_pf)(Param1, Param2);
    public:
        FunctionHandler2(void (*pf)(Param1, Param2))
            : _pf(pf)
        {
        }
        
        virtual void execute(Param1 p1, Param2 p2)
        {
            return (this->_pf)(p1, p2);
        }

        virtual ~FunctionHandler2()
        {
        }
};

// 1 parameters fun_ptr
template <class Param1>
class FunctionHandler1 : public BaseHandler<Param1>
{
    private:
        void (*_pf)(Param1);
    public:
        FunctionHandler1(void (*pf)(Param1))
            : _pf(pf)
        {
        }
        
        virtual void execute(Param1 p1)
        {
            return (this->_pf)(p1);
        }

        virtual ~FunctionHandler1()
        {
        }
};

// 0 parameters fun_ptr
template <typename = void>
class FunctionHandler0 : public BaseHandler<>
{
    private:
        void (*_pf)();
    public:
        FunctionHandler0(void (*pf)())
            : _pf(pf)
        {
        }
        
        virtual void execute()
        {
            return (this->_pf)();
        }

        virtual ~FunctionHandler0()
        {
        }
};

// Member function 6 parameters
template <class T, class Param1, class Param2, class Param3, class Param4, class Param5, class Param6>
class MemberFunctionHandler6 : public BaseHandler<Param1, Param2, Param3, Param4, Param5, Param6>
{
    private:
        T& _t;
        void (T::*_pf)(Param1, Param2, Param3, Param4, Param5, Param6);
    public:
        MemberFunctionHandler6(T& t, void (T::*pf)(Param1, Param2, Param3, Param4, Param5, Param6))
            : _t(t), _pf(pf)
        {
        }
        
        virtual void execute(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6)
        {
            return (_t.*_pf)(p1, p2, p3, p4, p5, p6);
        }

        virtual ~MemberFunctionHandler6()
        {
        }
};

// Member function 5 parameters
template <class T, class Param1, class Param2, class Param3, class Param4, class Param5>
class MemberFunctionHandler5 : public BaseHandler<Param1, Param2, Param3, Param4, Param5>
{
    private:
        T& _t;
        void (T::*_pf)(Param1, Param2, Param3, Param4, Param5);
    public:
        MemberFunctionHandler5(T& t, void (T::*pf)(Param1, Param2, Param3, Param4, Param5))
            : _t(t), _pf(pf)
        {
        }
        
        virtual void execute(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5)
        {
            return (_t.*_pf)(p1, p2, p3, p4, p5);
        }

        virtual ~MemberFunctionHandler5()
        {
        }
};

// Member function 4 parameters
template <class T, class Param1, class Param2, class Param3, class Param4>
class MemberFunctionHandler4 : public BaseHandler<Param1, Param2, Param3, Param4>
{
    private:
        T& _t;
        void (T::*_pf)(Param1, Param2, Param3, Param4);
    public:
        MemberFunctionHandler4(T& t, void (T::*pf)(Param1, Param2, Param3, Param4))
            : _t(t), _pf(pf)
        {
        }
        
        virtual void execute(Param1 p1, Param2 p2, Param3 p3, Param4 p4)
        {
            return (_t.*_pf)(p1, p2, p3, p4);
        }

        virtual ~MemberFunctionHandler4()
        {
        }
};

// Member function 3 parameters
template <class T, class Param1, class Param2, class Param3>
class MemberFunctionHandler3 : public BaseHandler<Param1, Param2, Param3>
{
    private:
        T& _t;
        void (T::*_pf)(Param1, Param2, Param3);
    public:
        MemberFunctionHandler3(T& t, void (T::*pf)(Param1, Param2, Param3))
            : _t(t), _pf(pf)
        {
        }
        
        virtual void execute(Param1 p1, Param2 p2, Param3 p3)
        {
            return (_t.*_pf)(p1, p2, p3);
        }

        virtual ~MemberFunctionHandler3()
        {
        }
};

// Member function 2 parameters
template <class T, class Param1, class Param2>
class MemberFunctionHandler2 : public BaseHandler<Param1, Param2>
{
    private:
        T& _t;
        void (T::*_pf)(Param1, Param2);
    public:
        MemberFunctionHandler2(T& t, void (T::*pf)(Param1, Param2))
            : _t(t), _pf(pf)
        {
        }
        
        virtual void execute(Param1 p1, Param2 p2)
        {
            return (_t.*_pf)(p1, p2);
        }

        virtual ~MemberFunctionHandler2()
        {
        }
};

// Member function 1 parameters
template <class T, class Param1>
class MemberFunctionHandler1 : public BaseHandler<Param1>
{
    private:
        T& _t;
        void (T::*_pf)(Param1);
    public:
        MemberFunctionHandler1(T& t, void (T::*pf)(Param1))
            : _t(t), _pf(pf)
        {
        }
        
        virtual void execute(Param1 p1)
        {
            return (_t.*_pf)(p1);
        }

        virtual ~MemberFunctionHandler1()
        {
        }
};

// Member function 1 parameters
template <class T>
class MemberFunctionHandler0 : public BaseHandler<>
{
    private:
        T& _t;
        void (T::*_pf)();
    public:
        MemberFunctionHandler0(T& t, void (T::*pf)())
            : _t(t), _pf(pf)
        {
        }
        
        virtual void execute()
        {
            return (_t.*_pf)();
        }

        virtual ~MemberFunctionHandler0()
        {
        }
};

// Signal 6 parameters
template <class Param1, class Param2, class Param3, class Param4, class Param5, class Param6>
class Signal6
{
    private:
        typedef typename std::vector<BaseHandler<Param1, Param2, Param3, Param4, Param5, Param6>* > handlers_type;
		typedef typename handlers_type::iterator handlers_iterator;
		typedef FunctionHandler6<Param1, Param2, Param3, Param4, Param5, Param6> function_handler_type;

		handlers_type _handlers;
    public:
        void connect(void (*f)(Param1, Param2, Param3, Param4, Param5, Param6))
        {
            function_handler_type* fun_handler = new function_handler_type(f);

            _handlers.push_back(fun_handler);
        }

        template <class T>
            void connect(void (T::*f)(Param1, Param2, Param3, Param4, Param5, Param6), T& t)
            {
				typedef MemberFunctionHandler6<T, Param1, Param2, Param3, Param4, Param5, Param6> member_function_handler_type;

                member_function_handler_type* member_handler = new member_function_handler_type(t, f);

                _handlers.push_back(member_handler);
            }

        void signal(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5, Param6 p6)
        {
            handlers_iterator it;
            for (it = _handlers.begin(); it != _handlers.end(); it++)
            {
                (*it)->execute(p1, p2, p3, p4, p5, p6);
            }
        }

		~Signal6()
		{
            handlers_iterator it;
            for (it = _handlers.begin(); it != _handlers.end(); it++)
            {
                delete (*it);
            }
		}
};

// Signal 5 parameters
template <class Param1, class Param2, class Param3, class Param4, class Param5>
class Signal5
{
    private:
        typedef typename std::vector<BaseHandler<Param1, Param2, Param3, Param4, Param5>* > handlers_type;
		typedef typename handlers_type::iterator handlers_iterator;
		typedef FunctionHandler5<Param1, Param2, Param3, Param4, Param5> function_handler_type;

		handlers_type _handlers;
    public:
        void connect(void (*f)(Param1, Param2, Param3, Param4, Param5))
        {
            function_handler_type* fun_handler = new function_handler_type(f);

            _handlers.push_back(fun_handler);
        }

        template <class T>
            void connect(void (T::*f)(Param1, Param2, Param3, Param4, Param5), T& t)
			{
				typedef MemberFunctionHandler5<T, Param1, Param2, Param3, Param4, Param5> member_function_handler_type;

				member_function_handler_type* member_handler = new member_function_handler_type(t, f);

				_handlers.push_back(member_handler);
			}

        void signal(Param1 p1, Param2 p2, Param3 p3, Param4 p4, Param5 p5)
        {
            handlers_iterator it;
            for (it = _handlers.begin(); it != _handlers.end(); it++)
            {
                (*it)->execute(p1, p2, p3, p4, p5);
            }
        }

		~Signal5()
		{
            handlers_iterator it;
            for (it = _handlers.begin(); it != _handlers.end(); it++)
            {
                delete (*it);
            }
		}
};

// Signal 4 parameters
template <class Param1, class Param2, class Param3, class Param4>
class Signal4
{
    private:
        typedef typename std::vector<BaseHandler<Param1, Param2, Param3, Param4>* > handlers_type;
		typedef typename handlers_type::iterator handlers_iterator;
		typedef FunctionHandler4<Param1, Param2, Param3, Param4> function_handler_type;

		handlers_type _handlers;
    public:
        void connect(void (*f)(Param1, Param2, Param3, Param4))
        {
            function_handler_type* fun_handler = new function_handler_type(f);

            _handlers.push_back(fun_handler);
        }

        template <class T>
            void connect(void (T::*f)(Param1, Param2, Param3, Param4), T& t)
            {
				typedef MemberFunctionHandler4<T, Param1, Param2, Param3, Param4> member_function_handler_type;

                member_function_handler_type* member_handler = new member_function_handler_type(t, f);

                _handlers.push_back(member_handler);
            }

        void signal(Param1 p1, Param2 p2, Param3 p3, Param4 p4)
        {
            handlers_iterator it;
            for (it = _handlers.begin(); it != _handlers.end(); it++)
            {
                (*it)->execute(p1, p2, p3, p4);
            }
        }

		~Signal4()
		{
            handlers_iterator it;
            for (it = _handlers.begin(); it != _handlers.end(); it++)
            {
                delete (*it);
            }
		}
};

// Signal 3 parameters
template <class Param1, class Param2, class Param3>
class Signal3
{
    private:
        typedef typename std::vector<BaseHandler<Param1, Param2, Param3>* > handlers_type;
		typedef typename handlers_type::iterator handlers_iterator;
		typedef FunctionHandler3<Param1, Param2, Param3> function_handler_type;

		handlers_type _handlers;
    public:
        void connect(void (*f)(Param1, Param2, Param3))
        {
            function_handler_type* fun_handler = new function_handler_type(f);

            _handlers.push_back(fun_handler);
        }

        template <class T>
            void connect(void (T::*f)(Param1, Param2, Param3), T& t)
            {
				typedef MemberFunctionHandler3<T, Param1, Param2, Param3> member_function_handler_type;

                member_function_handler_type* member_handler = new member_function_handler_type(t, f);

                _handlers.push_back(member_handler);
            }

        void signal(Param1 p1, Param2 p2, Param3 p3)
        {
            handlers_iterator it;
            for (it = _handlers.begin(); it != _handlers.end(); it++)
            {
                (*it)->execute(p1, p2, p3);
            }
        }

		~Signal3()
		{
            handlers_iterator it;
            for (it = _handlers.begin(); it != _handlers.end(); it++)
            {
                delete (*it);
            }
		}
};

// Signal 2 parameters
template <class Param1, class Param2>
class Signal2
{
    private:
        typedef typename std::vector<BaseHandler<Param1, Param2>* > handlers_type;
		typedef typename handlers_type::iterator handlers_iterator;
		typedef FunctionHandler2<Param1, Param2> function_handler_type;

		handlers_type _handlers;
    public:
        void connect(void (*f)(Param1, Param2))
        {
            function_handler_type* fun_handler = new function_handler_type(f);

            _handlers.push_back(fun_handler);
        }

        template <class T>
            void connect(void (T::*f)(Param1, Param2), T& t)
            {
				typedef MemberFunctionHandler2<T, Param1, Param2> member_function_handler_type;

                member_function_handler_type* member_handler = new member_function_handler_type(t, f);

                _handlers.push_back(member_handler);
            }

        void signal(Param1 p1, Param2 p2)
        {
            handlers_iterator it;
            for (it = _handlers.begin(); it != _handlers.end(); it++)
            {
                (*it)->execute(p1, p2);
            }
        }

		~Signal2()
		{
            handlers_iterator it;
            for (it = _handlers.begin(); it != _handlers.end(); it++)
            {
                delete (*it);
            }
		}
};

// Signal 1 parameters
template <class Param1>
class Signal1
{
    private:
        typedef typename std::vector<BaseHandler<Param1>* > handlers_type;
		typedef typename handlers_type::iterator handlers_iterator;
		typedef FunctionHandler1<Param1> function_handler_type;

		handlers_type _handlers;
    public:
        void connect(void (*f)(Param1))
        {
            function_handler_type* fun_handler = new function_handler_type(f);

            _handlers.push_back(fun_handler);
        }

        template <class T>
            void connect(void (T::*f)(Param1), T& t)
            {
				typedef MemberFunctionHandler1<T, Param1> member_function_handler_type;

                member_function_handler_type* member_handler = new member_function_handler_type(t, f);

                _handlers.push_back(member_handler);
            }

        void signal(Param1 p1)
        {
            handlers_iterator it;
            for (it = _handlers.begin(); it != _handlers.end(); it++)
            {
                (*it)->execute(p1);
            }
        }

		~Signal1()
		{
            handlers_iterator it;
            for (it = _handlers.begin(); it != _handlers.end(); it++)
            {
                delete (*it);
            }
		}
};

// Signal 0 parameters
template <typename = void>
class Signal0
{
    private:
        typedef typename std::vector<BaseHandler<>* > handlers_type;
		typedef typename handlers_type::iterator handlers_iterator;
		typedef FunctionHandler0<> function_handler_type;

		handlers_type _handlers;
    public:
        void connect(void (*f)())
        {
            function_handler_type* fun_handler = new function_handler_type(f);

            _handlers.push_back(fun_handler);
        }

        template <class T>
            void connect(void (T::*f)(), T& t)
            {
				typedef MemberFunctionHandler0<T> member_function_handler_type;

                member_function_handler_type* member_handler = new member_function_handler_type(t, f);

                _handlers.push_back(member_handler);
            }

        void signal()
        {
            handlers_iterator it;
            for (it = _handlers.begin(); it != _handlers.end(); it++)
            {
                (*it)->execute();
            }
        }

		~Signal0()
		{
            handlers_iterator it;
            for (it = _handlers.begin(); it != _handlers.end(); it++)
            {
                delete (*it);
            }
		}
};

}

#endif // TL_HANDLER_HPP
