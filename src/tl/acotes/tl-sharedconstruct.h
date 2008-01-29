// 
// File:   tl-sharedconstruct.h
// Author: drodenas
//
// Created on 30 / desembre / 2007, 18:27
//

#ifndef _TL_SHAREDCONSTRUCT_H
#define	_TL_SHAREDCONSTRUCT_H

#include <tl-langconstruct.hpp>
#include <tl-pragmasupport.hpp>

namespace TL { namespace Acotes {

    class SharedConstruct
    : TL::PragmaCustomConstruct
    {
    // -- LangConstruct support
    public:
        SharedConstruct(TL::LangConstruct langConstruct);
    private:
        TL::LangConstruct getConstruct();

    // -- CompilerPhase events
    public:
        void onPre();
        void onPost();
    private:
        void onPreCheck();
        void onPreUpdate();
    };
    
    
} /* end namespace Acotes */ } /* end namespace TL */


#endif	/* _TL_SHAREDCONSTRUCT_H */

