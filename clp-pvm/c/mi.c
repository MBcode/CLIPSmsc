        MakeInstance("(gensym) of THECLASS")
>        MakeInstance(" of THECLASS")
>
>
>Give a syntax errors for the make-instance function.  Is there a way to create
>instances from C without knowing the name beforehand?

This was an oversight in CLIPS 6.0.2 and will be fixed later. In the
meantime, use the following workaround:

#include "miscfun.h"

char myBuffer[80];

sprintf(myBuffer,"%s of THECLASS",ValueToString(GenSymStarFunction()));
MakeInstance(myBuffer);


Brian Donnell
NASA/JSC
