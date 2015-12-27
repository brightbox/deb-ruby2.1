/* $Id: rmd160ossl.c 52797 2015-11-30 12:08:16Z usa $ */

#include "../defs.h"
#include "rmd160ossl.h"

void RMD160_Finish(RMD160_CTX *ctx, char *buf) {
	RIPEMD160_Final((unsigned char *)buf, ctx);
}
