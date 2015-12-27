/* $Id: sha1ossl.c 52828 2015-12-01 04:07:10Z usa $ */

#include "../defs.h"
#include "sha1ossl.h"

void
SHA1_Finish(SHA1_CTX *ctx, char *buf)
{
	SHA1_Final((unsigned char *)buf, ctx);
}
