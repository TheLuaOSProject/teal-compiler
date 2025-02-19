#include <stdio.h>
#include <llvm-c/Core.h>

int main()
{
    unsigned int major, minor, patch;
    LLVMGetVersion(&major, &minor, &patch);
    printf("LLVM version %u.%u.%u\n", major, minor, patch);
}
