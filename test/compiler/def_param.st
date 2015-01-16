program def_param

int x = 1;

int fun(int parm = 2 + 3/* x */)
{
    return parm;
}

entry {
    printf("%d\n", fun());
    exit(0);
}

#include "../simple.st"
