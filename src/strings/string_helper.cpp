// string_helper.cpp

#include "string_helper.h"
#include <string>
#include <sstream>

using namespace std;

#define MIN(a, b) ((a) < (b) ? (a) : (b))

void split_string_delim(const char *src, char delim, char **buffer, 
                        size_t numBuffers, size_t bufferSize, size_t *n,
                        size_t *counts)
{
    // Local Variables
    istringstream iss(src);
    string item;
    *n = 0;
    while (getline(iss, item, delim)) {
        strcpy_s(buffer[*n], bufferSize, item.c_str());
        counts[*n] = item.size();
        *n += 1;
        if (*n >= numBuffers) break;
    }
}