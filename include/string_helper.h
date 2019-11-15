// string_helper.h
#ifndef STRING_HELPER_H_
#define STRING_HELPER_H_

#include <cstring>

#ifdef __cplusplus
extern "C" {
#endif


/* Splits a string by a delimiter.
 *
 * src: The source string.
 * delim: The delimiter string.
 * buffer: An array of string containers (buffers) into which each string will
 *      be written.
 * numBuffers: The number of buffers available in buffer.
 * bufferSize: The size of each buffer.
 * n: The actual number of buffers utilized.
 * counts: An array (size = numBuffers) containing the actual number of 
 *  characters, not including the null terminator, that were written in each
 *  buffer.
 */
void split_string_delim(const char *src, char delim, char **buffer, 
                        size_t numBuffers, size_t bufferSize, size_t *n,
                        size_t *counts);


#ifdef __cplusplus
}
#endif
#endif
