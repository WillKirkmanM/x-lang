#ifndef CACHE_RUNTIME_H
#define CACHE_RUNTIME_H
#include <stddef.h> // For size_t

// Opaque struct pointer for the cache.
typedef struct Cache Cache;

Cache* cache_create(int capacity);

/**
 * Retrieves a value from the cache using a generic key.
 * @param cache A pointer to the Cache.
 * @param key A pointer to the byte buffer representing the function's arguments.
 * @param key_len The length of the key buffer in bytes.
 * @return A generic pointer (void*) to the cached result, or NULL on a cache miss.
 */
void* cache_get(Cache* cache, const void* key, size_t key_len);

/**
 * Stores a key and a generic value in the cache.
 * @param cache A pointer to the Cache.
 * @param key A pointer to the key's byte buffer.
 * @param key_len The length of the key buffer.
 * @param value A generic pointer (void*) to the heap-allocated result to be stored.
 */
void cache_set(Cache* cache, const void* key, size_t key_len, void* value);

void cache_destroy(Cache* cache);

#endif // CACHE_RUNTIME_H

int __xlang_type_id(void* p);
