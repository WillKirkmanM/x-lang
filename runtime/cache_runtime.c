#include "cache_runtime.h"
#include <stdlib.h>
#include <string.h> // For memcmp and memcpy
#include <stdint.h> // For uint64_t

typedef struct Entry {
    void* key;
    size_t key_len;
    void* value;
    struct Entry* next;
} Entry;

struct Cache {
    int capacity;
    Entry** buckets;
};

// FNV-1a hash function for a generic byte buffer.
static uint64_t hash_key(const void* key, size_t key_len) {
    uint64_t hash = 0xcbf29ce484222325; // FNV_offset_basis
    const unsigned char* bytes = (const unsigned char*)key;
    for (size_t i = 0; i < key_len; ++i) {
        hash ^= bytes[i];
        hash *= 0x100000001b3; // FNV_prime
    }
    return hash;
}

Cache* cache_create(int capacity) {
    Cache* cache = (Cache*)malloc(sizeof(Cache));
    cache->capacity = capacity;
    cache->buckets = (Entry**)calloc(capacity, sizeof(Entry*));
    return cache;
}

void* cache_get(Cache* cache, const void* key, size_t key_len) {
    uint64_t hash = hash_key(key, key_len);
    unsigned int index = hash % cache->capacity;
    Entry* current = cache->buckets[index];

    while (current != NULL) {
        // Compare keys by length and then byte-for-byte.
        if (current->key_len == key_len && memcmp(current->key, key, key_len) == 0) {
            return current->value; // Cache Hit!
        }
        current = current->next;
    }

    return NULL; // Cache Miss
}

void cache_set(Cache* cache, const void* key, size_t key_len, void* value) {
    uint64_t hash = hash_key(key, key_len);
    unsigned int index = hash % cache->capacity;
    
    // Create a new entry.
    Entry* new_entry = (Entry*)malloc(sizeof(Entry));
    
    // The cache must own its own copy of the key.
    new_entry->key = malloc(key_len);
    memcpy(new_entry->key, key, key_len);
    new_entry->key_len = key_len;
    
    new_entry->value = value; // The cache takes ownership of the value pointer.

    // Insert at the head of the linked list.
    new_entry->next = cache->buckets[index];
    cache->buckets[index] = new_entry;
}

void cache_destroy(Cache* cache) {
    for (int i = 0; i < cache->capacity; ++i) {
        Entry* current = cache->buckets[i];
        while (current != NULL) {
            Entry* to_free = current;
            current = current->next;
            free(to_free->key);   // Free the copied key.
            free(to_free->value); // Free the value pointer.
            free(to_free);        // Free the entry itself.
        }
    }
    free(cache->buckets);
    free(cache);
}