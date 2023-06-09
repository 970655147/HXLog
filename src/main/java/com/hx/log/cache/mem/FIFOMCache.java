package com.hx.log.cache.mem;

import com.hx.common.interf.cache.CacheEntryFactory;
import com.hx.common.interf.cache.CacheEntry;
import com.hx.log.cache.mem.interf.MCache;

import java.util.*;

/**
 * first in first out cache [base on memory]
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 4/13/2017 11:54 AM
 */
public class FIFOMCache<K, V> extends MCache<K, V> {

    public FIFOMCache(int estimateSize, int capacity, boolean enableTimeout, int state,
                      CacheEntryFactory cacheEntryFactory) {
        super(capacity, enableTimeout, state, cacheEntryFactory);
        // true for access-order, false for insertion-order.
        cache = new LinkedHashMap<>(estimateSize, DEFAULT_LOADER_FACTOR, false);
    }

    public FIFOMCache(int capacity, int state, CacheEntryFactory cacheEntryFactory) {
        this(DEFAULT_ESTIMATE_SIZE, capacity, DEFAULT_ENABLE_TIMEOUT, state, cacheEntryFactory);
    }

    public FIFOMCache(int capacity, CacheEntryFactory cacheEntryFactory) {
        this(capacity, STATE_ALL, cacheEntryFactory);
    }

    public FIFOMCache(int estimateSize, int capacity) {
       this(estimateSize, capacity, DEFAULT_ENABLE_TIMEOUT, STATE_ALL, DEFAULT_CACHE_ENTRY_FACTORY);
    }

    public FIFOMCache(int capacity, boolean enableTimeout) {
        this(DEFAULT_ESTIMATE_SIZE, capacity, enableTimeout, STATE_ALL, DEFAULT_CACHE_ENTRY_FACTORY);
    }

    public FIFOMCache(int capacity) {
        this(DEFAULT_ESTIMATE_SIZE, capacity);
    }

    @Override
    protected V getAfterGetEntry(K key, CacheEntry<K, V> entry) {
        return entry.value();
    }

    @Override
    protected boolean putAfterGetEntry(K key, CacheEntry<K, V> entry) {
        // capacity must gt 0, so there at lease exists one element
        // and cause this method is in 'sync(cacheLock)', so iterator is thread-safe
        if(size() > capacity) {
            K first = cache.keySet().iterator().next();
            evict(first);
        }

        return true;
    }

    @Override
    protected boolean updateAfterGetEntry(K key, CacheEntry<K, V> entry) {
        return true;
    }

    @Override
    protected CacheEntry<K, V> evictAfterGetEntry(K key, CacheEntry<K, V> entry) {
        return entry;
    }

    @Override
    protected boolean afterStateUpdated(int state, boolean succ) {
        return succ;
    }

    @Override
    protected int afterClear(int cleaned) {
        return cleaned;
    }

    @Override
    protected boolean afterDestroyed(boolean succ) {
        return succ;
    }
}
