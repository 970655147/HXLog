package com.hx.log.cache.mem;

import com.hx.common.interf.cache.CacheEntryFactory;
import com.hx.common.interf.cache.CacheEntry;
import com.hx.log.cache.mem.interf.MCache;

import java.util.Comparator;
import java.util.HashMap;
import java.util.TreeMap;

/**
 * latest frequency used cache [base on memory]
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 4/13/2017 4:07 PM
 */
public class LFUMCache<K, V> extends MCache<K, V> {

    /**
     * 按照accessCount排序的 accessCnt -> key
     */
    protected TreeMap<CacheEntry<K, V>, K> freq2Key;

    public LFUMCache(int estimateSize, int capacity, boolean enableTimeout, int state,
                      CacheEntryFactory cacheEntryFactory) {
        super(capacity, enableTimeout, state, cacheEntryFactory);
        cache = new HashMap<>(estimateSize);
        freq2Key = new TreeMap<>(new CacheEntryComparator());
    }

    public LFUMCache(int capacity, int state, CacheEntryFactory cacheEntryFactory) {
        this(DEFAULT_ESTIMATE_SIZE, capacity, DEFAULT_ENABLE_TIMEOUT, state, cacheEntryFactory);
    }

    public LFUMCache(int capacity, CacheEntryFactory cacheEntryFactory) {
        this(capacity, STATE_ALL, cacheEntryFactory);
    }

    public LFUMCache(int estimateSize, int capacity) {
        this(estimateSize, capacity, DEFAULT_ENABLE_TIMEOUT, STATE_ALL, DEFAULT_CACHE_ENTRY_FACTORY);
    }

    public LFUMCache(int capacity, boolean enableTimeout) {
        this(DEFAULT_ESTIMATE_SIZE, capacity, enableTimeout, STATE_ALL, DEFAULT_CACHE_ENTRY_FACTORY);
    }

    public LFUMCache(int capacity) {
        this(DEFAULT_ESTIMATE_SIZE, capacity);
    }

    @Override
    protected V getAfterGetEntry(K key, CacheEntry<K, V> entry) {
        freq2Key.put(entry, key);
        return entry.value();
    }

    @Override
    protected boolean putAfterGetEntry(K key, CacheEntry<K, V> entry) {
        freq2Key.put(entry, key);
        if(size() > capacity) {
            K least = freq2Key.firstEntry().getValue();
            evict(least);
        }

        return true;
    }

    @Override
    protected boolean updateAfterGetEntry(K key, CacheEntry<K, V> entry) {
        return true;
    }

    @Override
    protected CacheEntry<K, V> evictAfterGetEntry(K key, CacheEntry<K, V> entry) {
        freq2Key.remove(entry);
        return entry;
    }

    @Override
    protected boolean afterStateUpdated(int state, boolean succ) {
        return succ;
    }

    @Override
    protected int afterClear(int cleaned) {
        freq2Key.clear();
        return cleaned;
    }

    @Override
    protected boolean afterDestroyed(boolean succ) {
        return succ;
    }

    /**
     * 根据accessCount比较两个CacheEntry的Comparator
     *
     * @author Jerry.X.He <970655147@qq.com>
     * @version 1.0
     * @date 4/13/2017 4:45 PM
     */
    private static class CacheEntryComparator implements Comparator<CacheEntry> {
        @Override
        public int compare(CacheEntry o1, CacheEntry o2) {
            long delta = o1.accessCount() - o2.accessCount();
            if(delta != 0) {
                return (delta > 0) ? 1 : ((delta < 0) ? -1 : 0);
            }

            if(o1.lastAccessed() != null && o2.lastAccessed() != null) {
                int accessDelta = o1.lastAccessed().compareTo(o2.lastAccessed());
                if (accessDelta != 0) {
                    return accessDelta;
                }
            }

            if(o1.createdAt() != null && o2.createdAt() != null) {
                int createDelta = o1.createdAt().compareTo(o2.createdAt());
                if(createDelta != 0) {
                    return createDelta;
                }
            }

            delta = o1.ttl() - o2.ttl();
            if(delta != 0) {
                return (delta > 0) ? 1 : ((delta < 0) ? -1 : 0);
            }

            return o1.hashCode() - o2.hashCode();
        }
    }

}
