package com.hx.log.cache;

import com.hx.common.interf.cache.CacheEntryFacade;
import com.hx.common.interf.cache.CacheEntryFactory;
import com.hx.common.interf.cache.CacheEntry;

/**
 * ´´½¨SimpleCacheEntryµÄcacheEntryFactory
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 4/13/2017 12:15 PM
 */
public class SimpleCacheEntryFactory implements CacheEntryFactory {

    @Override
    public <K, V> CacheEntry<K, V> create(K key, V value, long ttl) {
        return new SimpleCacheEntry<K, V>(key, value, ttl);
    }

    @Override
    public <K, V> CacheEntryFacade<K, V> createFacade(CacheEntry<K, V> entry) {
        return new SimpleCacheEntryFacade<>(entry);
    }
}
