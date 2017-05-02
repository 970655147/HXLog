package com.hx.log.cache.interf;

/**
 * CacheEntry��factory
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 4/13/2017 12:13 PM
 */
public interface CacheEntryFactory {

    /**
     * ���ݸ�����kv, ����һ��CacheEntry
     *
     * @param key   ������key
     * @param value ������value
     * @return the cacheEntry that created by this factory
     * @author Jerry.X.He
     * @date 4/13/2017 12:14 PM
     * @since 1.0
     */
    <K, V> CacheEntry<K, V> create(K key, V value, long ttl);

    /**
     * ���ݸ�����cacheEntry, ����һ��cacheEntryFacade
     *
     * @param entry   ������cacheEntry
     * @return the cacheEntry that created by this factory
     * @author Jerry.X.He
     * @date 4/13/2017 12:14 PM
     * @since 1.0
     */
    <K, V> CacheEntryFacade<K, V> createFacade(CacheEntry<K, V> entry);

}
