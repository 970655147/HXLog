package com.hx.log.cache.mem;

import com.hx.log.cache.SimpleCacheEntryFactory;
import com.hx.log.cache.interf.CacheEntryFacade;
import com.hx.log.cache.interf.CacheEntryFactory;
import com.hx.log.interf.Cache;
import com.hx.log.interf.CacheEntry;
import com.hx.log.util.Tools;

import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;

/**
 * ������ڴ��cache
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 4/13/2017 11:21 AM
 */
public abstract class MCache<K, V> implements Cache<K, V> {

    /**
     * Ĭ�ϵ�cacheEntryFactory
     */
    protected static final CacheEntryFactory DEFAULT_CACHE_ENTRY_FACTORY = new SimpleCacheEntryFactory();
    /**
     * ֻ��, ֻд, �ɶ���д, ���ɶ�����д
     */
    public static final int STATE_READ = 0b01;
    public static final int STATE_WRITE = 0b10;
    public static final int STATE_ALL = STATE_WRITE | STATE_READ;
    public static final int STATE_NONE = 0;
    /**
     * Ĭ�ϵ�size, Ĭ�ϵ�loadFactor
     */
    protected static final int DEFAULT_ESTIMATE_SIZE = 16;
    protected static final float DEFAULT_LOADER_FACTOR = 0.75f;
    /**
     * expireTimer������
     */
    protected static final int DEFAULT_EXPIRE_CHECK_INTERVAL = 1000;

    /**
     * ����KV��Map
     */
    protected Map<K, CacheEntry<K, V>> cache;
    /**
     * ��ǰ�������������ɵ�Ԫ�ص�����
     */
    protected int capacity;
    /**
     * �������еĴ���, ������ʵĴ���
     */
    protected AtomicLong hitted;
    protected AtomicLong visited;
    /**
     * ����cacheEntry�Ĺ���
     */
    protected CacheEntryFactory cacheEntryFactory;
    /**
     * ��ǵ�ǰCache��״̬
     */
    protected int state;
    /**
     * ttl�ǹ̶���ttl[�����createAt], �����������һ��access��ttl
     */
    protected boolean fixedTtl;
    /**
     * ��ǵ�ǰCache�Ƿ�����
     */
    protected boolean destroyed;
    /**
     * ���ڼ��kv�Ƿ���ڵ�timer
     */
    protected Timer expireTimer;
    /**
     * ���kv�Ƿ���ڵ�����
     */
    protected int expireCheckInterval;

    /**
     * ά��cacheͬ����lock
     */
    protected final Object cacheLock = new Object();

    public MCache(int capacity, int state, CacheEntryFactory cacheEntryFactory) {
        Tools.assert0(capacity > 0, "'capacity' must gt 0 !");
        Tools.assert0(cacheEntryFactory != null, "'cacheEntryFactory' can't be null !");

        this.capacity = capacity;
        this.hitted = new AtomicLong(0);
        this.visited = new AtomicLong(0);

        this.cacheEntryFactory = cacheEntryFactory;
        // update state, first authority all, the scale the state
        this.state = STATE_ALL;
        state(state);

        setExpireCheckInterval(DEFAULT_EXPIRE_CHECK_INTERVAL);
    }

    public MCache(int capacity, CacheEntryFactory cacheEntryFactory) {
        this(capacity, STATE_ALL, cacheEntryFactory);
    }

    public MCache(int capacity) {
        this(capacity, DEFAULT_CACHE_ENTRY_FACTORY);
    }

    @Override
    public V get(K key) {
        if (!readable()) {
            throw new RuntimeException("current cache is not readable !");
        }

        visited.incrementAndGet();
        CacheEntry<K, V> entry = getEntry0(key);
        if (entry == null) {
            return null;
        }

        hitted.incrementAndGet();
        return getEntry0(entry);
    }

    @Override
    public CacheEntryFacade<K, V> getEntry(K key) {
        if (!readable()) {
            throw new RuntimeException("current cache is not readable !");
        }

        CacheEntry<K, V> entry = getEntry0(key);
        if (entry == null) {
            return null;
        }

        getEntry0(entry);
        return cacheEntryFactory.createFacade(entry);
    }

    @Override
    public List<K> keys() {
        if (!readable()) {
            throw new RuntimeException("current cache is not readable !");
        }

        List<K> keys = new ArrayList<>(size());
        synchronized (cacheLock) {
            keys.addAll(cache.keySet());
        }

        return keys;
    }

    @Override
    public int size() {
        if (!readable()) {
            throw new RuntimeException("current cache is not readable !");
        }

        return cache.size();
    }

    @Override
    public int capacity() {
        if (!readable()) {
            throw new RuntimeException("current cache is not readable !");
        }

        return capacity;
    }

    @Override
    public long hitCount() {
        return hitted.get();
    }

    @Override
    public long visitCount() {
        return visited.get();
    }

    @Override
    public boolean put(K key, V value) {
        return put(key, value, CacheEntry.LONG_LIVE);
    }

    @Override
    public boolean put(K key, V value, long expire) {
        Tools.assert0(expire >= CacheEntry.LONG_LIVE, "'expire' must gt " + CacheEntry.LONG_LIVE);
        if (!writeable()) {
            throw new RuntimeException("current cache is not writeable !");
        }

        CacheEntry<K, V> entry = getEntry0(key);
        if (entry != null) {
            return updateEntry0(entry, value, expire);
        }

        entry = cacheEntryFactory.create(key, value, expire);
        synchronized (cacheLock) {
            cache.put(key, entry);
            return putAfterGetEntry(key, entry);
        }
    }

    @Override
    public boolean update(K key, V value) {
        return update(key, value, CacheEntry.NOT_UPDATE_TTL);
    }

    @Override
    public boolean update(K key, V value, long expire) {
        Tools.assert0(expire >= CacheEntry.NOT_UPDATE_TTL, "'expire' must gt " + CacheEntry.LONG_LIVE);
        if (!writeable()) {
            throw new RuntimeException("current cache is not writeable !");
        }

        CacheEntry<K, V> entry = getEntry0(key);
        if (entry == null) {
            return false;
        }
        return updateEntry0(entry, value, expire);
    }

    @Override
    public boolean evict(K key) {
        if (!writeable()) {
            throw new RuntimeException("current cache is not writeable !");
        }

        CacheEntry<K, V> entry = getEntry0(key);
        if (entry == null) {
            return false;
        }

        entry.evictedAt(new Date());
        synchronized (cacheLock) {
            cache.remove(key);
            return evictAfterGetEntry(key, entry);
        }
    }

    @Override
    public boolean evict(Collection<K> keys) {
        if (!writeable()) {
            throw new RuntimeException("current cache is not writeable !");
        }

        Set<K> distincted = new HashSet<>(Tools.estimateMapSize(keys.size()));
        distincted.addAll(keys);
        boolean allSucc = true;
        for (K key : distincted) {
            allSucc = allSucc & evict(key);
        }

        return allSucc;
    }

    @Override
    public boolean state(int state) {
        Tools.assert0(state <= STATE_ALL, "not a valid state !");
        if(destroyed) {
            throw new RuntimeException("current cache is not destroyed !");
        }
        if (!writeable()) {
            throw new RuntimeException("current cache is not writeable !");
        }

        this.state = state;
        return afterStateUpdated(state, true);
    }

    /**
     * �жϵ�ǰCache�Ƿ�ɶ�
     *
     * @return boolean true if current cache readable
     * @author Jerry.X.He
     * @date 4/13/2017 3:14 PM
     * @since 1.0
     */
    @Override
    public boolean readable() {
        return ((this.state & STATE_READ) > 0);
    }

    /**
     * �жϵ�ǰCache�Ƿ��д
     *
     * @return boolean true if current cache writeable
     * @author Jerry.X.He
     * @date 4/13/2017 3:14 PM
     * @since 1.0
     */
    @Override
    public boolean writeable() {
        return ((this.state & STATE_WRITE) > 0);
    }

    @Override
    public void fixedTtl(boolean isFixed) {
        this.fixedTtl = isFixed;
    }

    @Override
    public boolean fixedTtl() {
        return fixedTtl;
    }

    /**
     * ����checkInterval
     *
     * @param checkInterval ���µ�ʱ���checkInterval
     * @return void
     * @author Jerry.X.He
     * @date 4/13/2017 5:56 PM
     * @since 1.0
     */
    public void setExpireCheckInterval(int checkInterval) {
        expireCheckInterval = checkInterval;
        if(expireTimer != null) {
            expireTimer.cancel();
        }
        expireTimer = new Timer();
        expireTimer.schedule(new ExpireTimerTask(), checkInterval, checkInterval);
    }

    @Override
    public int clear() {
        int sz = 0;
        synchronized (cacheLock) {
            sz = cache.size();
            cache.clear();
            return afterClear(sz);
        }
    }

    @Override
    public boolean destroy() {
        destroyed = true;
        this.state = STATE_NONE;
        expireTimer.cancel();
        return afterDestroyed(true);
    }

    /**
     * ����������д�Ļ�ȡentry.value �Լ�������ҵ�����
     *
     * @param key   ��ǰ��key
     * @param entry ��ǰ��key��Ӧ��entry
     * @return value of key corresponding
     * @author Jerry.X.He
     * @date 4/13/2017 12:09 PM
     * @since 1.0
     */
    protected abstract V getAfterGetEntry(K key, CacheEntry<K, V> entry);

    /**
     * ����������д��put kv, �Լ�������ҵ�����
     * sync(cacheLock)
     *
     * @param key   ��ǰ��key
     * @param entry ��ǰ��key��Ӧ��entry
     * @return value of key corresponding
     * @author Jerry.X.He
     * @date 4/13/2017 12:09 PM
     * @since 1.0
     */
    protected abstract boolean putAfterGetEntry(K key, CacheEntry<K, V> entry);

    /**
     * ����������д��update kv, �Լ�������ҵ�����
     *
     * @param key   ��ǰ��key
     * @param entry ��ǰ��key��Ӧ��entry
     * @return value of key corresponding
     * @author Jerry.X.He
     * @date 4/13/2017 12:09 PM
     * @since 1.0
     */
    protected abstract boolean updateAfterGetEntry(K key, CacheEntry<K, V> entry);

    /**
     * ����������д��evict kv, �Լ�������ҵ�����
     * sync(cacheLock)
     *
     * @param key   ��ǰ��key
     * @param entry ��ǰ��key��Ӧ��entry
     * @return value of key corresponding
     * @author Jerry.X.He
     * @date 4/13/2017 12:09 PM
     * @since 1.0
     */
    protected abstract boolean evictAfterGetEntry(K key, CacheEntry<K, V> entry);

    /**
     * ����������д������״̬֮��֮����Ҫ���������ҵ��
     *
     * @param succ is config state success from now on
     * @return value of key corresponding
     * @author Jerry.X.He
     * @date 4/13/2017 12:09 PM
     * @since 1.0
     */
    protected abstract boolean afterStateUpdated(int state, boolean succ);

    /**
     * ����������д��������֮����Ҫ���������ҵ��
     * sync(cacheLock)
     *
     * @return value of key corresponding
     * @author Jerry.X.He
     * @date 4/13/2017 12:09 PM
     * @since 1.0
     */
    protected abstract int afterClear(int cleaned);

    /**
     * ����������д�����ٻ���֮����Ҫ���������ҵ��
     *
     * @return value of key corresponding
     * @author Jerry.X.He
     * @date 4/13/2017 12:09 PM
     * @since 1.0
     */
    protected abstract boolean afterDestroyed(boolean succ);

    // ---------------------------- �������� ----------------------------

    /**
     * ����key��ȡkey��Ӧ��cacheEntry
     *
     * @param key ������key
     * @return com.hx.log.interf.CacheEntry<K,V>
     * @author Jerry.X.He
     * @date 4/13/2017 3:34 PM
     * @since 1.0
     */
    protected CacheEntry<K, V> getEntry0(K key) {
        CacheEntry<K, V> entry = cache.get(key);
        if (entry == null) {
            return null;
        }

        return entry;
    }

    /**
     * ���µ�ǰentry����Ϣ
     *
     * @param entry  ��ǰ��Ҫ���µ�entry
     * @param value  ����entry��value
     * @param expire ����entry��ttl
     * @return boolean
     * @author Jerry.X.He
     * @date 4/13/2017 3:59 PM
     * @since 1.0
     */
    private boolean updateEntry0(CacheEntry<K, V> entry, V value, long expire) {
        entry.value(value);
        entry.lastUpdated(new Date());
        if (expire != CacheEntry.NOT_UPDATE_TTL) {
            entry.ttl(expire);
        }
        return updateAfterGetEntry(entry.key(), entry);
    }

    /**
     * �����ȡentry�����ҵ��
     *
     * @param entry ������entry
     * @return V
     * @author Jerry.X.He
     * @date 4/13/2017 4:56 PM
     * @since 1.0
     */
    private V getEntry0(CacheEntry<K, V> entry) {
        entry.incAccessCount(1);
        entry.lastAccessed(new Date());
        return getAfterGetEntry(entry.key(), entry);
    }

    /**
     * ��ȡ��ǰcache�����е�entry
     *
     * @return java.util.List<com.hx.log.interf.CacheEntry<K,V>>
     * @author Jerry.X.He
     * @date 4/13/2017 5:41 PM
     * @since 1.0
     */
    private List<CacheEntry<K, V>> getAllEntries() {
        List<CacheEntry<K, V>> entries = new ArrayList<>(size());
        synchronized (cacheLock) {
            entries.addAll(cache.values());
        }
        return entries;
    }

    /**
     * ���ڼ��kv�Ƿ���ڵ�task
     *
     * @author Jerry.X.He <970655147@qq.com>
     * @version 1.0
     * @date 4/13/2017 5:37 PM
     */
    private class ExpireTimerTask extends TimerTask {
        @Override
        public void run() {
            List<CacheEntry<K, V>> entries = getAllEntries();
            List<K> needToEvict = new ArrayList<>();
            for(CacheEntry<K, V> entry : entries) {
                Date start = entry.createdAt();
                if(! fixedTtl) {
                    if((entry.lastAccessed() != null) && (entry.lastAccessed().compareTo(start) > 0) ){
                        start = entry.lastAccessed();
                    }
                    if((entry.lastUpdated() != null) && (entry.lastUpdated().compareTo(start) > 0) ){
                        start = entry.lastUpdated();
                    }
                }

                if(System.currentTimeMillis() > (start.getTime() + entry.ttl()) ) {
                    needToEvict.add(entry.key());
                }
            }

            if(! needToEvict.isEmpty()) {
                evict(needToEvict);
            }
        }
    }

}
