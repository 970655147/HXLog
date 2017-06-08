package com.hx.log.cache.mem.interf;

import com.hx.common.interf.cache.*;
import com.hx.log.cache.SimpleCacheContext;
import com.hx.log.cache.SimpleCacheEntryFactory;
import com.hx.log.util.Tools;

import java.util.*;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicLong;

import static com.hx.log.util.Tools.assert0;

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
     * ����listener������
     */
    public static final int LISTENER_BEFORE_GET = 0;
    public static final int LISTENER_AFTER_HITTED = LISTENER_BEFORE_GET + 1;
    public static final int LISTENER_BEFORE_ADD = LISTENER_AFTER_HITTED + 1;
    public static final int LISTENER_AFTER_ADD = LISTENER_BEFORE_ADD + 1;
    public static final int LISTENER_BEFORE_UPDATE = LISTENER_AFTER_ADD + 1;
    public static final int LISTENER_AFTER_UPDATE = LISTENER_BEFORE_UPDATE + 1;
    public static final int LISTENER_BEFORE_EVICT = LISTENER_AFTER_UPDATE + 1;
    public static final int LISTENER_AFTER_EVICT = LISTENER_BEFORE_EVICT + 1;
    public static final int LISTENER_BEFORE_CLEAR = LISTENER_AFTER_EVICT + 1;
    public static final int LISTENER_AFTER_CLEAR = LISTENER_BEFORE_CLEAR + 1;
    public static final int LISTENER_BEFORE_DESTROY = LISTENER_AFTER_CLEAR + 1;
    public static final int LISTENER_AFTER_DESTROY = LISTENER_BEFORE_DESTROY + 1;
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
     * �Ƿ�������ʱ���
     */
    protected static final boolean DEFAULT_ENABLE_TIMEOUT = true;

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
     * �Ƿ�������ʱ���
     */
    protected boolean enableTimeout;
    /**
     * ���ڼ��kv�Ƿ���ڵ������Future
     */
    protected ScheduledFuture timeFuture;
    /**
     * ���kv�Ƿ���ڵ�����
     */
    protected int expireCheckInterval;

    /**
     * ��ǰ Cache ������ ���е� cacheListener
     */
    private List<CacheListener<K, V>> cacheListeners;

    /**
     * ά��cacheͬ����lock
     */
    protected final Object cacheLock = new Object();

    public MCache(int capacity, boolean enableTimeout, int state, CacheEntryFactory cacheEntryFactory) {
        assert0(capacity > 0, "'capacity' must gte 0 !");
        assert0(cacheEntryFactory != null, "'cacheEntryFactory' can't be null !");

        this.capacity = capacity;
        this.enableTimeout = enableTimeout;
        this.hitted = new AtomicLong(0);
        this.visited = new AtomicLong(0);

        this.cacheEntryFactory = cacheEntryFactory;
        // update state, first authority all, the scale the state
        this.state = STATE_ALL;
        state(state);

        setExpireCheckInterval(DEFAULT_EXPIRE_CHECK_INTERVAL);
        cacheListeners = new ArrayList<>();
    }

    public MCache(int capacity, CacheEntryFactory cacheEntryFactory) {
        this(capacity, DEFAULT_ENABLE_TIMEOUT, STATE_ALL, cacheEntryFactory);
    }

    public MCache(int capacity, boolean enableTimeout) {
        this(capacity, DEFAULT_CACHE_ENTRY_FACTORY);
    }

    public MCache(int capacity) {
        this(capacity, DEFAULT_CACHE_ENTRY_FACTORY);
    }

    @Override
    public void addCacheListener(CacheListener<K, V> cacheListener) {
        Tools.assert0(cacheListener != null, "'cacheListener' can't be null !");

        cacheListeners.add(cacheListener);
    }

    @Override
    public boolean removeCacheListener(CacheListener<K, V> cacheListener) {
        if (cacheListener == null) {
            return false;
        }

        boolean hasRemoved = false;
        Iterator<CacheListener<K, V>> ite = cacheListeners.iterator();
        while (ite.hasNext()) {
            CacheListener<K, V> listener = ite.next();
            if (cacheListener.equals(listener)) {
                ite.remove();
                hasRemoved = true;
            }
        }
        return hasRemoved;
    }

    @Override
    public V get(K key) {
        if (!readable()) {
            throw new RuntimeException("currentStartIdx cache is not readable !");
        }

        fireListeners(LISTENER_BEFORE_GET, cacheEntryFactory.createContext(this, null));
        visited.incrementAndGet();
        CacheEntry<K, V> entry = getEntry0(key);
        if (entry == null) {
            return null;
        }

        hitted.incrementAndGet();
        V result = getEntry0(entry);
        fireListeners(LISTENER_AFTER_HITTED, cacheEntryFactory.createContext(this, entry));
        return result;
    }

    @Override
    public CacheEntryFacade<K, V> getEntry(K key) {
        if (!readable()) {
            throw new RuntimeException("currentStartIdx cache is not readable !");
        }

        fireListeners(LISTENER_BEFORE_GET, cacheEntryFactory.createContext(this, null));
        CacheEntry<K, V> entry = getEntry0(key);
        if (entry == null) {
            return null;
        }

        getEntry0(entry);
        fireListeners(LISTENER_AFTER_HITTED, cacheEntryFactory.createContext(this, entry));
        return cacheEntryFactory.createFacade(entry);
    }

    @Override
    public List<K> keys() {
        if (!readable()) {
            throw new RuntimeException("currentStartIdx cache is not readable !");
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
            throw new RuntimeException("currentStartIdx cache is not readable !");
        }

        return cache.size();
    }

    @Override
    public int capacity() {
        if (!readable()) {
            throw new RuntimeException("currentStartIdx cache is not readable !");
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
        assert0(expire >= CacheEntry.LONG_LIVE, "'expire' must gte " + CacheEntry.LONG_LIVE);
        if (!writeable()) {
            throw new RuntimeException("currentStartIdx cache is not writeable !");
        }

        CacheEntry<K, V> entry = getEntry0(key);
        if (entry != null) {
            fireListeners(LISTENER_BEFORE_UPDATE, cacheEntryFactory.createContext(this, null));
            boolean result = updateEntry0(entry, value, expire);
            fireListeners(LISTENER_AFTER_UPDATE, cacheEntryFactory.createContext(this, entry));
            return result;
        }

        fireListeners(LISTENER_BEFORE_ADD, cacheEntryFactory.createContext(this, null));
        entry = cacheEntryFactory.create(key, value, expire);
        boolean result = false;
        synchronized (cacheLock) {
            cache.put(key, entry);
            result = putAfterGetEntry(key, entry);
        }
        fireListeners(LISTENER_AFTER_ADD, cacheEntryFactory.createContext(this, entry));
        return result;
    }

    @Override
    public boolean update(K key, V value) {
        return update(key, value, CacheEntry.NOT_UPDATE_TTL);
    }

    @Override
    public boolean update(K key, V value, long expire) {
        assert0(expire >= CacheEntry.NOT_UPDATE_TTL, "'expire' must gte " + CacheEntry.LONG_LIVE);
        if (!writeable()) {
            throw new RuntimeException("currentStartIdx cache is not writeable !");
        }

        fireListeners(LISTENER_BEFORE_UPDATE, cacheEntryFactory.createContext(this, null));
        CacheEntry<K, V> entry = getEntry0(key);
        if (entry == null) {
            return false;
        }
        boolean result = updateEntry0(entry, value, expire);
        fireListeners(LISTENER_AFTER_UPDATE, cacheEntryFactory.createContext(this, entry));
        return result;
    }

    @Override
    public boolean evict(K key) {
        if (!writeable()) {
            throw new RuntimeException("currentStartIdx cache is not writeable !");
        }

        fireListeners(LISTENER_BEFORE_EVICT, cacheEntryFactory.createContext(this, null));
        CacheEntry<K, V> entry = getEntry0(key);
        if (entry == null) {
            return false;
        }

        entry.evictedAt(new Date());
        boolean result = false;
        synchronized (cacheLock) {
            cache.remove(key);
            result = evictAfterGetEntry(key, entry);
        }
        fireListeners(LISTENER_AFTER_EVICT, cacheEntryFactory.createContext(this, entry));
        return result;
    }

    @Override
    public boolean evict(Collection<K> keys) {
        if (!writeable()) {
            throw new RuntimeException("currentStartIdx cache is not writeable !");
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
        assert0(state <= STATE_ALL, "not a valid state !");
        if (destroyed) {
            throw new RuntimeException("currentStartIdx cache is not destroyed !");
        }
        if (!writeable()) {
            throw new RuntimeException("currentStartIdx cache is not writeable !");
        }

        this.state = state;
        return afterStateUpdated(state, true);
    }

    /**
     * �жϵ�ǰCache�Ƿ�ɶ�
     *
     * @return boolean true if currentStartIdx cache readable
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
     * @return boolean true if currentStartIdx cache writeable
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
        if (enableTimeout) {
            if (timeFuture != null) {
                timeFuture.cancel(false);
            }
            timeFuture = Tools.scheduleWithFixedDelay(new ExpireTimerTask(), checkInterval, checkInterval, TimeUnit.MILLISECONDS);
        }
    }

    @Override
    public int clear() {
        int sz = 0;
        fireListeners(LISTENER_BEFORE_CLEAR, cacheEntryFactory.createContext(this, null));
        int cleared = 0;

        synchronized (cacheLock) {
            sz = cache.size();
            cache.clear();
            cleared = afterClear(sz);
        }
        fireListeners(LISTENER_AFTER_CLEAR, cacheEntryFactory.createContext(this, null));
        return cleared;
    }

    @Override
    public boolean destroy() {
        fireListeners(LISTENER_BEFORE_DESTROY, cacheEntryFactory.createContext(this, null));

        destroyed = true;
        this.state = STATE_NONE;
        if (enableTimeout && timeFuture != null) {
            timeFuture.cancel(false);
        }

        boolean result = afterDestroyed(true);
        fireListeners(LISTENER_AFTER_DESTROY, cacheEntryFactory.createContext(this, null));
        return result;
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
     * @return com.hx.common.interf.cache.CacheEntry<K,V>
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
     * @return java.util.List<com.hx.common.interf.cache.CacheEntry<K,V>>
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
     * ��������, ����listener
     *
     * @param type    type
     * @param context context
     * @return void
     * @author Jerry.X.He
     * @date 6/8/2017 8:12 PM
     * @since 1.0
     */
    private void fireListeners(int type, CacheContext<K, V> context) {
        if(LISTENER_BEFORE_GET == type) {
            for(CacheListener<K, V> listener : cacheListeners) {
                listener.beforeGet(context);
            }
        } else if(LISTENER_AFTER_HITTED == type) {
            for(CacheListener<K, V> listener : cacheListeners) {
                listener.afterHitted(context);
            }
        } else if(LISTENER_BEFORE_ADD == type) {
            for(CacheListener<K, V> listener : cacheListeners) {
                listener.beforeAdd(context);
            }
        } else if(LISTENER_AFTER_ADD == type) {
            for(CacheListener<K, V> listener : cacheListeners) {
                listener.afterAdd(context);
            }
        } else if(LISTENER_BEFORE_UPDATE == type) {
            for(CacheListener<K, V> listener : cacheListeners) {
                listener.beforeUpdate(context);
            }
        } else if(LISTENER_AFTER_UPDATE == type) {
            for(CacheListener<K, V> listener : cacheListeners) {
                listener.afterUpdate(context);
            }
        } else if(LISTENER_BEFORE_EVICT == type) {
            for(CacheListener<K, V> listener : cacheListeners) {
                listener.beforeEvict(context);
            }
        } else if(LISTENER_AFTER_EVICT == type) {
            for(CacheListener<K, V> listener : cacheListeners) {
                listener.afterEvict(context);
            }
        } else if(LISTENER_BEFORE_CLEAR == type) {
            for(CacheListener<K, V> listener : cacheListeners) {
                listener.beforeClear(context);
            }
        } else if(LISTENER_AFTER_CLEAR == type) {
            for(CacheListener<K, V> listener : cacheListeners) {
                listener.afterClear(context);
            }
        } else if(LISTENER_BEFORE_DESTROY == type) {
            for(CacheListener<K, V> listener : cacheListeners) {
                listener.beforeDestroy(context);
            }
        } else if(LISTENER_AFTER_DESTROY == type) {
            for(CacheListener<K, V> listener : cacheListeners) {
                listener.afterDestroy(context);
            }
        } else {
            Tools.assert0("unknown type !");
        }
    }

    /**
     * ���ڼ��kv�Ƿ���ڵ�task
     *
     * @author Jerry.X.He <970655147@qq.com>
     * @version 1.0
     * @date 4/13/2017 5:37 PM
     */
    private class ExpireTimerTask implements Runnable {
        @Override
        public void run() {
            List<CacheEntry<K, V>> entries = getAllEntries();
            List<K> needToEvict = new ArrayList<>();
            for (CacheEntry<K, V> entry : entries) {
                if (entry.ttl() != CacheEntry.LONG_LIVE) {
                    Date start = entry.createdAt();
                    if (!fixedTtl) {
                        if ((entry.lastAccessed() != null) && (entry.lastAccessed().compareTo(start) > 0)) {
                            start = entry.lastAccessed();
                        }
                        if ((entry.lastUpdated() != null) && (entry.lastUpdated().compareTo(start) > 0)) {
                            start = entry.lastUpdated();
                        }
                    }

                    if (System.currentTimeMillis() > (start.getTime() + entry.ttl())) {
                        needToEvict.add(entry.key());
                    }
                }
            }

            if (!needToEvict.isEmpty()) {
                evict(needToEvict);
            }
        }
    }

}
