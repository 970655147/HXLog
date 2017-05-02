package com.hx.log.cache.interf;

import java.util.Collection;
import java.util.List;

/**
 * ����ӿ�
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 4/13/2017 11:00 AM
 */
public interface Cache<K, V> {

    /**
     * ���ݸ�����key, �ӻ������ö�Ӧ��value
     *
     * @param key ������key
     * @return the value store in cache, or else null will be return
     * @author Jerry.X.He
     * @date 4/13/2017 11:09 AM
     * @since 1.0
     */
    V get(K key);

    /**
     * ���ݸ�����key, �ӻ������ö�Ӧ��entry
     *
     * @param key ������key
     * @return the cacheEntry store in cache, or else null will be return
     * @author Jerry.X.He
     * @date 4/13/2017 11:09 AM
     * @since 1.0
     */
    CacheEntryFacade<K, V> getEntry(K key);

    /**
     * ��ȡ���������е�key
     *
     * @return all key in cache
     * @author Jerry.X.He
     * @date 4/13/2017 11:09 AM
     * @since 1.0
     */
    List<K> keys();

    /**
     * ��ȡ��ǰ�����л����Ԫ�ظ���
     *
     * @return element size of current cache
     * @author Jerry.X.He
     * @date 4/13/2017 11:09 AM
     * @since 1.0
     */
    int size();

    /**
     * ��ȡ��ǰ�����л����Ԫ�ظ�������
     *
     * @return the max element size of current cache, -1 represents no limitation
     * @author Jerry.X.He
     * @date 4/13/2017 11:09 AM
     * @since 1.0
     */
    int capacity();

    /**
     * �������еĴ���
     *
     * @return the number of cache hitted
     * @author Jerry.X.He
     * @date 4/13/2017 11:09 AM
     * @since 1.0
     */
    long hitCount();

    /**
     * �û����ʻ���Ĵ���
     *
     * @return the number of cache visited
     * @author Jerry.X.He
     * @date 4/13/2017 11:09 AM
     * @since 1.0
     */
    long visitCount();

    /**
     * �򻺴������kv��
     *
     * @return true if put entry success, or else
     * @author Jerry.X.He
     * @date 4/13/2017 11:09 AM
     * @since 1.0
     */
    boolean put(K key, V value);

    /**
     * �򻺴������kv��, �����ù���ʱ��
     *
     * @return true if put entry success, or else
     * @author Jerry.X.He
     * @date 4/13/2017 11:09 AM
     * @since 1.0
     */
    boolean put(K key, V value, long expire);

    /**
     * ���»����е�kv��
     *
     * @return true if update entry success, or else
     * @author Jerry.X.He
     * @date 4/13/2017 11:09 AM
     * @since 1.0
     */
    boolean update(K key, V value);

    /**
     * ���»����е�kv��, �����¹���ʱ��
     *
     * @return true if update entry success, or else
     * @author Jerry.X.He
     * @date 4/13/2017 11:09 AM
     * @since 1.0
     */
    boolean update(K key, V value, long expire);

    /**
     * �������key��Ӧ����Ŀ
     *
     * @return true if evict entry success, or else
     * @author Jerry.X.He
     * @date 4/13/2017 11:09 AM
     * @since 1.0
     */
    boolean evict(K key);

    /**
     * �������keys��Ӧ����Ŀ
     *
     * @return true if evict entries success, or else
     * @author Jerry.X.He
     * @date 4/13/2017 11:09 AM
     * @since 1.0
     */
    boolean evict(Collection<K> keys);

    /**
     * ���õ�ǰ�����״̬[��д�ȵȿ���]
     *
     * @return true if config state success
     * @author Jerry.X.He
     * @date 4/13/2017 11:09 AM
     * @since 1.0
     */
    boolean state(int state);

    /**
     * �жϵ�ǰCache�Ƿ�ɶ�
     *
     * @return true if config state success
     * @author Jerry.X.He
     * @date 4/13/2017 11:09 AM
     * @since 1.0
     */
    boolean readable();

    /**
     * �жϵ�ǰCache�Ƿ��д
     *
     * @return true if config state success
     * @author Jerry.X.He
     * @date 4/13/2017 11:09 AM
     * @since 1.0
     */
    boolean writeable();

    /**
     * ����ttl�ǹ̶���ttl[�����createAt], �����������һ��access��ttl
     *
     * @param isFixed ttl�Ƿ�̶�
     * @return config the ttl type
     * @author Jerry.X.He
     * @date 4/13/2017 11:09 AM
     * @since 1.0
     */
    void fixedTtl(boolean isFixed);

    /**
     * ��ȡttl������
     *
     * @return get the ttl type
     * @author Jerry.X.He
     * @date 4/13/2017 11:09 AM
     * @since 1.0
     */
    boolean fixedTtl();

    /**
     * �������е�key
     *
     * @return the element count that removed
     * @author Jerry.X.He
     * @date 4/13/2017 11:09 AM
     * @since 1.0
     */
    int clear();

    /**
     * ���ٵ�ǰcache
     *
     * @return true if destroy success
     * @author Jerry.X.He
     * @date 4/13/2017 11:09 AM
     * @since 1.0
     */
    boolean destroy();

}
