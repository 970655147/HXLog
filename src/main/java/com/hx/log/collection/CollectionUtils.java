/**
 * file name : CollectionUtils.java
 * created at : 22:36:23 2016-12-30
 * created by 970655147
 */

package com.hx.log.collection;

import java.util.*;

import com.hx.common.math.GeometryUtils;
import com.hx.log.util.Tools;

import static com.hx.log.util.Tools.assert0;

/**
 * 集合处理相关的工具
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 5/4/2017 11:18 PM
 */
public final class CollectionUtils {

    // disable constructor
    private CollectionUtils() {
        assert0("can't instantiate !");
    }

    /**
     * 判断给定的集合是否为空
     *
     * @param arr 给定的集合
     * @return boolean
     * @author Jerry.X.He
     * @date 5/4/2017 9:19 PM
     * @since 1.0
     */
    public static <T> boolean isEmpty(Collection<T> arr) {
        return (arr == null) || (arr.size() == 0) || (arr.isEmpty());
    }

    public static <K, V> boolean isEmpty(Map<K, V> map) {
        return (map == null) || (map.size() == 0) || (map.isEmpty());
    }

    // add at 2016.06.02
    public static <T> boolean isEmpty(T[] arr) {
        return (arr == null) || (arr.length == 0);
    }

    public static boolean isEmpty(int[] arr) {
        return (arr == null) || (arr.length == 0);
    }

    public static boolean isEmpty(long[] arr) {
        return (arr == null) || (arr.length == 0);
    }

    public static boolean isEmpty(boolean[] arr) {
        return (arr == null) || (arr.length == 0);
    }

    public static boolean isEmpty(double[] arr) {
        return (arr == null) || (arr.length == 0);
    }


    // ------------ asList / Set / Map ------- 2016.04.24 -------------

    /**
     * 创建对应的集合, 将给定的元素添加到目标集合中
     *
     * @param eles 给定的元素数组
     * @return boolean
     * @author Jerry.X.He
     * @date 5/4/2017 9:17 PM
     * @since 1.0
     */
    public static <T> List<T> asList(T... eles) {
        return new ArrayList0<>(eles);
    }

    public static <T> List<T> asLinkedList(T... eles) {
        return new LinkedList0<>(eles);
    }

    public static <T> List<T> asList(T[]... eles) {
        List<T> res = new ArrayList0<T>();
        for (T[] ele : eles) {
            asList(res, ele);
        }
        return res;
    }

    public static <T> List<T> asLinkedList(T[]... eles) {
        List<T> res = new LinkedList0<T>();
        for (T[] ele : eles) {
            asList(res, ele);
        }
        return res;
    }

    public static <T> List<T> asList(List<T> ls, T... eles) {
        assert0(ls != null, "'ls' can't be null ");
        if (eles == null) return ls;

        for (T ele : eles) {
            ls.add(ele);
        }
        return ls;
    }

    public static <T> Set<T> asSet(T... eles) {
        return new HashSet0<>(eles);
    }

    public static <T> Set<T> asLinkedSet(T... eles) {
        return new LinkedHashSet0<>(eles);
    }

    public static <T> Set<T> asSortedSet(T... eles) {
        return new TreeSet0<>(eles);
    }

    public static <T> Set<T> asSet(T[]... eles) {
        Set<T> res = new HashSet0<T>();
        for (T[] ele : eles) {
            asSet(res, ele);
        }
        return res;
    }

    public static <T> Set<T> asLinkedSet(T[]... eles) {
        Set<T> res = new LinkedHashSet0<T>();
        for (T[] ele : eles) {
            asSet(res, ele);
        }
        return res;
    }

    public static <T> Set<T> asSortedSet(T[]... eles) {
        Set<T> res = new TreeSet0<T>();
        for (T[] ele : eles) {
            asSet(res, ele);
        }
        return res;
    }

    public static <T> Set<T> asSet(Set<T> set, T... eles) {
        assert0(set != null, "'set' can't be null ");
        if (eles == null) return set;

        for (T ele : eles) {
            set.add(ele);
        }
        return set;
    }

    public static <K, V> Map<K, V> asMap(K key, V val) {
        return new HashMap0<>(key, val);
    }

    public static <K, V> Map<K, V> asLinkedMap(K key, V val) {
        return new LinkedHashMap0<>(key, val);
    }

    public static <K, V> Map<K, V> asSortedMap(K key, V val) {
        return new TreeMap0<>(key, val);
    }

    public static <K, V> Map<K, V> asMap(K[] keys, V... vals) {
        return new HashMap0<>(keys, vals);
    }

    public static <K, V> Map<K, V> asLinkedMap(K[] keys, V... vals) {
        return new LinkedHashMap0<>(keys, vals);
    }

    public static <K, V> Map<K, V> asSortedMap(K[] keys, V... vals) {
        return new TreeMap0<>(keys, vals);
    }

    public static <K, V> Map<K, V> asMap(Map<K, V> map, K[] keys, V... vals) {
        assert0(map != null, "'map' can't be null ");
        if ((keys == null) || (vals == null)) return map;
        assert0(keys.length == vals.length, "keys's length must 'eq' vals's length !");

        for (int i = 0; i < keys.length; i++) {
            map.put(keys[i], vals[i]);
        }
        return map;
    }

    // add at 2016.08.11
    //

    /**
     * 判断给定的数组中是否包含给定的元素[int]
     *
     * @param arr 给定的数组
     * @param ele 需要检测的元素
     * @return boolean
     * @author Jerry.X.He
     * @date 5/4/2017 9:17 PM
     * @since 1.0
     */
    public static boolean contains(boolean[] arr, boolean ele) {
        if (arr == null) {
            return false;
        }

        for (boolean e : arr) {
            if (e == ele) {
                return true;
            }
        }
        return false;
    }

    public static boolean contains(char[] arr, char ele) {
        if (arr == null) {
            return false;
        }

        for (char e : arr) {
            if (e == ele) {
                return true;
            }
        }
        return false;
    }

    public static boolean contains(int[] arr, int ele) {
        if (arr == null) {
            return false;
        }

        for (int e : arr) {
            if (e == ele) {
                return true;
            }
        }
        return false;
    }

    public static boolean contains(long[] arr, long ele) {
        if (arr == null) {
            return false;
        }

        for (long e : arr) {
            if (e == ele) {
                return true;
            }
        }
        return false;
    }

    public static boolean contains(float[] arr, float ele) {
        if (arr == null) {
            return false;
        }

        for (float e : arr) {
            if (GeometryUtils.equals(e, ele)) {
                return true;
            }
        }
        return false;
    }

    public static boolean contains(double[] arr, double ele) {
        if (arr == null) {
            return false;
        }

        for (double e : arr) {
            if (GeometryUtils.equals(e, ele)) {
                return true;
            }
        }
        return false;
    }

    public static <T> boolean contains(T[] arr, T ele) {
        if (arr == null) {
            return false;
        }

        for (T e : arr) {
            if (Objects.equals(e, ele)) {
                return true;
            }
        }
        return false;
    }

    public static <E> boolean contains(Collection<E> arr, E ele) {
        if (arr == null) {
            return false;
        }

        for (E e : arr) {
            if (Objects.equals(e, ele)) {
                return true;
            }
        }
        return false;
    }

    public static <K, V> boolean valueContains(Map<K, V> arr, V ele) {
        if (arr == null) {
            return false;
        }

        for (Map.Entry<K, V> entry : arr.entrySet()) {
            if (Objects.equals(entry.getValue(), ele)) {
                return true;
            }
        }
        return false;
    }

    /**
     * 判断给定的数组中是否所有的元素 和目标元素相同
     *
     * @param arr 给定的数组
     * @param ele 需要检测的元素
     * @return boolean
     * @author Jerry.X.He
     * @date 5/4/2017 9:17 PM
     * @since 1.0
     */
    public static boolean all(boolean[] arr, boolean ele) {
        if (arr == null) {
            return false;
        }

        for (boolean e : arr) {
            if (e != ele) {
                return false;
            }
        }
        return true;
    }

    public static boolean all(char[] arr, char ele) {
        if (arr == null) {
            return false;
        }

        for (char e : arr) {
            if (e != ele) {
                return false;
            }
        }
        return true;
    }

    public static boolean all(int[] arr, int ele) {
        if (arr == null) {
            return false;
        }

        for (int e : arr) {
            if (e != ele) {
                return false;
            }
        }
        return true;
    }

    public static boolean all(long[] arr, long ele) {
        if (arr == null) {
            return false;
        }

        for (long e : arr) {
            if (e != ele) {
                return false;
            }
        }
        return true;
    }

    public static boolean all(float[] arr, float ele) {
        if (arr == null) {
            return false;
        }

        for (float e : arr) {
            if (!GeometryUtils.equals(e, ele)) {
                return false;
            }
        }
        return true;
    }

    public static boolean all(double[] arr, double ele) {
        if (arr == null) {
            return false;
        }

        for (double e : arr) {
            if (!GeometryUtils.equals(e, ele)) {
                return false;
            }
        }
        return true;
    }

    public static <T> boolean all(T[] arr, T ele) {
        if (arr == null) {
            return false;
        }

        for (T e : arr) {
            if (!Objects.equals(e, ele)) {
                return false;
            }
        }
        return false;
    }

    public static <E> boolean all(Collection<E> arr, E ele) {
        if (arr == null) {
            return false;
        }

        for (E e : arr) {
            if (!Objects.equals(e, ele)) {
                return false;
            }
        }
        return false;
    }

    public static <K, V> boolean valueAll(Map<K, V> arr, V ele) {
        if (arr == null) {
            return false;
        }

        for (Map.Entry<K, V> entry : arr.entrySet()) {
            if (!Objects.equals(entry.getValue(), ele)) {
                return false;
            }
        }
        return true;
    }

    /**
     * 判断给定的集合是否存在空元素
     *
     * @param coll 给定的集合
     * @return boolean
     * @author Jerry.X.He
     * @date 5/4/2017 9:21 PM
     * @since 1.0
     */
    public static <T> boolean isAnyNull(T[] coll) {
        return contains(coll, null);
    }

    public static <E> boolean isAnyNull(Collection<E> coll) {
        return contains(coll, null);
    }

    public static <K, V> boolean isAnyValueNull(Map<K, V> map) {
        return valueContains(map, null);
    }

    /**
     * 是否集合中所有元素均为空
     *
     * @param coll 给定的集合
     * @return boolean
     * @author Jerry.X.He
     * @date 5/4/2017 9:23 PM
     * @since 1.0
     */
    public static <E> boolean isAllNull(Collection<E> coll) {
        return all(coll, null);
    }

    public static <K, V> boolean isValueNull(Map<K, V> map) {
        return valueAll(map, null);
    }

    public static <T> boolean isAllNull(T[] coll) {
        return all(coll, null);
    }

    /**
     * 辅助数据结构
     */
    private static class ArrayList0<E> extends ArrayList<E> {
        public ArrayList0(E... array) {
            if (array == null) return;
            for (E ele : array) {
                add(ele);
            }
        }
    }

    private static class LinkedList0<E> extends LinkedList<E> {
        public LinkedList0(E... array) {
            if (array == null) return;
            for (E ele : array) {
                add(ele);
            }
        }
    }

    private static class HashSet0<E> extends HashSet<E> {
        public HashSet0(E... array) {
            if (array == null) return;
            for (E ele : array) {
                add(ele);
            }
        }
    }

    private static class LinkedHashSet0<E> extends LinkedHashSet<E> {
        public LinkedHashSet0(E... array) {
            if (array == null) return;
            for (E ele : array) {
                add(ele);
            }
        }
    }

    private static class TreeSet0<E> extends TreeSet<E> {
        public TreeSet0(E... array) {
            if (array == null) return;
            for (E ele : array) {
                add(ele);
            }
        }
    }

    private static class HashMap0<K, V> extends HashMap<K, V> {
        public HashMap0(K key, V val) {
            if ((key == null)) return;
            put(key, val);
        }

        public HashMap0(K[] keys, V... vals) {
            if ((keys == null) || (vals == null)) return;
            assert0(keys.length == vals.length, "keys's length must 'eq' vals's length !");
            for (int i = 0; i < keys.length; i++) {
                put(keys[i], vals[i]);
            }
        }
    }

    private static class LinkedHashMap0<K, V> extends LinkedHashMap<K, V> {
        public LinkedHashMap0(K key, V val) {
            if ((key == null)) return;
            put(key, val);
        }

        public LinkedHashMap0(K[] keys, V... vals) {
            if ((keys == null) || (vals == null)) return;
            assert0(keys.length == vals.length, "keys's length must 'eq' vals's length !");
            for (int i = 0; i < keys.length; i++) {
                put(keys[i], vals[i]);
            }
        }
    }

    private static class TreeMap0<K, V> extends TreeMap<K, V> {
        public TreeMap0(K key, V val) {
            if ((key == null)) return;
            put(key, val);
        }

        public TreeMap0(K[] keys, V... vals) {
            if ((keys == null) || (vals == null)) return;
            assert0(keys.length == vals.length, "keys's length must 'eq' vals's length !");
            for (int i = 0; i < keys.length; i++) {
                put(keys[i], vals[i]);
            }
        }
    }

}
