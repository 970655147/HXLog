package com.hx.log.util;

import java.util.*;

/**
 * InnerTools
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 5/1/2017 12:44 PM
 */
public final class InnerTools {

    // disable constructor
    private InnerTools() {
        assert0(false, "can't instantiate !");
    }

    // ----------------- 去掉依赖的部分方法 -----------------------

    /**
     * 判断给定的字符串是否为空
     *
     * @param str 给定的字符串
     * @return
     */
    public static boolean isEmpty0(String str) {
        return (str == null) || (str.trim().length() == 0);
    }

    /**
     * 确保boo为true, 否则 抛出异常
     *
     * @param boo 给定的表达式
     * @param msg 如果表达式为false, 需要抛出的异常附加的额信息
     * @return void
     * @author Jerry.X.He
     * @date 5/1/2017 11:31 AM
     * @since 1.0
     */
    public static void assert0(boolean boo, String msg) {
        if (msg == null) {
            Log.err("'msg' can't be null ");
            return;
        }
        if (!boo) {
            throw new RuntimeException("assert0Exception : " + msg);
        }
    }

    /**
     * 将给定的元素集合添加到目标List中, 并返回
     *
     * @param eles 给定的元素集合
     * @return java.util.Set<T>
     * @author Jerry.X.He
     * @date 5/1/2017 12:53 AM
     * @since 1.0
     */
    public static <T> List<T> asList(T... eles) {
        List<T> result = new ArrayList<>();
        if (eles == null) {
            return result;
        }

        for (T ele : eles) {
            result.add(ele);
        }
        return result;
    }

    /**
     * 将给定的元素集合添加到目标Set中, 并返回
     *
     * @param eles 给定的元素集合
     * @return java.util.Set<T>
     * @author Jerry.X.He
     * @date 5/1/2017 12:53 AM
     * @since 1.0
     */
    public static <T> Set<T> asSet(T... eles) {
        Set<T> result = new LinkedHashSet<>();
        if (eles == null) {
            return result;
        }

        for (T ele : eles) {
            result.add(ele);
        }
        return result;
    }

    /**
     * 将给定的元素集合添加到目标Map中, 并返回
     *
     * @param eles 给定的元素集合
     * @return java.util.Set<T>
     * @author Jerry.X.He
     * @date 5/1/2017 12:53 AM
     * @since 1.0
     */
    public static <K, V> Map<K, V> asMap(K[] keys, V... eles) {
        Map<K, V> result = new LinkedHashMap<>();
        if (eles == null) {
            return result;
        }

        for (int i = 0, len = Math.min(keys.length, eles.length); i < len; i++) {
            result.put(keys[i], eles[i]);
        }
        return result;
    }

}
