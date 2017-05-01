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

    // ----------------- ȥ�������Ĳ��ַ��� -----------------------

    /**
     * �жϸ������ַ����Ƿ�Ϊ��
     *
     * @param str �������ַ���
     * @return
     */
    public static boolean isEmpty0(String str) {
        return (str == null) || (str.trim().length() == 0);
    }

    /**
     * ȷ��booΪtrue, ���� �׳��쳣
     *
     * @param boo �����ı��ʽ
     * @param msg ������ʽΪfalse, ��Ҫ�׳����쳣���ӵĶ���Ϣ
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
     * ��������Ԫ�ؼ�����ӵ�Ŀ��List��, ������
     *
     * @param eles ������Ԫ�ؼ���
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
     * ��������Ԫ�ؼ�����ӵ�Ŀ��Set��, ������
     *
     * @param eles ������Ԫ�ؼ���
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
     * ��������Ԫ�ؼ�����ӵ�Ŀ��Map��, ������
     *
     * @param eles ������Ԫ�ؼ���
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
