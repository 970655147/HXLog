package com.hx.log.alogrithm.hash;

import com.hx.log.alogrithm.hash.interf.HashFunc;

/**
 * һ���򵥵�HashFunc��ʵ��, * ����, Ȼ��ȡģ
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 4/12/2017 10:35 PM
 */
public class SimpleHashFunc<T> implements HashFunc<T> {
    /**
     * hash ȡģ����ֵ
     */
    private int cap;
    /**
     * ����hash������
     */
    private int seed;
    /**
     * cap �Ƿ�Ϊ "����"
     */
    private boolean isInteger;

    /**
     * ��ʼ��
     *
     * @param cap  hash ȡģ����ֵ
     * @param seed ����hash������
     * @since 1.0
     */
    public SimpleHashFunc(int cap, int seed) {
        this.cap = cap;
        isInteger = isInteger(cap);
        this.seed = seed;
    }

    /**
     * ���������val��hash, �򵥵��ۼ�hash����
     *
     * @param value �����Ķ���
     * @return int the hash current HashFunc caclulated
     * @author Jerry.X.He
     * @date 5/4/2017 9:47 PM
     * @since 1.0
     */
    @Override
    public int hash(T value) {
        if (value == null) {
            return 0;
        }

        int result = 0;
        String valStr = String.valueOf(value);
        int len = valStr.length();
        for (int i = 0; i < len; i++) {
            result = seed * result + valStr.charAt(i);
        }

        if (isInteger) {
            return result & (cap - 1);
        }

        return result % cap;
    }

    // ----------------- �������� -----------------------

    /**
     * �жϸ�����num�Ƿ�Ϊ'����������'
     *
     * @param num ����������
     * @return boolean
     * @author Jerry.X.He
     * @date 5/4/2017 9:48 PM
     * @since 1.0
     */
    private static boolean isInteger(int num) {
        return ((num & (num - 1)) == 0);
//			return (Integer.bitCount(cap) == 1);
    }

}