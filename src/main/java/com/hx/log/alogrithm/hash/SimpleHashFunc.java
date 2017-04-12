package com.hx.log.alogrithm.hash;

import com.hx.log.interf.HashFunc;

/**
 * һ���򵥵�HashFunc��ʵ��, * ����, Ȼ��ȡģ
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 4/12/2017 10:35 PM
 */
// HashFunc�ļ�ʵ��
public class SimpleHashFunc implements HashFunc {
    // ����, hash����
    private int cap;
    private int seed;
    private boolean isInteger;

    // ��ʼ��
    public SimpleHashFunc(int cap, int seed) {
        this.cap = cap;
        isInteger = isInteger(cap);
        this.seed = seed;
    }

    // ���������val��hash
    // �򵥵��ۼ�hash����
    public int hash(Object value) {
        if(value == null) {
            return 0;
        }

        int result =0;
        String valStr = String.valueOf(value);
        int len = valStr.length();
        for (int i =0; i < len; i++) {
            result = seed * result + valStr.charAt(i);
        }

        if(isInteger) {
            return result & (cap -1);
        }

        return result % cap;
    }

    // �жϸ�����num�Ƿ�Ϊ'����������'
    private static boolean isInteger(int num) {
        return ((num & (num-1)) == 0);
//			return (Integer.bitCount(cap) == 1);
    }

}