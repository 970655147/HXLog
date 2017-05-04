package com.hx.log.alogrithm.huffman.interf;

/**
 * �ɱȽ�, �����ǿɹ鲢��
 * Լ���ĸ��������ݽṹ��Ҫʵ�ֵ�compareTo, merge�ӿ�
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 5/4/2017 9:58 PM
 */
public interface ComparableAndMergeable<T> {

    /**
     * ��ǰ���� �͸����Ķ������, >0 ��ʾ��ǰ���� > �����Ķ���, =0 ��ʾ��ǰ���� = �����Ķ���, <0 ��ʾ��ǰ���� < �����Ķ���
     *
     * @param other ��Ҫ�ȽϵĶ���
     * @return >0 represent currentObject > other, =0 represent currentObj = other, or else currentObj < other
     * @author Jerry.X.He
     * @date 5/4/2017 9:59 PM
     * @since 1.0
     */
    int compareTo(T other);

    /**
     * �鲢��ǰ���� ��other
     *
     * @param other ��Ҫ�鲢�Ķ���
     * @return the ComparableAndMergeable after merged
     * @author Jerry.X.He
     * @date 5/4/2017 10:00 PM
     * @since 1.0
     */
    T merge(T other);

}
