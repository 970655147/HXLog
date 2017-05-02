package com.hx.log.alogrithm.bloom_filter.interf;

/**
 * ��¡������
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 4/12/2017 9:07 PM
 */
public interface BloomFilter {

    /**
     * ���������ַ�����ӵ�filter��
     *
     * @param str ��Ҫ��ӵ�filter���ַ���
     * @return true if success, or else false
     * @author Jerry.X.He
     * @date 4/12/2017 9:09 PM
     * @since 1.0
     */
    boolean add(String str);

    /**
     * �жϵ�ǰfilter���Ƿ����str��Ӧ���ַ���[���ܴ���һ�����ʵ����]
     *
     * @param str ��Ҫ�����ַ���
     * @return true if contains, or else false
     * @author Jerry.X.He
     * @date 4/12/2017 9:09 PM
     * @since 1.0
     */
    boolean contains(String str);

}
